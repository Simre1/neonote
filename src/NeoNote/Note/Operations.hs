module NeoNote.Note.Operations where

import Data.Bifunctor (Bifunctor (..))
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace
import NeoNote.Note.Note
import NeoNote.Note.Syntax.Pattern
import NeoNote.Note.Syntax.RawNote
import NeoNote.Time
import Optics.Core

data Update a = Update a | NoChanges | Delete deriving (Functor, Eq, Show, Ord)

instance Foldable Update where
  foldMap _ NoChanges = mempty
  foldMap _ Delete = mempty
  foldMap f (Update a) = f a

instance Traversable Update where
  traverse _ NoChanges = pure NoChanges
  traverse _ Delete = pure Delete
  traverse f (Update a) = Update <$> f a

partitionUpdates :: [(key, Update a)] -> ([(key, a)], [key])
partitionUpdates [] = ([], [])
partitionUpdates ((key, x) : xs) = case x of
  NoChanges -> partitionUpdates xs
  Delete -> second (key :) $ partitionUpdates xs
  Update a -> first ((key, a) :) $ partitionUpdates xs

updateNote :: AutoProperties -> Note -> Time -> RawNote -> Update (Note, [JunkField])
updateNote autoProperties oldNote modificationTime newRawNote =
  if hasContent newRawNote
    then
      let (junkFields, Properties frontProps, noteContent) = parseRawNote newRawNote
          deletedProps = diffProperties (oldNote ^. #info % #properties) (Properties frontProps)
          updatedContent = updateContent autoProperties deletedProps noteContent
          Properties oldAutoProps = extractAutoProperties autoProperties (oldNote ^. #content)
          Properties newAutoProps = extractAutoProperties autoProperties updatedContent
          removedAutoProps = M.difference oldAutoProps newAutoProps
          newProperties = Properties $ M.difference frontProps removedAutoProps <> newAutoProps
          updatedNoteInfo =
            (oldNote ^. #info)
              { properties = newProperties,
                modified = modificationTime
              }
          updatedNote1 = Note updatedNoteInfo updatedContent
       in --
          -- traceShow (frontProps, oldAutoProps, newAutoProps, newProperties) $
          if oldNote ^. #content == updatedContent && newProperties == oldNote ^. #info % #properties
            then NoChanges
            else Update (updatedNote1, junkFields)
    else Delete

-- -- Check if new auto properties match
-- updatedNote2 = updateNote autoProperties updatedNote1 modificationTime (noteToRaw updatedNote1)
--       if oldNote ^. #content == noteContent && properties == oldNote ^. #info % #properties
--         then NoChanges
--         else case updatedNote2 of
--           Delete -> Delete
--           NoChanges -> Update (updatedNote1, junkFields)
--           Update (note, junkFields2) -> Update (note, junkFields ++ junkFields2)
-- else Delete

updateContent :: AutoProperties -> Properties -> NoteContent -> NoteContent
updateContent (AutoProperties autoProperties) deletedProperties (NoteContent noteContent) =
  let replacePatterns = mapMaybe ((`M.lookup` autoProperties) . fst) (propertyValues deletedProperties)
   in NoteContent $ foldl' (flip replacePattern) noteContent replacePatterns

diffProperties :: Properties -> Properties -> Properties
diffProperties (Properties p1) (Properties p2) =
  Properties $
    M.differenceWith diffSet p1 p2
  where
    diffSet s1 s2 =
      let diff = S.difference s1 s2
       in if S.null (diff) then Nothing else Just diff
