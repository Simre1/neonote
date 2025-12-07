module NeoNote.Note.Syntax.Pattern where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Bifunctor (Bifunctor (..))
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text
import Data.Text qualified as T
import Data.Traversable (for)
import Data.Void (Void)
import Debug.Trace
import NeoNote.Note.Note (Properties (..), PropertyName (..), Value)
import NeoNote.Note.Syntax.Common
import Text.Earley qualified as E
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

data Pattern
  = Exact Text
  | Merge Pattern Pattern
  | LineWhitespace
  | Whitespace
  | PropName
  | PropValue
  | Number
  | RestOfLine
  | And Pattern Pattern
  | Or Pattern Pattern
  | Capture Pattern (Maybe VarName) [Rule VarName] (Maybe Replace)
  deriving (Show, Ord, Eq)

type PatternParser a = StateT (S.Set VarName) Parser a

newtype VarName = VarName Text
  deriving (Show, Ord, Eq)

data PatternValue v = Var v | ConstText Text | ConstInt Int | EmptyValue
  deriving (Show, Ord, Eq)

data Rule v = Set PropertyName (PatternValue v)
  deriving (Show, Ord, Eq)

newtype Replace = Replace [ReplaceAction]
  deriving (Show, Ord, Eq)

data ReplaceAction = PutText Text | PutVar VarName
  deriving (Show, Ord, Eq)

-- `"exact match"` is done with "
-- `a b` connects two matches, whitespace may be inbetween (same as `awb`)
-- `ab` directly connects two matches a and b with no whitespace
-- `w` for line whitespace, `W` for all whitespace
-- `p` for property field name
-- `v` for property value
-- `n` for integers
-- `r` for rest of line
-- `a & b` matches if both `a` and `b` match
-- `a | b` matches if either `a` or `b` matches. they must provide the same captured patterns
-- `( pattern )` indicates scoping
-- `( pattern : foo)` captures the matched pattern into the variable foo
-- `( pattern $ rules )` opens up a new block, the rules get applied when the pattern matches
-- `fieldname=value` is a rule, where fieldname and value may be constant or a captured pattern
-- `( pattern -> replace)` describes the replacement when neonote wants to eliminate this autoproperty
-- replacements comprise exact strings enclosed with " and variable names which are currently in scope
-- `rule1 $ rule2` rules are separated with `$`
-- `(pattern : foo $ rules -> replace)` is also allowed

-- data Pattern = Pattern
--   { regex :: Text,
--     replace :: Text
--   }
--   deriving (Eq, Ord, Show)

compilePattern :: Text -> Either Text Pattern
compilePattern = first (T.pack . P.errorBundlePretty) . P.parse (evalStateT patternP S.empty) "auto property pattern"
  where
    patternP :: PatternParser Pattern
    patternP = recursiveP
    basicP :: PatternParser Pattern
    basicP = captureP <|> exactP <|> specialCharactersP
    exactP :: PatternParser Pattern
    exactP = lexeme $ P.try $ do
      void $ P.char '"'
      txt <- P.takeWhile1P (Just "Exact pattern") (/= '"')
      void $ P.char '"'
      pure $ Exact txt
    specialCharactersP :: PatternParser Pattern
    specialCharactersP = lexeme $ P.try $ do
      c <- P.satisfy (const True)
      case c of
        'w' -> pure LineWhitespace
        'W' -> pure Whitespace
        'p' -> pure PropName
        'v' -> pure PropValue
        'n' -> pure Number
        'r' -> pure RestOfLine
        _ -> fail "no special character"
    varP :: PatternParser VarName
    varP = lexeme $ do
      PropertyName name <- fieldNameP
      pure (VarName name)

    captureP :: PatternParser Pattern
    captureP = do
      void $ lexeme $ P.char '('

      (capture, varNames) <- lift $ flip runStateT S.empty $ do
        pattern <- patternP

        varname <- optional $ do
          _ <- lexeme $ P.char ':'
          varName <- varP
          modify (S.insert varName)
          pure varName

        rules <- many $ do
          _ <- lexeme $ P.char '$'
          ruleP

        replace <- optional $ do
          _ <- lexeme $ P.string "->"
          Replace <$> replaceActionP
        pure (Capture pattern varname rules replace)

      modify (S.union varNames)

      void $ lexeme $ P.char ')'
      pure capture

    capturedVarNameP :: PatternParser VarName
    capturedVarNameP = do
      w <- wordP
      names <- get
      unless (S.member (VarName w) names) $
        fail $
          unpack w <> " is not a captured variable"
      pure $ VarName w

    ruleP :: PatternParser (Rule VarName)
    ruleP = lexeme $ do
      fieldName <- lexeme fieldNameP
      value <-
        P.choice
          [ do
              _ <- lexeme $ P.char '='
              ConstText <$> stringLiteralP
                <|> ConstInt <$> intP
                <|> Var <$> capturedVarNameP,
            pure EmptyValue
          ]
      pure (Set fieldName value)
    replaceActionP :: PatternParser [ReplaceAction]
    replaceActionP = many $ lexeme $ do
      PutText <$> stringLiteralP <|> PutVar <$> capturedVarNameP

    recursiveP :: PatternParser Pattern
    recursiveP =
      chainl1 basicP $
        lexeme $
          P.choice
            [ And <$ (void (P.string "&&") <|> void (P.char '&')),
              Or <$ (void (P.string "||") <|> void (P.char '|')),
              pure Merge
            ]

replacePatterns :: [Pattern] -> Properties -> Text -> Text
replacePatterns patterns deleted content = content

matchPatterns :: [Pattern] -> Text -> Properties
matchPatterns patterns text = Properties M.empty

parser :: [Pattern] -> Parser [Rule ()]
parser patterns = undefined
  where
    patternGrammar :: Pattern -> Parser [Rule ()]
    patternGrammar p = snd <$> go p
    go :: Pattern -> Parser (M.Map VarName Text, [Rule ()])
    go (Exact txt) = mempty <$ P.string txt
    go (Merge p1 p2) = do
      (c1, r1) <- go p1
      (c2, r2) <- go p2
      pure (c1 <> c2, r1 <> r2)
    go LineWhitespace = mempty <$ P.takeWhileP (Just "w") (\c -> c == ' ' || c == '\t')
    go Whitespace = mempty <$ P.takeWhileP (Just "w") (<= ' ')
    go PropName = mempty <$ fieldNameP
    go PropValue = mempty <$ valueP
    go Number = mempty <$ numberP
    go RestOfLine = mempty <$ P.takeWhileP (Just "w") (\c -> c /= '\n' && c /= '\r')
    go (And p1 p2) = do
      ((c1, r1), (c2, r2)) <- both (go p1) (go p2)
      pure (c1 <> c2, r1 <> r2)
    go (Or p1 p2) = P.try (go p1) <|> go p2
    go (Capture p varname rules _replace) = do
      (t, (c, r)) <- P.match (go p)
      let c' = case varname of
            Nothing -> c
            Just name -> M.insert name t c
      rules' <- for rules $ \(Set propName value) ->
        Set propName <$> case value of
          EmptyValue -> pure EmptyValue
          ConstInt i -> pure (ConstInt i)
          (ConstText t) -> pure (ConstText t)
          (Var varname) -> case M.lookup varname c' of
            Nothing -> fail "Cannot find variable name"
            Just v -> case P.parseMaybe @Void (numberP >>= \i -> i <$ P.eof) v of
              Just i -> pure (ConstInt i)
              Nothing -> pure (ConstText v)
      pure (c', r <> rules')

both ::
  (P.MonadParsec Void Text p) =>
  p a -> p b -> p (a, b)
both p1 p2 = do
  (t1, r1) <- P.lookAhead (P.match p1)
  (t2, r2) <- P.lookAhead (P.match p2)
  void $
    if T.length t1 > T.length t2
      then P.chunk t1
      else P.chunk t2
  pure (r1, r2)

-- mapMaybe (extractValue . NE.tail) $
--   Regex.captures regex text

-- extractValue :: [Text] -> Maybe Value
-- extractValue [] = Nothing
-- extractValue (x : _) =
--   parseMaybe
--     ( do
--         value <- valueP
--         _ <- P.takeRest
--         pure value
--     )
--     x
