# NeoNote

NeoNote is a terminal app to manage notes.
The main goal is to speed up note-taking by cutting down on all but the essential tasks. It stores your notes for you and provides filtering and searching options to organize them semi-automatically.

![Animated showcase of NeoNote](recordings/showcase_0.3.0.0.gif)

## Features

- Create/Edit/Delete Notes
- Import/Export notes (-> no lock-in to NeoNote)
- Filter notes based on metadata, full text search and creation/modification date
- Use boolean logic to combine multiple filter conditions
- Dump your notes to the console for use in other programs

## Installation

### Binary files

You can find builds for linux x86 in the [releases section](https://github.com/Simre1/neonote/releases).

### Build from source

NeoNote is built with the Haskell build tool `cabal`. You'll need it along with a recent Haskell compiler.

To build from source, run:
```bash
cabal build all
````

Then, you will find the executable inside the `dist-newstyle` folder.

## Get Started

Take a look at the NeoNote help menu to get a quick overview of the commands and to explore the various options.
Most commands can be shortened to one letter, eg. `nn c` instead of `nn create`.

### Create a note

```
nn create
```

An editor will open where you can write your note. You can also start composing your note directly in the terminal by just typing the text:

```bash
nn c "Your first note with NeoNote"
```

### Frontmatter

NeoNote stores metadata for your notes, which you can edit in the frontmatter.

```md
---
answer: 42
foo: bar
my-tag
---

here is the content of the note
```

The frontmatter is surrounded by `---` delimiters. In the frontmatter, each line stores a simple key value property.
The field `answer` has value `42`, `foo` has value `bar` and `my-tag` has no value. Although `my-tag` has no value, it is still present and essentially serves as a tag.

Field names and values may contain alphanumerical characters and the special characters "-+_?.". For field names, special characters may not be at the start or at the end. Also, "created", "c", "modified" and "m" are reserved keywords any may not be used for field names.

Field values may only contain simple data: a single number, a single word or a string enclosed by `"`. In the future, I might add more complex types (e.g. dates or lists).

### Using the note picker

To review already existing notes, you will normally use the picker UI. It can be opened with the following command:

```
nn pick
```

Within this UI, you can incrementally search through your notes. It is possible to edit with `ENTER`, delete with `CTRL-X` and print to console with `CTRL-Y`.

### Processing notes without the picker

```
nn edit
```

`edit` will directly open your editor with all matched notes.
Per default, it will only match `1` note with no filtering.
This means it will open the note which was last modified. 

You can specify a filter, a search term and the amount of notes to match. The following command
will open at most 3 notes with the word `todo`.
```
nn edit --number 3 todo
```

Similarly to the `edit` command, there exist `delete` and `view` commands to delete and view multiple notes.

This will delete up to a hundred notes with the property  `#archived`.
```
nn delete --number 100 "#archived" 
```

This will view up to 10 notes with the word linux
```
nn view --number 10 linux
```

### List notes

The `list` command essentially functions as a way to print note metadata in a machine-readable way. It can give you information about note ids, properties and modification/creation dates. 

```
nn list
```

Per default, it lists the ids of the 10 most recently modified notes.

You can change which attributes of the note are shown with `--attribute` (`-a` for short):

```
nn list --attribute id --attribute created --attribute properties
```

Now, in addition to the id, the creation date and the properties will be shown. This command can also be shortened:
```
nn l -a i -a c -a p
```


You can combine this with filtering and searching. The following
command will list up to 5 notes with the word haskell. 
```
nn list --number 5 haskell
```

## Filter notes

You can filter notes using the filter grammar.

```
nn view "programming"
```

This will show you notes which contain the word `programming`.
This can also be used with `delete`, `edit`, `list` and `pick`.

### Boolean logic for filtering

- `*` matches every note
- `~ a` matches if `a` does not match
- `a | b` matches if either `a` or `b` matches
- `a & b` matches if both `a` and `b` match
- `a b` matches if both `a` and `b` match. Currently, `a b` is the same as `a & b`.
- `#foo` matches if note contains the property `foo`, no matter its value
- `#foo=bar` matches if note contains the property `foo` with value `bar`
- `#foo>4` matches if note contains the property `foo` with a value greater than `4`. Supported comparison operators are `=`, `>`, `<`, `>=`, and `<=`.
- `foo` matches if note contains the string foo
- Use brackets (`(a)`) to group expressions
- Use quotes (`"%many +words$"`) to escape search strings and search for special characters

Here are a few examples:

```
nn view "*" # matches everything
nn view "#programming & ~html" # matches notes with the property programming without the word html
nn view "#programming | #coding" # matches notes with either the property programming or the property coding
nn view "(#programming | #coding) & ~html" # matches programming/coding notes without html
```

### Date filters

In addition to searching for properties/full text, you can also filter based on the creation and modification date of a note.

- `#created` and `#modified` literals to access the date of a note
- `YYYY-MM-DD` for day literals
- `HH-MM-SS` for time literals

```
nn view "#created = 2023-07-01" # matches all notes created on 2023-07-01
nn view "#modified = 2023-07-01" # matches all notes modified on 2023-07-01
nn view "#modified > 2023-07-01" # matches all notes modified after 2023-07-01
nn view "#modified < 2023-07-01" # matches all notes modified before 2023-07-01
nn view "#modified > 2023-05-30 & modified < 2023-07-01" # matches all notes modified in June 2023
nn view "#modified > 2023-07-01 & #programming" # matches all notes modified after 2023-07-01 with the tag programming
nn view "#modified > 10:30:00" # matches all notes modified after 10:30:00
```

You can shorten the date keywords to `#c` and `#m` respectively.

In the future, less restrictive date specifications might be possible.
For example, you could apply the filter `modified = July` or `created = around 10am`.

## Importing/Exporting

```
nn import file1 file2 file3...
```

Importing notes is the same as creating the files one by one and copy-pasting their contents into the editor.

```
nn export <note filter>
```
Exporting notes will dump all filtered notes into a single folder. Thus, it is possible to use other tools to manage your notes.

## Configuration

You have two options to specify configurations:

- `config.ini` within the xdg config directory which will be created on the first neonote execution
- environment variables (`NEONOTE_PATH`, `NEONOTE_EDITOR`, `NEONOTE_EXTENSION`)

### Default note location

Per default, neonote stores notes as `md` (Markdown) text within a `notes.db` database in a folder `neonote` in the XDG data directory (`.local/share/neonote` on Linux).  

### Possible Editors

You can choose any editor you want by specifying it in the configuration.
However, you need to make sure that the editor blocks until the editing process is finished and that it can open multiple files simultanously.

If your editor command has a `%`, it will be replaced with the files to edit separated by a ` `.
If it does not have a `%`, the files are appended at the end of your command.

Here are some example editor configurations:
- Vim: `vim` or `vim %`
- Helix: `helix` or `helix %`
- Nano: `nano` or `nano %`
- VSCode: `code --wait` or `code --wait %`
- VSCodium: `codium --wait` or `code --wait %`

If you have not set any editor, NeoNote will default to the editor set in the `EDITOR` environment variable or fall back to `vim`.
