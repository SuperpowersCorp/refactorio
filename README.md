# refactorio

** PLEASE BE AWARE THIS IS ALPHA SOFTWARE AND NOT ONLY WILL THE API CHANGE BUT
IT WILL PROBABLY EAT YOUR CHILDREN.  YOU HAVE BEEN WARNED. **

Lens based Haskell refactoring toolkit.

![refactorio](refactorio.png)

> Study after study shows that the very best designers produce structures that
> are faster, smaller, simpler, clearer, and produced with less effort. The
> differences between the great and the average approach an order of magnitude.
>
> - Fred Brooks

## Usage

    Refactorio - Optical Refactoring Tool

    Usage: refactorio EXPR [-t|--target TARGET] [-g|--glob GLOB] [--prelude MODULE]
                      ([-a|--ask] | [-p|--preview] | [-r|--review] | [-m|--modify])
                      [--haskell|--hs] [--html] [--json] [--xml] [--yaml]
      Zen and the art of optical file maintenance.

    Available options:
      EXPR                     ByteString -> ByteString
      -t,--target TARGET       A file/directory to search/replace (default: ".")
      -g,--glob GLOB           Glob matches to include (eg '*.ini', 'f??b?r.c')
      --prelude MODULE         Use a specific Prelude module
      -a,--ask                 Ask before changing files (default)
      -p,--preview             Only show the changes that would be made
      -r,--review              Make the changes and show details of changes
      -m,--modify              Make the changes and summarize changed filenames
      --haskell,--hs           Include .hs files and make Haskell ops available
      --html                   Include .htm(l) files and make XML ops available
      --json                   Include .json files and make JSON ops available
      --xml                    Include .xml files and make XML ops available
      --yaml                   Include .y(a)ml files and make YAML ops available
      -h,--help                Show this help text

## Major Modes

In all modes Refactorio traverses one or more files executing a `ByteString
-> ByteString` function on them.  For a given file if the function does not
change the input then no output is logged for that file.  If the function does
change the file then what happens next is dependent on the mode:

### Ask Mode (-a / --ask)

This is the default mode.  In this mode Refactorio will show you the changes
that it's about to make and prompt you on a file by file basis whether you want
to accept the changes or not.  If you accept, the file will be replaced, if not
the file will be left intact (or you can 'Q'uit at any time).

### Preview Mode (-p / --preview)

In this mode Refactorio will just show all the changes that it would make, but
not touch any files.  You can think of it sort of like a `--dry-run`.

### Review Mode (-r / --review)

In this mode Refactorio will make changes to all files without asking but will
show the full set of changes as they are made.

### Modify Mode (-m / --modify)

This is basically a `--quiet` style mode that makes all changes without
confirmation and just reports which files changed with no further details.

## Examples

Here are a few examples to whet your appetite.  For more see [examples](examples/).

### JSON (via [lens-aeson](https://hackage.haskell.org/package/lens-aeson)):

"Increment the value at key 'baz' by 1962."

![JSON Example](examples/json.png)

### YAML (same operators):

"Multiply the value of the key 'baz' by 10."

![YAML Example](examples/yaml.png)

### HTML (via [xml-html-conduit-lens](https://hackage.haskell.org/package/xml-html-conduit-lens))

    HTML example coming soon.

### XML (also via [xml-html-conduit-lens](https://hackage.haskell.org/package/xml-html-conduit-lens))

C'mon, you've never needed to "find all the authors with names longer than 15
characters and then sort all of the letters in their name that are above 'm' in
place?" Pshaw.

![XML Example](examples/xml.png)

### Regex (via [lens-regex](https://hackage.haskell.org/package/lens-regex)):

Drop regex's in anywhere you like, eg. "uppercase and reverse the characters in
the value of the JSON object at this key that match this regular expression":

![Regex Example](examples/regex.png)

### Compressed Files (via [zlib-lens](https://hackage.haskell.org/package/zlib-lens)):

Reach inside eg. gzipped files and do what you gotta do:

![gzipped Example](examples/gzipped.png)

(Not sure what's up with that "trailing garbage.")

### Haskell (via [haskell-src-exts](https://hackage.haskell.org/package/haskell-src-exts) and [haskell-src-exts-prisms](https://hackage.haskell.org/package/haskell-src-exts-prisms)):

TODO: revamp after re-wiring

Try these on your projects:

    refio --haskell _Module.biplate._ModuleName.end
    refio --haskell _Module.biplate._Int.end
    refio --haskell _Module.biplate._String.end
    refio --haskell _Module.biplate._FieldUpdate.end
    refio --haskell _Module.biplate._Frac.end
    refio --haskell '_Module.biplate._Int.filtered(odd.view target).end'
    refio --haskell '_Module.biplate._Int.filtered(even.view target).end'
    refio --haskell '_Module.biplate._Int.filtered((>10).view target).end'

There are [more examples here](examples/).

## Pro-Tip

For now the easiest way to get it working is build it with `stack build` and then:

    alias refio="stack exec refactorio --"

and run it from the refactorio project root to get an experience something like:

    refio --json '& key "foo" . key "bar" . _Number *~ 15' -t ../voltron/test/fixtures

or

    refio --haskell _Module.biplate._ModuleName.end -t ../voltron/src

(where the `-t`/`--target` is a file or directory to process and can be outside
of the refactorio project root).

## TODOs

- [X] Finish custom prelude implementation
- [ ] Fully restore haskell-src-exts functionality.
- [ ] Special mode pre/post adapter fns
- [ ] Pandoc lens support for:
  - [ ] Docx
  - [ ] Markdown
  - [ ] others
- [X] Replace examples with open source examples
- [X] Display error messages (at least somewhat) nicely
- [ ] Sort out issue with `Control.Lens` not being available in installed executable
- [ ] Line Numbers
- [ ] Context lines
- [X] Loading of additional modules (via Prelude)
- [X] Refactor CLI into `ref view`, `ref fmap` and `ref set`
- [X] Suppress printing of filename when there are no matches (tardis?)
- [X] `-f/fmap`
- [ ] Eliminate unnecessary serialization round trips
  - [ ] eg when processing YAML via JSON
  - [ ] don't changes files when all that changed was formatting.
- [X] Handle '-' as filename for stdin->stdout
- [ ] Bail if modes are provided with stdin ('-') processing
- [ ] Seek guidance from the pros on
  - [ ] CT/lenses
  - [ ] Cool lens tricks that might be applicable
- [X] Allow storing of lenses in `~/.refactorio`  (replaced by custom prelude)
- [ ] Use mueval so that shared lenses can be used safely.
- [ ] Emacs integration
- [ ] Figure out which existing haskell function `concatStreams` can be reduced to.
- [ ] Can we cache generated lenses somehow?
- [ ] Allow Traversals for extraction of arbitrary info.
- [ ] Brick TUI with:
  - [ ] Keep files in memory across edits
  - [ ] Preview / review / selective application
  - [ ] Undo
  - [ ] Fetch/store/share lenses via:
    - [ ] GitHub/gist?
    - [ ] anything else?
- [ ] Better Themes
- [ ] Better Banner Image
- [ ] Approach Factorio people about permission to use a (better version of) the logo
- [X] I think I could eliminate the `LensOperator` and always be able to auto
      detect the operator to use by type correctly ...right? (replace by EXPR)
- [ ] `fileplate` to let you treat multiple files as a single unit and do
      `biplate` type stuff to them as a whole?
