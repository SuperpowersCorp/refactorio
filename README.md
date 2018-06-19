# refactorio

** PLEASE BE AWARE THIS IS ALPHA SOFTWARE AND NOT ONLY WILL THE API CHANGE BUT
IT WILL PROBABLY EAT YOUR CHILDREN.  YOU HAVE BEEN WARNED.**

Lens based Haskell refactoring toolkit.

![refactorio](refactorio.png)

> Study after study shows that the very best designers produce structures that
> are faster, smaller, simpler, clearer, and produced with less effort. The
> differences between the great and the average approach an order of magnitude.
>
> - Fred Brooks

## Usage

    Refactorio - Optical Refactoring Tool

    Usage: refactorio EXPR [-t|--target TARGET] [-g|--glob GLOB] [--prelude PRELUDE]
                      ([-a|--ask] | [-p|--preview] | [-r|--review] | [-m|--modify])
                      [--haskell|--hs] [--json] [--xml] [--yaml]
      Zen and the art of optical file maintenance.

    Available options:
      EXPR                     ByteString -> ByteString
      -t,--target TARGET       A file/directory to search/replace (default: ".")
      -g,--glob GLOB           Glob matches to include (eg '*.ini', 'f??b?r.c')
      --prelude PRELUDE        Use a specific Prelude
      -a,--ask                 Ask before changing files (default)
      -p,--preview             Only show the changes that would be made
      -r,--review              Make the changes and show details of changes
      -m,--modify              Make the changes and summarize changed filenames
      --haskell,--hs           Include .hs files and make Haskell ops available
      --json                   Include .json files and make JSON ops available
      --xml                    Include .xml files and make XML ops available
      --yaml                   Include .yaml or .yml files and make YAML ops
                               available
      -h,--help                Show this help text

## Pro-Tip

For now the easiest way to get it working is build it with `stack build` and then:

    alias refio="stack exec refactorio --"

and run it from the refactorio project root to get an experience something like:

    refio --json '& key "foo" . key "bar" . _Number *~ 15' -t ../voltron/test/fixtures

or

    refio --haskell _Module.biplate._ModuleName.end -t ../voltron/src

(where the `-t`/`--target` is a file or directory to process and can be outside
of the refactorio project root).

## Examples

Here are a few examples to whet your appetite.  For more see [examples](examples/).

### JSON (via [lens-aeson](https://hackage.haskell.org/package/lens-aeson)):

"Increment the value at key 'baz' by 1962."

![JSON Example](examples/json.png)

### YAML (same operators):

"Multiply the value of the key 'baz' by 10."

![YAML Example](examples/yaml.png)

### HTML (via `Text.Xml.Lens`) TODO: taggy

    HTML example coming soon.

### XML (via same):

    XML example coming soon.

### Regex (via TODO):

    XML example coming soon.

### Haskell:

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

## TODOs

- [ ] Finish interlude implementation
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
- [ ] Handle '-' as filename for stdin->stdout
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
