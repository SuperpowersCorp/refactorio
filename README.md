# refactorio

Lens based Haskell refactoring toolkit.

![refactorio](refactorio.png)

> Study after study shows that the very best designers produce structures that
> are faster, smaller, simpler, clearer, and produced with less effort. The
> differences between the great and the average approach an order of magnitude.
>
> - Fred Brooks

## Pro-Tip

For now:

    alias refio="stack exec refactorio --"

and run it from the project root to get an experience something like:

    refio _Module.biplate._ModuleName.end -d ../voltron/src

## Examples

Try these on your projects:

    refio _Module.biplate._ModuleName.end
    refio _Module.biplate._Int.end
    refio _Module.biplate._String.end
    refio _Module.biplate._FieldUpdate.end
    refio _Module.biplate._Frac.end
    refio '_Module.biplate._Int.filtered(odd.view target).end'
    refio '_Module.biplate._Int.filtered(even.view target).end'
    refio '_Module.biplate._Int.filtered((>10).view target).end'

## TODOs

- [X] Replace examples with open source examples
- [X] Display error messages (at least somewhat) nicely
- [ ] Sort out issue with `Control.Lens` not being available in installed executable
- [ ] Line Numbers
- [ ] Context lines
- [ ] Loading of additional modules
- [ ] Refactor CLI into `ref view`, `ref fmap` and `ref set`
- [ ] Suppress printing of filename when there are no matches (tardis?)
- [ ] `-f/fmap`
- [ ] Seek guidance from the pros on
  - [ ] CT/lenses
  - [ ] Cool lens tricks that might be applicable
- [ ] Allow storing of lenses in `~/.refactorio`
- [ ] Use mueval so that shared lenses can be used safely.
- [ ] Emacs integration
- [ ] Brick TUI with:
  - [ ] Keep files in memory across edits
  - [ ] Preview / review / selective application
  - [ ] Undo
  - [ ] Fetch/store/share lenses via:
    - [ ] GitHub/gist?
    - [ ] anything else?
- [ ] Figure out which existing haskell function `concatStreams` can be reduced to.
- [ ] Can we cache generated lenses somehow?
- [ ] Better Themes
- [ ] Better Banner Image
