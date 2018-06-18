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

    refio --json    '& key "foo" . key "bar" . _Number *~ 15' -t ../voltron/test/fixtures
    refio --haskell _Module.biplate._ModuleName.end           -t ../voltron/src

## Examples

TODO: Re-do since re-design.

Try these on your projects:

    refio --haskell _Module.biplate._ModuleName.end
    refio --haskell _Module.biplate._Int.end
    refio --haskell _Module.biplate._String.end
    refio --haskell _Module.biplate._FieldUpdate.end
    refio --haskell _Module.biplate._Frac.end
    refio --haskell '_Module.biplate._Int.filtered(odd.view target).end'
    refio --haskell '_Module.biplate._Int.filtered(even.view target).end'
    refio --haskell '_Module.biplate._Int.filtered((>10).view target).end'

See [examples](examples/).

## TODOs

- [X] Replace examples with open source examples
- [X] Display error messages (at least somewhat) nicely
- [ ] Sort out issue with `Control.Lens` not being available in installed executable
- [ ] Line Numbers
- [ ] Context lines
- [X] Loading of additional modules (via Prelude)
- [X] Refactor CLI into `ref view`, `ref fmap` and `ref set`
- [X] Suppress printing of filename when there are no matches (tardis?)
- [X] `-f/fmap`
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
