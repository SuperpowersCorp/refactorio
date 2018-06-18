# refactorio

Lens based Haskell refactoring toolkit.

![refactorio](refactorio.png)

> Study after study shows that the very best designers produce structures that
> are faster, smaller, simpler, clearer, and produced with less effort. The
> differences between the great and the average approach an order of magnitude.
>
> - Fred Brooks

See [examples](examples/).

## Pro-Tip

For now:

    alias refio="stack exec refactorio --"

and run it from the project root to get an experience something like:

    refio _Module.biplate._ModuleName.end -d ../voltron/src

## WISHFUL THINKING for FUTURE OF Replace

### TARGET RIGHT NOW

    refio --haskell view "__Module.biplate._Int" --pre-mqp "+32"

## FUTURE ETC

    refio -d ../voltron/src  --haskell _Module.biplate._ModuleName

    the .end is not longer necessary because it just needs anything from which it can somehow get both a unique SrcSpanInfo and an `a` yadda

    but also replace like:

    refio -d ../voltron/src  --haskell _Module.biplate._ModuleName toUpper
    ...etc...
    ...(13 Changes)....

    refio -d ../voltron/src --haskell _Module.biplate._Int '(+5)'
    ...etc...
    ...(6 Changes)....

    refio -d ../voltron/src --json 'key "user" . biplate . _String yadda..
    ...grep output...
    ....

    same on YAML

    changing the time in a json file just by pattern match etc



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
- [ ] Handle '-' as filename for stdin->stdout
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
- [ ] Approach Factorio people about permission to use a (better version of) the logo
- [ ] I think I could eliminate the `LensOperator` and always be able to auto
      detect the operator to use by type correctly ...right?
