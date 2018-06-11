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

- [ ] Replace examples with open source examples
- [X] Display error messages (at least somewhat) nicely
- [ ] Sort out issue with `Control.Lens` not being available in installed executable
- [ ] Line Numbers
- [ ] Context lines
- [ ] Suppress printing of filename when there are no matches (tardis?)
- [ ] Update traversals to point at `[(SrcSpan, a)]`
  - [ ] Just `-f/fmap` and figure out which it is instead.
    - [ ] Add `-f`/`--fmap` option for modifing `a`s
    - [ ] Add `-s`/`--smap` option for modifing the `SrcSpan`'s as:
  - [ ] Text ?
  - [ ] String ?
  - [ ] Other (ByteString)?
- [ ] Seek guidance from the pros on
  - [ ] CT/lenses
  - [ ] Cool lens tricks that might be applicable
- [ ] Allow storing of lenses in `~/.refactorio`
- [ ] Use mueval so that shared lenses can be used safely.
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
