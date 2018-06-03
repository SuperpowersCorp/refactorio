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

## TODOs

- [ ] Replace examples with open source examples
- [ ] Sort out issue with `Control.Lens` not being available in installed executable
- [ ] Update traversals to point at `[(SrcSpan, a)]`
  - [ ] Add `-f`/`--fmap` option for modifing `a`s
  - [ ] Add `-s`/`--smap` option for modifing the `SrcSpan`'s as:
    - [ ] Text ?
    - [ ] String ?
    - [ ] Other (ByteString)?
- [ ] Seek guidance from the pros on
  - [ ] CT/lenses
  - [ ] Cool lens tricks that might be applicable
- [ ] Use mueval or otherwise lock down evaluation so that shared scripts can
      be used safely.
- [ ] Line Numbers
- [ ] Context lines
- [ ] Allow storing of lenses in `~/.refactorio`
- [ ] Brick TUI with:
  - [ ] Keep files in memory across edits
  - [ ] Preview / review / selective application
  - [ ] Undo
  - [ ] Fetch/store/share lenses via:
    - [ ] GitHub/gist?
    - [ ] anything else?
- [ ] Figure out which existing haskell function `concatStreams` can be reduced to.
- [ ] Can we cache generated lenses somehow?
