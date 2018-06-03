# refactorio

Lens based Haskell refactoring toolkit.

![refactorio](refactorio.png)

> Study after study shows that the very best designers produce structures that
> are faster, smaller, simpler, clearer, and produced with less effort. The
> differences between the great and the average approach an order of magnitude.
>
> - Fred Brooks

## TODOs

- [ ] Sort out issue with `Control.Lens` not being available in installed executable
- [ ] Update traversals to point at [(SrcSpan, a)]
- [ ] Seek guidance from the pros on
  - [ ] CT/lenses
  - [ ] Cool lens tricks that might be applicable
- [ ] Add `-f`/`--fmap` option for modifing `a`s
- [ ] Add `-s`/`--smap` option for modifing the SrcSpan's
  - [ ] Text ?
  - [ ] String ?
  - [ ] Other (ByteString)?
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
