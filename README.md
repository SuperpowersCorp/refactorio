# refactorio

Lens based Haskell refactoring toolkit.

![refactorio](refactorio.png)

> Study after study shows that the very best designers produce structures that
> are faster, smaller, simpler, clearer, and produced with less effort. The
> differences between the great and the average approach an order of magnitude.
>
> - Fred Brooks

## TODOs

- [ ] Update traversals to point at [(SrcSpan, a)]
- [ ] Seek guidance on CT/lenses from the pros
- [ ] Add `-f`/`--fmap` option for modifing `a`s
- [ ] Add `-s`/`--smap` option for modifing the SrcSpan's
  - [ ] Text ?
  - [ ] String ?
  - [ ] Other (ByteString)?
- [ ] Use mueval or otherwise lock down evaluation so that shared scripts can
      be used safely.
- [ ] Line Numbers
- [ ] Context lines
- [ ] Brick TUI with:
  - [ ] Keep files in memory across edits
  - [ ] Preview / review / selective application
  - [ ] Undo
