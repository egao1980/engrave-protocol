# engrave-protocol

Engraving for cl-stack: a [score-protocol](https://github.com/egao1980/score-protocol) score (or one of its views) becomes SVG pages whose element ids lead back to the score, a timemap for playback cursors, and PDF. Rendering happens in an engine behind the protocol; the first native engine is planned as a separate backend.

**Status:** EN0 (0.1.0) implemented: the API, the rules in the file headers of `src/`, the reference `null-engine`, and the Rove suite. The design is in the capability brief [cl-stack/docs/capabilities/engrave.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/engrave.md).

```lisp
(asdf:load-system "engrave-protocol")
(multiple-value-bind (pages id-map)
    (stack-engrave:render-svg (stack-engrave:make-null-engine) score
                              :view :condensed :layout '(:page-size :a4 :scale 60))
  ;; PAGES: svg-page values (number, xml-protocol SVG document, systems)
  ;; ID-MAP: FSet map, score element id → ((page . svg-id) ...)
  ...)
(stack-engrave:render-timemap engine score)   ; → timemap-entry list, unfolded
(stack-engrave:render-pdf engine score :printer printer)   ; → PDF octets
```

- **Engines** subclass `engrave-engine` and implement `engine-render-pages`. `null-engine` is the reference engine: deterministic placeholder SVG, one group per element, no music layout.
- **Views** are score-protocol's: `:choral` (the score as notated) and `:condensed` (`condensed-view`), optionally restricted to `:parts`.
- **Conformance:** `check-engraving` and `check-timemap` say whether an engine's output keeps the protocol's promises (complete id map, valid and unique SVG ids, systems covering every measure, a timemap consistent with `unfold`).
- **Embedding:** pages are xml-protocol trees; canvas-protocol's `svg-fragment` embeds them in a page through its allowlist.

```sh
cl-repo test --system engrave-protocol
```

## License

MIT, see [LICENSE](LICENSE).
