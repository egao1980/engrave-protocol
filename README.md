# engrave-protocol

Engraving for cl-stack: a [score-protocol](https://github.com/egao1980/score-protocol) score (or one of its views) becomes SVG pages whose element ids lead back to the score, a timemap for playback cursors, and PDF. Rendering happens in an engine behind the protocol; the first native engine is planned as a separate backend.

**Status:** skeleton (0.0.1). The design is in the capability brief [cl-stack/docs/capabilities/engrave.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/engrave.md). The API and tests land through contract branches and team-solve.

```sh
cl-repo test --system engrave-protocol
```

## License

MIT, see [LICENSE](LICENSE).
