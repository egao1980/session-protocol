# session-protocol

Anonymous server-side sessions for Clack apps on [cl-stack](https://github.com/egao1980/cl-stack): a session model with JSON-compatible values, a store protocol whose first store is any [cache-protocol](https://github.com/egao1980/cache-protocol) cache, session ids from [secrets-protocol](https://github.com/egao1980/secrets-protocol), a cookie policy, and two transport-neutral phases (begin a session from a request, finish it on the response) that a middleware binds.

**Status:** skeleton (0.0.1). The design is in the capability brief [cl-stack/docs/capabilities/session.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/session.md) (slice S0). The API and tests land through contract branches and team-solve.

```sh
cl-repo test --system session-protocol
```

## License

MIT, see [LICENSE](LICENSE).
