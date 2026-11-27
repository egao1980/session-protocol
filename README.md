# session-protocol

Anonymous server-side sessions for Clack apps on [cl-stack](https://github.com/egao1980/cl-stack). A visitor's browser holds one cookie with a random id; the session's data (string keys mapped to JSON values) lives on the server in a store. The first store keeps sessions as JSON text in any [cache-protocol](https://github.com/egao1980/cache-protocol) cache (the in-memory TTL cache, or Redis through cache-backend-redis). Ids come from [secrets-protocol](https://github.com/egao1980/secrets-protocol), JSON goes through [json-protocol](https://github.com/egao1980/json-protocol), and the Set-Cookie header is written by cl-cookie.

The protocol is transport-neutral: two phases over a Clack env and a Clack response. `begin-session` reads the cookie and loads the session, or starts one lazily. `finish-session` commits it (save, rotate or delete) and adds the Set-Cookie. A middleware binds the two phases (slice S0b: a `session-handler` mixin on middleware-protocol). This package defines no middleware of its own and uses no Lack middleware.

**Status:** slice S0 (0.1.0), implemented. The design is in the capability brief [cl-stack/docs/capabilities/session.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/session.md). Each file header in `src/` states its rules, and the Rove suite in `tests/` (308 tests) is the spec.

| File | What |
|------|------|
| `src/conditions.lisp` | `session-error`, `session-value-error`, `session-store-error`, `session-write-error`; restart functions `skip-value`, `retry` (given) |
| `src/id.lisp` | contract: `generate-session-id` (`token-urlsafe 32`), `valid-session-id-p` |
| `src/model.lisp` | `session` and `make-session` (given); contract: `session-value` and its `setf`, `remove-session-value`, `clear-session`, `regenerate-session-id`, `end-session` |
| `src/json.lisp` | contract: the JSON value domain, `session-data-json`, `encode-session-record`, `decode-session-record` |
| `src/store.lisp` | `session-store` generic functions and `cache-session-store` (given); contract: the cache store's methods |
| `src/cookie.lisp` | `cookie-policy` and `make-cookie-policy` (given); contract: `read-session-cookie`, `format-session-cookie` |
| `src/manager.lisp` | `session-manager` and `make-session-manager` (given); contract: `begin-session`, `finish-session`, `env-session` |

```lisp
(asdf:load-system "session-protocol")
(asdf:load-system "json-backend-jzon")         ; a JSON backend
(asdf:load-system "crypto-backend-ironclad")   ; a secrets backend

(defvar *sessions* (stack-session:make-session-manager :idle-ttl 3600))

;; around any Clack app, as a middleware would:
(defun app (env)
  (multiple-value-bind (session env) (stack-session:begin-session *sessions* env)
    (incf (stack-session:session-value session "visits" 0))
    (stack-session:finish-session *sessions* session env
                                  '(200 (:content-type "text/plain") ("hi")))))
```

```sh
cl-repo test --system session-protocol
```

## License

MIT, see [LICENSE](LICENSE).
