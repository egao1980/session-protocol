# session-protocol

Anonymous server-side sessions for Clack apps on [cl-stack](https://github.com/egao1980/cl-stack). A visitor's browser holds one cookie with a random id; the session's data (string keys mapped to JSON values) lives on the server in a store. The first store keeps sessions as JSON text in any [cache-protocol](https://github.com/egao1980/cache-protocol) cache (the in-memory TTL cache, or Redis through cache-backend-redis). Ids come from [secrets-protocol](https://github.com/egao1980/secrets-protocol), JSON goes through [json-protocol](https://github.com/egao1980/json-protocol), and the Set-Cookie header is written by cl-cookie.

The protocol is transport-neutral: two phases over a Clack env and a Clack response. `begin-session` reads the cookie and loads the session, or starts one lazily. `finish-session` commits it (save, rotate or delete) and adds the Set-Cookie. The core system defines no middleware and uses no Lack middleware. The secondary system `session-protocol/middleware` binds the two phases to [middleware-protocol](https://github.com/egao1980/middleware-protocol): `session-handler` is a mixin whose `prepare-request` begins the session and whose `finish-response` commits it, also for streamed responses.

**Status:** slice S0 (0.1.0), implemented. Slice S0b (0.2.0, `session-protocol/middleware`), implemented. The design is in the capability brief [cl-stack/docs/capabilities/session.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/session.md). Each file header in `src/` states its rules, and the Rove suite in `tests/` is the spec (S0: 308 tests; S0b: 49 tests in the `middleware-*.lisp` files).

| File | What |
|------|------|
| `src/conditions.lisp` | `session-error`, `session-value-error`, `session-store-error`, `session-write-error`; restart functions `skip-value`, `retry` (given) |
| `src/id.lisp` | contract: `generate-session-id` (`token-urlsafe 32`), `valid-session-id-p` |
| `src/model.lisp` | `session` and `make-session` (given); contract: `session-value` and its `setf`, `remove-session-value`, `clear-session`, `regenerate-session-id`, `end-session` |
| `src/json.lisp` | contract: the JSON value domain, `session-data-json`, `encode-session-record`, `decode-session-record` |
| `src/store.lisp` | `session-store` generic functions and `cache-session-store` (given); contract: the cache store's methods |
| `src/cookie.lisp` | `cookie-policy` and `make-cookie-policy` (given); contract: `read-session-cookie`, `format-session-cookie` |
| `src/manager.lisp` | `session-manager` and `make-session-manager` (given); contract: `begin-session`, `finish-session`, `env-session` |
| `src/middleware.lisp` | system `session-protocol/middleware` (S0b): `session-handler` with `:session-manager` (given); contract: its `prepare-request` and `finish-response` methods on `clack-request`, `request-session` |

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

As middleware, with any other middleware-protocol mixins (S0b):

```lisp
(asdf:load-system "session-protocol/middleware")

(defun my-app (env)                            ; a Clack app
  (let ((session (stack-session:env-session env)))
    (incf (stack-session:session-value session "visits" 0))
    '(200 (:content-type "text/plain") ("hi"))))

(stack-middleware-clack:clack-app
 (stack-middleware:make-handler
  '(stack-middleware-clack:response-headers-handler
    stack-session-middleware:session-handler
    stack-middleware-clack:clack-handler)
  :session-manager *sessions*
  :response-headers '(:x-frame-options "DENY")
  :app #'my-app))
```

```sh
cl-repo test --system session-protocol         # S0 and S0b
```

## License

MIT, see [LICENSE](LICENSE).
