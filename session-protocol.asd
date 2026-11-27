(defsystem "session-protocol"
  :version "0.2.0"
  :description "Anonymous server-side sessions for Clack apps: a session model with JSON values, a store protocol over cache-protocol, ids from secrets-protocol, a cookie policy, and begin / finish phases for middleware"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cache-protocol" "0.2.0")
               (:version "secrets-protocol" "0.1.3")
               (:version "json-protocol" "0.2.4")
               (:version "cl-cookie" "0.2.0")
               (:version "quri" "0.7.2"))
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "id")
               (:file "model")
               (:file "json")
               (:file "store")
               (:file "cookie")
               (:file "manager"))
  :in-order-to ((test-op (test-op "session-protocol/tests"))))

(defsystem "session-protocol/middleware"
  :version "0.2.0"
  :description "session-protocol as a middleware-protocol mixin: SESSION-HANDLER begins a session in PREPARE-REQUEST and commits it in FINISH-RESPONSE for Clack requests, streamed responses included"
  :author "egao1980"
  :license "MIT"
  :depends-on ("session-protocol"
               (:version "middleware-protocol" "0.1.0")
               (:version "middleware-protocol/clack" "0.1.0"))
  :pathname "src"
  :components ((:file "middleware"))
  :in-order-to ((test-op (test-op "session-protocol/tests"))))

(defsystem "session-protocol/tests"
  :version "0.2.0"
  :depends-on ("session-protocol"
               "session-protocol/middleware"
               (:version "rove" "0.11.1")
               (:version "json-backend-jzon" "0.2.4")
               (:version "crypto-backend-ironclad" "0.2.2"))
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "conditions-test")
               (:file "config-test")
               (:file "id-test")
               (:file "model-test")
               (:file "json-test")
               (:file "cookie-test")
               (:file "store-test")
               (:file "flow-test")
               (:file "expiry-test")
               (:file "lifecycle-test")
               (:file "streaming-test")
               (:file "failure-test")
               ;; S0b: SESSION-HANDLER (package SESSION-PROTOCOL/MIDDLEWARE/TESTS)
               (:file "middleware-package")
               (:file "middleware-flow-test")
               (:file "middleware-streaming-test")
               (:file "middleware-compose-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
