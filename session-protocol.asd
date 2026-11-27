(defsystem "session-protocol"
  :version "0.0.1"
  :description "Server-side sessions for Clack apps: model, store protocol (cache-protocol), cookie policy, request and response phases for middleware"
  :author "egao1980"
  :license "MIT"
  :pathname "src"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "session-protocol/tests"))))

(defsystem "session-protocol/tests"
  :depends-on ("session-protocol" (:version "rove" "0.11.1"))
  :pathname "tests"
  :serial t
  :components ((:file "session-protocol-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
