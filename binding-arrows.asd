(asdf:defsystem #:binding-arrows
  :version "1.0.0"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :description
  "An implementation of threading macros based on binding anonymous variables"
  :pathname "src"
  :components ((:file "binding-arrows")
               (:file "documentation"))
  :in-order-to ((asdf:test-op (asdf:test-op #:binding-arrows/test))))

(asdf:defsystem #:binding-arrows/test
  :version "1.0.0"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :description "Test suite for Binding Arrows"
  :depends-on (#:binding-arrows #:hu.dwim.stefil)
  :pathname "t"
  :components ((:file "test"))
  :perform (asdf:test-op (c v) (uiop:symbol-call '#:binding-arrows/test
                                                 '#:test-binding-arrows)))
