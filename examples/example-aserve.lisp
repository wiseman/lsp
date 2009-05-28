(defpackage :example-webapp
  (:use #:common-lisp #:com.lemonodor.lsp #:net.aserve #:net.html.generator))


;; Start an HTTP server.

(net.aserve:start :port 8000)


;; Publish the LSP file at http://localhost:8000/example.lsp

(com.lemonodor.lsp:publish-lsp
 :file (merge-pathnames #p"example-aserve.lsp"
			*load-pathname*)
 :path "/example.lsp"
 :package :example-webapp)
