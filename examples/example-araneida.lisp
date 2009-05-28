(defpackage :example-webapp
  (:use #:common-lisp #:com.lemonodor.lsp))


;; Create a new HTTP listener, using threads if we have them,
;; serve-event otherwise.

(defparameter *listener*
  (make-instance #+araneida-threads 'araneida:threaded-http-listener
                 #-araneida-threads 'araneida:serve-event-http-listener
                 :port 8000))


;; Publish an LSP file at http://localhost:8000/example.lsp

(com.lemonodor.lsp:publish-lsp
 :file (merge-pathnames "example-araneida.lsp"
			*load-pathname*)
 :path "http://localhost:8000/example.lsp"
 :package :example-webapp
 :server *listener*)


;; Start the HTTP server.
(araneida:start-listening *listener*)
