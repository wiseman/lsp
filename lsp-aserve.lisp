;;; --------------------------------------------------------------------
;;; LSP - Lisp Server Pages
;;;
;;; Copyright 2001, 2002, 2004 I/NET Inc. (http://www.inetmi.com/)
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2004-11-21
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE file.

(in-package :com.lemonodor.lsp)


(defmacro lsp-html (&rest args)
  `(net.html.generator:html ,@args))

(defun publish-lsp (&key path file (package *package*) (server net.aserve:*wserver*))
  "Publishes an LSP file.  PATH is a string containing the name part of the URL at which to publish the file,e.g. \"/math/sum.lsp\"; FILE is a pathname that specifies the file containing the page to publish. PACKAGE is the package the reader should use when loading and compiling this file (*package* is the default).  SERVER is the HTTP server on which to publish the file."
  (let ((package (find-package package)))
    (net.aserve:publish :path path
			:server server
			:function #'(lambda (request entity)
				      (do-lsp-request request entity file package)))))


(defun do-lsp-request (request entity file package)
  "Handles the request for an LSP URL." 
  (lsp-log "Executing LSP file ~S." file)
  (funcall (get-lsp-function file package) request entity))


(defun lsp-function-with-prologue (body)
  `(lambda (request entity)
     (net.aserve:with-http-response (request entity)
       (net.aserve:with-http-body (request entity)
	 ,@body))))
  

(defun lsp-log (fmt &rest args)
  ;; There seems to be no reason for DEBUG-FORMAT to be a macro.  It'd
  ;; be nice if it weren't, because then we could just APPLY it.
  (let ((message (apply #'format nil fmt args)))
    (net.aserve::debug-format :info "~A~%" message)))


(provide :lsp-aserve)
