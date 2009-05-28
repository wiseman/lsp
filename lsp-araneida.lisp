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
  `(araneida:html-stream (araneida:request-stream request) ,@args))

(defclass lsp-handler (araneida:handler)
  ((file :initarg :file :accessor lsp-handler-file)
   (package :initarg :package :accessor lsp-handler-package)))

(defmethod araneida:handle-request-response ((self lsp-handler) method request)
  (do-lsp-request method request
		  (lsp-handler-file self)
		  (lsp-handler-package self)))

(defun publish-lsp (&key path file server (package *package*))
  "Publishes an LSP file.  PATH is a string containing the name
   part of the URL at which to publish the
   file,e.g. \"/math/sum.lsp\"; FILE is a pathname that specifies
   the file containing the page to publish. PACKAGE is the
   package the reader should use when loading and compiling this
   file (*package* is the default).  SERVER is the HTTP listener
   on which to publish the file."
  (let ((package (find-package package)))
    (araneida:install-handler (araneida:http-listener-handler server)
			      (make-instance 'lsp-handler :file file :package package)
			      path
			      NIL)))


(defun do-lsp-request (method request file package)
  "Handles the request for an LSP URL." 
  (lsp-log "Executing LSP file ~S." file)
  (funcall (get-lsp-function file package) method request))


(defun lsp-function-with-prologue (body)
  `(lambda (method request)
    (declare (ignorable method))
    (araneida:request-send-headers request)
    ,body))
  

(defun lsp-log (fmt &rest args)
  (declare (ignore fmt args))
  (apply #'format araneida::*log-stream* fmt args)
  (terpri araneida::*log-stream*))


(provide :lsp-araneida)
