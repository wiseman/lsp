;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; LSP -- Lisp Server Pages
;;;
;;; Copyright 2001, 2002, 2004 I/NET Inc. (http://www.inetmi.com/)
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2004-11-21
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE file.
;;;
;;; ASDF system definition of base package.

(in-package :asdf)

(defsystem :lsp
  :name "LSP"
  :author "John Wiseman <jjwiseman@yahoo.com>"
  :version "1.1"
  :maintainer "John Wiseman <jjwiseman@yahoo.com>"
  :licence "MIT"
  :description "Lisp Server Pages"
  :long-description "Lisp Server Pages (LSP) allow developers to dyamically create web pages by embedding Lisp code into HTML."
  
  :components ((:file "package")
	       (:file "lsp" :depends-on ("package"))))
