;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; LSP -- Lisp Server Pages
;;;
;;; Copyright 2001, 2002, 2004 I/NET Inc. (http://www.inetmi.com/)
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2004-11-21
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE file.
;;;
;;; ASDF system definition of LSP AllegroServe support.

(in-package :asdf)

(defsystem :lsp-aserve
  :name "LSP-ASERVE"
  :author "John Wiseman <jjwiseman@yahoo.com>"
  :version "1.1"
  :maintainer "John Wiseman <jjwiseman@yahoo.com>"
  :licence "MIT"
  :description "LSP Support for the AllegroServe web server"
  :components ((:file "lsp-aserve"))
  :depends-on (:lsp :aserve))
