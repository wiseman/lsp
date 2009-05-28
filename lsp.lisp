;;; --------------------------------------------------------------------
;;; LSP - Lisp Server Pages
;;;
;;; Copyright 2001, 2002, 2004 I/NET Inc. (http://www.inetmi.com/)
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2004-11-21
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE file.

(in-package :com.lemonodor.lsp)


(defvar *lsp-functions* (make-hash-table :test #'equal)
  "The table mapping LSP filespecs to function-time pairs.")

(defun get-lsp-function (file package)
  "Returns the function implementing a given LSP file.  Builds
   and compiles the function the first time it's requested, or if
   the file has been modified."
  (let ((func.time (gethash file *lsp-functions*)))
    (if (or (null func.time)
	    (> (file-write-date file) (cdr func.time)))
      (progn
	(lsp-log "LSP file ~S is new, compiling." file)
	(handler-case
	    (register-lsp-function file
				   (construct-lsp-function (contents-of-file file)
							   package))
	  (error (e) (error "While processing LSP file ~S, the following error occurred: ~A"
			    file e))))
      (car func.time))))

(defun register-lsp-function (file function)
  (setf (gethash file *lsp-functions*) (cons function (get-universal-time)))
  function)


(defun construct-lsp-function (lsp-string package)
  "Builds and compiles the request-handling LSP function for the
   page whose contents are in LSP-STRING."
  (let* ((*package* package)
	 (form 
	  (lsp-function-with-prologue
	   (read-from-string
	    (format nil "(progn ~A)"
		    (construct-lsp-body-string lsp-string))))))
    (compile nil form)))


(defun contents-of-file (pathname)
  "Returns a string with the entire contents of the specified
  file."
  ;; This is excl:file-contents in ACL.
  (with-open-file (in pathname :direction :input)
    (let* ((bytes-to-read (file-length in))
	   (buffer (make-string bytes-to-read))
	   (bytes-read 0))
      (loop until (= bytes-read bytes-to-read)
	 do (incf bytes-read
		  (print (read-sequence buffer in :start bytes-read))))
      buffer)))


;; (i) Converts text outside <% ... %> tags (straight HTML) into calls
;; to lsp-html, (ii) Text inside <% ... %> ("scriptlets") is straight
;; lisp code, (iii) Text inside <%= ... %> ("expressions") becomes the
;; body of lsp-html macro.

(defun construct-lsp-body-string (lsp-string &optional (start 0))
  "Takes a string containing an LSP page and returns a string
   containing the lisp code that implements that page."
  (multiple-value-bind (start-tag start-code tag-type)
      (next-code lsp-string start)
    (if (not start-tag)
      (format nil "(com.lemonodor.lsp::lsp-html ~S)" (subseq lsp-string start))
      (let ((end-code (search "%>" lsp-string :start2 start-code)))
	(if (not end-code)
	  (error "EOF reached in LSP inside open '<%' tag begun near file position ~S." start-code)
	  (format nil "(com.lemonodor.lsp::lsp-html ~S) ~A ~A"
		  (subseq lsp-string start start-tag)
		  (format nil (tag-template tag-type)
			  (subseq lsp-string start-code end-code))
		  (construct-lsp-body-string lsp-string (+ end-code 2))))))))


;; Finds the next scriptlet or expression tag in LSP source.  Returns
;; nil if none are found, otherwise returns 3 values:
;;  1. The position of the opening bracket (<) of the tag.
;;  2. The position of the contents of the tag.
;;  3. The type of tag (:scriptlet or :expression).

(defun next-code (string start)
  (let ((start-tag (search "<%" string :start2 start)))
    (if (not start-tag)
      nil
      (if (and (> (length string) (+ start-tag 2))
	       (eql (char string (+ start-tag 2)) #\=))
        (values start-tag (+ start-tag 3) :expression)
	(values start-tag (+ start-tag 2) :scriptlet)))))


;; Given a tag type (:scriptlet or :expression), returns a format
;; string to be used to generate source code from the contents of the
;; tag.

(defun tag-template (tag-type)
  (ecase tag-type
    ((:scriptlet) "~A")
    ((:expression) (format nil "(com.lemonodor.lsp::lsp-html ~~A)"))))


(provide :lsp)

