;;  -*- lexical-binding: t; -*-

;; TODO: submit PR https://github.com/radian-software/apheleia?tab=readme-ov-file#adding-a-formatter
(with-eval-after-load 'apheleia-formatters
  (add-to-list 'apheleia-formatters '(ess-r "R" "-s" "--no-save" "--no-restore" "-e" "styler::style_text(readLines(file('stdin')))"))
  (add-to-list 'apheleia-mode-alist '(ess-r-mode . ess-r)))

;; Auto-save every 5 seconds destroys what youre doing with mc!
;; Nameless-mode also does not cope well.
(with-eval-after-load 'multiple-cursors
  (add-to-list 'mc/unsupported-minor-modes 'auto-save-visited-mode)
  (add-to-list 'mc/unsupported-minor-modes 'nameless-mode))

;; Bugfix (look for "xml-escape-string")
(with-eval-after-load 'dom
  (defun dom-print (dom &optional pretty xml)
    "Print DOM at point as HTML/XML.
If PRETTY, indent the HTML/XML logically.
If XML, generate XML instead of HTML."
    (let ((column (current-column)))
      (insert (format "<%s" (dom-tag dom)))
      (let ((attr (dom-attributes dom)))
        (dolist (elem attr)
	  ;; In HTML, these are boolean attributes that should not have
	  ;; an = value.
	  (insert (if (and (memq (car elem)
			         '(async autofocus autoplay checked
			                 contenteditable controls default
			                 defer disabled formNoValidate frameborder
			                 hidden ismap itemscope loop
			                 multiple muted nomodule novalidate open
			                 readonly required reversed
			                 scoped selected typemustmatch))
			   (cdr elem)
			   (not xml))
		      (format " %s" (car elem))
		    (format " %s=\"%s\"" (car elem)
	                    (url-insert-entities-in-string (cdr elem)))))))
      (let* ((children (dom-children dom))
	     (non-text nil))
        (if (null children)
	    (insert " />")
	  (insert ">")
          (dolist (child children)
	    (if (stringp child)
	        (insert (xml-escape-string child))
	      (setq non-text t)
	      (when pretty
                (insert "\n" (make-string (+ column 2) ?\s)))
	      (dom-print child pretty xml)))
	  ;; If we inserted non-text child nodes, or a text node that
	  ;; ends with a newline, then we indent the end tag.
          (when (and pretty
		     (or (bolp)
		         non-text))
	    (unless (bolp)
              (insert "\n"))
	    (insert (make-string column ?\s)))
          (insert (format "</%s>" (dom-tag dom))))))))
