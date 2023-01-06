;;;; xprint.el v1.2.2                ;;;;
;;;; Last Modified: 2023/01/07 03:39 ;;;;

(require 'cl-lib)
(require 'cl-extra)

(defun xprint (&rest args)
  (let ((raw nil))
    (when (eq (car args) :raw)
      (setq raw t)
      (setq args (cdr args))
      )
    (prog1 args
      (let ((msg ""))
        (dotimes (i (length args))
          (if (zerop i) nil
            (setq msg (concat msg " "))
            )
          (setq msg (concat msg (format (if raw "%s" "%S") (nth i args))))
          )
        (if noninteractive (message "%s" msg)
          (let ((cb (current-buffer))
                (cw (selected-window)))
            (if (equal (buffer-name) "*xprint*") nil
              (switch-to-buffer-other-window "*xprint*")
              )
            ;;(emacs-lisp-mode)
            ;;(lisp-interaction-mode)
            (unless (derived-mode-p 'lisp-interaction-mode)
              (lisp-interaction-mode))
            (goto-char (point-max))
            (insert msg)
            (insert "\n")
            (let ((wins (window-list)))
              (dolist (win wins)
                (select-window win)
                (when (equal (buffer-name) "*xprint*")
                  (goto-char (point-max))
                  (cond
                   ((pos-visible-in-window-p (point)) nil)
                   ((< (point) (window-start)) (recenter 0))
                   (t (recenter -1)))
                  )
                )
              )
            (select-window cw)
            (switch-to-buffer cb)
            )
          )
        )
      )
    )
  )

(defmacro xdump (&rest list)
  (let ((exp '(xprint)))
    (dolist (x list)
      (if (and (not (consp x)) (not (and (symbolp x) (not (keywordp x))))) (push x exp)
        (push (list 'quote x) exp)
        (push := exp)
        (push x exp)
        )
      )
    (reverse exp)
    )
  )

(defun xclear ()
  (interactive)
  (let ((cb (current-buffer))
        (cw (selected-window)))
    (let ((wins (window-list)))
      (dolist (win wins)
        (select-window win)
        (when (equal (buffer-name) "*xprint*")
          (ignore-errors (delete-window win))
          )
        )
      )
    (ignore-errors (kill-buffer "*xprint*"))
    (ignore-errors (select-window cw))
    nil)
  )

(defun xsleep (millisec)
  (when millisec
    (sit-for 0)
    (sleep-for 0 millisec)
    )
  )

(defmacro xmessage (&rest list)
  (let ((sleep nil))
    (if (and (not (integerp (nth 0 list))) (not (symbolp (nth 0 list)))) nil
      (setq sleep (pop list))
      )
    (if (not sleep)
        `(message ,@list)
      `(progn (message ,@list) (xsleep ,sleep))
      )
    )
  )

(defmacro xformat (&rest list)
  `(xprint :raw (format ,@list))
  )

(defun xpp (x)
  (xprint :raw (xpp-to-string x))
  )

(defun xpp-to-string (form)
  (with-temp-buffer
    (cl-prettyprint form)
    (let ((str (buffer-string)))
      (replace-regexp-in-string "\\`[ \t\n\r]*\\|[ \t\n\r]*\\'" "" str)
      )
    )
  )

(defun xpand-macro-scan (form callback data)
  (cond
   ((symbolp form) (funcall callback form data))
   ((consp form)
    (cons
     (xpand-macro-scan (car form) callback data)
     (xpand-macro-scan (cdr form) callback data)))
   (t form)
   )
  )

(defun xpand-macro (form)
  (let ((result (macroexpand-all form))
        (hash (make-hash-table :test #'equal)))
    (xprint :raw "")
    (xprint :raw ";;; Expanding Macro:")
    (xprint
     :raw
     (xpp-to-string form)
     )
    (xprint :raw "    |")
    (xprint :raw "    |")
    (xprint :raw "    v")
    (xpand-macro-scan
     result
     #'(lambda (sym data)
         (let ((lst (gethash (symbol-name sym) data)))
           (when (not (member sym lst))
             (push sym lst)
             (puthash (symbol-name sym) lst data)
             )
           )
         )
     hash
     )
    (setq result
          (xpand-macro-scan
           result
           #'(lambda (sym data)
               (let ((lst (gethash (symbol-name sym) data)))
                 (if (= 1 (length lst)) sym
                   (intern (format "%s_%d" sym (length (member sym lst))))
                   )
                 )
               )
           hash
           )
          )
    (xprint
     :raw
     (xpp-to-string result)
     )
    result
    )
  )

(defmacro xpand (form)
  `(xpand-macro (quote ,form))
  )

(provide 'xprint)
