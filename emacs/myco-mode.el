(defun lisp-add-keywords (face-name keyword-rules)
  (let* ((keyword-list (mapcar #'(lambda (x)
                                   (symbol-name (cdr x)))
                               keyword-rules))
         (keyword-regexp (concat "(\\("
                                 (regexp-opt keyword-list)
                                 "\\)[ \n]")))
    (font-lock-add-keywords 'myco-mode
                            `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
            (put (cdr x)
                 'lisp-indent-function
                 (car x)))
        keyword-rules))

(define-derived-mode myco-mode lisp-mode
  (setq major-name "Myco Lisp"))

(lisp-add-keywords
 'font-lock-keyword-face
 '((1 . when)
   (1 . unless)
   (2 . let)
   (1 . error)
   (1 . fn)
   (1 . def)
   (1 . do)))

(autoload 'myco-mode "myco" "myco docs" t)
(add-to-list 'auto-mode-alist '("\\.myco\\'" . myco-mode))

(provide 'myco-mode)
