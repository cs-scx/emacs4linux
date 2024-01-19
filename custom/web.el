(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-css-colorization t)
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "royalblue")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "powderblue")
  (set-face-attribute 'web-mode-doctype-face nil :foreground "lightskyblue")
  (use-package company-web
    :ensure t
    :config
    (add-hook 'web-mode-hook (lambda ()
                               (cond ((equal web-mode-content-type "html")
                                      (web-html-setup)))))

    )
  )

;;
;; html
;;
(defun web-html-setup()
  ;; for web-mode html files
  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-select-checker 'html-tidy)
  (add-to-list (make-local-variable 'company-backends)
               '(company-web-html company-files company-css company-capf company-dabbrev))
  (add-hook 'before-save-hook #'sgml-pretty-print)

  )


(use-package emmet-mode
  :ensure t
  :hook (web-mode css-mode scss-mode sgml-mode rjsx-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda ()
                               (setq emmet-indent-after-insert t)))
  )

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (add-to-list (make-local-variable 'company-backends)
               '(company-css company-files company-yasnippet company-capf))
  (setq css-indent-offset 2)
  (setq flycheck-stylelintrc "~/.stylelintrc")
  )


(use-package scss-mode
  :ensure t
  :mode "\\scss\\'")
;;
;; eslint use local
;;
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx\\'")
  :config
  (setq js2-basic-offset 2)
  (add-hook 'rjsx-mode-hook (lambda()
                              (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
                              (my/use-eslint-from-node-modules)
                              (flycheck-select-checker 'javascript-eslint)
                              ))
  (setq js2-basic-offset 2))

(use-package mode-local
  :ensure t
  :config
  (setq-mode-local rjsx-mode emmet-expand-jsx-className? t)
  (setq-mode-local web-mode emmet-expand-jsx-className? nil))
  

(use-package prettier-js
  :ensure t
  :hook ((typescript-mode . prettier-js-mode)
         (js2-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--bracket-spacing" "false"
                           )))

(use-package react-snippets
  :ensure t)

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))

(provide 'init-web)


