(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq user-emacs-directory "~/.emacs.d/var")
(setq auto-save-list-file-prefix "~/.emacs/var/auto-save-list/.saves-")

(show-paren-mode t)
(electric-pair-mode t)

(if (fontp (font-spec :name "DejaVu Sans Mono"
                      :style "Bold"))
    (set-face-attribute 'default nil
                        :font (font-spec :name "DejaVu Sans Mono"
                                         :style "Bold"
                                         :size 19)))

(scroll-bar-mode -1)
(tool-bar-mode -1)

(defun setup-for-nw()
  ;;settings for terminal emacs
  (menu-bar-mode -1)
  (add-hook 'dired-mode-hook (lambda() (all-the-icons-dired-mode -1)))
  (load-theme 'manoj-dark t))

(if (display-graphic-p)
    (menu-bar-mode 1)
  (setup-for-nw))

(global-auto-revert-mode 1)

(setq make-backup-files nil auto-save-default nil)
(setq create-lockfiles nil)
(setq load-prefer-newer t)
(setq inhibit-compacting-font-caches nil)
(setq ring-bell-function 'ignore blink-cursor-mode nil)
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)    ; pretty
(set-terminal-coding-system  'utf-8)    ; pretty
(set-keyboard-coding-system  'utf-8)    ; pretty
(set-selection-coding-system 'utf-8)    ; please
(prefer-coding-system        'utf-8)    ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

(setq auto-window-vscroll nil)
(setq mouse-yank-at-point nil)
(setq-default fill-column 80)

(add-hook 'after-change-major-mode-hook (lambda ()
                                          (modify-syntax-entry ?_ "w")))
(add-hook 'after-change-major-mode-hook (lambda ()
                                          (modify-syntax-entry ?- "w")))
(show-paren-mode 1)

(require 'package)

(package-initialize) ;; You might already have this line

(setq package-archives '(
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package ivy
  :ensure t
  :diminish (ivy-mode ."")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "%d:%d")
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  )

(use-package swiper
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package counsel
  :hook
  ('counsel-mode . 'dashboard-mode)
  :ensure t
  :bind
  (("C-x C-r" . 'counsel-recentf)
   ("C-x d" . 'counsel-dired))
  :config
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)))


(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;(set-frame-parameter nil 'alpha '(96 .100))
(setq-default cursor-type 'box)
(setq inhibit-startup-message nil)
(global-hl-line-mode -1)

(use-package
  yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init (autoload 'yas-minor-mode-on "yasnippet")
  (setq yas-snippet-dirs '("~/.emacs.d/etc/snippets"))
  (dolist (x '(org-mode-hook prog-mode-hook snippet-mode-hook))
    (add-hook x #'yas-minor-mode-on)))

(use-package
  yasnippet-snippets
  :ensure t)

(use-package magit
  :ensure t
  :commands (magit))

(use-package
  doom-themes
  :ensure t
  ;;:defer
  ;;:config (load-theme 'doom-dark+ t)
  )

(use-package
  dashboard
  :ensure t
  :config (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))

  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t))


(use-package linum-relative
  ;;:disabled
  :ensure t
  :hook ('prog-mode . 'linum-relative-mode))


(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (blink-cursor-mode 1)
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "o h" 'org-hide-entry
      "o s" 'org-show-entry
      "t t" 'treemacs-select-directory
      "t c" 'treemacs-create-file
      "t d" 'treemacs-delete-file
      "t r" 'treemacs-rename-file
      "t g" 'treemacs-refresh
      "t <" 'treemacs-decrease-width
      "t >" 'treemacs-increase-width
      "t =" 'treemacs-fit-window-width
      "t l" 'treemacs-TAB-action
      "s s" 'swiper
      "d x w" 'delete-trailing-whitespace)))


(use-package
  hideshow
  :ensure t
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
              ("C-c TAB" . hs-toggle-hiding)
              ("C-c p +" . hs-show-all)
              )
  :hook (prog-mode . hs-minor-mode))

(use-package
  command-log-mode
  :ensure t
  :init (setq command-log-mode t)
  )

(use-package
  company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-backends
	'((company-files company-yasnippet company-capf company-keywords)
	  (company-abbrev company-dabbrev)))
  )

(add-hook 'emacs-lisp-mode-hook (lambda () (set (make-local-variable 'company-backends) '(company-elisp))))

;;
;; change C-n C-p
;;
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil))

;;
;; change company complete common
;;
(advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
(advice-add 'company-complete-common :after (lambda () (when (equal my-company-point (point))
                                                         (yas-expand))))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package
  all-the-icons-dired
  :ensure t
  :hook ('dired-mode . 'all-the-icons-dired-mode))

(use-package
  posframe
  :ensure t)

(set-background-color "floralwhite")
(set-cursor-color "darkgreen")

(load-file "/home/scx/.emacs.d/custom/web.el")
(load-file "/home/scx/.emacs.d/custom/gp.el")
(load-file "/home/scx/.emacs.d/custom/org-bullets.el")
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
(load-file "/home/scx/.emacs.d/custom/js.el")

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package doom-modeline
  :ensure t)
(if (display-graphic-p) (doom-modeline-mode))

(use-package typescript-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" default))
 '(package-selected-packages
   '(typescript-mode doom-modeline lsp-ivy helm-lsp lsp-ui lsp-mode tide yasnippet-snippets which-key web-mode vue-mode use-package scss-mode s rjsx-mode react-snippets prettier-js posframe magit linum-relative flycheck evil-leader emmet-mode doom-themes dashboard counsel company command-log-mode all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
