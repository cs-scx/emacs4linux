(setq user-emacs-directory "~/.emacs.d/var")
;; 设置自动保存路径
(setq auto-save-list-file-prefix "~/.emacs/var/auto-save-list/.saves-")
;; 设置eshell历史记录
(setq eshell-history-file-name "~/.emacs/var/eshell/history")

;; toggle滚动条
(scroll-bar-mode -1)

;; toggle工具栏
(tool-bar-mode -1)

;; toggle菜单栏
(menu-bar-mode 1)

;; 自动刷新被修改过的文件
(global-auto-revert-mode 1)
;; 选中文本后输入会覆盖
(delete-selection-mode 1)

;; 关闭备份
(setq make-backup-files nil auto-save-default nil)

;; 关闭锁文件
(setq create-lockfiles nil)

;; 总是加载最新的文件
(setq load-prefer-newer t)

;; 关闭字体缓存gc
(setq inhibit-compacting-font-caches nil)

;; 关闭烦人的提示
(setq ring-bell-function 'ignore blink-cursor-mode nil)

;; 任何地方都使用UTF-8
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)    ; pretty
(set-terminal-coding-system  'utf-8)    ; pretty
(set-keyboard-coding-system  'utf-8)    ; pretty
(set-selection-coding-system 'utf-8)    ; please
(prefer-coding-system        'utf-8)    ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; 更友好和平滑的滚动
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

;; 关闭自动调节行高
(setq auto-window-vscroll nil)

;; 创建新行的动作
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

;; 让光标无法离开视线
(setq mouse-yank-at-point nil)

;; 行宽
(setq-default fill-column 80)

;; 让'_'被视为单词的一部分
(add-hook 'after-change-major-mode-hook (lambda ()
                                          (modify-syntax-entry ?_ "w")))
;; "-" 同上)
(add-hook 'after-change-major-mode-hook (lambda ()
                                          (modify-syntax-entry ?- "w")))
;; 没有制表符
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)

;; 高亮括号
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


;; 增强了搜索功能
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

;; 集成了很多非常有用的的功能
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

(set-frame-parameter nil 'alpha '(85 .100))
;; 设置光标样式
(setq-default cursor-type 'box)
;; 去除默认启动界面
(setq inhibit-startup-message nil)
;; 设置英文字体
(if (fontp (font-spec :name "Fira Code Nerd Font"
                      :style "Retina"))
    (set-face-attribute 'default nil
                        :font (font-spec :name "Fira Code Nerd Font"
                                         :style "Retina"
                                         :size 17))
  (message "无法找到Fira Code Nerd Font字体，你可以更换其他字体或安装它让这条消息消失."))

;; 高亮当前行
(global-hl-line-mode -1)


;; 代码片段
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



;; 主题包
(use-package
  doom-themes
  :ensure t
  ;;:defer
  :config (load-theme 'manoj-dark t)
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


;; 相对行号，默认未开启
(use-package linum-relative
  ;;:disabled
  :ensure t
  :hook ('prog-mode . 'linum-relative-mode))


;; Evil Mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-set-cursor-color "navy")
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


;; 折叠和收缩代码
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

;; 著名的Emacs补全框架
(use-package
  company
  :ensure t
  :defer 2
  :hook (after-init . global-company-mode)
  :init (setq company-tooltip-align-annotations t company-idle-delay 0 company-echo-delay 0
              company-minimum-prefix-length 1 company-require-match nil company-dabbrev-ignore-case
              nil company-dabbrev-downcase nil company-show-numbers t)
  :config
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . #'company-select-next)
              ("C-p" . #'company-select-previous))
)

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
          ("\\.json\\'" . javascript-mode))
  :init
  (setq indent-tabs-mode nil)
  (setq js2-basic-offset 2)
  (setq js-indent-level 2)
  (setq js2-global-externs '("module" "require" "assert" "setInterval" "console" "__dirname__") )
  )


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))


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
  :hook ((js2-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--bracket-spacing" "false"
                           )))

(use-package react-snippets
  :ensure t)


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq doom-modeline-height 25)
(setq doom-modeline-height 1) ; optional
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:family "Noto Sans" :height 1.0))))
 '(mode-line-active ((t (:family "Noto Sans" :height 1.0))))
 '(mode-line-inactive ((t (:family "Noto Sans" :height 1.0)))))

(use-package 
  all-the-icons 
  :ensure t) 
(use-package 
  all-the-icons-dired 
  :ensure t 
  :hook ('dired-mode . 'all-the-icons-dired-mode)) 
(use-package 
  posframe 
  :ensure t)

;; 括号匹配
;;(use-package
;;  smartparens
;;  :ensure t
;;  :hook ('prog-mode . 'smartparens-global-mode))


(load-theme 'manoj-dark t)

(set-cursor-color "red")

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs-tab-bar treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs yasnippet-snippets which-key web-mode vterm use-package tide scss-mode rvm rufo ruby-electric rubocop rtags robe rjsx-mode restclient react-snippets py-autopep8 projectile-rails prettier-js posframe ox-reveal ox-gfm org2blog org-gcal org-download org-bullets omnisharp magit linum-relative irony-eldoc go-rename go-guru go-eldoc flycheck-irony evil-leader emmet-mode elpy doom-themes doom-modeline dashboard counsel company-web company-jedi company-irony-c-headers company-irony company-go command-log-mode cmake-mode cmake-ide clang-format all-the-icons-dired)))
