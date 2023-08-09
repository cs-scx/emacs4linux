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

(set-frame-parameter nil 'alpha '(80 .100))
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
(global-hl-line-mode 1)


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
  :hook (web-mode css-mode scss-mode sgml-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda ()
                               (setq emmet-indent-after-insert t)))
  )


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
  (setq js2-basic-offset 2)
  )

(use-package react-snippets
  :ensure t)

;; 括号匹配
;;(use-package
;;  smartparens
;;  :ensure t
;;  :hook ('prog-mode . 'smartparens-global-mode))


(load-theme 'manoj-dark)

(set-cursor-color "red")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-web react-snippets rjsx-mode emmet-mode web-mode which-key flycheck js2-mode company command-log-mode evil-leader evil linum-relative dashboard doom-themes scss-mode magit yasnippet-snippets yasnippet counsel use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
