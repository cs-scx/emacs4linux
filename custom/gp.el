(defun evaluate-gnuplot-buffer()
  "Run gnuplot script."
  (interactive)
  (shell-command (concat "gnuplot -p " buffer-file-name)))

(global-set-key (kbd "<f8>") 'evaluate-gnuplot-buffer)

(add-to-list 'load-path "~/.emacs.d/var/elpa/gnuplot")

(autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))


(defvar tscount 0)
(defvar wd)
(defvar cmd)

(defun trans-at-point()
  "Translate word to chinese at point."
  (interactive)
  (setq wd (thing-at-point 'word 'no-properties))
  (setq cmd (concat "spd-say -r -70 -t female2 " wd ))
  (shell-command cmd)
  (setq cmd (concat "trans en:zh-CN " wd " > /home/scx/.trans.log"))
  (shell-command cmd)
  (if (> tscount 0)
    (kill-buffer ".trans.log"))
  (find-file-other-window "/home/scx/.trans.log")
  (read-only-mode '.trans.log)
  (other-window 1)
  (setq tscount (+ tscount 1))
  )

(defun trans-selected()
  "Translate selected text to chinese."
  (interactive)
  (setq selected-text (buffer-substring (region-beginning) (region-end)))
  (setq cmd (concat "trans en:zh-CN \"" selected-text "\"" " > /home/scx/.trans.log"))
  (shell-command cmd)
  (if (> tscount 0)
    (kill-buffer ".trans.log"))
  (find-file-other-window "/home/scx/.trans.log")
  (read-only-mode '.trans.log)
  (other-window 1)
  (setq tscount (+ tscount 1))
  )

(global-set-key (kbd "C-x p") 'trans-at-point)
(global-set-key (kbd "C-x i") 'trans-selected)

(add-hook 'c++-mode-hook
  (lambda ()
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
       (let ((file (file-name-nondirectory buffer-file-name)))
         (concat "g++ -g -std=c++17 -Wall -Wextra -Wpedantic -Wshadow -o " 
             (file-name-sans-extension file)".out"
             " " file))))))

(add-hook 'python-mode-hook
  (lambda ()
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
       (let ((file (file-name-nondirectory buffer-file-name)))
         (concat "/usr/bin/python3" 
             " " file))))))

(global-set-key (kbd "<f5>") 'compile)


;;;;
(defun file-to-disk()
  (setq current_line (number-to-string (line-number-at-pos)))
  (setq file_name (buffer-file-name))
  (setq buffer_name (buffer-name))
  (setq cmd (concat "cat /home/scx/.emacs.d/var/file_pos | grep " file_name " | cut -d@ -f2"))
  ;;
  (setq line (shell-command-to-string cmd))
  (setq line_number (string-to-number line))
  (setq line (number-to-string line_number))
  ;;
  (setq cmd (concat "sed -i \"s/" buffer_name "@" line "/" buffer_name "@" current_line "/g\" /home/scx/.emacs.d/var/file_pos"))
  (if (> line_number 0)
      (shell-command cmd))
  (setq cmd (concat "echo " file_name "@" current_line " >> /home/scx/.emacs.d/var/file_pos"))
  (if
      (= line_number 0) (shell-command cmd))
  )

(defun goto-latest-line()
  (setq file_name (buffer-file-name))
  (setq cmd (concat "cat /home/scx/.emacs.d/var/file_pos | grep " file_name " | cut -d@ -f2"))
  (setq line (shell-command-to-string cmd))
  (goto-line (string-to-number line))
  )

(add-hook 'before-save-hook (lambda () (file-to-disk)))
(add-hook 'prog-mode-hook (lambda () (goto-latest-line)))


(setq org-log-done 'time)  ;; 任务完成时记录时间戳
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d@)" "CANCELED(c@)")))  ;; @添加备注
