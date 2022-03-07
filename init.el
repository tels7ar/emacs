;; init.el --- my emacs config file

;;; Commentary:

; none to speak of

;;; Code:

; here it is

(setq load-path (cons "~/.emacs.d/lisp" load-path))

; point custom config at a different file, so it won't change my
; init.el underneath me.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;assume server is already started by launchd
;(server-start)

; make sure gpg is always in the path
(add-to-list 'exec-path "/usr/local/bin")

(set-keyboard-coding-system nil)

(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq transient-mark-mode t)
(setq mouse-yank-at-point t)
(setq next-line-add-newlines nil)
(setq minibuffer-max-depth nil)
(setq lpr-command "a2ps")

(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; Install straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
    (url-retrieve-synchronously
     "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
;; Always use straight in use-package
(setq straight-use-package-by-default t)

; Minimal package install, so I can still browse melpa.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

; change the modeline text for a mode (usually abbreviate it)
(use-package diminish)

(use-package ido
  :commands ido-mode
  :config
  (progn
    (ido-mode 1)
    (setq ido-auto-merge-work-directories-length -1
          ido-default-buffer-method 'selected-window
          ido-use-virtual-buffers t
          ido-use-filename-at-point nil
          ido-create-new-buffer 'always)
    (ido-everywhere 1)
    (setq ido-enable-flex-matching t)
    (use-package flx-ido
      :config
      (progn
        ;; disable ido faces to see flx highlights.
        ;; This makes flx-ido much faster.
        (setq gc-cons-threshold 20000000)
        ;; disable ido faces to see flx highlights.
        (setq ido-use-faces nil)
        (flx-ido-mode 1)))
    (use-package ido-grid-mode
      :config
      (progn
        (ido-grid-mode 1)
        (ido-grid-mode-prefix-scrolls 1)))
    (use-package flx-ido)))

; improved find program, used by projectile
(use-package ag
  :ensure-system-package ag
  :commands (ag ag-regexp ag-project))

(use-package kill-ring-ido
  :straight (kill-ring-ido :type git :host github :repo "tels7ar/emacs")
  :init
  (global-set-key (kbd "M-y") 'kill-ring-ido))

(use-package ido-preview
  :straight (ido-preview :type git :host github :repo "tels7ar/emacs")
  :init
  (add-hook 'ido-setup-hook
            (lambda()
              (define-key ido-completion-map
                (kbd "C-M-p") (lookup-key ido-completion-map (kbd "C-p")))
              (define-key ido-completion-map
                (kbd "C-M-n") (lookup-key ido-completion-map (kbd "C-n")))
                                        ; currently, this makes nothing.
              (define-key ido-completion-map (kbd "C-p") 'ido-preview-backward)
              (define-key ido-completion-map (kbd "C-n") 'ido-preview-forward))))


(use-package flycheck-pycheckers
  :init
  (setq flycheck-pycheckers-checkers '(pylint flake8))
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(use-package flycheck
  :init
  (global-flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)))

(use-package exec-path-from-shell
  :init
  ; suppress warning from exec-path-from-shell about setting PATH in
  ; interactive shells.
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package a2ps
  :straight (a2ps :type git :host github :repo "tels7ar/emacs")
  :init
  (setq a2ps-switches '("--medium=Letter" "-1" "-s2")))

; projectile is a project management system
(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

; save more emacs backup files and put them in a central location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq ispell-program-name "aspell")

; bind hippie-expand to meta-space
(global-set-key "\M- " 'hippie-expand)

(defun mark-line-and-copy ()
  "Copy the current line into the kill ring."
  (interactive)
  (beginning-of-line)
  (push-mark)
  (forward-line 1)
  (kill-ring-save (region-beginning) (region-end))
  (message "line copied"))

(global-set-key (kbd "C-c l") 'mark-line-and-copy)
(global-set-key (kbd "C-c g") 'goto-line)

(add-to-list 'auto-mode-alist '("\\.\\(asciidoc\\|adoc\\)\\'" . adoc-mode))

; Cleanup whitespace every time I save.
(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package magit
  :init
  ; bind magit-status to a key combo
  (global-set-key (kbd "C-x g") 'magit-status))

; less spacing in shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
; no tabs!
(setq-default indent-tabs-mode nil)

; now go and mess withe the modeline colors with powerline and some custom colors
(use-package powerline
  :init
  (powerline-default-theme))

; make the region selector a proper solarized color
(set-face-attribute 'region nil :background "#93a1a1")

(set-face-attribute 'mode-line nil
                    :foreground "#7CCAFA"
                    :background "#434444"
                    :box nil
                    :inverse-video nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :foreground "#56A8D3"
                    :background "#444544"
                    :inverse-video nil)

;; use python-mode instead of the one that comes with emacs
(use-package python-mode
  :init
  (setq py-shell-name "ipython")

  (setq-default py-split-windows-on-execute-function 'split-window-horizontally))

(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(setq python-shell-interpreter "ipython"
  python-shell-interpreter-args "-i --simple-prompt")

(use-package py-isort
  :init
  :hook (before-save-hook . py-isort-before-save))

(use-package blacken
  :init
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package buftra
  :straight (buftra :type git :host github :repo "tels7ar/emacs"))

(use-package py-autoflake
  :straight (py-autoflake :type git :host github :repo "humitos/py-cmd-buffer.el")
  :hook (python-mode . py-autoflake-enable-on-save)
  :config
  (setq py-autoflake-options
        '("--expand-star-imports --remove-all-unused-imports --remove-unused-variables")))

(use-package py-docformatter
  :straight (py-docformatter :type git :host github :repo "humitos/py-cmd-buffer.el")
  :hook (python-mode . py-docformatter-enable-on-save)
  :config
  (setq py-docformatter-options
        '("--pre-summary-newline --make-summary-multi-line --wrap-summaries 100 --wrap-descriptions 100")))

(use-package py-pyment
  :straight (py-pyment :type git :host github :repo "humitos/py-cmd-buffer.el"))

(use-package python-docstring
  :straight t
  :hook (python-mode . python-docstring-mode))
