(require 'package)
(package-initialize)

;; Add package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/") t)

(require 'cl)

(defvar my-packages
  '(ack-and-a-half use-package)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; ------------------------------------------
;; Config
;; ------------------------------------------

;; Enable line numbers
(global-linum-mode t)

;; Always prefere utf-8 encoding
(prefer-coding-system 'utf-8)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Show whitespace characters (for whitespace-mode)
(setq whitespace-style '(trailing tabs tab-mark))

;; Enable whitespace mode globally
(global-whitespace-mode 1)   

;; Backups
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq make-backup-files t            ; backup of a file the first time it is saved.
      backup-by-copying t            ; don't clobber symlinks
      version-control t              ; version numbers for backup files
      delete-old-versions t          ; delete excess backup files silently
      delete-by-moving-to-trash t    ; delete files by moving to trash
      kept-old-versions 6            ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9            ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t            ; auto-save every buffer that visits a file
      auto-save-timeout 20           ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200         ; number of keystrokes between auto-saves (default: 300)
      )

(setq vs-follow-symlinks t) ;; Always follow symlinks

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; ------------------------------------------
;; Interface enhancement
;; ------------------------------------------

;; ido
(use-package ido
  :ensure t
  :config (progn
            (ido-mode t)
            (ido-everywhere t)))

;; ido-vertical-mode
(use-package ido-vertical-mode
  :ensure t
  :config (progn
            (ido-vertical-mode t)
            (setq ido-vertical-show-count t
                  ido-use-faces t)
            (set-face-attribute 'ido-vertical-first-match-face nil
                                :background nil
                                :foreground "orange")
            (set-face-attribute 'ido-vertical-only-match-face nil
                                :background nil
                                :foreground nil)
            (set-face-attribute 'ido-vertical-match-face nil
                                :foreground nil)))

;; flx
(use-package flx
  :ensure t)

;; flx-ido
(use-package flx-ido
  :ensure t
  :config (flx-ido-mode t))

;; ------------------------------------------
;; Programming
;; ------------------------------------------

;; A template system for Emacs
;; (https://github.com/capitaomorte/yasnippet)
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

;; ------------------------------------------
;; Project management
;; ------------------------------------------

;; Projectile (project management library)
(use-package projectile
  :ensure t
  :init (setq projectile-require-project-root nil)
  :config (projectile-global-mode)
  :diminish projectile-mode)
          

;; Perspective (work with multiple projects in different perspectives)
(use-package perspective
  :ensure t
  :config (persp-mode))

;; ------------------------------------------
;; Erlang
;; ------------------------------------------

(use-package erlang
  :ensure
  :init (progn
          (setq load-path (cons "~/.kerl/installs/r15b02/lib/tools-2.6.8/emacs" load-path))
          (setq erlang-root-dir "~/.kerl/installs/r15b02")
          (setq exec-path (cons "~/.kerl/installs/r15b02/bin" exec-path))))

;; ------------------------------------------
;; Javascript
;; ------------------------------------------

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :mode "\\.json\\'")

;; ------------------------------------------
;; Python
;; ------------------------------------------

(use-package jedi
  :ensure t)

;; elpy (https://github.com/jorgenschaefer/elpy)
;; Emacs Python Development Environment
(use-package elpy
  :ensure t)

;; ------------------------------------------
;; Appearence
;; ------------------------------------------

;; powerline
(use-package powerline
  :ensure t)

;; ------------------------------------------
;; Auto-completion
;; ------------------------------------------

;; auto-complete
(use-package auto-complete
  :ensure t
  :config (global-auto-complete-mode t))

;; ------------------------------------------
;; Themes
;; ------------------------------------------

;; color-theme
(use-package color-theme
  :defer t
  :ensure t)

;; zenburn
(use-package zenburn-theme
  :defer t
  :ensure t)

;; solarized
(use-package solarized-theme
  :defer t
  :ensure t)

;; ample
(use-package ample-theme
  :ensure t
  :defer t
  :init (progn
          (load-theme 'ample t t)
          (load-theme 'ample-flat t t)
          (load-theme 'ample-light t t)
          (enable-theme 'ample-flat)))

;; Moe-theme
(use-package moe-theme
  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "c006bc787154c31d5c75e93a54657b4421e0b1a62516644bd25d954239bc9933" "e8a9dfa28c7c3ae126152210e3ccc3707eedae55bdc4b6d3e1bb3a85dfb4e670" "de8fa309eed1effea412533ca5d68ed33770bdf570dcaa458ec21eab219821fd" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "d7088a7105aa09cc68e3d058f89917e07e0505e0f4ab522a6045ec8092d67c44" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
