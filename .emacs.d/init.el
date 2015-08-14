(require 'package)
(package-initialize)

;; Add package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
         '("marmalade" . "http://marmalade-repo.org/packages/") t)


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

(require 'use-package)

;; ------------------------------------------
;; Config
;; ------------------------------------------

;; Enable line numbers
(global-linum-mode t)

;; Always prefere utf-8 encoding
(prefer-coding-system 'utf-8)

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
;; Project management
;; ------------------------------------------

;; Projectile (project management library)
(use-package projectile
  :ensure t
  :config (progn
        (projectile-global-mode t)
        (setq projectile-require-project-root nil)))

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
;; Appearence
;; ------------------------------------------

;; powerline
(use-package powerline
  :ensure t)

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
