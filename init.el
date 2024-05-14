;;; Startup
;;; PACKAGE LIST
(require 'package)
(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

;;; BOOTSTRAP USE-PACKAGE
(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Clean up startup screen.
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Autocompletion.
(use-package ivy
  :diminish ;; Ignore ivy's minor modes.
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package swiper)
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; Modeline.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Theme.
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" default))
 '(package-selected-packages
   '(helpful ivy-rich which-key rainbow-delimiters doom-modeline counsel swiper ivy command-log-mode go-mode vertico undo-fu gruvbox-theme evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Which key.
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Programming languages.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package go-mode)

;; Enable line numbers.
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes.
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;; UNDO
;; Vim style undo not needed for emacs 28
;; (use-package undo-fu)

;;; Vim Bindings
;; (use-package evil
;;   :demand t
;;   :bind (("<escape>" . keyboard-escape-quit))
;;   :init
;;   ;; allows for using cgn
;;   ;; (setq evil-search-module 'evil-search)
;;   (setq evil-want-keybinding nil)
;;   ;; no vim insert bindings
;;   (setq evil-undo-system 'undo-fu)
;;   :config
;;   (evil-mode 1))

;;; Vim Bindings Everywhere else
;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (setq evil-want-integration t)
;;   (evil-collection-init))
