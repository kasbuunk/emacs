

  (require 'package)

  ;;; PACKAGE LIST
  (setq package-archives 
        '(("melpa" . "https://melpa.org/packages/")
          ("org" . "https://orgmode.org/elpa/")
          ("elpa" . "https://elpa.gnu.org/packages/")))

  ;;; BOOTSTRAP USE-PACKAGE
  (package-initialize)
  (package-refresh-contents)
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


  (setq auto-save-file-name-transforms
            `((".*" ,(concat user-emacs-directory "auto-save/") t)))

  (use-package restart-emacs)
  

  (defvar buunk/default-font-size 120)
  (defvar buunk/default-variable-font-size 120)

  ;; Icons.
  ;; The first time this is run on a new machine, run the following command:
  ;;
  ;; M-x all-the-icons-install-fonts
  (use-package all-the-icons)

  ;; Modeline.
  (use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 15)))

  ;; Theme.
  (use-package doom-themes)
  (use-package gruvbox-theme
    :config
    (load-theme 'gruvbox t))
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(custom-safe-themes
     '("7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" default))
   '(package-selected-packages
     '(terraform-mode dired-hide-dotfiles dired-open all-the-icons-dired dired-single eshell-git-prompt evil-nerd-commenter lsp-ivy treemacs lsp-treemacs lsp-ui company-box company typescript-mode lsp-mode visual-fill-column org-bullets forge magit evil-magit counsel-projectile projectile hydra general all-the-icons peach-melpa doom-themes helpful ivy-rich which-key rainbow-delimiters doom-modeline counsel swiper ivy command-log-mode go-mode vertico undo-fu gruvbox-theme evil-collection)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  


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
    (setq ivy-re-builders-alist
          '((ivy-switch-buffer . ivy--regex-plus)))
    (setq ivy-initial-inputs-alist nil))

  ;; Which key.
  (use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.3))
  
  ;; Enable line numbers.
  (column-number-mode)
  (global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes.
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  ;; Set the default font.
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height buunk/default-font-size)

  ;; Set the fixed pitch face.
  (set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font Mono" :height buunk/default-font-size)

  ;; Set the variable pitch face.
  (set-face-attribute 'variable-pitch nil :font "Thonburi" :height buunk/default-variable-font-size :weight 'regular)

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

  ;; Key bindings.
  (use-package general
    :config
    (general-create-definer buunk/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")

    (buunk/leader-keys
     "t" '(:ignore t :which-key "toggles")
     "tt" '(counsel-load-theme :which-key "choose theme")))

  ;;; Hydra
  (use-package hydra)

  (defhydra hydra-text-scale (:timeout 4)
            "scale text"
            ("j" text-scale-increase "in")
            ("k" text-scale-decrease "out")
            ("f" nil "finished" :exit t))

  (buunk/leader-keys
     "ts" '(hydra-text-scale/body :which-key "scale text"))
 
  ;; Switch between windows.
  (global-set-key (kbd "M-<left>")  'windmove-left)
  (global-set-key (kbd "M-<right>") 'windmove-right)
  (global-set-key (kbd "M-<up>")    'windmove-up)
  (global-set-key (kbd "M-<down>")  'windmove-down)
        

  ; Copied from YouTuber System Crafters, needs revision.
  (defun buunk/evil-hook ()
    (dolist (mode '(custom-hook
                    eshell-mode
                    git-rebase-mode
                    erc-mode
                    circe-server-mode
                    circe-chat-mode
                    sauron-mode
                    term-mode))
      (add-to-list 'evil-emacs-state-modes mode)))

  ;;; Vim Bindings
  (use-package evil
    :demand t
    :bind (("<escape>" . keyboard-escape-quit))
    :init
    ;; Allows for using cgn
    ;; (setq evil-search-module 'evil-search)
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    ;; No vim insert bindings.
    (setq evil-undo-system 'undo-fu)
    :hook (evil-mode . buunk/evil-hook)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    ;; J and K navigate per visual line if it's wrapped.
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
    ;; Start in normal mode.
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

  ;;; Vim Bindings everywhere else.
  (use-package evil-collection
    :after evil
    :config
    (setq evil-want-integration t)
    (evil-collection-init))
  

  (use-package magit
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  (use-package forge)

  (setq auth-sources '("~/.authinfo.gpg"))
  

  ;; Language server protocol (LSP).
  (defun buunk/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))

  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook (lsp-mode . buunk/lsp-mode-setup)
    :init
    (setq lsp-keymap-prefix "C-c l")
    :config
    (lsp-enable-which-key-integration t))

  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))

  ;; Tree file drawer.
  ;; Error, can't find/open the treemacs dependency at melpa.
  (use-package treemacs)
  (use-package lsp-treemacs
    :after lsp)

  (use-package lsp-ivy)

  (use-package evil-nerd-commenter
    :bind ("M-/" . evilnc-comment-or-uncomment-lines))

  ;; Autocompletion.
  (use-package company
    :after lsp-mode
    :hook (lsp-mode . company-mode)
    :bind
    (:map company-active-map
          ("<tab>" . company-complete-selection))
    (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))

  (use-package company-box
    :hook (company-mode . company-box-mode))

  (use-package typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . lsp-deferred)
    :config
    (setq typescript-indent-level 2))

  ;; Go.
  (use-package go-mode)
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred)

  ;; Terraform.
  (use-package terraform-mode
    :custom (terraform-indent-level 2)
    :config
    (defun my-terraform-mode-init ()
                                   (outline-minor-mode)))
  (add-hook 'terraform-mode-hook #'outline-minor-mode)

  ;; Markdown.
  (use-package markdown-mode
    :mode ("README\\.md\\'" . gfm-mode)
    :init (setq markdown-command "multimarkdown"))

  ;; Nix.
  (use-package nix-mode
    :mode "\\.nix\\'")



  ;; Programming languages.
     (use-package rainbow-delimiters
       :hook (prog-mode . rainbow-delimiters-mode))
 

  (use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    ;; Show dired when switching projects.
    (setq projectile-project-search-path '(
                                           "~/git/github.com/kasbuunk/"
                                           "~/.config/"
                                           ("~/git/lab.weave.nl/" . 2)
                                           ("~/git/lab.weave.nl/" . 3)))
    (setq projectile-switch-project-action #'projectile-dired))

  (use-package counsel-projectile
    :config (counsel-projectile-mode))

  ;; Terminal - term.
  (use-package term
    :config
    (setq explicit-shell-file-name "zsh")
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

  ;; Terminal - vterm.
  (use-package vterm
    :commands vterm
    :config
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
    (setq vterm-max-scrollback 10000))

  ;; Eshell.
  (use-package eshell-git-prompt)

  (defun buunk/configure-eshell ()
    ;; Save command history.
    (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

    (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

    (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
    (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
    (evil-normalize-keymaps)

    (setq eshell-history-size 10000
          eshell-buffer-maximum-lines 10000
          eshell-hist-ignoredups t
          eshell-scroll-to-bottom-on-input t))

  (use-package eshell
    :hook (eshell-first-time-mode . buunk/configure-eshell)
    :config
    (with-eval-after-load 'esh-opt
      (setq eshell-destroy-buffer-when-process-dies t)
      (setq eshell-visual-commands '("htop" "zsh" "vim" "nvim" "vi")))
    (eshell-git-prompt-use-theme 'powerline))

  
  ;; File management
  ;; Dired
  (use-package dired
    :ensure nil
    :commands (dired dired-jump)
    :bind (("C-x C-j" . dired-jump))
    :custom ((dired-listing-switches "-agho --group-directories-first"))
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer))

  ;; Use and reuse single dired buffer.
  (use-package dired-single)

  ;; Show icons in dired.
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

  ;; Open file in particular program.
  (use-package dired-open
    :config
    (setq dired-open-extension '(("png" . "feh")
                                 ("mkv" . "mpv"))))

  (use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "H" 'dired-hide-dotfiles-mode))




   (defun buunk/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
            (set-face-attribute (car face) nil :font "Hack Nerd Font Mono" :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

  (defun buunk/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))

  (use-package org
    :pin org
    :hook (org-mode . buunk/org-mode-setup)
    :config
    (setq org-ellipsis " ▾")

    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

    (setq org-agenda-files
          '("~/git/github.com/kasbuunk/org/index.org"
            "~/git/github.com/kasbuunk/org/habits.org"))

    (require 'org-habit)
    (add-to-list 'org-modules 'org-habit)
    (setq org-habit-graph-column 60)

    (setq org-agenda-include-diary t)

    (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

    (setq org-refile-targets
      '(("archive.org" :maxlevel . 1)
        ("tasks.org" :maxlevel . 1)))

    ;; Save Org buffers after refiling!
    (advice-add 'org-refile :after 'org-save-all-org-buffers)

    (setq org-tag-alist
      '((:startgroup)
         ; Put mutually exclusive tags here
         (:endgroup)
         ("@errand" . ?E)
         ("@home" . ?H)
         ("@work" . ?W)
         ("agenda" . ?a)
         ("planning" . ?p)
         ("publish" . ?P)
         ("batch" . ?b)
         ("note" . ?n)
         ("idea" . ?i)))

    ;; Configure custom agenda views
    (setq org-agenda-custom-commands
     '(("d" "Dashboard"
       ((agenda "" ((org-deadline-warning-days 7)))
        (todo "NEXT"
          ((org-agenda-overriding-header "Next Tasks")))
        (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

      ("n" "Next Tasks"
       ((todo "NEXT"
          ((org-agenda-overriding-header "Next Tasks")))))

      ("W" "Work Tasks" tags-todo "+work-email")

      ;; Low-effort next actions
      ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
       ((org-agenda-overriding-header "Low Effort Tasks")
        (org-agenda-max-todos 20)
        (org-agenda-files org-agenda-files)))

      ("w" "Workflow Status"
       ((todo "WAIT"
              ((org-agenda-overriding-header "Waiting on External")
               (org-agenda-files org-agenda-files)))
        (todo "REVIEW"
              ((org-agenda-overriding-header "In Review")
               (org-agenda-files org-agenda-files)))
        (todo "PLAN"
              ((org-agenda-overriding-header "In Planning")
               (org-agenda-todo-list-sublevels nil)
               (org-agenda-files org-agenda-files)))
        (todo "BACKLOG"
              ((org-agenda-overriding-header "Project Backlog")
               (org-agenda-todo-list-sublevels nil)
               (org-agenda-files org-agenda-files)))
        (todo "READY"
              ((org-agenda-overriding-header "Ready for Work")
               (org-agenda-files org-agenda-files)))
        (todo "ACTIVE"
              ((org-agenda-overriding-header "Active Projects")
               (org-agenda-files org-agenda-files)))
        (todo "COMPLETED"
              ((org-agenda-overriding-header "Completed Projects")
               (org-agenda-files org-agenda-files)))
        (todo "CANC"
              ((org-agenda-overriding-header "Cancelled Projects")
               (org-agenda-files org-agenda-files)))))))

    (setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "~/git/github.com/kasbuunk/org/tasks.org" "Inbox")
             "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

        ("j" "Journal Entries")
        ("jj" "Journal" entry
             (file+olp+datetree "~/git/github.com/kasbuunk/org/journal.org")
             "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
             ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
             :clock-in :clock-resume
             :empty-lines 1)
        ("jm" "Meeting" entry
             (file+olp+datetree "~/git/github.com/kasbuunk/org/journal.org")
             "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
             :clock-in :clock-resume
             :empty-lines 1)

        ("w" "Workflows")
        ("we" "Checking Email" entry (file+olp+datetree "~/git/github.com/kasbuunk/org/journal.org")
             "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

        ("m" "Metrics Capture")
        ("mw" "Weight" table-line (file+headline "~/git/github.com/kasbuunk/org/metrics.org" "Weight")
         "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

    (define-key global-map (kbd "C-c j")
                (lambda () (interactive) (org-capture nil "jj")))

    (buunk/org-font-setup))

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  (require 'org-tempo)


  ;; Make org mode feel more like a rich text editor.
  (defun buunk/org-mode-visual-fill ()
    (setq visual-fill-column-width 120
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :hook (org-mode . buunk/org-mode-visual-fill))



      (use-package org-roam
        :ensure t
        :init
        (setq org-roam-v2-ack t)
        :custom
        (org-roam-directory "~/git/github.com/kasbuunk/org/roam")
        (org-roam-completion-everywhere t)
        (org-roam-dailies-capture-templates
              '(("d" "default" entry "* %<%H:%M>: %?"
               :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
        ;; Templates.
        (org-roam-capture-templates '(
          ("d" "default" plain
           "%?"
           :if-new (file+head "${slug}-%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)
          ("l" "programming language" plain
           "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("b" "book notes" plain
           (file "~/git/github.com/kasbuunk/org/template/book_notes.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "project" plain
           "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
           :unnarrowed t)
          ))
        :bind (("C-c n l" . org-roam-buffer-toggle)
               ("C-c n f" . org-roam-node-find)
               ("C-c n i" . org-roam-node-insert)
               :map org-mode-map
               ("C-M-i" . completion-at-point))
        :bind-keymap
        ("C-c n d" . org-roam-dailies-map)
        :config
        (require 'org-roam-dailies) ;; Ensure the keymap is available.
        (org-roam-setup))


  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  ;; Use python3 instead of pyhton(2).
  (setq org-babel-python-command "python3")

  ;; Disable evaluation confirmation.
  (setq org-confirm-babel-evaluate nil)



  ;; Automatically tangle Emacs.org config file upon save.
  (defun buunk/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "~/git/github.com/kasbuunk/org/emacs.org"))

      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
            (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'buunk/org-babel-tangle-config)))


