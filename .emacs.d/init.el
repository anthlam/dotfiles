;; ===== BASE CONFIG =====

;; Disable backup files
(setq make-backup-files nil)

;; Disable autosave files
(setq auto-save-default nil)
 
;; Keep customization settings out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ===== PACKAGE SYSTEM =====

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ===== KEYBINDINGS =====

;; Key-binding to open init.el
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Key-binding to make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; TODO add "s" to lal/leader-keys for save-buffer?
(use-package general
  :config
  (general-create-definer lal/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (lal/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(counsel-ibuffer :which-key "switch buffer")
    "bd" '(kill-current-buffer :which-key "delete buffer")
    "bk" '(kill-buffer :which-key "kill buffer (select)")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")))

(use-package evil
  :init
  (setq evil-want-integrqtion t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)

  ;; Use C-g to exit insert mode
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use C-h as backspace in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Set default mode of certain buffer types to normal
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ===== UI =====

(setq inhibit-startup-message t)    ; Don't show startup screen

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room

(menu-bar-mode -1)      ; Disable the menu bar

(desktop-save-mode 1)   ; Auto-save and restore sessions

;; Display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Don't display line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font face and size
(set-face-attribute 'default nil :font "Fira Code Retina" :height 150)

(use-package doom-themes
  :init (load-theme 'doom-monokai-octagon t))

;; NOTE: Run M-x all-the-icons-install-fonts on new machine to install fonts
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-name
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
  	 ("C-x b" . counsel-ibuffer)
  	 ("C-x C-f" . counsel-find-file)
  	 :map minibuffer-local-map
  	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-zoom (:timeout 4)
  "zoom text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(lal/leader-keys
  "ts" '(hydra-text-scale/body :which-key "zoom text"))

;; ===== ORG MODE =====

(defun lal/org-font-setup ()
  ;; Set faces for org-mode heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face))))

(defun lal/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . lal/org-mode-setup)
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  
  (setq org-agenda-files
	'("~/code/me/org-mode-practice/tasks.org"
	  "~/code/me/org-mode-practice/birthdays.org"
	  "~/code/me/org-mode-practice/habits.org"))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))

  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)
	  ("tasks.org" :maxlevel . 1)))

  ;; Save org buffers after a refile
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("note" . ?n)
	  ("idea" . ?i)))

  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))))

  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/code/me/org-mode-practice/tasks.org" "Inbox")
	   "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
	  ("ts" "Clocked Entry Subtask" entry (clock)
	   "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
	   (file+olp+datetree "~/code/me/org-mode-practice/journal.org")
	   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	   :clock-in :clock-resume
	   :empty-lines 1)
	  ("jm" "Meeting" entry
	   (file+olp+datetree "~/code/me/org-mode-practice/journal.org")
	   "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
	   :clock-in :clock-resume
	   :empty-lines 1)

	  ("w" "Workflows")
	  ("we" "Checking Email" entry
	   (file+olp+datetree "~/code/me/org-mode-practice/journal.org")
	   "* Checking Email :email:\n\n%?"
	   :clock-in :clock-resume
	   :empty-lines 1)

	  ("m" "Metrics Capture")
	  ("mw" "Weight" table-line
	   (file+headline "~/code/me/org-mode-practice/metrics.org" "Weight")
	   "| %U | %^{Weight} | %^{Notes} |"
	   :kill-buffer t)))
  
  (define-key global-map (kbd "C-c j")
	      (lambda () (interative (org-capture nil "jj"))))

  (lal/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t )
   (emacs-lisp . t)
   (python . t)
   (js . t)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun lal/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/code/me/dotfiles/emacs-config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'lal/org-babel-tangle-config)))

;; ===== DEVELOPMENT =====

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (projectile-discover-projects-in-search-path)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code/me")
    (setq projectile-project-search-path '("~/code/me")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(lal/leader-keys
  "g" '(:ignore t :which-key "git")
  "gs" '(magit-status :which-key "magit status")
  "gb" '(magit-blame :which-key "magit blame"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
