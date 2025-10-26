(setq inhibit-startup-message t)    ; Don't show startup screen

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room

(menu-bar-mode -1)      ; Disable the menu bar

;; Font face and size
(set-face-attribute 'default nil :font "Fira Code Retina" :height 150)

(load-theme 'misterioso)

;; Display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Don't display line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Key-binding to open init.el
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Key-binding to make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package command-log-mode)    ; Logs commands to a pane

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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package swiper
  :bind (("C-s" . swiper)))

;; NOTE: Run M-x all-the-icons-install-fonts on new machine to install fonts
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-monokai-octagon t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-name
  :config
  (setq which-key-idle-delay 0.3))

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

;; TODO add "s" to lal/leader-keys for save-buffer?
(use-package general
  :config
  (general-create-definer lal/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (lal/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integrqtion t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(command-log-mode commaond-log-mode counsel doom-modeline doom-themes
		      helpful ivy ivy-rich rainbow-delimiters swiper)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
