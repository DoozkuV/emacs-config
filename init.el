;;; -*- lexical-binding: t -*-

(setq gc-cons-threshold (* 50 1000 1000))

(setq native-comp-async-report-warnings-errors 'silent)

(setq user-mail-address "georgenpadron@gmail.com")

(setq inhibit-startup-message t)

(global-auto-revert-mode 1) ; Revert buffers when underlying file changes
(setq global-auto-revert-non-file-buffers t) ; Revert Dired and other buffers

(recentf-mode 1) ; Enable file history

(setq sentence-end "[.?!] ")

(defvar gp/is-laptop nil
  "Whether the config is on the laptop")
(defvar gp/is-desktop nil
  "Whether the config is on the desktop")

(cond ((string= (system-name) "doozkulaptop")
       (setq gp/is-laptop t))
      ((string= (system-name) "doozkudesktop")
       (setq gp/is-desktop t)))

(defun gp/config-path-file-expand (file-name)
  "Returns a canonicalized path of this Emacs configuration, based on the variable
`config-path'. It does not matter if you begin the path with a '/' character, the
final output will be the same.

If the file does not exist, it will be created at the specified directory."
  (let ((file-path (expand-file-name
	       (concat user-emacs-directory
		       (if (string-match "^/" file-name) "" "/")
		       file-name))))
  (unless (file-exists-p file-path)
    (write-region "" nil file-path))
  file-path))

(defun gp/set-hook-on-modes (mode-list function)
  (when (null mode-list)
    (error "`mode-list' cannot be empty!"))
  (dolist (mode (if (nlistp mode-list)
		    (list mode-list)
		  mode-list))
    (add-hook mode function)))

(setq custom-file (gp/config-path-file-expand "custom.el"))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(setq backup-by-copying t     ; don't fuck-up symlinks
   backup-directory-alist
   '(("." . "~/.emacs-backups")) ;don't litter my filesystem
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)      ; use versioned backups

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(defcustom gp/line-numbers-disabled-modes
  '(term-mode-hook
    shell-mode-hook
    vterm-mode-hook
    eshell-mode-hook
    eat-mode-hook
    inferior-python-mode-hook
    helpful-mode-hook
    mu4e-view-mode-hook
    treemacs-mode-hook
    inferior-emacs-lisp-mode-hook
    doc-view-mode-hook
    image-minor-mode-hook
    pdf-tools-enabled-hook
    mu4e-main-mode-hook)
  "A list of modes that will have their line numbers disabled by default."
  :type 'list
  ;; Update the hooks when this variable is changed
  :set (lambda (SYMBOL VALUE)
	 (set-default-toplevel-value SYMBOL VALUE)
	 (gp/set-hook-on-modes
	  gp/line-numbers-disabled-modes
	  (lambda () (display-line-numbers-mode 0)))))

(gp/set-hook-on-modes gp/line-numbers-disabled-modes
		      (lambda () (display-line-numbers-mode 0)))

(defcustom gp/electric-pair-enabled-modes
  '(prog-mode-hook
    eshell-mode-hook
    vterm-mode-hook
    term-mode-hook
    shell-mode-hook
    org-mode-hook) 
  "A list of modes that will have `electric-pair-local-mode' enabled by default."
  :type 'list
  :set (lambda (SYMBOL VALUE)
	 (set-default-toplevel-value SYMBOL VALUE)
	 (gp/set-hook-on-modes
	  gp/electric-pair-enabled-modes
	  (lambda () (electric-pair-local-mode 1)))))

(gp/set-hook-on-modes gp/electric-pair-enabled-modes
		      (lambda () (electric-pair-local-mode 1)))

(defcustom gp/auto-fill-enabled-modes
  '(org-mode-hook
    text-mode-hook)
  "A list of modes that will have `auto-fill-mode' enabled by default"
  :type 'list
  :set (lambda (SYMBOL VALUE)
	 (set-default-toplevel-value SYMBOL VALUE)
	 (gp/set-hook-on-modes
	  gp/auto-fill-enabled-modes
	  (lambda () (auto-fill-mode 1)))))

(gp/set-hook-on-modes gp/auto-fill-enabled-modes
			(lambda () (auto-fill-mode 1)))

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq use-package-always-ensure t) 
(elpaca elpaca-use-package
	(elpaca-use-package-mode))

(elpaca-wait)

(use-package gcmh
  :init (gcmh-mode 1))

(use-package evil
  :demand t ;; Prevent lazy loading
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t)
  ;; Enables evil within the minibuffer
  ;; (setq evil-want-minibuffer t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-auto-indent t)
  ;; (setq evil-lookup-func 'embark-act)

  :config
  (evil-mode 1)
  ;; Make "C-g" act like an escape button when you are in insert mode
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (gp/setup-evil-lookup-modes))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-want-unimpaired-p t)
  ;; (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-org
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; Set up variables for the function
(defvar gp/evil-lookup-modes-list
  '((lsp-mode-hook . lsp-describe-thing-at-point)
    (sh-mode-hook . gp/man-at-point)
    (org-mode-hook . gp/dict-at-point))
  "A list containing mode hooks and corresponding functions to be
  called by 'evil-lookup-func' within those modes.")

(defvar gp/evil-lookup-func-default 'helpful-at-point
  "The default function to be called by 'evil-lookup-func'")

(defun gp/setup-evil-lookup-modes ()
  "Sets up the evil lookup mode hooks" 
  (setq-default evil-lookup-func gp/evil-lookup-func-default)
  (dolist (mode-pair gp/evil-lookup-modes-list)
     (add-hook (car mode-pair)
	       (lambda ()
		 (setq-local evil-lookup-func (cdr mode-pair))))))

(defun gp/dict-at-point ()
  "Calls the `dictionary-search' function on the word at point."
  (interactive)
  (dictionary-search (word-at-point)))

(defun gp/man-at-point ()
"Runs the `man' command on the word at point"
    (interactive)
    (man (word-at-point)))

(use-package general
  :config
  (general-create-definer gp/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer gp/local-leader-keys
    :states '(normal insert visual emacs)
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")

  (gp/leader-keys
    "u" '(universal-argument :which-key "universal arg")
    ;; Toggles
    "t" '(:ignore t :which-key "toggles")
    "ta" '(auto-fill-mode :which-key "Toggle auto fill")
    "tt" '(consult-theme :which-key "Choose Theme")
    "tc" '(corfu-mode :which-key "Toggle corfu")
    "tp" '(electric-pair-mode :which-key "Toggle electric pairs")
    "to" '(gp/opacity-mode :which-key "Toggle opacity")
    "tf" '(flyspell-mode :which-key "Toggle flyspell mode")
    "tj" '(jinx-mode :which-key "Toggle jinx mode")
    "tF" '(flyspell-prog-mode :which-key "Toggle flyspell prog mode")

    ;; Window Management
    "w" '(evil-window-map :which-key "window")
    ";" '(other-window-prefix :which-key "Display Buffer New Window")
    "`" '(evil-switch-to-windows-last-buffer
	  :which-key "Switch To Last Buffer")

    ;; Buffer management
    "," '(consult-buffer :which-key "Switch Buffer")
    "<" '(consult-buffer-other-window :which-key "Switch Buffer Other Window")
    "b" '(:ignore t :which-key "buffer")
    "." '(find-file :which-key "Find Files")
    ">" '(find-file-other-window :which-key "Find Files Other Window")
    "bb" '(consult-buffer :which-key "Kill Current Buffer")
    "bk" '(kill-current-buffer :which-key "Kill Current Buffer")
    "bK" '(gp/kill-all-buffers :which-key "Kill Buffer List")
    "bc" '(clone-buffer :which-key "Clone Buffer")
    "bx" '(scratch-buffer :which-key "Scratch Buffer")
    "bi" '(ibuffer :which-key "Ibuffer")
    "bs" '(switch-to-buffer :which-key "Switch Buffer")
    "bl" '(list-buffers :which-key "List Buffers")
    "br" '(revert-buffer :which-key "Revert Buffers")

    ;; Project management
    ;; NOTE: For some reason I can't get the 'project-prefix-map' to work properly
    ;; with this keybinding, so instead this simulate key is used instead. 
    "p" '(projectile-command-map :which-key "project")
    "SPC" '(projectile-find-file :which-key "Find Project Files")
    "C-SPC" '(projectile-find-file :which-key "Find Project Files")

    ;; Open utilities
    "o" '(:ignore t :which-key "open")
    "oe" '(eshell :which-key "Open Eshell")
    "x" '(scratch-buffer :which-key "Open Org Capture")
    "X" '(org-capture :which-key "Open Org Capture")
    "oc" '(calc :which-key "Open Calculator")
    "or" '(gts-do-translate :which-key "Open Translator")
    "od" '(dictionary-search :which-key "Consult Dictionary")
    "oa" '(org-agenda :which-key "Open Org Agenda")
    "ot" '(eat :which-key "Open Terminal")
    "oT" '(eat-other-window :which-key "Open Terminal")
    "oi" '(ielm :which-key "Open Ielm")
    "or" '(gts-do-translate :which-key "Open Translator")
    "oe" '(eshell :which-key "Open Eshell")
    "oE" '(eshell-other-window :which-key "Open Eshell")
    ;; "op" '(treemacs :which-key "Open File-Tree") ; No file tree for now
    "om" '(mu4e :which-key "Open Mail")
    "ob" '(eww :which-key "Open Browser")
    ;; "j" '((lambda () (interactive) (org-capture nil "jj")) :which-key "Capture Journal")
    ;; "c" '((lambda () (interactive)
    ;;         (find-file (concat config-path "/config.org")))
    ;;       :which-key "Open Config")

    "cw" '(count-words :which-key "Count Words")

    ;; Help
    "h" '(help-command :which-key "help")

    ;; Search
    "s" '(:ignore t :which-key "search")
    "sr" '(consult-recent-file :which-key "Search Recent Files")
    "sb" '(consult-buffer :which-key "Search Buffers")
    "sg" '(consult-grep :which-key "Search Grep")
    "sm" '(consult-man :which-key "Search Man")
    "si" '(consult-info :which-key "Search Info")
    "sh" '(consult-history :which-key "Search History")
    "/" '(consult-line :which-key "Search By Buffer")

    ;; Quit
    "q" '(:ignore t :which-key "quit")
    "qr" '(restart-emacs :which-key "Restart Emacs")
    "qq" '(kill-emacs :which-key "Kill Emacs")))
;;; General adds new keys to `use-package', so we stall it here.
(elpaca-wait)

(use-package evil-nerd-commenter
  :general
  (general-define-key
   :states 'motion
   "gc" 'evilnc-comment-operator
   "gy" 'evilnc-yank-and-comment-operator))

(use-package evil-snipe
  :diminish
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  ; Set the scope of searches and repeated searches
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-repeat-scope 'visible)
  (setq evil-snipe-spillover-scope 'whole-visible))

(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;;; WHICH KEY - Pop-up keybinds 
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order-reverse)
  :config
  (setq which-key-idle-delay 0.25))

(use-package evil-cleverparens
  :hook ((racket-mode emacs-lisp-mode) . evil-cleverparens-mode))

(use-package vertico
  :diminish
  :bind (:map vertico-map ; Neat vimlike binds
	      ("C-j" . vertico-next)  
	      ("C-k" . vertico-previous)
	      ("C-f" . vertico-exit)
	      :map minibuffer-local-map
	      ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable icons for corfu. 
(use-package nerd-icons-corfu
  :after corfu)
(use-package corfu
  :custom
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-auto t) ; Enables auto-completion
  (corfu-auto-prefix 2) 
  (corfu-auto-delay 0.15) ; Delay between typing and the completion window appearing
  (corfu-quit-at-boundry 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first nil)
  :bind (:map corfu-map
	      ("M-SPC" . corfu-insert-separator)
	      ;; ("<tab>" . corfu-next)
	      ("C-j" . corfu-next)
	      ("C-k" . corfu-previous))
  :init
  ;; Use corfu everywhere
  (global-corfu-mode)
  ;; Save completion history for better sorting
  (corfu-history-mode)
  ;; Pop-up documentation by hitting `M-h'. 
  (corfu-popupinfo-mode)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

(use-package corfu-terminal 
  :if (not (display-graphic-p))
  :config (corfu-terminal-mode 1))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-elisp-symbol)
  ;;        ("C-c p e" . cape-elisp-block)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p :" . cape-emoji)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p _" . cape-tex)
  ;;        ("C-c p ^" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-fd)                  ;; Alternative: consult-find
	 ("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(setq major-mode-remap-alist
      '((css-mode . css-ts-mode)
	(javascript-mode . js-ts-mode)
	(c-mode . c-ts-mode)
	(c++-mode . c++-ts-mode)
	(python-mode . python-ts-mode)))

(use-package racket-mode
  :ensure (:source "MELPA")
  :general
  (gp/local-leader-keys
    :keymaps 'racket-mode-map
    "t" '(racket-test :which-key "Run Racket Tests")
    "r" '(:ignore t :which-key "run")
    "rr" '(racket-run-and-switch-to-repl :which-key "Run and Switch to REPL")
    "rp" '(racket-run-module-at-point :which-key "Run Module at
  Point")))

(defun gp/markdown-preview-eww ()
  "Generates a preview of the currently open markdown file in eww"
  (interactive)
  (shr-render-buffer (markdown)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do))
  :general
  (gp/local-leader-keys
    :keymaps 'markdown-mode-map
    "r" '(gp/markdown-preview-eww :which-key "Preview in Eww")))

(use-package sweeprolog)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode 1)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '( "~/Projects")))
  (setq projectile-switch-project-action #'projectile-find-file))

(use-package all-the-icons
  :if (display-graphic-p))

(defun gp/dired-ripdrag (&optional args)
  "Call ripdrag on current file or all marked (or next ARG) files."
  (interactive (list (dired-get-marked-files nil current-prefix-arg))
	       dired-mode)
  (apply 'call-process "ripdrag" nil nil nil (mapcar 'expand-file-name args)))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package dirvish
  :general
  (gp/leader-keys
    "j" '(dirvish-dwim :which-key "Dired Jump")) 
  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   "H" 'dired-hide-dotfiles-mode ; See dired-hide-dotfiles
   ;; "z" 'zoxide-travel  
   "q" 'dirvish-quit
   "h" 'dired-up-directory
   "l" 'dired-find-file
   "E" 'gp/dired-ripdrag)
  :custom
  ;; Sets the attributes that are shown on each file 
  (dirvish-attributes '(file-size file-time all-the-icons vc-state))
  :init (dirvish-override-dired-mode))

(use-package org
  :ensure nil
  :preface
  (defvar gp/org-directory "~/Documents/org"
    "Directory of org files within this configuration")
  ;; :hook
  ;; (org-mode . flyspell-mode)
  :commands
  (org-timer-set-timer)
  :general
  (gp/local-leader-keys
    :keymaps 'org-mode-map
    "b" '(org-babel-tangle :which-key "Babel Tangle")
    "i" '(org-insert-link :which-key "Insert Link")
    "y" '(org-store-link :which-key "Store Link")
    "r" '(org-id-get-create :which-key "Generate ID for heading")
    "d" '(org-deadline :which-key "Set Deadline")
    "q" '(org-set-tags-command :which-key "Set Tags")
    "e" '(org-export-dispatch :which-key "Export")
    "l" '(org-latex-preview :which-key "Preview Latex")
    "h" '(gp/org-toggle-emphasis-markers :which-key "Toggle Emphasis Markers")
    "o" '(consult-outline :which-key "Toggle Emphasis Markers")
    "x" '(org-toggle-checkbox :which-key "Toggle Emphasis Markers"))
  ;; Open links with the enter key
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "RET" 'org-open-at-point)

  :config
  ;; Make it so org mode always starts folded
  (setq org-startup-folded 'showeverything)
  ;; Change how org folds display when minimized
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)

  ;; Basisc org agenda setup
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Where org mode looks for agenda files
  (setq org-agenda-files
	`(,gp/org-directory))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Custom org links are set here
  (setq org-link-abbrev-alist
	'(("spellwiki" . "http://dnd5e.wikidot.com/spell:")))
  ;; Custom todo keywords
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)
	  ("tasks.org" :maxlevel . 1)))

  ;; Template for org capture
  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp ,(concat gp/org-directory "/tasks.org") "Inbox")
	   "* TODO %?\n %U\n %i" :empty-lines 1)
	  ("n" "Notes")
	  ("na" "Algorithm Notes" entry
	   (file+olp+datetree ,(concat gp/org-directory "/notes/algorithms.org"))
	   "* %<%I:%M %p> - CS 3250 Algorithms :notes:\n\n%?\n")
	  ("np" "Progamming Languages Notes" entry
	   (file+olp+datetree ,(concat gp/org-directory
				       "/notes/programming-languages.org"))
	   "* %<%I:%M %p> - CS 3270 Programming Languages :notes:\n\n%?\n")
	  ("ng" "Geology Notes" entry
	   (file+olp+datetree ,(concat gp/org-directory
				       "/notes/geology.org"))
	   "* %<%I:%M %p> - EES 1510 Dynamic Earth: Intro Geology :notes:\n\n%?\n")
	  ("j" "Journal / Writing")
	  ("jm" "Musings Journal" entry
	   (file+olp+datetree ,(concat gp/org-directory "/journal/musings.org"))
	   "* %<%I:%M %p> - %^{Insert Name|Musing} :journal:\n\n%?\n"
	   :clock-in :clock-resume
	   :empty-lines 1)
	  ("jj" "Personal Journal" entry
	   (file+olp+datetree ,(concat gp/org-directory "/journal/journal.org"))
	   "* %<%I:%M %p> - Journal :journal:\n\n%?\n"
	   :clock-in :clock-resume
	   :empty-lines 1)))
  ;; Load org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  ;; Load exporting org-mode into markdown
  (require 'ox-md nil t)
  ;; Enable tempo in org mode
  (require 'org-tempo)
  ;; Create babel tangle presets 
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (add-to-list 'org-structure-template-alist '("gd" . "src gdscript")))

(defun gp/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (gp/config-path-file-expand "config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; Hook it
(add-hook 'org-mode-hook
	  (lambda () (add-hook 'after-save-hook #'gp/org-babel-tangle-config )))

(use-package org-roam
  ;; :after org
  :commands (org-roam-node-insert org-roam-node-find org-roam-capture)
  :general
  (gp/leader-keys
    "r" '(:ignore t :which-key "roam")
    "ri" '(org-roam-node-insert :which-key "Node Insert")
    "rf" '(consult-org-roam-file-find :which-key "Node Find")

    "rl" '(consult-org-roam-backlinks :which-key "Find Roam Backlinks")
    "rL" '(consult-org-roam-forward-links :which-key "Find Roam Forward Links")

    "rs" '(consult-org-roam-search :which-key "Search in Roam")
    "rb" '(consult-org-roam-buffer :which-key "Search Roam Buffers") 
    "rc" '(org-roam-capture :which-key "Node Capture")

    "rq" '(org-roam-tag-add :which-key "Add Filetags")
    "ru" '(org-roam-ui-open) :which-key "Open Roam UI")
  :config
  (setq org-roam-directory (file-truename (concat gp/org-directory "/roam")))
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?" :target
	   (file+head "${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t))))

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  ;; Set `ripgrep' as the default 
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-."))

(use-package websocket
  :after org-roam)
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable))

(use-package transient) ;; Fix a weird bug with elpaca
(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :general
  (gp/leader-keys
    "g" '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "git status")
    "gg" '(magit :which-key "git open")
    "gd" '(magit-dispatch :which-key "git dispatch")
    "gf" '(magit-file-dispatch :which-key "git file dispatch")))

(use-package eat
  :general
  (gp/local-leader-keys
    :keymaps 'eat-mode-map
    "j" '(eat-mode-map :which-key "Semi-Char Mode")
    "l" '(eat-line-mode :which-key "Line Mode")
    "n" '(eat-next-shell-prompt :which-key "Next Prompt")
    "p" '(eat-previous-shell-prompt :which-key "Previous Prompt")
    "c" '(eat-char-mode :which-key "Char Mode")))

(defun gp/configure-eshell ()
  (setq eshell-history-size 10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(defun eshell-other-window ()
  "Open `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))
;; Eshell
(use-package eshell
  :ensure nil
  :hook (eshell-first-time-mode . gp/configure-eshell)
  :bind
  ("C-c o e" . eshell)
  ("C-x 4 e" . eshell-other-window))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-did-you-mean
  :after eshell
  :config
  (eshell-did-you-mean-setup))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol))

(use-package jinx
  :hook
  ((prog-mode text-mode org-mode conf-mode) .
   jinx-mode)
  :bind (:map jinx-mode-map
	      ("C-;" . jinx-correct)
	      ("M-$" . jinx-correct)
	      ("C-M-$" . jinx-languages)))

(use-package powerthesaurus
  :general
  (gp/leader-keys
    "op" '(powerthesaurus-transient :which-key "Open Powerthesaurus")))

(use-package password-store
  :defer)

(use-package pdf-tools
  :init
  (pdf-loader-install))

(use-package speed-type
  :commands (speed-type-text
	     speed-type-top-x
	     speed-type-buffer
	     speed-type-region
	     speed-type-top-100)
  :config
  (add-hook 'speed-type-mode-hook (lambda () (setq-local evil-default-state 'insert))))

;; page-break-lines to start on dashboard mode
(use-package page-break-lines
  :hook (dashboard-mode . page-break-lines-mode))

;; Dashboard configuration
(use-package dashboard
  :init
  ;; Set what appears on the dashboard
  (setq dashboard-items '((projects . 10)
			  (recents . 10)
			  (agenda . 5)))
  ;; Make sure that projectile is used for projects
  (setq dashboard-projects-backend 'projectile)
  ;; Set it to use the weekly agenda
  (setq dashboard-week-agenda t)

  ;; Enable dashboard icons
  (setq dashboard-set-heading-icons t
	dashboard-icon-type 'nerd-icons
	dashboard-set-file-icons t)
  ;; Set a nicer default logo
  (setq dashboard-startup-banner 'logo)
  ;; (setq dashboard-startup-banner 
  ;; 	  (gp/config-path-file-expand "logos/black-hole.png"))
  ;; More dashboard info
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer t)

  ;; Utilize the page separator package
  (setq dashboard-page-separator "\n\n")
  ;; Initial buffer choice
  (setq initial-buffer-choice (lambda () (dashboard-open)))
  :config
  ;; Elpaca setup
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(defun gp/test-config (config-path)
  "Creates an emacs instance and loads the configuration at the specified path"
  (interactive "DChoose a config path: ")
  (let ((config-path (expand-file-name config-path)))
    (unless (f-directory-p config-path)
      (error "Invalid directory entered"))
    (start-process
     "Emacs-Test-Config" nil
     "emacs" "--init-directory"
     config-path
     "--debug-init")))

(use-package gptel
  :preface
  ;; Define utility functions for the config
  (defun gp/get-ollama-installed-models ()
    "Uses awk to get a list of installed ollama models."
    (split-string (shell-command-to-string
		   "ollama list | awk 'NR>1 {print $1}'")))
  :commands (gptel gptel-menu gptel-mode gptel-send gptel-set-topic)
  :general
  (gp/leader-keys
    "l" '(:ignore t :which-key "llm")
    "ls" '(gptel-send :which-key "Send point to LLM")
    ;;  Calls gptel-send with the universal arg enabled
    "lS" '((lambda () (interactive)
	    (let ((current-prefix-arg 4))
	      (call-interactively 'gptel-send)))
	  :which-key "Transient send point to LLM")
    "lb" '(gptel :which-key "Start Chat")
    ;; :keymaps 'org-mode-map
    "lt" '(gptel-set-topic :which-key "Set context to Org Heading"))
  :config
  (setq
   gptel-model "mistral:latest"
   gptel-backend (gptel-make-ollama "Ollama"
				    :host "localhost:11434"
				    :stream t
				    :models (gp/get-ollama-installed-models)))
  ;; Set gptel buffers to use org mode
  (setq gptel-default-mode 'org-mode)
  ;; Doesn't work with `auto-fill-mode', so instead we use `visual-line-mode'
  (add-hook 'gptel-mode-hook 'visual-line-mode))

;;; UTILITY FUNCTIONS FOR DEALING WITH ARCH/PACMAN

;; NOTE: These functions are all run utilizing the yay package
;; which can be downloaded from the AUR
;; THEY WILL NOT WORK WITHOUT YAY INSTALLED

(defvar gp/sudo-program "sudo"
  "A string referring to the command to be used by arch package install commands")
;; (setq gp/sudo-program "doas")

(defvar gp/arch-use-yay t
  "Use yay for arch commands if installed")

(defun gp/arch-update ()
  "Runs the pacman/yay shell command to automatically update the system on Arch Linux"
  (interactive)
  (gp/arch-command "-Syyu" nil))

(defun gp/arch-install (program)
  "Runs the Yay shell command to install the inputted program"
  (interactive "MProgram Name: ")
  (gp/arch-command "-S" program))

(defun gp/arch-uninstall (program)
  "Runs the shell command to delete the inputted program"
  (interactive "MProgram Name: ")
  (gp/arch-command "-Rns" program))

(defun gp/arch-search (query)
  "Runs pacman -Ss utilizing the inputted query"
  (interactive "MQuery: ")
  (gp/arch-command "-Ss" query))

(defun gp/arch-query (query)
  "Runs pacman -Qs utilizing the inputted query"
  (interactive "MQuery: ")
  (gp/arch-command "-Qs" query))

(defun gp/arch-find-package-with-file (file)
  "Runs pacman -F to search for package containing `file'"
  (interactive "MQuery: ")
  (gp/arch-command "-F" file))

(defun gp/arch-update-file-database (file)
  "Runs pacman -Fy to update the file database"
  (interactive)
  (gp/arch-command "-Fy" nil))


(defun gp/arch-command (args programs)
  "Runs either arch or pacman with `gp/sudo-program', with the specified args and programs
If programs is nil, it will act as if nothing is there."
  (let ((pacman-executable (if (and (executable-find "yay") gp/arch-use-yay)
			       (format "yay --sudo %s" gp/sudo-program)
			     (format "%s pacman" gp/sudo-program))))
    (async-shell-command (concat pacman-executable " " args " " programs))))

(gp/leader-keys
  "a" '(:ignore t :which-key "arch")
  "au" '(gp/arch-update :which-key "Arch Update")
  "ai" '(gp/arch-install :which-key "Arch Install")
  "ad" '(gp/arch-uninstall :which-key "Arch Delete")
  "as" '(gp/arch-search :which-key "Arch Search")
  "ay" '(gp/arch-update-file-database :which-key "Arch Update File Database")
  "af" '(gp/arch-find-package-with-file :which-key "Arch Find Package With File")
  "aq" '(gp/arch-query :which-key "Arch Query"))

(scroll-bar-mode -1)    ; Disable visual scrollbar
(tool-bar-mode -1)      ; Disable toolbar
(tooltip-mode -1)       ; Disable tooltips
(menu-bar-mode -1)      ; Disable menubar
(set-fringe-mode 10)    ; Fringes on the sides
(setq use-dialog-box nil) ; Don't pop up UI dialogs when prompting

(setq tab-bar-show 1 ; Show tab bar only when more than 1 tab present
      tab-bar-new-button-show nil ; Disable new and close button on tab bar
      tab-bar-close-button-show nil
      tab-bar-auto-width nil) ; Make tab-bar width change dynamically

;; Disable margin in specific buffer types
(gp/set-hook-on-modes
 '(shell-mode-hook
   compilation-mode-hook
   rustic-compilation-mode-hook
   TeX-output-mode-hook)
 (lambda () (setq-local scroll-margin 0)))

(use-package doom-themes
  :if gp/is-desktop
  :config (load-theme 'doom-dracula :no-confirm)
  :commands (load-theme consult-theme))

(use-package catppuccin-theme
  :if gp/is-laptop
  :config
  (load-theme 'catppuccin :no-confirm))

(set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height 110)

(use-package doom-modeline
  :custom
  (doom-modeline-icon t)
  (doom-modeline-enable-word-count nil)
  :init (doom-modeline-mode 1)
  :config
  (when gp/is-laptop
    (display-battery-mode 1)))

(defvar gp/background-opacity 75
  "The default opacity of the background when transparency mode is toggled on.")

(define-minor-mode gp/opacity-mode
  "Enables background frame opacity"
  :lighter " op"
  :global t
  (if gp/opacity-mode
      ;; Turn on opacity by setting the alpha value of the current
      ;; and all future frames
      (progn
	(set-frame-parameter nil 'alpha-background gp/background-opacity)
	(add-to-list 'default-frame-alist `(alpha-background . ,gp/background-opacity)))
    ;; Turn off the opacity otherwise 
    (set-frame-parameter nil 'alpha-background 100)
    (assq-delete-all 'alpha-background default-frame-alist)))

(provide 'gp/opacity-mode)
;; Automatically enable transparency at launch
(gp/opacity-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(pixel-scroll-precision-mode)

(use-package mu4e
  ;; Mu is a package installed /outside/ of emacs
  :ensure nil
  :bind
  ("C-c o m" . mu4e)
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t
	mu4e-use-maildirs-extension nil)


  ;; Referesh mail using isync every 10 minutes
  ;; NOTE: This is disabled in this config as this is being handled instead
  ;; by a bash script
  (setq mu4e-update-interval (* 10 60)
	mu4e-get-mail-command "mbsync -a"
	mu4e-maildir "~/.local/share/mail")

  ;; Configuring SMTP to work properly with gmail
  (setq message-send-mail-function 'smtpmail-send-it
	starttls-use-gnutls t
	smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587)

  ;; Enable authentication via `pass' 
  (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  (setq auth-source-debug t)

  (setq mu4e-contexts
	(list
	 ;; Personal Account
	 (make-mu4e-context
	  :name "Professional"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/georgenpadron@gmail.com" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "georgenpadron@gmail.com")
		  (user-full-name . "George N Padron")
		  (mu4e-drafts-folder . "/georgenpadron@gmail.com/[Gmail]/Drafts")
		  (mu4e-sent-folder . "/georgenpadron@gmail.com/[Gmail]/Sent")
		  (mu4e-refile-folder . "/georgenpadron@gmail.com/[Gmail]/All Mail")
		  (mu4e-trash-folder . "/georgenpadron@gmail.com/[Gmail]/Trash")
		  (mu4e-maildir-shortcuts .
					  (("/georgenpadron@gmail.com/INBOX" . ?i)
					   ("/georgenpadron@gmail.com/[Gmail]/Sent Mail" . ?s)
					   ("/Georgenpadron@gmail.com/[Gmail]/Trash" . ?t)
					   ("/georgenpadron@gmail.com/[Gmail]/Drafts" . ?d)
					   ("/georgenpadron@gmail.com/[Gmail]/All Mail" . ?a)))
		  (smtpmail-mail-address . "georgenpadron@gmail.com")
		  (smtpmail-smtp-user . "georgenpadron@gmail.com")))

	 ;; Wealth Account
	 (make-mu4e-context
	  :name "Wealth"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/wealth2005@gmail.com" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "wealth2005@gmail.com")
		  (user-full-name . "George N Padron")
		  (mu4e-drafts-folder . "/wealth2005@gmail.com/[Gmail]/Drafts")
		  (mu4e-sent-folder . "/wealth2005@gmail.com/[Gmail]/Sent Mail")
		  (mu4e-refile-folder . "/wealth2005@gmail.com/[Gmail]/All Mail")
		  (mu4e-trash-folder . "/wealth2005@gmail.com/[Gmail]/Trash")
		  (mu4e-maildir-shortcuts .
					  (("/wealth2005@gmail.com/INBOX" . ?i)
					   ("/wealth2005@gmail.com/[Gmail]/Sent Mail" . ?s)
					   ("/wealth2005@gmail.com/[Gmail]/Trash" . ?t)
					   ("/wealth2005@gmail.com/[Gmail]/Drafts" . ?d)
					   ("/wealth2005@gmail.com/[Gmail]/All Mail" . ?a)))
		  (smtpmail-mail-address . "wealth2005@gmail.com")
		  (smtpmail-smtp-user . "wealth2005@gmail.com")))

	 ;; george.n.padron@vanderbilt.edu Account
	 (make-mu4e-context
	  :name "Vanderbilt"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/george.n.padron@vanderbilt.edu" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "george.n.padron@vanderbilt.edu")
		  (user-full-name . "George N Padron")
		  (smtpmail-smtp-server . "smtp.gmail.com")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type . ssl)
		  (mu4e-drafts-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/Drafts")
		  (mu4e-sent-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/Sent Mail")
		  (mu4e-refile-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/All Mail")
		  (mu4e-trash-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/Trash")
		  (mu4e-maildir-shortcuts .
					  (("/george.n.padron@vanderbilt.edu/INBOX" . ?i)
					   ("/george.n.padron@vanderbilt.edu/[Gmail]/Sent Mail" . ?s)
					   ("/george.n.padron@vanderbilt.edu/[Gmail]/Trash" . ?t)
					   ("/george.n.padron@vanderbilt.edu/[Gmail]/Drafts" . ?d)
					   ("/george.n.padron@vanderbilt.edu/[Gmail]/All Mail" . ?a)))
		  (smtpmail-mail-address . "george.n.padron@vanderbilt.edu")
		  (smtpmail-smtp-user . "george.n.padron@vanderbilt.edu"))))))

(use-package org-msg
  :after mu4e
  :init
  ;; First we set the default mail agent to mu4e
  (setq mail-user-agent 'mu4e-user-agent)
  ;; Now, we set some default options
  (setq org-msg-options
	"html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil"))
