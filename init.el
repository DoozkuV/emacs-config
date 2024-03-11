(setq gc-cons-threshhold (* 50 1000 1000))

(setq native-comp-async-report-warnings-errors 'silent)

(setq user-mail-address "georgenpadron@gmail.com")

(setq inhibit-startup-message t)

(global-auto-revert-mode 1) ; Revert buffers when underlying file changes
(setq global-auto-revert-non-file-buffers t) ; Revert Dired and other buffers

(recentf-mode 1) ; Enable file history

(defvar config-path "~/.config/emacs-new"
  "The default path of all of the init.el configuration files within
this Emacs config. Useful when calling Emacs from an altered
config directory using `init-directory'.")

(defun gp/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name (concat config-path "/config.org")))
    ;; Dynamic Scoping
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; Hook it
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'gp/org-babel-tangle-config )))

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

(pixel-scroll-precision-mode)

(setq gc-cons-threshhold (* 2 1000 1000))
