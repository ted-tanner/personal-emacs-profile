;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower GC threshold back to a reasonable value. Also, split frame into
;; two windows
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 40 1024 1024))
            (split-window-horizontally)))

;; Set theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'deeper-blue t)

;; Stop Emacs from losing undo information by setting very high limits for undo buffers
(setq undo-limit (* 20 1024 1024))
(setq undo-strong-limit (* 20 1024 1024))

;; Default frame size
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 50))
(set-frame-position (selected-frame) 240 120)

;; Use M-<arrow> to navigate directionally between windows
(windmove-default-keybindings 'meta)

;; Don't show Emacs welcome screen
(setq inhibit-startup-screen t)

;; Use ibuffer instead of buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use ~/.emacs.d/desktops to save desktops
(setq desktop-path '("~/.emacs.d/desktops"))

;; Show column number
(setq column-number-mode t)

;; Enable Ido mode and set <tab> (and Shift-<tab>) to cycle through options
(ido-mode t)
(define-key ido-file-completion-map (kbd "<tab>") #'ido-next-match)
(define-key ido-buffer-completion-map (kbd "<tab>") #'ido-next-match)
(define-key ido-file-completion-map (kbd "<S-tab>") #'ido-prev-match)
(define-key ido-buffer-completion-map (kbd "<S-tab>") #'ido-prev-match)

(defun ido-kill-emacs-hook ()
  (ignore-errors (ido-save-history)))

;; Indent with spaces, not tabs
(setq-default indent-tabs-mode nil)

;; Open shell and buffer list in current window
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
(push (cons "\\*Buffer List\\*" display-buffer--same-window-action) display-buffer-alist)

;; Don't try to perfrom slow operations on really long lines
(global-so-long-mode 1)
(setq so-long-threshold 10000)
(setq so-long-max-lines 1000000)

;; Always accept 'y' or 'n' instead of requiring 'yes' or 'no'
(defalias 'yes-or-no-p #'y-or-n-p)

;; Don't show menubar or toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

;; Some trust and customize extensions
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("35cbbf5522ad9df6fea6ea312d5225538a6e97c589b508bdf18cfdc9f4b305a4" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "05ab3d9e5b552e594f1b6e83d0540542c599083c8ac87c58ff5bff3a18fcba19" "399bce2ec203f474cdd3e4463863011dab044da9618b9f398785714d64e1cb1c" "d2e44214a7dc0bd5b298413ed6c3ba9719f1d96794d9de3bdf7a9808902fd098" default))
 '(horizontal-scroll-bar-mode nil)
 '(linum-format " %5i ")
 '(scroll-bar-mode nil)
 '(so-long-variable-overrides
   '((bidi-inhibit-bpa . t)
     (bidi-paragraph-direction . left-to-right)
     (global-hl-line-mode)
     (line-move-visual . t)
     (show-paren-mode)
     (truncate-lines)
     (which-func-mode)))
 '(window-divider-mode t))

;; Custom theme overrides
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:extend t :background "DodgerBlue4"))))
 '(window-divider ((t (:background "gray60" :foreground "gray80")))))

;; Don't poop backup files everywhere (put them in a temp directory)
(setq temporary-file-directory "~/.emacs.d/.tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Put lock files in a temp directory as well
;; (setq lock-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

;; Disable lock files altogether
(setq create-lockfiles nil)

;; Automatically revert buffers when changed on disk
(global-auto-revert-mode 1)

;; Don't remap C-x to C-c when using `M-x term`
(add-hook 'term-mode-hook
          (lambda ()
            (term-set-escape-char ?\C-x)
            (local-set-key (kbd "M-y") #'yank-pop)
            (local-set-key (kbd "M-w") #'kill-ring-save)))

;; Bind next/previous-error to M-} and M-{

(global-set-key (kbd "M-}") #'next-error)
(global-set-key (kbd "M-{") #'previous-error)

;; Bind scroll 6 lines at a time to M-n and M-p. If prog-mode, remap
;; to forward-paragraph and backward-paragraph respectively
(defun jump-multiple-lines-forward ()
  (interactive)
  (forward-line 6))

(defun jump-multiple-lines-backward ()
  (interactive)
  (forward-line -6))

(global-set-key (kbd "M-n") #'jump-multiple-lines-forward)
(global-set-key (kbd "M-p") #'jump-multiple-lines-backward)

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") #'forward-paragraph)
            (local-set-key (kbd "M-p") #'backward-paragraph)))

;; Respect M-n and M-p key bindings in shell, eww, and occur modes. For
;; shell mode, remap next and previous command scrolling to M-N and M-P
;; respectively
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") #'jump-multiple-lines-forward)
            (local-set-key (kbd "M-p") #'jump-multiple-lines-backward)
            (local-set-key (kbd "M-N") #'comint-next-input)
            (local-set-key (kbd "M-P") #'comint-previous-input)))

(add-hook 'eww-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") #'jump-multiple-lines-forward)
            (local-set-key (kbd "M-p") #'jump-multiple-lines-backward)))

(add-hook 'occur-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") #'jump-multiple-lines-forward)
            (local-set-key (kbd "M-p") #'jump-multiple-lines-backward)))

;; C-c M-o should clear buffer in Eshell
(defun eshell-clear-buffer ()
  "Clear Eshell buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") #'eshell-clear-buffer)))

;; Be intelligent when using tab to indent or autocomplete
(defun indenting-and-completing-tab ()
  "Will indent if cursor is at beginning of a line, or if a region is
selected, or if it otherwise makes sense to indent rather than autocomplete.
Otherwise, it will use dabbrev-expand to auto-complete. It leaves shells and
the minibuffer alone."
  (interactive)
  (if (string-match "Minibuf" (buffer-name))
      (minibuffer-complete)
    (if (or (derived-mode-p 'comint-mode) (derived-mode-p 'eshell-mode))
        (completion-at-point)
      (if mark-active
          (indent-region (region-beginning)
                         (region-end))
        (if (looking-at "\\>")
            (dabbrev-expand nil)
          (indent-for-tab-command))))))

(global-set-key (kbd "<tab>") #'indenting-and-completing-tab)

;; dabbrev-expand should be case-sensitive
(setq dabbrev-case-fold-search nil)

;; Search in all buffers
(defun search-in-all-buffers (regexp &optional allbufs)
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers "." regexp allbufs))

(global-set-key (kbd "M-s") 'search-in-all-buffers)

;; Bind C-c C-q to quick-calc (override the key binding for C mode)
(global-set-key (kbd "C-c C-q") #'quick-calc)
(add-hook 'c-mode-hook
          (lambda () (local-set-key (kbd "C-c C-q") #'quick-calc)))

;; Bind C-x j to imenu (jump between function declarations)
(global-set-key (kbd "C-x j") #'imenu)

;; Bind C-x r to replace-string
(global-set-key (kbd "C-x r") #'replace-string)

;; Bind C-<tab> to cycle through buffers
(global-set-key (kbd "C-<tab>") #'next-buffer)

;; Rust Language support
(add-to-list 'load-path "~/.emacs.d/master-modes/rust-mode-master")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Go Language support
(add-to-list 'load-path "~/.emacs.d/master-modes/go-mode-master")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; Dart Language Support
(add-to-list 'load-path "~/.emacs.d/master-modes/dart-mode-master")
(autoload 'dart-mode "dart-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

;; Swift Language Support
(add-to-list 'load-path "~/.emacs.d/master-modes/swift-mode-master")
(autoload 'swift-mode "swift-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

;; Kotlin Language Support
(add-to-list 'load-path "~/.emacs.d/master-modes/kotlin-mode-master")
(autoload 'kotlin-mode "kotlin-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))

;; Markdown support
(add-to-list 'load-path "~/.emacs.d/master-modes/markdown-mode-master")
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Associate .env files with shell script maste mode
(add-to-list 'auto-mode-alist '("\\.env\\'" . shell-script-mode))

;; In C, don't indent braces according to GNU style
(setq c-default-style "linux"
      c-basic-offset 4)

;; In C, use C++ style comments (only supported for C99 and beyond)
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end "")))

;; Some functions for seeing garbage collection information
(defun report-gc-elapsed-time ()
  "Reports how much time has been spent doing garbage collection in the
   current session"
  (interactive)
  (message (format "Spent a total of %.4f seconds in the garbage collector during this session"
                   gc-elapsed)))

(defun report-gc-count ()
  "Reports how many times the garbage collector has been run during the
   current session"
  (interactive)
  (message (format "%d garbage collection(s) during this session" gcs-done)))

(defun report-avg-time-per-gc ()
  "Reports how long, on average, each garbage collection has taken during
   this session"
  (interactive)
  (message (format "Garbage collections have taken an average of %.4f seconds during this session"
                   (/ gc-elapsed gcs-done))))

;; Display init time on startup
(defun display-startup-echo-area-message ()
  (message (concat "Startup time: " (emacs-init-time))))

;; Create function for simultaneously renaming file and buffer
(defun rename-file-and-buffer (new-name)
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "No file associated with buffer '%s'" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
(fset 'rename-buffer-and-file (symbol-function 'rename-file-and-buffer))

;; Enable shortcuts that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
