;; Set theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'naysayer t)

;; Default window size
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 50))
(set-frame-position (selected-frame) 240 120)

;; Some Emacs-generated stuff to trust the naysayer theme
(custom-set-variables
 '(custom-safe-themes
   '("399bce2ec203f474cdd3e4463863011dab044da9618b9f398785714d64e1cb1c" "d2e44214a7dc0bd5b298413ed6c3ba9719f1d96794d9de3bdf7a9808902fd098" default))
 '(linum-format " %5i ")
 '(window-divider-mode t))
(custom-set-faces
 '(window-divider ((t (:background "gray60" :foreground "gray80")))))

;; Don't poop backup files everywhere (put them in a system temp directory)
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

;; Don't remap C-x to C-c when using `M-x term`
(add-hook 'term-mode-hook
   (lambda ()
     (term-set-escape-char ?\C-x)
     (define-key term-raw-map "\M-y" 'yank-pop)
     (define-key term-raw-map "\M-w" 'kill-ring-save)))

;; Don't show menubar or toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Rust Language support
(add-to-list 'load-path "/Users/tanner/.emacs.d/rust-mode-master")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
