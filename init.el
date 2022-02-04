;; Set theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'naysayer t)

;; Default window size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
    (progn
      ;; Use fewer columns for smaller windows
      (if (> (x-display-pixel-width) 1280)
          (add-to-list 'default-frame-alist (cons 'width 180))
        (add-to-list 'default-frame-alist (cons 'width 100)))
      ;; Subtract a few pixels off of total screen height
      (add-to-list 'default-frame-alist 
		   (cons 'height (/ (- (x-display-pixel-height) 260)
				    (frame-char-height)))))))
(set-frame-size-according-to-resolution)



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

;; Function for commenting current line
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; Keybindings
(global-set-key (kbd "C-c C-c") 'toggle-comment-on-line)

;; Rust Language support
(add-to-list 'load-path "/Users/tanner/.emacs.d/rust-mode-master")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
