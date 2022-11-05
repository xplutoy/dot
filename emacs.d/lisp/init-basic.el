;; -*- coding: utf-8; lexical-binding: t; -*-

(setq-default major-mode 'fundamental-mode
              fill-column 78
              tab-width 4
              indent-tabs-mode nil
              cursor-type 'box
              abbrev-mode t)

;; windows & buffer
(global-set-key (kbd "C-S-w") 'kill-buffer-and-window)
(setq switch-to-buffer-obey-display-actions t
      switch-to-buffer-in-dedicated-window 'pop)
;; @see https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(defun yx-split-below (arg)
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'below nil))
(defun yx-split-right (arg)
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'right nil))
(defun yx-toggle-dedicated ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

(setq window-sides-slots '(3 3 3 3))
(add-to-list 'display-buffer-alist
             `(,(rx (| "*Help*" "*Dictionary*"))
               display-buffer-in-side-window
               (side . right)
               (window-width . 60)))

;; winmove
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;; auto revert
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 80)
(setq recentf-exclude '(
                        "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                        "^/tmp/"))
(setq recentf-auto-cleanup 120)
(advice-add 'recentf-cleanup :around #'(lambda (function)
                                         (let ((inhibit-message  t))
                                           (funcall function)))    
            )

;; savelist
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(savehist-mode 1)
(save-place-mode 1)

;; auto-save
(setq auto-save-default t)
(setq delete-auto-save-files t)
(setq kill-buffer-delete-auto-save-files 1)
(setq auto-save-no-message t)
(setq auto-save-visited-interval 10)
(auto-save-visited-mode 1)

;; backup
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; tab-bar
(setq tab-bar-show 1
      tab-bar-new-button-show nil
      tab-bar-new-tab-to 'right
      tab-bar-close-button-show nil)
(tab-bar-mode 1)
(tab-bar-history-mode 1)
(global-set-key (kbd "C-<tab>") 'tab-next)
(global-set-key (kbd "C-S-<tab>") 'tab-previous)

;; minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;; completion
(setq completion-ignore-case t
      completion-cycle-threshold 5 ;;or t
      tab-always-indent 'complete
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; isearch
(setq isearch-lazy-count t
      lazy-highlight-no-delay-length 3
      lazy-count-prefix-format "%s/%s ")
(setq-default case-fold-search t)
(setq isearch-allow-motion t)
(setq apropos-sort-by-scores t)

;; imenu
(global-set-key (kbd "M-i") 'imenu)

;; mouse
(setq mouse-yank-at-point t)
(if (> emacs-major-version 28)
    (pixel-scroll-precision-mode 1)
  ;; Better scroll behavior
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)
  )

;; dictionary
(global-set-key (kbd "C-c d") #'dictionary-search)
(setq dictionary-use-single-buffer t)
(setq dictionary-server "dict.tw")

;; eww
(setq-default shr-inhibit-images t
              shr-use-fonts nil)
(setq browse-url-browser-function 'eww-browse-url)  ;; in emacs use eww as web browser
(setq browse-url-generic-program
     (cond
      (ON-MAC "open")
      (ON-LINUX (executable-find "firefox"))
      (t nil)))
;; (with-eval-after-load 'eww
;;   ;; eww-auto-rename-buffe
;; )

;; desktop.el
(setq desktop-save t
      desktop-auto-save-timeout   60
      sesktop-load-locked-desktop nil
      desktop-files-not-to-save   "^$"
      desktop-dirname (concat user-emacs-directory "/desktop.saved")
      desktop-path (list desktop-dirname))
(let ((desktop-id (if (and (featurep 'server)  server-process) (concat "." server-name) "")))
  (setq desktop-base-file-name (concat "emacs.desktop" desktop-id)
        desktop-base-lock-name (concat "emacs.desktop" desktop-id ".lock")))
(when (daemonp)
  (desktop-save-mode 1)
  (desktop-read))

;; diary calendar
(setq calendar-week-start-day 1)
(setq calendar-latitude 31
      calendar-longitude 103)
(setq calendar-mark-holidays-flag t
      calendar-mark-diary-entries-flag t)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

;; dired
(add-to-list 'exec-path "/usr/local/bin")
(let ((gls (executable-find "gls")))
  (when gls
    (setq insert-directory-program gls)))
(setq dired-listing-switches "-aBhl  --group-directories-first")
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)

;; remember
(setq remember-handler-functions '(remember-diary-extract-entries))

;; ispell
(setq ispell-dictionary "en_US")
(cond
 ((executable-find "aspell")
  (setq ispell-list-command "--list") ;; @see https://www.emacswiki.org/emacs/FlySpell
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-dictionary-alist
        '((nil "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US") nil utf-8)))
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))
 )
(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; dabbrev
(global-set-key (kbd "M-/") #'dabbrev-completion)
(global-set-key (kbd "C-M-/") #'dabbrev-expand)

;; ctrl-x-x-map emacs 28
(let ((map ctl-x-x-map))              ; Emacs 28
  (define-key map "e" #'eval-buffer)
  (define-key map "f" #'follow-mode)  ; override `font-lock-update'
  (define-key map "r" #'rename-uniquely))

;; uniquity
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-strip-common-suffix t)
(setq uniquify-after-kill-buffer-p t)

;; bibtex

;; Change fields and format
(setq bibtex-dialect 'biblatex)
(setq bibtex-user-optional-fields
      '(("keywords" "Keywords to describe the entry")
        ("file" "Link to document file."))
      bibtex-align-at-equal-sign t)
(add-hook 'bibtex-mode-hook 'flyspell-mode)

;; text-mode
(add-hook 'text-mode-hook
          #'(lambda ()
              (setq word-wrap t
                    word-wrap-by-category t
                    fill-column 100)
              (auto-fill-mode 1)
              (visual-line-mode 1)
              (variable-pitch-mode 1)
              ))

;; elisp
(find-function-setup-keys)

;; paren
(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)

;; prog-mode
(setq require-final-newline t)
(add-hook 'prog-mode-hook
          #'(lambda ()
              ;; (elide-head-mode 1) emacs 29
              (subword-mode 1)
              (display-line-numbers-mode 1)
              (electric-pair-local-mode 1)
              (hs-minor-mode 1)))

;; whitespace
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook #'delete-trailing-whitespace 0 t)

;; epa
(setq epa-file-encrypt-to nil
      epa-file-select-keys nil) 
(setq epa-file-inhibit-auto-save t
      epa-file-cache-passphrase-for-symmetric-encryption nil)
(setq epa-pinentry-mode 'loopback)
;; auth-sources-pass and other
;; (setq auth-source-debug 'trivia)  ;; debug
(let ((dir (getenv "PASSWORD_STORE_DIR")))
  (when dir
    (setq auth-source-pass-filename dir))
  )
;; (auth-source-pass-enable)
(add-to-list 'auth-sources (concat user-emacs-directory "authinfo.gpg"))

;; misc global minor mode
(global-tab-line-mode -1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(global-superword-mode 1)
(repeat-mode 1)
(winner-mode 1)
(global-so-long-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode -1)
(midnight-mode 1)
(file-name-shadow-mode 1) ;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-File.html
(auto-compression-mode 1)

;; init-basic end ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-basic)
