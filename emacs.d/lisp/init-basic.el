;; -*- coding: utf-8; lexical-binding: t; -*-
(setq frame-title-format '("%b"))
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (fringe-mode 4)) ;;default 8
(menu-bar-mode -1)
;; (setq initial-buffer-choice t)

(setq-default major-mode 'text-mode
              fill-column 78
              tab-width 4
              indent-tabs-mode nil)

(dolist (c '(narrow-to-region
             narrow-to-page
             upcase-region
             downcase-region
             dired-find-alternate-file))
  (put c 'disabled nil))
(dolist (c '(overwrite-mode))
  (put c 'disabled t))

(setq inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash  t)  ; Deleting files go to OS's trash folder

(setq-default abbrev-mode t)
(setq kill-buffer-delete-auto-save-files 1)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq use-dialog-box nil)
;; (setq mode-line-compact t)
(setq apropos-sort-by-scores t)
(setq auto-save-default t)
(setq auto-save-no-message t)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq kill-do-not-save-duplicates t)
(setq delete-auto-save-files t)  ;; when save or kill buffer
(setq isearch-allow-motion t)
(setq browse-url-browser-function 'eww-browse-url)  ;; in emacs use eww as web browser
(setq create-lockfiles nil)
(setq require-final-newline t)

(if (> emacs-major-version 28)
    (pixel-scroll-precision-mode 1)
  ;; Better scroll behavior
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)
  )

(setq mouse-yank-at-point t)
(setq x-underline-at-descent-line t)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)

(global-set-key (kbd "C-<tab>") 'tab-next)
(global-set-key (kbd "C-S-<tab>") 'tab-previous)

(defun yx-global-inbuilt-mirror-mode-setup ()
  (global-tab-line-mode -1)
  (electric-pair-mode 1)
  (windmove-default-keybindings)
  (delete-selection-mode 1)
  (global-superword-mode 1)
  (repeat-mode 1)
  (winner-mode 1)
  (global-so-long-mode 1)
  (global-hl-line-mode 1)
  (blink-cursor-mode -1)
  (midnight-mode 1)
  (file-name-shadow-mode 1) ;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-File.html

  ;; auto revert
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1)
  ;; recentf
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude '(
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "^/tmp/"))
  (setq recentf-auto-cleanup 10)
  (recentf-mode 1)
  ;; savelist
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1)
  (save-place-mode 1)
  ;; auto-save
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 10)
  ;; tab-bar
  (setq tab-bar-show 1
        tab-bar-new-button-show nil
        tab-bar-new-tab-to 'right
        tab-bar-close-button-show nil)
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  ;; minibuffer
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)
  )
(add-hook 'after-init-hook 'yx-global-inbuilt-mirror-mode-setup)

;; completion
(setq completion-ignore-case t
      completion-cycle-threshold 5 ;;or t
      tab-always-indent 'complete
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(setq-default case-fold-search t)
(setq echo-keystrokes 0.25)
(setq kill-ring-max 60)
;; eww
;; (with-eval-after-load 'eww
;;   ;; eww-auto-rename-buffe
;;   )
(dolist (m '(electric-pair-local-mode
             display-line-numbers-mode
             show-paren-mode
             hs-minor-mode))
  (add-hook 'prog-mode-hook m))
(add-hook 'prog-mode-hook
          #'(lambda ()
              (add-hook 'before-save-hook #'delete-trailing-whitespace 0 t)))

;; desktop.el
(setq sesktop-load-locked-desktop t)
(setq desktop-path (list user-emacs-directory))
(setq desktop-dirname user-emacs-directory)
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (when (daemonp)
                                    (setq desktop-save t)
                                    (desktop-save-mode 1)
                                    (desktop-read)))
          )

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

;; flyspell
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

;;user defined
;;scroll 1/3 page
(defun previous-multilines ()
  "scroll down multiple lines"
  (interactive)
  (scroll-down (/ (window-body-height) 3)))
(defun next-multilines ()
  "scroll up multiple lines"
  (interactive)
  (scroll-up (/ (window-body-height) 3)))
(global-set-key "\M-n" 'next-multilines)
(global-set-key "\M-p" 'previous-multilines)


(provide 'init-basic)
