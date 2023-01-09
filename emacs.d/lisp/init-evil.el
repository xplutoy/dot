;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-default-state 'emacs
        evil-search-module 'evil-search
        evil-respect-visual-line-mode t
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t
        evil-move-cursor-back t
        evil-lookup-func #'helpful-at-point)
  ;; necessary for evil-collection
  (setq evil-want-integration t
        evil-want-keybinding nil)

  :config
  (evil-mode 1)

  ;; mode with normal states
  (defvar my-initial-evil-state-setup
    '((messages-buffer-mode . normal)
      (prog-mode . normal)
      (emacs-lisp-mode . normal)
      (org-mode . normal)
      (text-mode . normal))
    "Default evil state per major mode.")
  (dolist (p my-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))

  ;; evil re-assign "M-." to `evil-repeat-pop-next' which I don't use actually.
  ;; Restore "M-." to original binding command
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

  )

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :init
  (setq evil-collection-mode-list
        '(corfu vertico vterm
                consult bookmark diff-hl
                dired eglot ediff
                doc-view eldoc elfeed
                elisp-mode embark emoji
                eshell flymake gnus ibuffer
                imenu info lua-mode org
                org-roam python simple w3m
                which-key women xref))
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (defun evil-surround-prog-mode-hook-setup ()
    "Set up surround shortcuts."
    (when (memq major-mode '(c++-mode))
      (push '(?< . ("<" . ">")) evil-surround-pairs-alist))
    (when (memq major-mode '(sh-mode))
      (push '(?$ . ("${" . "}")) evil-surround-pairs-alist))
    (when (memq major-mode '(org-mode))
      (push '(?\[ . ("[[" . "]]")) evil-surround-pairs-alist) ; [
      (push '(?= . ("=" . "=")) evil-surround-pairs-alist))
    (when (memq major-mode '(emacs-lisp-mode))
      (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
      (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
    ;; generic
    (push '(?/ . ("/" . "/")) evil-surround-pairs-alist))
  (add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)
  )

(use-package evil-escape
  :init
  (setq-default evil-escape-delay 0.3
                evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t)
  :config
  (evil-escape-mode 1)
  :bind ("C-c C-g" . evil-escape)
  )

(use-package evil-matchit
  :hook (prog-mode . turn-on-evil-matchit-mode)
  )

(use-package evil-mark-replace)
(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys nil t)
  )

(use-package general
  :config
  (general-evil-setup t)

  ;; some generic key-binddings
  (general-define-key
   [remap dabbrev-completion] 'hippie-expand
   )

  ;; enable some emacs keybings in evil-mode
  (general-define-key
   :keymaps 'evil-insert-state-map
   "C-a" 'crux-move-beginning-of-line
   "C-e" 'move-end-of-line
   "C-k" 'kill-line
   "C-y" 'yank)
  (general-define-key
   :keymaps 'evil-ex-completion-map
   "C-a" 'move-beginning-of-line
   "C-b" 'backward-char
   "M-n" 'next-complete-history-element
   "M-p" 'previous-complete-history-element)

  ;; {{ use `,` as leader key
  (general-create-definer my-comma-leader-def
    :prefix ","
    :states '(normal visual))

  (my-comma-leader-def
    "bf" 'beginning-of-defun
    "bu" 'backward-up-list
    "ef" 'end-of-defun
    ;; "m" 'evil-set-marker
    "eb" 'eval-buffer
    "ee" 'eval-expression
    "aw" 'ace-swap-window
    "af" 'ace-maximize-window
    "dj" 'dired-jump ;; open the dired from current file
    "xo" 'ace-window
    "ff" 'find-file
    "fo" 'find-file-other-window
    "hf" 'find-function
    "tff" 'toggle-frame-fullscreen
    "tfm" 'toggle-frame-maximized
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "ck" 'evilnc-comment-and-kill-ring-save
    ;; {{ window move
    "wh" 'evil-window-left
    "wl" 'evil-window-right
    "wk" 'evil-window-up
    "wj" 'evil-window-down
    ;; }}
    "rb" 'evilmr-replace-in-buffer
    "ts" 'evilmr-tag-selected-region ;; recommended
    "bk" 'kill-buffer-and-window
    "og" 'org-agenda
    "cxr" 'org-clock-report ; `C-c C-x C-r'
    "di" 'evilmi-delete-items
    "si" 'evilmi-select-items
    ;; {{ @see http://ergoemacs.org/emacs/emacs_pinky_2020.html
    ;; `keyfreq-show' proved sub-window operations happen most.
    "x0" 'delete-window
    "x1" 'delete-other-windows
    "x2" 'split-window-vertically
    "x3" 'split-window-horizontally
    "xq" 'delete-window
    "xa" 'split-window-vertically
    "xd" 'split-window-horizontally
    "s0" 'delete-window
    "s1" 'delete-other-windows
    "s2" 'split-window-vertically
    "s3" 'split-window-horizontally
    "sq" 'delete-window
    "sa" 'split-window-vertically
    "sd" 'split-window-horizontally
    "oo" 'delete-other-windows
    ;; }}
    "fe" 'flyspell-goto-next-error
    "fa" 'flyspell-auto-correct-word
    "lb" 'langtool-check-buffer
    "ll" 'langtool-goto-next-error

    "ar" 'align-regexp
    "xv" 'vc-next-action ; 'C-x v v' in original
    "vg" 'vc-annotate ; 'C-x v g' in original
    "vm" 'vc-msg-show
    ) ;;}}

  ;; {{ Use `SPC` as leader key
  ;; all keywords arguments are still supported
  (general-create-definer my-space-leader-def
    :prefix "SPC"
    :states '(normal visual))

  (my-space-leader-def
    "n" (lambda ()
          (interactive)
          (if (derived-mode-p 'diff-mode) (my-search-next-diff-hunk)
            (my-search-next-merge-conflict)))
    "p" (lambda ()
          (interactive)
          (if (derived-mode-p 'diff-mode) (my-search-prev-diff-hunk)
            (my-search-prev-merge-conflict)))
    "mf" 'mark-defun
    "mh" 'mark-whole-buffer
    "sc" 'shell-command

    "jj" 'scroll-other-window
    "kk" 'scroll-other-window-up

    "hml" 'yx-hide-mode-line
    "te" 'eshell/yx-eshell-toggle
    "tv" 'vterm-toggle-cd
    ) ;;}}

  ;; {{ Use `;` as leader key, for searching something
  (general-create-definer my-semicolon-leader-def
    :prefix ";"
    :states '(normal visual))

  (my-semicolon-leader-def
    "ds" 'sdcv-search-input             ; sdcv
    "dt" 'sdcv-search-input+
    "tdp" 'hl-todo-previous             ;hl-todo
    "tdn" 'hl-todo-next
    "tdo" 'hl-todo-occur
    "tdi" 'hl-todo-insert
    )
  ;; }}

  (general-create-definer my-org-leader-def
    :prefix ";"
    :non-normal-prefix "M-;"
    :states '(normal motion visual)
    :keymaps 'org-mode-map)

  (my-org-leader-def
    "c<" 'org-do-promote ; `C-c C-<'
    "c>" 'org-do-demote ; `C-c C->'
    "cxi" 'org-clock-in ; `C-c C-x C-i'
    "cxo" 'org-clock-out ; `C-c C-x C-o'
    )
  ;; }}
  )


;; end evil
(provide 'init-evil)
