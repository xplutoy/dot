;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package evil
  :init
  (setq evil-default-state 'emacs
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-move-cursor-back t)
  (setq evil-want-integration t
        evil-want-keybinding nil)

  :config
  (evil-mode 1)

  ;; As a general rule, mode specific evil leader keys started
  ;; with upper cased character or 'g' or special character except "=" and "-"
  (evil-define-key 'normal org-mode-map
    "gh" 'outline-up-heading
    "$" 'org-end-of-line ; smarter behavior on headlines etc.
    "^" 'org-beginning-of-line ; ditto
    "<" (lambda () (interactive) (org-demote-or-promote 1)) ; out-dent
    ">" 'org-demote-or-promote ; indent
    (kbd "TAB") 'org-cycle)

  (evil-define-key 'normal markdown-mode-map
    "gh" 'outline-up-heading
    (kbd "TAB") 'markdown-cycle)

  ;; I prefer Emacs way after pressing ":" in evil-mode
  (evil-define-key nil 'evil-ex-completion-map
    "C-a" 'move-beginning-of-line
    "C-e" 'move-end-of-line
    "C-b" 'backward-char
    "M-p" 'previous-complete-history-element
    "M-n" 'next-complete-history-element
    )

  (evil-define-key nil 'evil-insert-state-map
    "C-e" 'move-end-of-line
    "C-k" 'kill-line
    "M-j" 'yas-expand
    )

  ;; evil re-assign "M-." to `evil-repeat-pop-next' which I don't use actually.
  ;; Restore "M-." to original binding command
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

  )

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-mode-list '(
                                    corfu vertico vterm
                                    consult bookmark diff-hl
                                    dired eglot ediff
                                    doc-view eldoc elfeed
                                    elisp-mode embark emoji
                                    eshell flymake gnus ibuffer
                                    imenu info lua-mode org
                                    org-roam python simple w3m
                                    which-key women xref
                                    ))
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
  :bind ("C-;" . evilnc-comment-or-uncomment-lines)
  )

(use-package general
  :config
  (general-evil-setup t)

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
   "dd" 'pwd
   "mf" 'mark-defun
   "mh" 'mark-whole-buffer
   "sc" 'shell-command

   "jj" 'scroll-other-window
   "kk" 'scroll-other-window-up
   ) ;;}}

  ;; {{ Use `;` as leader key, for searching something
  (general-create-definer my-semicolon-leader-def
    :prefix ";"
    :states '(normal visual))

  (my-semicolon-leader-def
   "db" 'sdcv-search-input ; details
   "dt" 'sdcv-search-input+ ; summary
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
