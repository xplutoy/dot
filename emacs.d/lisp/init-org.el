;;; -*- lexical-binding: t no-byte-compile: t -*-
;; org
(use-package org
  :ensure nil
  :init
  (setq org-directory yx-org-root
        diary-file (concat org-directory "diary")
        org-default-notes-file (concat org-directory "gtd.org")
        org-agenda-files '("gtd.org")
        org-agenda-include-diary t
        org-agenda-diary-file diary-file)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING" . t) ("HOLD" . t))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO [#B] %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n %i\n%?")
          ("p" "Project" entry (file+headline org-default-notes-file "Projects")
           "* TODO [#B] %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n %i\n%?")
          ("h" "Habit" entry (file+headline org-default-notes-file "Habits")
           "* NEXT [#B] %^{Title}\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n" :empty-lines-after 1)))
  (setq org-refile-targets '((nil :maxlevel . 2)
                             ("daily/2022-10-27.org" :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2))
        org-refile-use-cache nil
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-log-refile nil)
  ;; org ui
  (setq org-ellipsis "⤵")
  (setq org-startup-folded "content"
        org-startup-indented t
        org-hide-leading-stars t
        org-hidden-keywords nil
        org-hide-emphasis-markers nil
        org-pretty-entities t
        org-use-sub-superscripts "{}"
        ;; org-odd-levels-only t
        org-hide-block-startup nil
        ;; org-auto-align-tags nil
        org-list-description-max-indent 4
        org-fontify-done-headline nil
        org-fontify-todo-headline nil
        org-fontify-whole-heading-line nil
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-preserve-indentation t
        org-priority-faces '((?a . error) (?b . warning) (?c . success))
        org-startup-with-inline-images t
        org-image-actual-width '(600))
  (setq org-log-done 'time
        org-log-repeat 'time
        org-log-redeadline 'time
        org-log-reschedule 'time)
  (setq org-log-into-drawer t
        org-log-state-notes-into-drawer t)
  (setq org-agenda-span 'day)
  (setq org-deadline-warning-days 14)
  (setq org-use-fast-todo-selection t)
  (setq org-return-follows-link t)
  (setq org-reverse-note-order t)
  (setq-default org-enforce-todo-dependencies t)

  ;; @see https://www.n16f.net/blog/org-mode-headline-tips/
  (setq org-insert-heading-respect-content t)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-goto-max-level 2)

  (setq org-agenda-custom-commands
        '(("x" agenda)
          ("y" agenda*)
          ("w" todo "WAITING")
          ("W" todo-tree "WAITING")
          ("n" todo "NEXT")))

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agend)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb))

  :config
  ;;org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (emacs-lisp . t)))
  )




;; org-roam
(use-package org-roam
  :after org
  :init
  (setq org-roam-directory (file-truename org-directory))
  (setq org-roam-db-location (concat yx-share-data-path "org-roam.db"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(("n" "note" plain "%?"
           :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("f" "fleeting-note" plain "%?"
           :if-new (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+begin_quote\n%i\n#+end_quote\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))
  ;; (add-hook 'org-roam-capture-new-node-hook #'(lambda () (org-roam-tag-add '("draft"))))
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %^{desc}\n%?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%Y-%m-%d>\n"))))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  :config
  (org-roam-db-autosync-mode)

  :bind (("C-c n c" . org-roam-capture)
         ("C-c n n" . org-roam-node-find)
         ("C-c n d" . org-roam-dailies-capture-date)
         :map org-mode-map
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n T" . org-roam-tag-remove)
         ("C-c n r" . org-roam-ref-add)
         ("C-c n R" . org-roam-ref-remove)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n A" . org-roam-alias-remove))
  )

(use-package consult-org-roam
  :after (consult org-roam)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "SPC"))
  (consult-org-roam-mode 1)
  :bind (("C-c n e" . consult-org-roam-file-find)  ;; some bug?
         ("C-c n b" . consult-org-roam-backlinks)
         ("C-c n f" . consult-org-roam-forward-links)
         ("C-c n s" . consult-org-roam-search))
  )

(use-package org-appear
  :after org
  :init
  (setq org-appear-trigger 'always)
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t)
  :hook (org-mode . org-appear-mode)
  )

;; deft
(use-package deft
  :bind ("C-c C-d" . deft)
  :commands (deft)
  :init (setq deft-extensions '("org" "md")
              deft-directory "~/personal/org"
              deft-recursive t
              deft-text-mode 'org-mode
              deft-use-filename-as-title t
              deft-use-filter-string-for-filename t)
  )

;; end init-org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-org)
