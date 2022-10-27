;;; -*- lexical-binding: t no-byte-compile: t -*-
;; org
(require 'org)
(setq org-directory "~/personal/org")
(setq diary-file (concat org-directory "/diary"))
(setq org-default-notes-file (concat org-directory "/gtd.org"))
(setq org-agenda-files '("gtd.org"))

(setq org-agenda-include-diary t)
(setq org-agenda-diary-file diary-file)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c b") #'org-switchb)

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
(setq org-priority-faces '((?A :foreground "red")
			               (?B :foreground "orange")
			               (?C :foreground "yellow")))
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

(setq org-startup-folded "content"
      org-startup-indented t
      org-hide-leading-stars t
      org-hidden-keywords t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-use-sub-superscripts "{}"
      ;; org-odd-levels-only t
      org-hide-block-startup t
      org-startup-with-inline-images t
      ;; org-auto-align-tags nil
      org-image-actual-width '(300))

(setq org-fontify-done-headline t
      org-src-fontify-natively t
      org-src-preserve-indentation t
      )

(setq org-log-done 'time
      org-log-repeat 'time
      org-log-redeadline 'time
      org-log-reschedule 'time
      )
(setq org-log-into-drawer t
      org-log-state-notes-into-drawer t)
(setq org-agenda-span 'day)
(setq org-deadline-warning-days 14)
(setq org-use-fast-todo-selection t)
(setq org-return-follows-link t)
(setq org-reverse-note-order t)
(setq-default org-enforce-todo-dependencies t)

(setq org-agenda-custom-commands
      '(("x" agenda)
        ("y" agenda*)
        ("w" todo "WAITING")
        ("W" todo-tree "WAITING")
        ("n" todo "NEXT")))

;; org-roam
(yx-require-package 'org-roam)
(global-set-key (kbd "C-c n c") 'org-roam-capture)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-,") nil) ;;unbind org-cycle-agenda-files
  ;; org-modules
  (add-to-list 'org-modules 'org-habit)
  ;; org-roam
  (define-key org-mode-map (kbd "C-c n l") 'org-roam-buffer-toggle)
  (define-key org-mode-map (kbd "C-c n i") 'org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
  (define-key org-mode-map (kbd "C-c n T") 'org-roam-tag-remove)
  (define-key org-mode-map (kbd "C-c n r") 'org-roam-ref-add)
  (define-key org-mode-map (kbd "C-c n R") 'org-roam-ref-remove)
  (define-key org-mode-map (kbd "C-c n a") 'org-roam-alias-add)
  (define-key org-mode-map (kbd "C-c n A") 'org-roam-alias-remove)

  (setq org-roam-directory (file-truename org-directory))
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

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  (org-roam-db-autosync-mode)

  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %^{desc}\n%?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+TITLE: %<%Y-%m-%d>\n"))))
  (global-set-key (kbd "C-c n d") 'org-roam-dailies-capture-date)
  )

(yx-require-package 'org-appear)
(with-eval-after-load 'org
  (setq org-appear-trigger 'always)
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t))
(add-hook 'org-mode-hook 'org-appear-mode)

;; end init-org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-org)
