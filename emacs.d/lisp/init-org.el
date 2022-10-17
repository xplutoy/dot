;;; -*- lexical-binding: t no-byte-compile: t -*-
;; diary
(setq diary-file (concat org-directory "/diary.org"))

;; org
(add-hook 'org-mode-hook
          #'(lambda ()
              (setq word-wrap t
                    word-wrap-by-category t
                    fill-column 100)
              (visual-line-mode 1)
              ))
(setq org-directory "~/org")
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
(setq org-capture-templates
      '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
         "* TODO [#B] %^{Title}\t%^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%?" :empty-lines-before 1)
        ("p" "Project" entry (file+headline org-default-notes-file "Projects")
         "* TODO [#B] %^{Title}\t%^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%?" :empty-lines-before 1)
        ("h" "Habit" entry (file+headline org-default-notes-file "Habits")
         "* NEXT [#B] %^{Title}\t%^g\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n" :empty-lines-before 1)))

(setq org-refile-targets '((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 2))
      org-refile-use-cache nil
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-log-refile nil)

(setq org-startup-folded "content"
      org-hide-block-startup t)
(setq org-log-done 'time)
(setq org-log-repeat 'time)
(setq org-log-into-drawer t)
(setq org-agenda-span 'day)
(setq org-deadline-warning-days 14)
(setq org-use-fast-todo-selection t)
(setq org-return-follows-link t)
(setq-default org-enforce-todo-dependencies t)

(setq org-agenda-custom-commands
      '(("n" todo "NEXT")
        ("w" . "work+tag search")
        ("wu" tags-todo "+@work+urgent")
        ("l" . "life+tag search")
        ("li" tags-todo "+@life+important")))

;; org-roam
(yx/require-package 'org-roam)
(with-eval-after-load 'org
  ;; org-modules
  (add-to-list 'org-modules 'org-habit)

  ;; org-roam
  (define-key org-mode-map (kbd "C-,") nil) ;;unbind org-cycle-agenda-files
  (define-key org-mode-map (kbd "C-c n l") 'org-roam-buffer-toggle)
  (define-key org-mode-map (kbd "C-c n f") 'org-roam-node-find)
  (define-key org-mode-map (kbd "C-c n i") 'org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
  (define-key org-mode-map (kbd "C-c n r") 'org-roam-ref-add)
  (global-set-key (kbd "C-c n c") 'org-roam-capture)
  ()
  (setq org-roam-directory (file-truename org-directory))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(("n" "note" plain "%?"
           :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("f" "fleeting-note" plain "%?"
           :if-new
           (file+head "inbox/%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  (add-hook 'org-roam-capture-new-node-hook #'(lambda () (org-roam-tag-add '("draft"))))

  (org-roam-db-autosync-mode)
  ;; (require 'org-roam-protocol)
  )

(provide 'init-org)
