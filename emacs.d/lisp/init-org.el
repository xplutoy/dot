;;; -*- lexical-binding: t no-byte-compile: t -*-

(yx/require-package 'mixed-pitch)

;; org
(add-hook 'org-mode-hook
          #'(lambda ()
              (setq word-wrap t
                    word-wrap-by-category t
                    fill-column 100)
              (auto-fill-mode 1)
              (visual-line-mode 1)
              (mixed-pitch-mode 1)
              ;; (variable-pitch-mode 1)
              ))
(setq org-directory "~/org")
(setq diary-file (concat org-directory "/diary.org"))
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
      org-startup-indented t
      org-fontify-done-headline t
      org-src-fontify-natively t
      org-hide-leading-stars t
      org-hidden-keywords t
      org-hide-emphasis-markers t
      org-pretty-entities t
      ;; org-odd-levels-only t
      org-hide-block-startup t
      org-startup-with-inline-images t
      ;; org-auto-align-tags nil
      org-image-actual-width '(300))

 (let* ((base-font-color (face-foreground 'default nil 'default))
        (headline   `(:inherit default :weight bold :foreground ,base-font-color)))
   (custom-theme-set-faces
    'user
    `(org-level-8 ((t (,@headline))))
    `(org-level-7 ((t (,@headline))))
    `(org-level-6 ((t (,@headline))))
    `(org-level-5 ((t (,@headline))))
    `(org-level-4 ((t (,@headline :height 1.1))))
    `(org-level-3 ((t (,@headline :height 1.25))))
    `(org-level-2 ((t (,@headline :height 1.5))))
    `(org-level-1 ((t (,@headline :height 1.75))))
    `(org-document-title ((t (,@headline :height 2.0 :underline nil))))))

(setq org-log-done 'time)
(setq org-log-repeat 'time)
(setq org-log-into-drawer t)
(setq org-agenda-span 'day)
(setq org-deadline-warning-days 14)
(setq org-use-fast-todo-selection t)
(setq org-return-follows-link t)
(setq-default org-enforce-todo-dependencies t)

(setq org-agenda-custom-commands
      '(("x" agenda)
        ("y" agenda*)
        ("w" todo "WAITING")
        ("W" todo-tree "WAITING")
        ("n" todo "NEXT")
        ("w" . "work+tag search")
        ("wu" tags-todo "+@work+urgent")
        ("l" . "life+tag search")
        ("li" tags-todo "+@life+important")))

;; org-roam
(yx/require-package 'org-roam)
(with-eval-after-load 'org
  ;; org-modules
  (add-to-list 'org-modules 'org-habit)
  ;; (add-to-list 'org-modules 'org-tempo)

  ;; org-roam
  (define-key org-mode-map (kbd "C-,") nil) ;;unbind org-cycle-agenda-files
  (define-key org-mode-map (kbd "C-c n l") 'org-roam-buffer-toggle)
  (define-key org-mode-map (kbd "C-c n f") 'org-roam-node-find)
  (define-key org-mode-map (kbd "C-c n i") 'org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
  (define-key org-mode-map (kbd "C-c n r") 'org-roam-ref-add)
  (define-key org-mode-map (kbd "C-c n R") 'org-roam-ref-remove)
  (define-key org-mode-map (kbd "C-c n a") 'org-roam-alias-add)
  (define-key org-mode-map (kbd "C-c n A") 'org-roam-alias-remove)
  (global-set-key (kbd "C-c n c") 'org-roam-capture)

  (setq org-roam-directory (file-truename org-directory))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(("n" "note" plain "%?"
           :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("f" "fleeting-note" plain "%?"
           :if-new
           (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n\n#+begin_quote\n%i\n#+end_quote\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :article:\n")
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

(yx/require-package 'org-appear)
(with-eval-after-load 'org-appear
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t))
(add-hook 'org-mode-hook 'org-appear-mode)


(provide 'init-org)
