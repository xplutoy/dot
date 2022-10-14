;;; -*- lexical-binding: t no-byte-compile: t -*-
;; org
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-agenda-files '("inbox.org"))

(defvar yx-daily-file (concat org-directory "/daily.org"))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c b") #'org-switchb)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
(setq org-log-done 'time)

(setq org-capture-templates
      '(("t" "Todo" entry
         (file org-default-notes-file) "* TODO %^{Title}\nOPENED: %U\n%?")
        ("j" "Journal" entry
         (file+datetree yx-daily-file) "* %^{Title}\n%U\n%?")))

;; (setq org-refile-use-outline-path t)
;; (setq org-refile-allow-creating-parent-nodes (quote confirm))


(setq org-deadline-warning-days 14)

(setq org-use-fast-todo-selection t)
(setq-default org-enforce-todo-dependencies t)


;; org-roam
(yx/require-package 'org-roam)
(with-eval-after-load 'org
  (setq org-roam-directory (file-truename org-directory))
  (org-roam-db-autosync-mode)
  (setq org-roam-completion-everywhere t)
  )

(provide 'init-org)
