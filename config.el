;; (setq doom-theme 'compline)
(add-to-list 'custom-theme-load-path "~/.config/doom/themes/")
(load-theme 'compline t)
(setq doom-font (font-spec :family "Menlo" :size 15))

(map! :leader
      :desc "Comment line" "-" #'comment-line)

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle eshell split"            "e" #'+eshell/toggle
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle line numbers"            "l" #'doom/toggle-line-numbers
       :desc "Toggle markdown-view-mode"      "m" #'dt/toggle-markdown-view-mode
       :desc "Toggle truncate lines"          "t" #'toggle-truncate-lines
       :desc "Toggle treemacs"                "T" #'+treemacs/toggle
       :desc "Toggle vterm split"             "v" #'+vterm/toggle))

(setq display-line-numbers-type t)
(map! :leader
      (:prefix ("o" . "open here")
       :desc "Open eshell here"    "e" #'+eshell/here
       :desc "Open vterm here"     "v" #'+vterm/here))

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.25))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.15))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.05))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0)))))

(defun dt/toggle-markdown-view-mode ()
  "Toggle between `markdown-mode' and `markdown-view-mode'."
  (interactive)
  (if (eq major-mode 'markdown-view-mode)
      (markdown-mode)
    (markdown-view-mode)))

(setq org-log-done 'time)

(setq org-archive-location "~/org/done.org::")

(custom-theme-set-faces!
'doom-one
'(org-level-8 :inherit outline-3 :height 1.0)
'(org-level-7 :inherit outline-3 :height 1.0)
'(org-level-6 :inherit outline-3 :height 1.1)
'(org-level-5 :inherit outline-3 :height 1.15)
'(org-level-4 :inherit outline-3 :height 1.2)
'(org-level-3 :inherit outline-3 :height 1.25)
'(org-level-2 :inherit outline-2 :height 1.3)
'(org-level-1 :inherit outline-1 :height 1.35)
'(org-document-title  :height 1.8 :bold t :underline nil))

(after! org-journal
  (setq org-journal-dir "~/org/"
        org-journal-file-type 'yearly
        org-journal-file-format "journal.org"
        org-journal-date-prefix "* "
        org-journal-date-format "%Y-%m-%d %A"
        org-journal-time-prefix "** %H:%M "
        org-journal-enable-agenda-integration t))

(after! org-journal
  (map! :leader
        :desc "New journal entry"
        "n j" #'org-journal-new-entry))

(setq display-line-numbers-type t)   ;; Turn line numbers on
(setq confirm-kill-emacs nil)        ;; Don't confirm on exit
(setq initial-buffer-choice "~/org/inbox.org") ;; Inbox is initial buffer
