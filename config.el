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

(setq org-archive-location "~/org/done.org::datetree/")

;; Mark tasks with a CLOSED timestamp on DONE
(after! org
(setq org-log-done 'time)

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/org/inbox.org" "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

        ("e" "Event" entry
         (file+headline "~/org/calendar.org" "Events")
         "* %^{Event}\n%^{SCHEDULED}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:CONTACT: %(org-capture-ref-link \"~/org/roam/contacts.org\")\n:END:\n%?")

        ("d" "Deadline" entry
         (file+headline "~/org/calendar.org" "Deadlines")
         "* TODO %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

        ("b" "Bookmark" entry
        (file+headline "~/org/bookmarks.org" "Inbox")
        "** [[%^{URL}][%^{Title}]]\n:PROPERTIES:\n:CREATED: %U\n:TAGS: %(org-capture-bookmark-tags)\n:END:\n\n"
        :empty-lines 0)

        ("c" "Contact" entry
         (file "~/org/roam/contacts.org")
"* %^{Name} %^g
:PROPERTIES:
:ID: %(org-id-new)
:CREATED: %U
:CAPTURED: %a
:EMAIL: %^{Email}
:PHONE: %^{Phone}
:BIRTHDAY: %^{Birthday (use <YYYY-MM-DD +1y> format)}t
:LOCATION: %^{Address}
:LAST_CONTACTED: %U
:END:
%?"
 :empty-lines 1)

        ("n" "Note" entry
         (file+headline "~/org/notes.org" "Inbox")
         "* [%<%Y-%m-%d %a>] %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?"
         :prepend t)))

(defun org-capture-bookmark-tags ()
  "Get tags from existing bookmarks and prompt for tags with completion."
  (save-window-excursion
    (let ((tags-list '()))
      ;; Collect existing tags
      (with-current-buffer (find-file-noselect "~/org/bookmarks.org")
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^:TAGS:\\s-*\\(.+\\)$" nil t)
            (let ((tag-string (match-string 1)))
              (dolist (tag (split-string tag-string "[,;]" t "[[:space:]]"))
                (push (string-trim tag) tags-list))))))
      ;; Remove duplicates and sort
      (setq tags-list (sort (delete-dups tags-list) 'string<))
      ;; Prompt user with completion
      (let ((selected-tags (completing-read-multiple "Tags (comma-separated): " tags-list)))
        ;; Return as a comma-separated string
        (mapconcat 'identity selected-tags ", ")))))

;; Helper function to select and link a contact
(defun org-capture-ref-link (file)
  "Create a link to a contact in contacts.org"
  (let* ((headlines (org-map-entries
                     (lambda ()
                       (cons (org-get-heading t t t t)
                             (org-id-get-create)))
                     t
                     (list file)))
         (contact (completing-read "Contact: "
                                   (mapcar #'car headlines)))
         (id (cdr (assoc contact headlines))))
    (format "[[id:%s][%s]]" id contact))))

;; Set archive location to done.org under current date
;; (defun my/archive-done-task ()
;;   "Archive current task to done.org under today's date"
;;   (interactive)
;;   (let* ((date-header (format-time-string "%Y-%m-%d %A"))
;;          (archive-file (expand-file-name "~/org/done.org"))
;;          (location (format "%s::* %s" archive-file date-header)))
;;     ;; Only archive if not a habit
;;     (unless (org-is-habit-p)
;;       ;; Add COMPLETED property if it doesn't exist
;;       (org-set-property "COMPLETED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
;;       ;; Set archive location and archive
;;       (setq org-archive-location location)
;;       (org-archive-subtree))))

;; Automatically archive when marked DONE, except for habits
;; (add-hook 'org-after-todo-state-change-hook
;;           (lambda ()
;;             (when (and (string= org-state "DONE")
;;                        (not (org-is-habit-p)))
;;               (my/archive-done-task))))

;; Optional key binding if you ever need to archive manually
(after! org
  (map! :map org-mode-map
        :localleader
        "a" #'my/archive-done-task))

;; Configure habit graph display
(setq org-habit-show-habits-only-for-today t)  ; or nil to show all days
(setq org-habit-graph-column 50)  ; adjust based on your screen

(setq org-agenda-remove-tags t)
(setq org-agenda-block-separator 32)
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "\n HIGHEST PRIORITY")
                 (org-agenda-prefix-format "   %i %?-2 t%s")))

          (agenda ""
                  ((org-agenda-start-day "+0d")
                   (org-agenda-span 3)  ; Show 3 days for better habit tracking
                   (org-agenda-time)
                   (org-agenda-remove-tags t)
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("" ""))
                   (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈┈ NOW")
                   (org-agenda-overriding-header "\n TODAY'S SCHEDULE & HABITS")
                   (org-agenda-prefix-format "   %i %?-2 t%s")))

          (tags-todo "-STYLE=\"habit\""  ; This still excludes habits from TODO list
                     ((org-agenda-overriding-header "\n ALL TODO")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "   %i %?-2 t%s")))))))

(defun my/org-agenda-dashboard ()
  "Open the custom org-agenda dashboard."
  (interactive)
  (org-agenda nil "d"))
