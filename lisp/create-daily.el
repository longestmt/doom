;;; ../../dotfiles/doom/.config/doom/lisp/create-daily.el -*- lexical-binding: t; -*-
(defun create-daily-file ()
  "Create a daily journal file organized by year and week number."
  (interactive)

  (let* ((days-offset (read-number "Days from today (0=today, 1=tomorrow, -1=yesterday): " 0))
         (target-time (time-add (current-time) (days-to-time days-offset)))
         (decoded-time (decode-time target-time))

         ;; Get the year (like 2025)
         (year (format-time-string "%Y" target-time))

         ;; Get week number (1-53) - using %V instead of %U
         ;; %V gives ISO week number where weeks start on Monday
         ;; This should correctly identify March 24, 2025 as week 13
         (week-number (string-to-number (format-time-string "%V" target-time)))

         ;; Get friendly date format like "March 24, 2025"
         (date-string (format-time-string "%B %d, %Y" target-time))

         ;; Create folder paths
         (year-dir (expand-file-name year "~/org/journal/"))
         (week-dir (expand-file-name (format "Week %d" week-number) year-dir))

         ;; Create file path/name
         (file-path (expand-file-name (concat date-string ".org") week-dir)))

    ;; Step 2: Make sure folders exist
    (unless (file-exists-p year-dir)
      (make-directory year-dir t))

    (unless (file-exists-p week-dir)
      (make-directory week-dir t))

    ;; Step 3: Create the file (or open it if it exists)
    (find-file file-path)

    ;; Step 4: Insert template if file is empty
    (when (= (buffer-size) 0)
      (yas-expand-snippet
       (with-temp-buffer
         (insert-file-contents "~/.doom.d/snippets/org-mode/daily")
         ;; Skip the yasnippet header (lines starting with # until the -- line and any blank lines after)
         (goto-char (point-min))
         (when (re-search-forward "^# --\n+" nil t)
           (buffer-substring (point) (point-max))))))))
