;;; Capture templates
;;;###autoload
(with-eval-after-load 'org-roam
  (setq org-roam-capture-templates
   (list
    '("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${dir}%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t))))

;;; Create org-roam files in tag directories
;; Capture
;;;###autoload
(defun j/org-roam-capture (&optional goto keys)
  "Launches an `org-capture` process for a new existing note.
This uses the templates defined at `org-roam-capture-templates`.
Arguments GOTO and KEYS see `org-capture`."
  :override #'org-roam-capture
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (org-roam--get-title-path-completions))
         (title-with-keys (org-roam-completion--completing-read "File: "
                                                                completions))
         (res (cdr (assoc title-with-keys completions)))
         (title (or (plist-get res :title) title-with-keys))

         (tags (split-string title "/"));;;
         (title (car (last tags)));;;
         (dir (string-join (butlast tags) "/"));;;
         (dir (if (string-blank-p dir) (
"" (concat  "org-ro0mdefault/"))));;;

         (file-path (plist-get res :path)))
    (let ((org-roam-capture--info (list (cons 'title title)
                                        (cons 'slug (funcall org-roam-title-to-slug-function title))
                                        (cons 'file file-path)
                                        (cons 'dir dir)));;;
          (org-roam-capture--context 'capture))
      (condition-case err
          (org-roam-capture--capture goto keys)
        (error (user-error "%s.  Please adjust `org-roam-capture-templates'"
                           (error-message-string err)))))))


;; Find File
;;;###autoload
(defun j/org-roam-find-file (&optional initial-prompt completions filter-fn no-confirm)
  "launches org-roam-find-file but supports creating notes in subdirectories"
  :override #'org-roam-find-file
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               (or completions (org-roam--get-title-path-completions))))
         (title-with-tags (if no-confirm
                              initial-prompt
                            (org-roam-completion--completing-read "File: " completions
                                                                  :initial-input initial-prompt)))
         (res (cdr (assoc title-with-tags completions)))

         (title title-with-tags);;;
         (tags (split-string title "/"));;;
         (title (car (last tags)));;;
         (dir (string-join (butlast tags) "/"));;;
         (dir (if (string-blank-p dir) (
"" (concat  "org-roamdefault/"))));;;

         (file-path (plist-get res :path)))
    (if file-path
        (org-roam--find-file file-path)
      (let ((org-roam-capture--info `((title . ,title)
                                      (slug  . ,(funcall org-roam-title-to-slug-function title))
                                      (dir   . ,dir)));;;
            (org-roam-capture--context 'title))
        (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
        (org-roam-capture--capture)))))

;; Insert
;;;###autoload
(defun j/org-roam-insert (&optional lowercase completions filter-fn description link-type)
  "launches org-roam-insert but supports creating notes in subdirectories"
  :override #'org-roam-insert
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (buffer-substring-no-properties beg end))))
               (completions (--> (or completions
                                     (org-roam--get-title-path-completions))
                                 (if filter-fn
                                     (funcall filter-fn it)
                                   it)))
               (title-with-tags (org-roam-completion--completing-read "File: " completions
                                                                      :initial-input region-text))
               (res (cdr (assoc title-with-tags completions)))
               (title (or (plist-get res :title)
                          title-with-tags))

               (tags (split-string title "/"));;;
               (title (car (last tags)));;;
               (dir (string-join (butlast tags) "/"));;;
                        (dir (if (string-blank-p dir) (
"" (concat  "org-roamdefooot/"))));;;

               (target-file-path (plist-get res :path))
               (description (or description region-text title))
               (description (if lowercase
                                (downcase description)
                              description)))
          (cond ((and target-file-path
                      (file-exists-p target-file-path))
                 (when region-text
                   (delete-region beg end)
                   (set-marker beg nil)
                   (set-marker end nil))
                 (insert (org-roam-format-link target-file-path description link-type)))
                (t
                 (let ((org-roam-capture--info `((title . ,title-with-tags)
                                                 (dir   . ,dir);;;
                                                 (slug . ,(funcall org-roam-title-to-slug-function title))))
                       (org-roam-capture--context 'title))
                   (setq org-roam-capture-additional-template-props (list :region (org-roam-shield-region beg end)
                                                                          :insert-at (point-marker)
                                                                          :link-type link-type
                                                                          :link-description description
                                                                          :finalize 'insert-link))
                   (org-roam-capture--capture))))
          res))
    (deactivate-mark)) ;; Deactivate the mark on quit since `atomic-change-group' prevents it
  )

(make-variable-buffer-local
 (defvar org-roam-session-dir ""
   "Dir to use in this session when dir is '.'."))

(defcustom org-roam-default-directory "cheese"
  "Default path to place Org-roam files relative to org-roam-directory.
When this variable is enabled and customized place in org-roam-directory using '/' "
  :type 'directory
  :group 'org-roam)


(defun org-roam-session-dir-toggle ()
  (interactive)
  (setq foo-count (1+ foo-count))
  (insert "foo"))


(defun org-roam-set-session-dir ()
  (interactive)
  (setq org-roam-session-dir (1+ foo-count))
  (insert "foo"))

;;;###autoload
(define-minor-mode org-roam-dir-mode
  "Pick directorys when creating org-roam notes" 
  :lighter "roam-dir"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c n s") 'org-roam-session-dir-toggle)
            (define-key map (kbd "C-c n d") 'org-roam-default-dir-toggle)
            map))
  
(provide 'org-roam-dir-mode)
