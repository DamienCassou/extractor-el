;;; extractor.el --- Facilitates extraction of archive files
;;; Commentary:
;;; Code:
(require 'cl)

(defvar extractor-ignored-files
  '("." ".." "__MACOSX")
  "Set of files that can safely be ignored.")

(defun extractor-ignored-file-p (file)
  "Check if FILE is part of `extractor-ignored-files'."
  (member file extractor-ignored-files))

(defun extractor-archive-name (archive)
  "Return ARCHIVE without directory name and extension."
  (let ((extension (file-name-extension archive)))
    (if (or (null extension) (zerop (length extension)))
        (file-name-nondirectory archive)
      (file-name-nondirectory
       (substring-no-properties
        archive
        0
        (- (1+ (length extension))))))))

(defun extractor-extract-to-tempdir (archive dest)
  "Extract ARCHIVE to a temporary directory under DEST."
  (let* ((archive (expand-file-name archive))
         (dest (expand-file-name dest))
         (void (make-directory dest t))
         (temporary-file-directory dest)
         (tempdir (make-temp-file "emacs-extractor" t ".tmp")))
    (shell-command (format "unzip -d %s %s" tempdir archive))
    tempdir))

(defun extractor-move-files (from dest)
  "Move all files in FROM to DEST."
  (let ((from (expand-file-name from))
        (dest (expand-file-name dest))
        (stderrbuf (get-buffer-create " *Extractor output*")))
    (assert (file-directory-p from) t)
    (assert (file-directory-p dest) t)
    (let ((cmd (call-process-shell-command
                "mv"
                nil ;; input
                (list stderrbuf t)
                nil ;; display
                (format "%s/*" from)
                (format "%s/" dest))))
      (when (or (stringp cmd)
                (and (numberp cmd) (> cmd 0)))
        (with-current-buffer stderrbuf
           (error "%s" (buffer-string)))))))

(defun extractor-clean-directory (directory)
  "Remove undesired files in DIRECTORY."
  (let ((tobecleaned (expand-file-name "__MACOSX" directory)))
    (cond
     ((file-regular-p tobecleaned) (delete-file tobecleaned))
     ((file-directory-p tobecleaned) (delete-directory tobecleaned t)))))

(defun extractor-extract-internal (archive destination mode
                                           &optional clean)
  "Extract ARCHIVE to DESTINATION following extraction MODE.
MODE can be:

- 'subdir to force creation of a new subdir with same name as
  ARCHIVE;
- 'local to force extraction of files in current directory even
  if ARCHIVE has all files inside one subdirectory;
- 'respect to keep ARCHIVE's directory structure.

If CLEAN is t, some useless files are deleted such as __MACOSX."
  (let ((archive (expand-file-name archive))
        (destination (expand-file-name destination)))
    (save-window-excursion
      (case mode
        ('subdir (extractor-extract-subdir archive destination))
        ('local (extractor-extract-local archive destination))
        ('respect (extractor-extract-respect archive destination))
        (t (error "Unknown extraction mode %s" mode)))
      (when clean
        (extractor-clean-directory destination)))))

(defun extractor-extract-subdir (arch dest)
  "Extract ARCH into a dedicated sub-directory under DEST.
The sub-directory name is generated from ARCH name using
`extractor-archive-name'."
  (let ((subdir (expand-file-name (extractor-archive-name arch)
                                  dest)))
    (extractor-extract-local arch subdir)))

(defun extractor-extract-local (arch dest)
  "Extract ARCH files to DEST. Ignore ARCH potential directory."
  (let* ((tempdir (extractor-extract-to-tempdir arch dest))
         (extracted-files (directory-files tempdir nil nil t))
         (interesting-files (remove-if #'extractor-ignored-file-p
                                       extracted-files))
         (first-file (expand-file-name (car interesting-files)
                                       tempdir))
         (from (if (and (= 1 (length interesting-files))
                        (file-directory-p first-file))
                   first-file
                 tempdir)))
    (extractor-move-files from dest)
    (delete-directory tempdir t)))

;;;###autoload
(defun extractor-extract (&optional local)
  "Extract archive file at point.
Set LOCAL to t or interactivelly call with prefix arg when no
sub-directory is desired."
  (interactive "P")
  (extractor-extract-internal (dired-file-name-at-point)
                              (dired-current-directory)
                              (if local 'local 'subdir)
                              t)
  (revert-buffer))

(eval-after-load 'dired+
  '(define-key dired-mode-map [remap diredp-compress-this-file] 'extractor-extract))

(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "z") 'extractor-extract))

(provide 'extractor)
;;; extractor.el ends here
