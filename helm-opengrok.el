;;; A Helm interface to output from OpenGrok
(require 'cl-lib)
(require 'helm)
(require 'helm-grep)

(defgroup helm-opengrok nil
  "Settings for helm-opengrok.")

(defcustom helm-opengrok-java-file-name (helm-opengrok-guess-java-command)
  "The path to the Java(TM) executable to be used with OpenGrok."
  :group helm-opengrok
  :type string)

(defcustom helm-opengrok-java-file-name-alist
    '((gnu/linux .  "/usr/bin/java")
     (bsd-unix  . "/usr/local/bin/java")
     (windows-nt . "java.exe"))
  :group helm-opengrok
  :type (alist :key-type symbol :value-type string))

(defcustom helm-opengrok-java-args '()
  "The list of command-line arguments to pass to Java(TM) when running OpenGrok JAR."
  :group helm-opengrok
  :type (repeat string))

(defcustom helm-opengrok-opengrok-jar-file-name
  (file-name-nondirectory (helm-opengrok-locate-opengrok-jar))
  "The file name of the OpenGrok JAR."
  :group helm-opengrok
  :type string)

(defcustom helm-opengrok-jar-directory
  (file-name-as-directory (file-name-directory (helm-opengrok-locate-opengrok-jar)))
  "The directory in which `helm-opengrok-opengrok-jar-file-name' is located."
  :group helm-opengrok
  :type string)

(defcustom helm-opengrok-jar-directories-list
  `("/usr/share/java" ; Debian Java Policy Manual
   "/usr/local/share/java" ; sounds like a good idea
   ,(expand-file-name "src/OpenGrok/dist" "~")
   ,(expand-file-name "src/OpenGrok/dist" "/usr/local/src"))
  :group helm-opengrok
  :type (repeat string)
  "A list of directories to probe for the presence of \"opengrok*.jar\".")

(defcustom helm-opengrok-configuration-file-name "configuration.xml"
  "The OpenGrok configuration file.
This is the argument to the \"-R\" command-line option.")

(defcustom helm-opengrok-data-directory "user/opengrok"
  "A path, relative to the root of the source tree, to the OpenGrok data directory."
  :group helm-opengrok
  :type string
  :set (lambda (name value)
	 (set-default name (file-name-as-directory value)))
  :initialize (lambda (name value)
		(custom-initialize-set name
				       (file-name-as-directory value))))

(defcustom helm-opengrok-configuration-directory
  (expand-file-name "etc" helm-opengrok-data-directory)
  "A path, relative to the root of the source tree, to the OpenGrok configuration file (`helm-opengrok-configuration-file-name')."
  :group helm-opengrok
  :type string
  :set (lambda (name value)
	 (set-default name (file-name-as-directory value)))
  :initialize (lambda (name value)
		(custom-initialize-set name
				       (file-name-as-directory value))))

(defcustom helm-opengrok-search-class-name "org.opensolaris.opengrok.search.Search"
  "The name of the class to use for command-line-searches.")

(defvar helm-opengrok-history nil
  "History of the OpenGrok searches.")

(defun helm-opengrok-guess-java-command ()
  (assoc-default system-type #'eq "java"))

(defun helm-opengrok-locate-opengrok-jar ()
   (or (cl-loop for dir in helm-opengrok-jar-directories-list
		for files = (file-expand-wildcards (expand-file-name "opengrok*.jar" dir))
		if (or (and (car files) (null (cdr files)))
		       (cl-loop for f in files
				for p = nil then f
				(always (or (null p)
					    (string= (file-truename f)
						     (file-truename p))))))
		return (car files))
       (signal 'file-missing "Cannot locate OpenGrok JAR.")))

;;; Running OpenGrok
(defcustom helm-opengrok-class-variables-list nil
  "Used internally by helm-opengrok.el")

;; restore the custom values (presumably set in previous Emacs sessions)
(when helm-opengrok-class-variables-list
  (dir-locals-set-class-variables 'helm-opengrok helm-opengrok-class-variables-list)
  (cl-loop for lst in helm-opengrok-class-variables-list
	   when (file-directory-p (car lst))
	   do (dir-locals-set-directory-class (car lst) 'helm-opengrok)))

(defun helm-opengrok-base-command ()
  "The basic incantation to invoke OpenGrok."
  (helm-opengrok-java-file-name
	      helm-opengrok-java-file-name
	      ,@helm-opengrok-java-args
	      "-jar"
	      ,(expand-file-name helm-opengrok-opengrok-jar-file-name
				 helm-opengrok-jar-directory)))

(defun helm-opengrok-get-configuration-directory (src-dir &optional config-dir)
  "Auxiliary function for `helm-opengrok-run-query'."
  (or config-dir
      (file-name-as-directory (expand-file-name helm-opengrok-configuration-directory
						src-dir))))

(defun helm-opengrok-record-data-directory (src-dir data-dir)
  "If DATA-DIR is different from the default, save its value as a directory-local variable.
The default may be changed by setting `helm-opengrok-configuration-directory'.
The \"directory-local-variable\" in which the value is saved is local to SRC-DIR."
  (unless (string= (file-truename (helm-opengrok-get-configuration-directory src-dir))
		   data-dir)
    (helm-aif (cl-loop for file in `(,(concat (file-name-sans-extension  dir-locals-file)
					      "-2." (file-name-extension dir-locals-file))
				      ,dir-locals-file)
		       when (let ((fn (expand-file-name file src-dir)))
			      (and (not (file-exists-p fn)) (file-writable-p fn)))
		       return file)
	(let ((default-directory src-dir)
	      (dir-locals-file it))
	  (add-dir-local-variable nil 'helm-opengrok-configuration-directory
				  (file-relative-name (file-truename data-dir) src-dir)))
      (add-to-list 'helm-opengrok-class-variables-list
		   `(,src-dir (nil helm-opengrok-configuration-directory .
				   ,(file-relative-name (file-truename data-dir) src-dir))))
      (dir-locals-set-class-variables 'helm-opengrok helm-opengrok-class-variables-list)
      (customize-save-variable 'helm-opengrok-class-variables-list
			       helm-opengrok-class-variables-list
			       "Used internally by helm-opengrok")
      (dir-locals-set-directory-class src-dir 'helm-opengrok))))

(defun helm-opengrok-make-index (src-root data-root)
  "Create opengrok index."
  (interactive
   (let* ((src-root (read-directory-name "Directory to index: " default-directory nil t))
	  (data-root (read-directory-name (format "Directory which stores OpenGrok data for source tree in %s: " src-root)
					  (and helm-opengrok-configuration-directory
					       (expand-file-name helm-opengrok-configuration-directory src-root))))
	  (list src-root data-root))))
  (helm-opengrok-record-data-directory src-root data-root)
  (make-process
   :name "opengrok-index"
   :command `(,@(helm-opengrok-base-command) "-s" src-root "-d" data-root)
   :sentinel (lambda (p s) (message "opengrok-index: %s" s))))

(defun helm-opengrok-run-query (src-dir query-arg query-string &optional config-dir)
  "Run OpenGrok query in SRC-DIR."
  (let ((config-dir (helm-opengrok-get-configuration-directory src-dir config-dir)))
    (make-process
   :name "opengrok-index"
   :command `(,@(helm-opengrok-base-command)
	      "-R" ,(expand-file-name helm-opengrok-configuration-file-name config-dir)
	      ,query-arg ,query-string)
   :buffer helm-buffer)))

;;; Helm sources definitions
(defun helm-opengrok-get-src-root (dir)
  (or (locate-dominating-file
       (expand-file-name "dummy" dir)
       (lambda (dir)
	 (let ((config-dir (helm-opengrok-get-configuration-directory src-dir)))
	   (file-readable-p
	    (expand-file-name helm-opengrok-configuration-file-name config-dir)))))
      dir))

;; mostly copied from `helm-grep-ag-init`
(defun helm-opengrok-init (query-arg query-string)
  "Start OpenGrok process, passing to it QUERY-STRING as the value of the QUERY-ARG option."
  (let ((default-directory (helm-opengrok-get-src-root
			    (or helm-ff-default-directory
				(helm-default-directory)
				default-directory)))
        (start-time (float-time))
        (proc-name (format "OpenGrok %s %s in %s"
			   query-arg (shell-quote-argument query-string) default-directory)))
    (helm-log "Starting %s" proc-name)
    (prog1
        (helm-opengrok-run-query default-directory query-arg query-string)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (let* ((err      (process-exit-status process))
                (noresult (= err 1)))
           (cond (noresult
                  (with-helm-buffer
                    (insert (concat "* Exit with code 1, no result found,"
                                    " command line was:\n\n "
                                    (propertize helm-grep-last-cmd-line
                                                'face 'helm-grep-cmd-line)))
                    (setq mode-line-format
                          '(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format
                                     "[%s process finished - (no results)] "
                                     (upcase proc-name))
                                    'face 'helm-grep-finish))))))
                 ((string= event "finished\n")
                  (helm-log "%s process finished with %s results in %fs"
                            proc-name
                            (helm-get-candidate-number)
                            (- (float-time) start-time))
                  (with-helm-window
                    (setq mode-line-format
                          `(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format
                                     "[%s process finished in %.2fs - (%s results)] "
                                     ,(upcase proc-name)
                                     ,(- (float-time) start-time)
                                     (helm-get-candidate-number))
                                    'face 'helm-grep-finish))))
                    (force-mode-line-update)
                    (when helm-allow-mouse
                      (helm--bind-mouse-for-selection helm-selection-point))))
                 (t (helm-log
                     "Error: %s %s"
                     proc-name
                     (replace-regexp-in-string "\n" "" event))))))))))

(defun helm-opengrok-tag-name-to-face-text-property (name)
  (cl-case name
    ("b" 'bold)
    ("i" 'italic)
    (t nil)))

(defun helm-opengrok-remove-html-markup (str)
  (with-temp-buffer
    (goto-char (point-min))
    (insert str)
    (goto-char (point-min))
    (while (search-forward "</" (point-max) t)
      (skip-chars-forward "^>")
      (let* ((name (sgml-beginning-of-tag))
	     (marker (point-marker)))
	(unwind-protect
	    (progn (sgml-skip-tag-backward 1)
		   (sgml-delete-tag 1)
		   (helm-aif (helm-opengrok-tag-name-to-face-text-property name)
		       (add-face-text-property (point) marker it)))
	  (set-marker marker nil))))
    (buffer-substring (point-min) (pount-max))))

;; mostly copied from helm-grep--filter-candidate-1
(defun helm-opengrok-filter-candidate-1 (candidate &optional dir)
  (let* ((root   (or dir (and helm-grep-default-directory-fn
                              (funcall helm-grep-default-directory-fn))))
	 (ansi-p (string-match-p helm--ansi-color-regexp candidate))
	 (line   (if ansi-p (helm--ansi-color-apply candidate) candidate))
	 (no-html (helm-opengrok-remove-html-markup line))
         (split  (helm-grep-split-line no-html))
         (fname  (if (and root split)
                     (expand-file-name (car split) root)
                   (car-safe split)))
         (lineno (nth 1 split))
         (str    (nth 2 split))
         (display-fname (cl-ecase helm-grep-file-path-style
                          (basename (and fname (file-name-nondirectory fname)))
                          (absolute fname)
                          (relative (and fname root
                                         (file-relative-name fname root))))))
    (if (and display-fname lineno str)
        (cons (concat (propertize display-fname
                                  'face 'helm-grep-file
                                  'help-echo (abbreviate-file-name fname)
                                  'helm-grep-fname fname)
                      ":"
                      (propertize lineno 'face 'helm-grep-lineno)
                      ":"
                      (if ansi-p str (helm-grep-highlight-match str t)))
              line)
        "")))

;; mostly copied from helm-grep-filter-one-by-one
(defun helm-opengrok-filter-one-by-one (candidate)
  "`filter-one-by-one' transformer function for `helm-do-grep-1'."
  (let ((helm-grep-default-directory-fn
         (or helm-grep-default-directory-fn
             (lambda () (or helm-ff-default-directory
                            (and (null (eq major-mode 'helm-grep-mode))
                                 (helm-default-directory))
                            default-directory)))))
    (if (consp candidate)
        ;; Already computed do nothing (default as input).
        candidate
        (and (stringp candidate)
             (helm-opengrok-filter-candidate-1 candidate)))))


;; mostly copied from helm-grep-ag-class
(defclass helm-opengrok-class (helm-source-async)
  ((nohighlight :initform t)
   (history :initform 'helm-opengrok-history)
   (filter-one-by-one :initform 'helm-opengrok-filter-one-by-one)
   (persistent-action :initform 'helm-grep-persistent-action)
   (persistent-help :initform "Jump to line (`C-u' Record in mark ring)")
   (candidate-number-limit :initform 99999)
   (requires-pattern :initform 2)
   (nomark :initform t)
   (action :initform 'helm-grep-actions)))

(defvar helm-opengrok-source-query-full nil)

(defun helm-opengrok-make-source-query-full (query)
  (helm-make-source (format "OpenGrok -f %s" (shell-quote-argument query))
	    'helm-opengrok-class
          :header-name (lambda (name)
                         (format "%s [%s]"
                                 name (abbreviate-file-name directory)))
          :candidates-process
          (lambda () (helm-opengrok-init "-f" query))))

(defun helm-opengrok-query-full (query)
  (interactive "P")
  (setq helm-opengrok-source-query-full
        (helm-opengrok-make-source-query-full query))
  (helm :sources 'helm-opengrok-source-query-full
        :keymap helm-grep-map
        :history 'helm-opengrok-history
        :truncate-lines helm-grep-truncate-lines
        :buffer (format "*helm OpenGrok -f %s*" (shell-quote-argument query))))

