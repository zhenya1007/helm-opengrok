;;; A Helm interface to output from OpenGrok. -*- lexical-binding: t -*-
;; Copyright (C) 2017 Zhenya Roubinchtein <zhenya1007@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-grep)
(require 'sgml-mode)
(require 'xml)

(defun helm-opengrok-find-glob-in-dirs (glob dirs)
  (helm-aif
      (cl-loop for dir in dirs
	       for files = (file-expand-wildcards (expand-file-name glob dir))
	       if files
	       if (or (null (cdr files))
			 (cl-loop for f in files
				  for p = nil then f
				  always (or (null p)
					     (string= (file-truename f)
						      (file-truename p)))))
	       return (car files))
      it
    (signal 'file-missing (format "Cannot locate \"%s\" in %s" glob dirs))))

(defmacro helm-opengrok-per-platform (&rest clauses)
  (cl-labels ((process-clauses (clauses)
			       (cl-loop for c in clauses
					collect `(,(car c) (backquote ,(cadr c))))))
   `(cl-case system-type ,@(process-clauses clauses))))

(defgroup helm-opengrok nil
  "Settings for helm-opengrok.")

(defcustom helm-opengrok-java-directory-list
  (helm-opengrok-per-platform
   ((gnu/linux berkeley-unix) ("/usr/local/bin/" "/usr/bin/"))
   (windows-nt  ("C:\\Program Files\\Java")))
    "A list of directories to check for the presence of Java executable."
  :group 'helm-opengrok
  :type '(repeat string))

(defcustom helm-opengrok-java-file-name
  (helm-opengrok-per-platform
   ((gnu/linux berkeley-unix) "java")
   (windows-nt "java.exe"))
  "The name of the Java(TM) executable to be used with OpenGrok."
  :group 'helm-opengrok
  :type 'string)

(defcustom helm-opengrok-java-options '("-Xmx2048m")
  "The list of command-line arguments to pass to Java(TM) when running OpenGrok JAR."
  :group 'helm-opengrok
  :type '(repeat string))

(defcustom helm-opengrok-java-debug '()
  "The list of command-line arguments to pass to Java(TM) for debugging.")

(defcustom helm-opengrok-jar-directory-list
  (helm-opengrok-per-platform
   ((gnu/linux berkeley-unix)
    ("/usr/share/java"			; Debian Java Policy Manual
     "/usr/local/share/java"		; sounds like a good idea
     ,(expand-file-name "src/OpenGrok/dist" "~")
     ,(expand-file-name "src/OpenGrok/dist" "/usr/local"))))
  "A list of directories to probe for the OpenGrok JAR."
  :group 'helm-opengrok
  :type '(repeat string))

(defcustom helm-opengrok-opengrok-jar-file-name
  "opengrok*.jar"
  "The file name of the OpenGrok JAR."
  :group 'helm-opengrok
  :type 'string)

(defcustom helm-opengrok-configuration-file-name "configuration.xml"
  "The OpenGrok configuration file.
This is the argument to the \"-R\" command-line option."
  :group 'helm-opengrok
  :type 'string)

(defcustom helm-opengrok-instance-base (file-name-as-directory "user/opengrok")
  "A path, relative to the root of the source tree, to the OpenGrok data directory."
  :group 'helm-opengrok
  :type 'string)

(defcustom helm-opengrok-configuration-directory
  (file-name-as-directory "etc")
  "A path, relative to `helm-opengrok-instance-base', to the OpenGrok configuration file (`helm-opengrok-configuration-file-name')."
  :group 'helm-opengrok
  :type 'string)

(defcustom helm-opengrok-search-class-name "org.opensolaris.opengrok.search.Search"
  "The name of the class to use for command-line-searches."
  :group 'helm-opengrok
  :type 'string)

(defcustom helm-opengrok-logging-properties-github-url
  "https://raw.githubusercontent.com/OpenGrok/OpenGrok/master/logging.properties"
  "The URL to OpenGrok's logging.properties file."
  :group 'helm-opengrok
  :type 'string)

(defcustom helm-opengrok-logging-properties-use-github-p t
  "Whether to attempt to retrieve the logging.properties file from Github.
If nil, a copy included in this file is used instead."
  :group 'helm-opengrok
  :type 'boolean)

(defvar helm-opengrok-history nil
  "History of the OpenGrok searches.")


;; a transcription into Elisp of FindCtags in the OpenGrok wrapper script
(defun helm-opengrok-find-ctags ()
  (cl-loop for b in '("ctags-exuberant" "exctags" "ctags")
	   for p = (and b (executable-find b))
	   if (and p (with-temp-buffer
		       (goto-char (point-min))
		       (when (zerop (call-process p nil t nil "--version"))
			 (let ((pt (point)))
			   (goto-char (point-min))
			   (search-forward "Exuberant Ctags" pt t)))))
	   return p))

(defun helm-opengrok-shell-command-sans-newline (cmd &optional infile)
  (with-temp-buffer
    (call-process-shell-command cmd infile t)
    (goto-char (point-min))
    (skip-chars-forward "[:space:]")
    (let ((pt (point)))
      (buffer-substring-no-properties pt (progn (end-of-line) (point))))))

;; a transliteration of FindJavaHome into Elisp
(defun helm-opengrok-find-java-home ()
  (cl-flet ((dirname (fn) (directory-file-name (file-name-directory fn))))
    (cl-case system-type
      (darwin (helm-opengrok-shell-command-sans-newline "/usr/libexec/java_home"))
      (linux (dirname (dirname (file-truename "/etc/alternatives/java"))))
      (berkeley-unix (dirname (dirname (executable-find java)))))))

(defun helm-opengrok-java-properties ()
  (cl-loop for (base prop) in '(("bk" "BitKeeper") ("hg" "Mercurial")
				("cvs" "cvs") ("svn" "Subversion")
				("sccs" "SCCS") ("cleartool" "ClearCase")
				("git" "git") ("p4" "Perforce")
				("mtn" "Monotone") ("bzr" "Bazaar"))
	   for exe = (executable-find base)
	   when exe collect (format "-Dorg.opensolaris.opengrok.history.%s=%s"
				    prop exe)))

(defun helm-opengrok-java-minimal-invocation (instance-base &optional args)
  (let ((java (helm-opengrok-find-glob-in-dirs helm-opengrok-java-file-name
					       helm-opengrok-java-directory-list)))
    `(,java
      ,@helm-opengrok-java-options
      ,@(helm-opengrok-java-properties)
      ,@helm-opengrok-java-debug
      ,(format "-Djava.util.logging.config.file=%s"
	       (expand-file-name "logging.properties" instance-base))
      "-jar" ,(helm-opengrok-find-glob-in-dirs helm-opengrok-opengrok-jar-file-name
					       helm-opengrok-jar-directory-list)
      ,@args)))

(defun helm-opengrok-common-invocation (instance-base &optional args)
  (helm-opengrok-java-minimal-invocation instance-base
   `("-G" "-r" "on" "-D" "-C"
     ,@(helm-aif (helm-opengrok-find-ctags) (list "-c" it))
     ,@(let ((fn (expand-file-name "etc/ctags.config" instance-base)))
	 (when (file-readable-p fn) (list "-o" fn)))
     "-a" "on"
     ,@args)))

(defun helm-opengrok-std-invocation (src-root instance-base &optional args)
  (helm-opengrok-common-invocation instance-base
   `("-W" ,(expand-file-name helm-opengrok-configuration-file-name
			     (expand-file-name helm-opengrok-configuration-directory
					       instance-base))
     "-S" "-P"
     "-s" ,src-root
     "-d" ,(expand-file-name "data" instance-base)
     ,@args)))

(defvar helm-opengrok-canned-logging-properties
"############################################################
#  	Logging Configuration File
#
# You can use a different file by specifying a filename
# with the java.util.logging.config.file system property.  
# For example java -Djava.util.logging.config.file=myfile
############################################################

############################################################
#  	Global properties
############################################################

# \\\"handlers\\\" specifies a comma separated list of log Handler 
# classes.  These handlers will be installed during VM startup.
# Note that these classes must be on the system classpath.
# By default we only configure a ConsoleHandler, which will only
# show messages at the INFO and above levels.
#handlers= java.util.logging.ConsoleHandler

# To also add the FileHandler, use the following line instead.
handlers= java.util.logging.FileHandler, java.util.logging.ConsoleHandler

# Default global logging level.
# This specifies which kinds of events are logged across
# all loggers.  For any given facility this global level
# can be overridden by a facility specific level
# Note that the ConsoleHandler also has a separate level
# setting to limit messages printed to the console.
#
# Available logging levels: OFF, SEVERE, WARNING, INFO, CONFIG, FINE, FINER, FINEST, ALL
# OFF will give no output, SEVERE will give very little output, FINEST and ALL will give lots of output.
#
.level= ALL

############################################################
# Handler specific properties.
# Describes specific configuration info for Handlers.
############################################################

# default file output is in user's home directory.
#java.util.logging.FileHandler.pattern = %hjava%u.log
java.util.logging.FileHandler.pattern = opengrok%g.%u.log
java.util.logging.FileHandler.limit = 52428800
java.util.logging.FileHandler.count = 3
java.util.logging.FileHandler.level = ALL
#java.util.logging.FileHandler.formatter = java.util.logging.XMLFormatter
java.util.logging.FileHandler.formatter = org.opensolaris.opengrok.logger.formatter.SimpleFileLogFormatter

# Limit the message that are printed on the console to WARNING and above (default \\\"quiet\\\" mode).
# if verbose is set, then log INFO and above
java.util.logging.ConsoleHandler.level = WARNING
java.util.logging.ConsoleHandler.formatter = org.opensolaris.opengrok.logger.formatter.SimpleConsoleFormatter


############################################################
# Facility specific properties.
# Provides extra control for each logger.
############################################################

# For example, set the com.xyz.foo logger to only log SEVERE
# messages:
#com.xyz.foo.level = SEVERE
org.opensolaris.opengrok.level = ALL
")



(defun helm-opengrok-create-logging-properties (instance-base)
  (let ((target-file-name (expand-file-name "logging.properties" instance-base)))
    (unless (file-exists-p target-file-name)
      (with-temp-buffer
	(helm-aif (and helm-opengrok-logging-properties-use-github-p
		       (url-retrieve-synchronously helm-opengrok-logging-properties-github-url))
	    (let ((target (current-buffer)))
	      (with-current-buffer it
		(append-to-buffer target (point-min) (point-max))))
	  (insert helm-opengrok-canned-logging-properties))
	(goto-char (point-min))
	(when (re-search-forward "java.util.logging.FileHandler.pattern=\\(.+\\)" (point-max) t)
	  (replace-match (expand-file-name "log/opengrok%g.%u.log" instance-base) nil nil nil 1))
	(write-region (point-min) (point-max) target-file-name)))))

;; A transcription into Elisp of CreateRuntimeRequirements in OpenGrok script
(defun helm-opengrok-create-runtime-requirements (instance-base)
  (unless (file-directory-p instance-base)
    (make-directory instance-base t))
  (cl-loop for dir in `("data" ,helm-opengrok-configuration-directory "log")
	   for target = (expand-file-name dir instance-base)
	   unless (file-directory-p target)
	   do (make-directory target))
  (helm-opengrok-create-logging-properties instance-base))

(defun helm-opengrok-make-index (src-root instance-base)
  "Create opengrok index."
  (interactive
   (let* ((src-root (read-directory-name "Directory to index: " default-directory nil t))
	  (instance-base (read-directory-name
		      (format "OpenGrok instance base for source tree in %s: " src-root)
		      (and helm-opengrok-instance-base
			   (expand-file-name helm-opengrok-instance-base src-root)))))
     (list src-root instance-base)))
  (helm-opengrok-create-runtime-requirements instance-base)
  (helm-opengrok-record-instance-base src-root instance-base)
  (make-process
   :name "opengrok-index"
   :command (helm-opengrok-std-invocation src-root instance-base)
   :buffer (get-buffer-create (format "helm-opengrok-make-index %s %s" src-root instance-base))
   :sentinel (lambda (p s) (message "opengrok-index: %s" s))))

(defun helm-opengrok-record-instance-base (src-dir instance-base)
  "If INSTANCE-BASE is different from the default, save its value as a directory-local variable.
The default may be changed by setting `helm-opengrok-instance-base'.
The \"directory-local-variable\" in which the value is saved is local to SRC-DIR."
  (let ((src-dir (file-truename src-dir))
	(instance-base (file-truename instance-base)))
    (unless (string= (expand-file-name helm-opengrok-instance-base src-dir) instance-base)
      (helm-aif (cl-loop for file in `(,(concat (file-name-sans-extension  dir-locals-file)
						"-2." (file-name-extension dir-locals-file))
				       ,dir-locals-file)
			 for fn = (expand-file-name file src-dir)
			 when (and (not (file-exists-p fn)) (file-writable-p fn))
			 return file)
	  (let ((default-directory src-dir)
		(dir-locals-file it))
	    (add-dir-local-variable 'helm-opengrok 'helm-opengrok-instance-base
				    (file-relative-name instance-base src-dir))
	    (helm-aif (find-buffer-visiting (expand-file-name dir-locals-file src-dir))
		(let ((backup-inhibited t))
		  (save-buffer it))))))))

;; the `hack-dir-local-variables' function is a bit too attached to the notion that
;; "the current buffer is a file-visiting buffer" to be usable for my purposes as-is
;; so I am copying in just the part that I want from the definition of that function
(defun helm-opengrok-retrieve-instance-base (src-root)
  (let ((variables-file (dir-locals-find-file
                         (expand-file-name "dummy" (file-truename src-root))))
	class dir-name)
    (cond
     ((stringp variables-file)
      (setq dir-name (file-name-directory variables-file)
	    class (dir-locals-read-from-file variables-file)))
     ((consp variables-file)
      (setq dir-name (nth 0 variables-file))
      (setq class (nth 1 variables-file))))
    (when class
      (or (helm-aif (assoc 'helm-opengrok
			   (dir-locals-get-class-variables class))
	      (helm-aif (assoc-default 'helm-opengrok-instance-base (cdr it))
		  (expand-file-name it dir-name)))
	  (helm-opengrok-retrieve-instance-base (file-name-as-directory
						 (file-name-directory dir-name)))))))

;;; Helm sources definitions

;; FIXME: parse the output of OpenGrok here instead
(defun helm-opengrok-get-types ()
  (list "-f" "-r" "-d"))

(defun helm-opengrok-base-command ()
  "The basic incantation to invoke OpenGrok search."
  `(,helm-opengrok-java-file-name
    ,@helm-opengrok-java-options
    "-cp"
    ,(helm-opengrok-find-glob-in-dirs helm-opengrok-opengrok-jar-file-name
				      helm-opengrok-jar-directory-list)))

;; mostly copied from helm-grep-ag-prepare-cmd-line
(defun helm-opengrok-prepare-cmd-line (pattern directory type)
  "Prepare OpenGrok command line to search PATTERN in DIRECTORY.
When TYPE is specified it is one of the types returned by `helm-opengrok-get-types'."
  (let* ((instance-base (helm-opengrok-retrieve-instance-base directory))
	 (config-dir (expand-file-name helm-opengrok-configuration-directory
				       instance-base))
	 (lst `(,@(helm-opengrok-base-command)
		,helm-opengrok-search-class-name
		"-R" ,(expand-file-name helm-opengrok-configuration-file-name config-dir)))
	 (cmd (mapconcat #'identity
			 `(,@lst ,type ,(shell-quote-argument pattern))
			 " ")))
    cmd))

;; mostly copied from helm-grep-ag-init
(defun helm-opengrok-init (directory type)
  "Start OpenGrok process in DIRECTORY with the query type TYPE."
  (let ((default-directory directory)
        (cmd-line (helm-opengrok-prepare-cmd-line helm-pattern directory type))
        (start-time (float-time))
        (proc-name (format "OpenGrok %s %s" directory type)))
    (set (make-local-variable 'helm-opengrok-cmd-line) cmd-line)
    (with-current-buffer (get-buffer-create "*helm-opengrok-init-debug*")
      (insert (format "Starting %s process in directory `%s'"
		      proc-name directory))
      (insert "Command line used was:\n\n%s"
	      (concat ">>> " cmd-line "\n\n")))
    (prog1
        (start-file-process-shell-command
         proc-name helm-buffer cmd-line)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (let* ((err      (process-exit-status process))
                (noresult (= err 1)))
           (cond (noresult
                  (with-helm-buffer
                    (insert (concat "* Exit with code 1, no result found,"
                                    " command line was:\n\n "
                                    (propertize cmd-line
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
  (cl-loop for (prop . tags) in '((bold "b" "/b")
				  (italic "i" "/i"))
	   when (cl-member name tags :test #'string=) return prop))

(defun helm-opengrok-remove-html-markup (str)
  (with-temp-buffer
    (sgml-mode)
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
    (buffer-substring (point-min) (point-max))))

(defun helm-opengrok-split-line (line)
  (interactive (list (read-string "Line: ")))
  (cl-labels ((mk-syn-tab ()
			  (let ((tab (make-syntax-table)))
			    (modify-syntax-entry ?\[ "(]" tab)
			    (modify-syntax-entry ?\] ")[" tab)
			    tab))
	      (find-rsquare ()
			    (goto-char (point-max))
			    (when (search-backward "]" (point-min) t)
			      (find-lsquare (point))))
	      (find-lsquare (pt)
			    (when (= 1 (skip-chars-forward "]"))
			      (with-syntax-table (mk-syn-tab)
				(condition-case e
				    (backward-list)
				  (error nil)))
			      (when (= 1 (skip-chars-forward "["))
				(find-line-num (xml-substitute-special
						(buffer-substring (point) pt))))))
	      (find-line-num (txt)
			     (when (and (= -1 (skip-chars-backward "["))
					(> 0 (skip-chars-backward "[:space:][:digit:]"))
					(looking-back (regexp-quote ":")))
			       (find-file-name
				(list (buffer-substring (point)
							(progn (skip-chars-forward "[:digit:]")
							       (point)))
				      txt))))
	      (find-file-name (lst)
			      (when (and (< (skip-chars-backward "[:digit:]") 0)
					 (looking-back (regexp-quote ":")))
				(cons (buffer-substring
				       (progn (skip-chars-backward ":") (point))
				       (progn (beginning-of-line) (point)))
				      lst))))
    (with-temp-buffer
      (goto-char (point-min))
      (insert line)
      (find-rsquare))))

;; mostly copied from helm-grep--filter-candidate-1
(defun helm-opengrok-filter-candidate-1 (candidate &optional dir)
  (let* ((root   (or dir (and helm-grep-default-directory-fn
                              (funcall helm-grep-default-directory-fn))))
	 (ansi-p (string-match-p helm--ansi-color-regexp candidate))
	 (line   (if ansi-p (helm--ansi-color-apply candidate) candidate))
	 (no-html (helm-opengrok-remove-html-markup line))
         (split  (helm-opengrok-split-line no-html))
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

;; mostly copied from helm-grep-action
(defun helm-opengrok-action (candidate &optional where mark)
  "Define a default action for `helm-do-grep-1' on CANDIDATE.
WHERE can be one of other-window, other-frame."
  (let* ((split        (helm-opengrok-split-line candidate))
         (lineno       (string-to-number (nth 1 split)))
         (fname        (or (with-current-buffer
                                   (if (eq major-mode 'helm-grep-mode)
                                       (current-buffer)
                                       helm-buffer)
                                 (get-text-property (point-at-bol) 'helm-grep-fname))
                               (car split))))
    (cl-case where
      (other-window (find-file-other-window fname))
      (other-frame  (find-file-other-frame fname))
      (grep         (helm-grep-save-results-1))
      (t            (find-file fname)))
    (unless (eq where 'grep)
      (helm-goto-line lineno))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))
    ;; Save history
    (unless (or helm-in-persistent-action
                (eq major-mode 'helm-grep-mode)
                (string= helm-pattern ""))
      (setq helm-grep-history
            (cons helm-pattern
                  (delete helm-pattern helm-grep-history)))
      (when (> (length helm-grep-history)
               helm-grep-max-length-history)
        (setq helm-grep-history
              (delete (car (last helm-grep-history))
                      helm-grep-history))))))

;;mostly copied from helm-grep-persistent-action
(defun helm-opengrok-persistent-action (candidate)
  "Persistent action for `helm-do-grep-1'.
With a prefix arg record CANDIDATE in `mark-ring'."
  (if current-prefix-arg
      (helm-opengrok-action candidate nil 'mark)
    (helm-opengrok-action candidate))
  (helm-highlight-current-line))

(defcustom helm-opengrok-actions
  (helm-make-actions
   "Find File" 'helm-opengrok-action
   "Find file other frame" 'helm-opengrok-other-frame
   "Save results in grep buffer" 'helm-opengrok-save-results
   "Find file other window" 'helm-opengrok-other-window)
  "Actions for helm opengrok."
  :group 'helm-opengrok
  :type '(alist :key-type string :value-type function))

(defun helm-opengrok-other-frame (candidate)
  "Jump to result in other frame from helm grep."
  (helm-opengrok-action candidate 'other-frame))

(defun helm-opengrok-save-results (candidate)
  (helm-opengrok-action candidate 'grep))

(defun helm-opengrok-other-window (candidate)
  "Jump to result in other window from helm grep."
  (helm-opengrok-action candidate 'other-window))

;; mostly copied from helm-grep-ag-class
(defclass helm-opengrok-class (helm-source-async)
  ((nohighlight :initform t)
   (history :initform 'helm-opengrok-history)
   (filter-one-by-one :initform 'helm-opengrok-filter-one-by-one)
   (persistent-action :initform 'helm-opengrok-persistent-action)
   (persistent-help :initform "Jump to line (`C-u' Record in mark ring)")
   (candidate-number-limit :initform 99999)
   (requires-pattern :initform 2)
   (nomark :initform t)
   (action :initform 'helm-opengrok-actions)))

(defvar helm-source-opengrok nil)

(defmethod helm--setup-source ((source helm-opengrok-class))
  (call-next-method)
  (helm-aif (and helm-follow-mode-persistent
                 helm-source-opengrok
                 (assoc-default 'follow helm-source-opengrok))
      (setf (slot-value source 'follow) it)))

(defvar helm-opengrok-history nil)

(defun helm-opengrok-query-1 (directory)
  (let ((str (if (region-active-p)  ; copied from definition of helm-etags-select
                 (buffer-substring-no-properties
                  (region-beginning) (region-end))
                 ;; Use a raw syntax-table to determine tap.
                 ;; This may be wrong when calling etags
                 ;; with hff from a buffer that use
                 ;; a different syntax, but most of the time it
                 ;; should be better.
                 (with-syntax-table (standard-syntax-table)
                   (thing-at-point 'symbol))))
	(type (helm-comp-read
	       "Query type: " (helm-opengrok-get-types)
	       :must-match t
	       :fc-transformer 'helm-adaptive-sort
	       :buffer "*helm OpenGrok query type*")))
    (setq helm-source-opengrok
		(helm-make-source "OpenGrok" 'helm-opengrok-class
		  :header-name (lambda (name)
				 (format "%s [%s]"
					 name (abbreviate-file-name directory)))
		  :candidates-process
		  (lambda () (helm-opengrok-init directory type))))
    (helm :sources 'helm-source-opengrok
	  :keymap helm-grep-map
	  :history 'helm-opengrok-history
	  :truncate-lines helm-grep-truncate-lines
	  :default str
	  :buffer (format "*helm-OpenGrok %s %s*" type directory))))

;; without an argument, if invoked from a file-visiting buffer, assume that the directory
;; that was indexed with OpenGrok is a parent of the buffers file name
;; with a universal argument, prompt for a file name
(defun helm-opengrok-query (&optional arg)
  (interactive "P")
  (helm-opengrok-query-1
   (if (null arg)
       (if (and helm-buffer-file-name
		(file-directory-p (file-name-directory helm-buffer-file-name)))
	   (file-name-directory helm-buffer-file-name)
	 (helm-read-file-name "source directory: "))
     (helm-read-file-name "source directory: "))))
