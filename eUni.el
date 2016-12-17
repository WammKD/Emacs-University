(require 'button)

(defconst eUni-CLASS-FILE-DELIMETER "\n######\n"
  "The delimeter to parse Emacs University class files.")
(defconst eUni-CLASSES-DIRECTORY "~/.emacs.d/emacs-university/"
  "The default directory that university lessons are kept in, under the .emacs.d directory.")
(defconst eUni-TEMP-DIRECTORY "/tmp/"
  "The temporary directory to output files for interpreting code.")
(defconst eUni-INTERPRETED-FILE-EXT ".txt"
  "The file extention (just a .txt file) of the output of asserting something against the user's code.")
(defconst eUni-CLASSES-BUFFER-NAME "*Emacs University*"
  "The name of the buffer/window that will hold the current available classes.")
(defconst eUni-INSTRUCTION-BUFFER-NAME "*Instructions*"
  "The name of the buffer/window that will hold the current instructions.")
(defconst eUni-EVALUATION-BUFFER-NAME "*Evaluations*"
  "The name of the buffer/window that will hold the evaluated results of the user's code.")
(defconst eUni-LANGUAGE-FUNCTIONS '((Ruby . ((eUni-RubyAssert . eUni-RubyEvaluate) . ruby-mode))
				    (Java . ((eUni-JavaAssert . eUni-JavaEvaluate) . java-mode)))
  "An a-list of language names and their respective assert functions.")
(defvar   eUni-current-class-lessons '()
  "The files of the next lessons for the particular class.")
(defvar   eUni-current-lesson-buffer-name "*Lesson Name*"
  "The name of the buffer/window that will hold where the lesson will be written.")
(defvar   eUni-current-lesson-language ""
  "The language in which the lesson is written.")
(defvar   eUni-current-lesson-commands '()
  "The list of commands for the current lesson.")

(defun eUni-start ()
  (interactive)

  (when (not (file-exists-p eUni-CLASSES-DIRECTORY))
    (make-directory eUni-CLASSES-DIRECTORY t))
 
  (delete-other-windows)

  (switch-to-buffer eUni-CLASSES-BUFFER-NAME)
  (setq buffer-read-only nil)
  (erase-buffer)

  (insert (concat
	    "'||''''|\n"
	    " ||   .\n"
	    " ||'''|  '||),,(|,   '''|.  .|'', (''''\n"
	    " ||       || || ||  .|''||  ||     `'')\n"
	    ".||....| .||    ||. `|..||. `|..' `...'\n"
	    "\n"
	    "\n"
	    "'||   ||`                                                ||\n"
	    " ||   ||            ''                             ''    ||\n"
	    " ||   ||  `||''|,   ||  \\  // .|''|, '||''| (''''  ||  ''||''  '||  ||`\n"
	    " ||   ||   ||  ||   ||   \\//  ||..||  ||     `'')  ||    ||     `|..||\n"
	    " `|...|'  .||  ||. .||.   \/   `|...  .||.   `...' .||.   `|..'      ||\n"
	    "                                                                 ,  |'\n"
	    "              T h e   p l a c e   t o   c o d e                   ''\n\n\n\n"))

  (insert (concat
	    "\tWelcome! Listed below are the available courses you "
	    "currently have stored on your computer. Click one to "
	    "begin a lesson!\n\n"))

  (let ((num 1))
    (dolist (lesson (cddr (directory-files eUni-CLASSES-DIRECTORY)))
      (when (string-match "_lssn\\'" lesson)
	(insert (concat "  " (number-to-string num) ". "))

	(insert-button
	  (replace-regexp-in-string "_lssn\\'" "" lesson)
	  'action
	  (lambda (x)
	    (setq eUni-current-class-lessons
	      (cddr (directory-files (concat
				       eUni-CLASSES-DIRECTORY
				       (button-get x 'lssn)) t)))
	    (eUni-load-class))
	  'lssn
	  lesson)

	(insert "\n")

	(setq num (1+ num)))))

  (turn-on-visual-line-mode)

  (setq buffer-read-only t))

(defun eUni-load-class ()
  (let* ((lesson       (car eUni-current-class-lessons))
	 (commands     (with-temp-buffer
			 (insert-file-contents lesson)
			 (split-string
			   (buffer-string)
			   eUni-CLASS-FILE-DELIMETER
			   t)))
	 (instructions (car  commands))
	 (lesson-code  (cadr commands)))
    (setq eUni-current-class-lessons   (cdr eUni-current-class-lessons))
    (setq eUni-current-lesson-language (intern (file-name-extension lesson)))
    (setq eUni-current-lesson-commands (cddr commands))
    (setq eUni-current-lesson-buffer-name
       (concat
	 "*"
	 (replace-regexp-in-string "[0-9]*_" "" (file-name-base lesson))
	 "*"))

    (eUni-load-lesson instructions lesson-code)))

(defun eUni-load-lesson (instructions lesson-code)
  (defun switch-erase-insert-toBeg (name contents)
    (switch-to-buffer name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert contents)
    (beginning-of-buffer))

  (delete-other-windows)

  (switch-erase-insert-toBeg eUni-INSTRUCTION-BUFFER-NAME instructions)
  (setq buffer-read-only t)
  (split-window-horizontally (floor (* .27 (window-width))))
  (turn-on-visual-line-mode)

  (other-window 1)

  (switch-erase-insert-toBeg eUni-EVALUATION-BUFFER-NAME "")
  (setq buffer-read-only t)
  (split-window-vertically (floor (* .75 (window-height))))

  (switch-erase-insert-toBeg eUni-current-lesson-buffer-name lesson-code)
  (funcall (eUni-get-language-emacs-mode eUni-current-lesson-language)))



(defun eUni-execute-lesson ()
  (interactive)

  (catch 'err
    (dolist (command eUni-current-lesson-commands)
      (if (string-match "^ASSERT\n" command)
	  (let* ((assert-args (split-string command "ASSERT\n\\|\nASSERT\n" t))
		 (assertion   (car  assert-args))
		 (error-msg   (cadr assert-args)))
	    (when (not (eUni-generalAssert-init
			 eUni-current-lesson-language
			 assertion))
	      (eUni-generalEvaluate-init eUni-current-lesson-language)
	      (message error-msg)
	      (throw 'err t)))
	(when (string-match "^EVALUATE " command)
	  (eUni-generalEvaluate-init eUni-current-lesson-language)
	  (message (substring command 5))
	  (throw 'err t)))))

  (eUni-load-class))
      

(defun eUni-make-file-bash-safe (file)
  (replace-regexp-in-string " " "\\\\ " file))
(defun eUni-get-language-assert-function (symbol)
  (caadr (assoc symbol eUni-LANGUAGE-FUNCTIONS)))
(defun eUni-get-language-eval-function   (symbol)
  (cdadr (assoc symbol eUni-LANGUAGE-FUNCTIONS)))
(defun eUni-get-language-emacs-mode      (symbol)
  (cddr (assoc symbol eUni-LANGUAGE-FUNCTIONS)))

(defun eUni-generalAssert-init (assert-function-symbol assert)
  (let* ((lesson-name            (substring
				   eUni-current-lesson-buffer-name 1 -1))
	 (beginning-of-file-name (concat eUni-TEMP-DIRECTORY lesson-name)))
    (funcall (eUni-get-language-assert-function assert-function-symbol)
	       assert
	       lesson-name
	       beginning-of-file-name
	       (concat beginning-of-file-name eUni-INTERPRETED-FILE-EXT))))

(defun eUni-generalAssert (append           beginning-of-file-name
			   interpreted-file language-file-ext      interpreter)
  (let* ((language-file  (concat beginning-of-file-name language-file-ext))
	 (bash-safe-name (eUni-make-file-bash-safe language-file))
	 (users-work     (concat (replace-regexp-in-string
				   (rx (or
					 (: bos (* (any " \t\n")))
					 (: (* (any " \t\n")) eos)))
				   ""
				   (replace-regexp-in-string
				     "\n+"
				     "\n"
				     (buffer-substring-no-properties
				       (point-min)
				       (point-max)))) "\n")))
    (write-region (concat users-work append) nil language-file)

    (with-temp-buffer
      (shell-command (concat
		       interpreter
		       " "
		       bash-safe-name) t))

    (write-region users-work nil language-file)

    (string-equal
      "true"
      (with-temp-buffer
	(insert-file-contents interpreted-file)
	(buffer-string)))))

(defun eUni-generalEvaluate-init (eval-function-symbol)
  (funcall (eUni-get-language-eval-function eval-function-symbol)
	     (substring eUni-current-lesson-buffer-name 1 -1)))
  
(defun eUni-generalEvaluate (outputer lesson-name language-file-ext)
  (let* ((beginning-of-file-name (concat eUni-TEMP-DIRECTORY          lesson-name))
	 (language-file          (concat beginning-of-file-name language-file-ext)))
    (message "Checking answers…")

    (async-shell-command
      (concat outputer " " (eUni-make-file-bash-safe language-file))
      eUni-EVALUATION-BUFFER-NAME)))


(defun eUni-RubyAssert (assert lesson-name beginning-of-file-name interpreted-file)
  (eUni-generalAssert
    (concat "\n\nIO.write('" interpreted-file "', " assert ")")
    beginning-of-file-name
    interpreted-file
    ".rb"
    "ruby"))
(defun eUni-RubyEvaluate (lesson-name)
  (when (not (file-exists-p (concat eUni-TEMP-DIRECTORY lesson-name ".rb")))
    (eUni-generalAssert-init eUni-current-lesson-language "1 == 1"))

  (eUni-generalEvaluate "irb" lesson-name ".rb"))
