(defconst eUni-temp-directory "/tmp/"
  "The temporary directory to output files for interpreting code.")
(defconst eUni-instruction-buffer-name "*Instructions*"
  "The name of the buffer/window that will hold the current instructions.")
(defconst eUni-lesson-buffer-name "*Lesson Name*"
  "The name of the buffer/window that will hold where the lesson will be written.")
(defconst eUni-evaluation-buffer-name "*Evaluations*"
  "The name of the buffer/window that will hold the evaluated results of the user's code.")

(setq same-window-buffer-names
  (cons eUni-evaluation-buffer-name same-window-buffer-names))

(defun eUni-start (instructions lesson)
  (interactive)

  (defun switch-erase-insert-toBeg (name contents)
    (switch-to-buffer name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert contents)
    (beginning-of-buffer))

  (delete-other-windows)

  (switch-erase-insert-toBeg eUni-instruction-buffer-name instructions)
  (setq buffer-read-only t)
  (split-window-horizontally (floor (* .27 (window-width))))
  (turn-on-visual-line-mode)

  (other-window 1)

  (switch-erase-insert-toBeg eUni-evaluation-buffer-name "")
  (setq buffer-read-only t)
  (split-window-vertically (floor (* .75 (window-height))))

  (switch-erase-insert-toBeg eUni-lesson-buffer-name lesson)
  (ruby-mode))

(defun eUni-generalAssert (append           language-file
			   interpreted-file interpreter   outputer)
  (setq users-work (concat
		     (buffer-substring-no-properties (point-min) (point-max))
		     "\n"))

  (write-region (concat users-work append) nil language-file)

  (with-temp-buffer
    (shell-command (concat interpreter " " language-file) t))

  (write-region users-work nil language-file)

  (message "Checking answers…")
  (async-shell-command (concat
			 outputer
			 " "
			 language-file) eUni-evaluation-buffer-name)

  (string-equal
    "true"
    (with-temp-buffer
      (insert-file-contents interpreted-file)
      (buffer-string))))

(defun eUni-RubyAssert (assert lesson-name)
  (setq beginning-of-file-name (concat eUni-temp-directory lesson-name))

  (eUni-generalAssert
    (concat "\n\nIO.write('" beginning-of-file-name ".txt', " assert ")")
    (concat                  beginning-of-file-name ".rb")
    (concat                  beginning-of-file-name ".txt")
    "ruby"
    "irb"))
