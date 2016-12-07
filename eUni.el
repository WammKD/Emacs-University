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
    (erase-buffer)
    (insert contents)
    (beginning-of-buffer))

  (delete-other-windows)

  (switch-erase-insert-toBeg eUni-instruction-buffer-name instructions)
  (setq buffer-read-only t)
  (split-window-horizontally (floor (* .27 (window-width))))
  (turn-on-visual-line-mode)

  (other-window 1)

  (switch-to-buffer eUni-evaluation-buffer-name)
  (setq buffer-read-only t)
  (split-window-vertically (floor (* .75 (window-height))))

  (switch-erase-insert-toBeg eUni-lesson-buffer-name lesson)
  (ruby-mode))

(defun eUni-RubyAssert (assert)
  (message "Checking answers…")

  (write-region
    (concat
      (buffer-substring-no-properties (point-min) (point-max))
      (concat "\n\nIO.write('/tmp/crap.txt', " assert ")"))
    nil
    "/tmp/crap.rb")

  (with-temp-buffer
    (shell-command
      "ruby /tmp/crap.rb"
      t))

  (write-region
    (buffer-substring-no-properties (point-min) (point-max))
    nil
    "/tmp/crap.rb")

  (async-shell-command "irb /tmp/crap.rb" eUni-evaluation-buffer-name)

  ;; (call-process-shell-command
  ;;   (concat "ruby " (concat 
  ;; 		      (buffer-substring-no-properties (point-min) (point-max))
  ;; 		      (concat "\n\nIO.write('/tmp/crap.txt', " assert ")")))
  ;;   nil
  ;;   "*Shell Command Output*"
  ;;   t)
  (string-equal
    "true"
    (with-temp-buffer
      (insert-file-contents "/tmp/crap.txt")
      (buffer-string))))
