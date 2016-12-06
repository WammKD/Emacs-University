(defconst eUni-instruction-buffer-name "*Instructions*"
  "The name of the buffer/window that will hold the current instructions.")
(defconst eUni-lesson-buffer-name "*Lesson Name*"
  "The name of the buffer/window that will hold where the lesson will be written.")
(defconst eUni-evaluation-buffer-name "*Evaluations*"
  "The name of the buffer/window that will hold the evaluated results of the user's code.")

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

  (switch-erase-insert-toBeg eUni-lesson-buffer-name lesson))
