(defun eUni-start ()
  (interactive)

  (delete-other-windows)

  (split-window-horizontally (floor (* .27 (window-width))))

  (other-window 1)

  (split-window-vertically (floor (* .75 (window-height)))))
