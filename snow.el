;; -*- lexical-binding: t; -*-

(defvar snowflakes-flakes nil)

(defvar snowflakes-amount 1)
(defvar snowflakes-rate 0.1)
(defvar snowflakes-timer nil)

(defface snowflakes-face
  '((t :foreground "white"))
  "The face.")

(defvar snowflakes-flake "*")

(defvar-local snowflakes-string nil)

(cl-defstruct snowflake
  x y)

(defun snowflakes--update-buffer (buffer)
  (with-current-buffer buffer
    (let ((lines (window-text-height))
          (cols (window-width)))
      (setq snowflakes-flakes
            (append snowflakes-flakes
                    (cl-loop for i from 0 to snowflakes-amount
                             for x = (random cols)
                             for flake = (make-snowflake :x x :y 0)
                             do (snowflakes-draw flake)
                             collect flake)))
      (setq snowflakes-flakes
            (cl-loop for flake in snowflakes-flakes
                     for new-flake = (progn
                                       ;; Delete old flake
                                       (snowflakes-draw flake 'delete)
                                       ;; Move flake
                                       (when (> (random 10) 8)
                                         (cl-incf (snowflake-x flake) (cl-case (random 2)
                                                                        (0 -1)
                                                                        (1 1)))
                                         (setf (snowflake-x flake) (clamp 0 (snowflake-x flake) (1- cols))))
                                       (cl-incf (snowflake-y flake)
                                                (pcase (random 10)
                                                  ((pred (> 2)) 0)
                                                  ((pred (< 7)) 2)
                                                  (_ 1))
                                                )
                                       (unless (>= (snowflake-y flake) (1- lines))
                                         ;; Redraw flake
                                         (snowflakes-draw flake)
                                         ;; Return moved flake
                                         flake))
                     when new-flake
                     collect new-flake)))))

(defun snowflakes-draw (flake &optional delete)
  (save-excursion
    (goto-char (point-min))
    (forward-line (snowflake-y flake))
    (forward-char (snowflake-x flake))
    (delete-char 1)
    (insert (if delete
                " "
              snowflakes-string))))

(defun let-it-snow ()
  (interactive)
  (with-current-buffer (get-buffer-create "*snow*")
    (if snowflakes-timer
        (progn
          (cancel-timer snowflakes-timer)
          (setq snowflakes-timer nil))
      ;; Start
      (setq-local cursor-type nil)
      (setq-local snowflakes-string (propertize snowflakes-flake 'face 'snowflakes-face))
      (erase-buffer)
      (save-excursion
        (dotimes (_i (window-text-height))
          (insert (make-string (window-text-width) ? )
                  "\n")))
      (goto-char (point-min))
      (setq snowflakes nil)
      (setq snowflakes-timer
            (run-at-time nil 0.1 (apply-partially #'snowflakes--update-buffer (current-buffer))))
      (pop-to-buffer (current-buffer)))))
