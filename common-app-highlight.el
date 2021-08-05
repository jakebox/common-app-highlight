;;; package --- common-app-highlight.el

;; Author: Jake B
;; URL: https://github.com/jakebox/common-app-highlight

;;; Commentary:

;; Shows you how close to 150 characters a buffer is.
;; Highlight code from:
;; https://emacs.stackexchange.com/questions/14491/how-to-change-the-background-foreground-color-of-some-characters-words (wasamasa)

;;; Code:

(defvar common-app-highlight--hl-color "#00ff00") ;; Default

(defun common-app-highlight--calc-highlight ()
  "Calculate and apply highlight according to buffer character count."
  (interactive)
  (let ((buff-char-count (string-bytes (buffer-substring-no-properties (point-min) (point-max))))) ;; Let buff-char-count = byte count of entire buffer
	(cond ((< buff-char-count 100) (setq common-app-highlight--hl-color "default")) ;; Set common-app-highlight--hl-color based on character count
		  ((< buff-char-count 125) (setq common-app-highlight--hl-color "#f06a11"))
		  ((< buff-char-count 135) (setq common-app-highlight--hl-color "#ff5811"))
		  ((< buff-char-count 140) (setq common-app-highlight--hl-color "#ff4111"))
		  ((< buff-char-count 150) (setq common-app-highlight--hl-color "#35fc5a"))
		  ((= buff-char-count 150) (setq common-app-highlight--hl-color "#35fc5a"))
		  ((> buff-char-count 150) (setq common-app-highlight--hl-color "#ff0000")))
	(common-app-highlight--hl-buffer)))

(defun common-app-highlight--hl-thing (boundaries)
  (put-text-property (car boundaries) (cdr boundaries)
                     'font-lock-face `(:foreground ,common-app-highlight--hl-color)))

(defun common-app-highlight--reset (boundaries)
  "Reset the face within 'BOUNDARIES'."
  (put-text-property (car boundaries) (cdr boundaries)
                     'font-lock-face `(:foreground default)))

(defun common-app-highlight--hl-buffer ()
  "Set color of text in buffer."
  (common-app-highlight--hl-thing (cons (point-min) (point-max))))

(defun common-app-highlight--reset-buffer ()
  "Interactive function to reset buffer's face."
  (common-app-highlight--reset (cons (point-min) (point-max))))

(defun common-app-highlight-enable ()
  "Enable common-app-highlight."
  (interactive)
  (add-hook 'post-command-hook #'common-app-highlight--calc-highlight nil t))

(defun common-app-highlight-disable ()
  "Disable common-app-highlight."
  (interactive)
  (remove-hook 'post-command-hook #'common-app-highlight--calc-highlight t)
  (common-app-highlight--reset-buffer))

(provide 'common-app-counter)

;;; common-app-counter.el ends here
