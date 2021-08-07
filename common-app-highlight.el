;;; package --- common-app-highlight.el

;; Author: Jake B
;; URL: https://github.com/jakebox/common-app-highlight

;;; Commentary:

;; Shows you how close to 150 characters a buffer is.
;; Highlight code from:
;; https://emacs.stackexchange.com/questions/14491/how-to-change-the-background-foreground-color-of-some-characters-words (wasamasa)

;;; Code:

(defvar common-app-highlight--hl-color)
(defvar common-app-highlight--tgt-chars 150) ;; Default
(setq common-app-highlight--color-list (list "#FF8C00" "#FF9E00" "#FFD300" "#FFF600" "#35FF00" "#24FF00" "#FFD300" "#FF8C00" "#FF5700" "#FF0000"))
(setq common-app-highlight--diff-list (list 50 40 30 20 15 0 0 3 5 8 10))
;; Idea - alist (so, ex #FF8C00 . 50)


(defun common-app-highlight--calc-highlight ()
  "Calculate and apply highlight according to buffer character count."
  (interactive)
  (let ((diffls common-app-highlight--diff-list) (tgt common-app-highlight--tgt-chars)(clrls common-app-highlight--color-list) (buff-char-count (string-bytes (buffer-substring-no-properties (point-min) (point-max))))) ;; Let buff-char-count = byte count of entire buffer
	(cond
		  ((< buff-char-count (- tgt (nth 0 diffls))) (setq common-app-highlight--hl-color (nth 0 clrls))) ;; Set common-app-highlight--hl-color based on character count
		  ((< buff-char-count (- tgt (nth 1 diffls))) (setq common-app-highlight--hl-color (nth 1 clrls)))
		  ((< buff-char-count (- tgt (nth 2 diffls))) (setq common-app-highlight--hl-color (nth 2 clrls)))
		  ((< buff-char-count (- tgt (nth 3 diffls))) (setq common-app-highlight--hl-color (nth 3 clrls)))
		  ((< buff-char-count (- tgt (nth 4 diffls))) (setq common-app-highlight--hl-color (nth 4 clrls)))
		  ((< buff-char-count tgt) (setq common-app-highlight--hl-color (nth 5 clrls)))
		  ((= buff-char-count tgt) (setq common-app-highlight--hl-color (nth 6 clrls)))
		  ((< buff-char-count (+ tgt (nth 7 diffls))) (setq common-app-highlight--hl-color (nth 7 clrls)))
		  ((< buff-char-count (+ tgt (nth 8 diffls))) (setq common-app-highlight--hl-color (nth 8 clrls)))
		  ((< buff-char-count (+ tgt (nth 9 diffls))) (setq common-app-highlight--hl-color (nth 9 clrls))))
	(common-app-highlight--hl-buffer)))

(defun common-app-highlight--hl-buffer ()
  "Set color of text in buffer."
  (put-text-property (point-min) (point-max)
                     'font-lock-face `(:foreground ,common-app-highlight--hl-color)))

(defun common-app-highlight--reset-buffer ()
  "Reset the color of text in buffer."
  (put-text-property (point-min) (point-max)
                     'font-lock-face `(:foreground default)))

(defun common-app-highlight-enable ()
  "Enable common-app-highlight."
  (interactive)
  (add-hook 'post-command-hook #'common-app-highlight--calc-highlight nil t))

(defun common-app-highlight-disable ()
  "Disable common-app-highlight."
  (interactive)
  (remove-hook 'post-command-hook #'common-app-highlight--calc-highlight t)
  (common-app-highlight--reset-buffer))

(provide 'common-app-highlight)

;;; common-app-highlight.el ends here
