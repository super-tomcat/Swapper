;; SWAPPER for Emacs
;; Randomly swap all lines in buffer or region
;;
;; Copyright (C) 2020 Neil Higgins
;; Created: 15-Aug-2020
;; Author: Neil Higgins
;;
;;
(defun neils-swapper (&optional start end)
      "Randomly swap lines in the whole buffer or region"
      (interactive)
      (let ((total-lines 0)
            (saved-line (line-number-at-pos))
            (line-number-to-start 0)
            (our-line-numbers (list))
            (random-line-one 0)
            (random-line-two 0)
            (random-line-one-stored "")
            (random-line-two-stored "")
            (start (if mark-active (region-beginning) (point-min)))
            (end (if mark-active (region-end) (point-max))))
        (goto-char start)
        (setq line-number-to-start (line-number-at-pos)) 
        (setq total-lines (count-lines start end))
        (if (< total-lines 2)
            (message "%s" "You need at least 2 lines of text in the region or buffer before you can use Swapper!")    
          (progn
            (require 'cookie1)
            (setq our-line-numbers (cookie-shuffle-vector (vconcat (number-sequence line-number-to-start (+ line-number-to-start (- total-lines 1))))))
            (dotimes (i (/ total-lines 2))
              (setq random-line-one (aref our-line-numbers (* i 2)))
              (setq random-line-two (aref our-line-numbers (+ 1 (* 2 i))))
              (goto-line random-line-one)
              (setq random-line-one-stored (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (delete-char (- (line-end-position) (point)))
              (goto-line random-line-two)
              (setq random-line-two-stored (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (delete-char (- (line-end-position) (point)))
              (insert random-line-one-stored)
              (goto-line random-line-one)
              (insert random-line-two-stored))
            (message "%s" "Swapper has done swapping random lines!")))
        (goto-line saved-line)))
;;
;;
;;
;; ============================================================================
;; Example Configuration....put this in your Emacs init file...restart Emacs
;; and then with a suitable file selected...try pressing M-s s
;; ============================================================================
;; 
;;(global-set-key (kbd "M-s s") 'neils-swapper)
;;
;;


