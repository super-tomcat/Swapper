# Swapper
Randomly swap all lines of text in an Emacs buffer or region

Just something that came in real handy for me, and still does!

Copy the following code to your Emacs init file, restart Emacs then switch to a buffer with text and do M-x neils-swapper, to swap all lines in the buffer, or select a region first to swap only the lines in the region. 

Add a keybinding or menu entry to the function... neils-swapper

Note that if you are operating on an odd number of lines then 1 line may not get swapped, if this is a problem then just run Swapper again 


```
;;; swapper.el --- Randomly swap all lines of text in an Emacs buffer or region
;; Copyright (C) 2020 Neil Higgins
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
       ;; 
           (goto-char start)
          ;; We should now be on the region start line? And should also be able to use this
          ;; to get the number of lines in the region: (count-lines start end)
          ;; And from that we can say if we either have enough lines (>= 2) to continue, plus
          ;; work out what the line numbers are... starting from the line at point, which is
          ;; got with (line-number-at-pos) and then add the number of lines we got above with
          ;; (count-lines start end) minus 1, to get the last line number
          (setq line-number-to-start (line-number-at-pos)) 
          (setq total-lines (count-lines start end))
      ;; There needs to be more than 1 line to work with, if not exit
          (if (< total-lines 2)
          (message "%s" "You need at least 2 lines of text in the region or buffer before you can use Swapper!")    
          (progn
            ;; We create a list of line numbers from the first line of the region (or the first line of the
            ;; buffer) to the end line of the region (or last line of buffer), we then convert
            ;; the list to a vector so we can use an in built function that shuffles vectors, our line
            ;; numbers are now mixed up randomly and all we have to do is read 2 in at a time and swap
            ;; them in the buffer....
            ;; To shuffle a vector we need to include the cookie1 file that contains the function we need
            (require 'cookie1)
            ;; There is a lot going on in this next line, lets break it down:
            ;; (number-sequence line-number-to-start (+ line-number-to-start (- total-lines 1))).. First
            ;; thing we do is create a list containing all the line numbers we need to swap using number-sequence,
            ;; We have to feed number-sequence with the start number (line-number-to-start, here) and then the
            ;; ending number of the sequence (+ line-number-to-start (- total-lines 1))
            ;; (vconcat ) the list then gets converted to a vector with vconcat so we can shuffle it next
            ;; (cookie-shuffle-vector ) the vector then gets shuffled
            ;; Note: If you then needed to convert to a list you would wrap it with: (append  nil)
             (setq our-line-numbers (cookie-shuffle-vector (vconcat (number-sequence line-number-to-start (+ line-number-to-start (- total-lines 1))))))
            ;; Start loop until we have done half the total number of lines in the buffer.
            ;; The reason we only do half is because we read from the vector of line no's 2 elements at a time.
            ;; The only downside to this is that if you have an odd number of lines then 1 may not get
            ;; swapped, you can always run it again though!
    (dotimes (i (/ total-lines 2))
    ;; get the first line number to swap from the vector, then accessing it every 2nd element (0,2,4 etc)
      (setq random-line-one (aref our-line-numbers (* i 2)))
    ;; get the second line to swap with from the vector, then accessing it every 3rd element (1,3,5 etc)
     (setq random-line-two (aref our-line-numbers (+ 1 (* 2 i))))
    ;; get text on line1 and save it etc etc
     (goto-line random-line-one)
     (setq random-line-one-stored (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
     (delete-char (- (line-end-position) (point)))
    ;; now save random line 2, delete it, copy the stored random line 1 into it
     (goto-line random-line-two)
     (setq random-line-two-stored (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
     (delete-char (- (line-end-position) (point)))
     (insert random-line-one-stored)
    ;; now move back to random line one and copy the stored random line 2 into it
     (goto-line random-line-one)
     (insert random-line-two-stored)
     )
    (message "%s" "Swapper has done swapping random lines!")
    ))
          (goto-line saved-line)))

```
