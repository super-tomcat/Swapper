# Swapper
Randomly swap all lines of text in an Emacs buffer

Just something that came in real handy for me, and still does!

Copy the following code to your Emacs init file, restart Emacs then switch to a buffer with text and do M-x neils-swapper, or add a keybinding or menu entry to the function... neils-swapper


```
;;
;;
;;
(defun neils-swapper ()
      "Randomly swap lines in the whole buffer"
    (interactive)
    ;; save and restore the points position after we have finished
    (save-excursion
    ;; get total number of lines in buffer:
      (setq-local total-lines (count-lines (point-min) (point-max)))
      ;; There needs to be more than 1 line in the buffer, if not exit
      (if (> total-lines 1)
          (progn
            ;; We create a list of line numbers from 1 to the total lines in the buffer, we then convert
            ;; the list to a vector so we can use an in built function that shuffles vectors, our line
            ;; numbers are now mixed up randomly and all we have to do is read 2 in at a time and swap
            ;; them in the buffer....
            ;; To shuffle a vector we need to include the cookie1 file that contains the function we need
            (require 'cookie1)
            ;; There is a lot going on in this next line, lets break it down:
            ;; (number-sequence 1 total-lines).. First thing we do is create a list containing all the line
            ;; numbers we need to swap using number-sequence
            ;; (vconcat ) the list then gets converted to a vector with vconcat so we can shuffle it next
            ;; (cookie-shuffle-vector ) the vector then gets shuffled
            ;; Note: If you then needed to convert to a list you would wrap it with: (append  nil)
            (setq-local our-line-numbers (cookie-shuffle-vector (vconcat (number-sequence 1 total-lines))))
            ;; Start loop until we have done half the total number of lines in the buffer.
            ;; The reason we only do half is because we read from the vector of line no's 2 elements at a time.
            ;; The only downside to this is that if you have an odd number of lines then 1 may not get
            ;; swapped, you can always run it again though!
    (dotimes (i (/ total-lines 2))
    ;; get the first line number to swap from the vector, then accessing it every 2nd element (0,2,4 etc)
      (setq-local random-line-one (aref our-line-numbers (* i 2)))
    ;; get the second line to swap with from the vector, then accessing it every 3rd element (1,3,5 etc) 
     (setq-local random-line-two (aref our-line-numbers (+ 1 (* 2 i))))
    ;; get text on line1 and save it etc etc
     (goto-line random-line-one)
     (setq-local random-line-one-stored (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
     (delete-char (- (line-end-position) (point)))
    ;; now save random line 2, delete it, copy the stored random line 1 into it
     (goto-line random-line-two)
     (setq-local random-line-two-stored (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
     (delete-char (- (line-end-position) (point)))
     (insert random-line-one-stored)
    ;; now move back to random line one and copy the stored random line 2 into it
     (goto-line random-line-one)
     (insert random-line-two-stored)
     )
    (message "%s" "Swapper has done swapping random lines!")
    )
        (message "%s" "You need at least 2 lines of text in the buffer before you can use Swapper!")))

```
