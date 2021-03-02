# Swapper
Randomly swap all lines of text in an Emacs buffer or region

Just something that came in real handy for me, and still does!


Note that if you are operating on an odd number of lines then 1 line may not get swapped, if this is a problem then just run Swapper again 

Installing into Emacs....
============================================================================

You need to put the file (neils_swapper.el) in a folder where Emacs
can see it.
If you are not sure how to do this then you can do what i do...

I have created a new folder inside my Emacs (.emacs.d) folder
called: site-lisp

In this folder i put all my seperate .el files that i want to load and
use when Emacs starts.

Once you have created this folder, put the file: neils_swapper.el inside it.

To make Emacs see the files in this folder you need to add this line to
the top of your Emacs init.el file:

```(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))```

If you know how to you can also byte-compile the neils_swapper.el file, just
open it in Emacs and go: Emacs-Lisp > Byte Compile This File, from the Emacs
menu and its done.

Example Configuration....
============================================================================
Add these lines to the end of your Emacs init.el file.
This creates the key binding ```M-s s``` and sets it to Swapper.

```
(global-set-key (kbd "M-s s") 'neils-swapper)
```
