Faux Screen
==============================

Basically a personal journey of encoding my
[Gnu Screen](http://www.gnu.org/software/screen/) configuration into
Emacs. While I know there are others that have blazed the trails in
improving the basic Emacs term.el support, I just wanted to encode my
workflow along with taking advantage of other folks work.

Installation
------------------------------

I tend to use el-get and here is a recipe:

    (:name faux-screen
           :description "Faux Gnu Screen in Emacs"
           :type github
           :pkgname "jonEbird/faux-screen")

You can otherwise clone the repo and update your `load-path` appropriately.

Configuration
------------------------------

Quick start. Add this block to your config:

    (require 'faux-screen)
    (faux-screen-global-mode)
    (faux-screen-terminals 10)

In that example, I'm starting 10 ansi terminals. If you prefer to use
regular `term`, then you can call:

    (faux-screen-terminals 10 nil t)

Much like Gnu Screen, we are setting up an "escape" key or in Emacs terms,
a prefix. I prefer to use "C-\\" but you may set another value like so:

    (setq faux-screen-keymap-prefix (kbd "C-8"))

What does Faux Screen Provide Me?
---------------------------------

With each terminal started, you can use the prefix / escape key to jump to
the started terminals. That means with a prefix of `C-\\` you can jump to
Term 3 via `C-\ 3`.

Once you are within a Terminal, you can switch to next and previous via
`C-\ n` and `C-\ -p`.

You can finally run the escape key twice to return to the previous terminal
by way of running `previous-buffer`.

Extra, bonus config. I like to use C-pgdn and C-pgup to move about my
terminals and when I'm not in a terminal, I use it to move to next/prior
buffer. You can use my `faux-screen-next-dwim` and `faux-screen-prev-dwim`
functions for that:

    (global-set-key [C-next]  'faux-screen-next-dwim)
    (global-set-key [C-prior] 'faux-screen-prev-dwim)

Contribution
------------------------------

While this project is primarily an expression of my own workflow, if you
find it useful and have useful contribution, please send me a pull request.
