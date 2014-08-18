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
           :pkgname "jonEbird/faux-screen"
           :depends (term+ term+ki term+mux))

You can otherwise clone the repo and update your `load-path`
appropriately. If you do go this route, you'll want to install
[term+](tarao/term-plus-el), [term+ki](tarao/term-plus-ki-el) and
[term+mux](tarao/term-plus-mux-el). Those are all links to the correct
project pages but I'd suggest using package.el and install them that
way. Something like:

    (dolist (package '(term+ term+key-intercept term+mux))
      (package-install package))

Configuration
------------------------------

Quick start. Add this block to your config:

    (setq faux-screen-shell "/bin/bash"
          faux-screen-num-terminals 10
          faux-screen-keymap-prefix (kbd "C-\\")
          faux-screen-terminal-ps1 "(\\[\\e[1;36m\\]%d\\[\\e[0m\\]) \\W $ ")
    (require 'faux-screen)
    (faux-screen-global-mode)

In that example, I'm starting 10 ansi terminals. Most of those values are also defaults but I'm just re-defining them as an example for those that want to change the values a bit.

An easy way to get an idea of the options available with this project is to
use the customize routines. Run M-x `customize-group` RET `faux-screen`
RET. There you will see the available options and be able to apply your
preferred values.

Integration with your shell. While launching terminals, we will set an
environment variable `EMACS_PS1` with the value set in
`faux-screen-terminal-ps1`. Here is a helper bourne shell excerpt that you
may want to use in your personal dotfile:

    if [ "x$EMACS_PS1" != "x" ]; then
        PS1="$EMACS_PS1"
    fi


What does Faux Screen Provide Me?
---------------------------------

With each terminal started, you can use the prefix/escape key + terminal
number to jump to the started terminals. That means with a prefix of `C-\`
you can jump to Term 3 via `C-\ 3`.

Once you are within a Terminal, you can switch to next and previous via
`C-\ n` and `C-\ p`.

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
