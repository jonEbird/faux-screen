Faux Screen
==============================

Basically a personal journey of encoding my [Gnu Screen][GS] configuration
into Emacs. While I know there are others that have blazed the trails in
improving the basic Emacs term.el support, I just wanted to encode my
workflow along with taking advantage of other folks work.

Installation
------------------------------

I tend to use el-get and here is a recipe you can use:

    (:name faux-screen
           :description "Faux Gnu Screen in Emacs"
           :type github
           :pkgname "jonEbird/faux-screen")

You can otherwise clone the repo and update your `load-path` appropriately.

Configuration
------------------------------

Quick start. Add this block to your config:

    (setq faux-screen-shell "/bin/bash"
          faux-screen-num-terminals 10
          faux-screen-keymap-prefix (kbd "C-\\")
          faux-screen-terminal-ps1 "(\\[\\e[1;36m\\]%d\\[\\e[0m\\]) \\W $ ")
    (require 'faux-screen)
    (faux-screen-global-mode)

In that example, I'm starting 10 ansi terminals. Most of those values are
also defaults but I'm just re-defining them as an example for those that
want to change the values a bit.

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

Utility or Project Specific Shells
----------------------------------

It can also be helpful to launch a utility shell or one that is specific to
your project. For this situation, I have a `faux-screen-utility-terminal`
function that can be used to create a uniquely named terminal. It
technically returns a function that you can then bind to a key or just
immediately call. The returned function also supported an optional
DIRECTORY parameter that will have the effect of sending a "cd" command for
you within the shell.

Here are a few examples that I use:

1. Start or jump to my utility shell with the current directory my buffer is located at

        ;; Setup a utility terminal to be used in ad-hoc situations using the
        ;; current default-directory location
        (global-set-key (kbd "<f12>")
                        (lambda ()
                          (interactive)
                          (funcall (faux-screen-utility-terminal "Utility") default-directory)))

2. Integrate with [Projectile][Projectile] to use their known, base
   directory of current project for a `prj` shell.

        ;; Integrate with projectile
        (defun projectile-utility-shell ()
          (interactive)
          (funcall (faux-screen-utility-terminal "prj") (projectile-project-root)))
        (define-key projectile-command-map (kbd "$") 'projectile-utility-shell)


Contribution
------------------------------

While this project is primarily an expression of my own workflow, if you
find it useful and have useful contribution, please send me a pull request.

[GS]: http://www.gnu.org/software/screen/
[Projectile]: https://github.com/bbatsov/projectile
