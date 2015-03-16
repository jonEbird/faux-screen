;;; faux-screen.el --- Faux Gnu Screen in Emacs

;; Author: Jon Miller <jonEbird at gmail.com>
;; URL: https://github.com/jonEbird/faux-screen
;; Version: 0.2
;; Keywords: terminal, emulation

;;; Commentary:

;; This package started out as a way to imitate my personal GNU screen
;; setup within Emacs while also working to make terminals more usable in
;; the process. That is why it is called faux-screen, as in a fake GNU
;; screen setup within Emacs.
;;
;; At it's most basic level, this package setups up a set of terminals
;; numerically numbered and establishes a keyboard prefix for switching to
;; them quickly. E.g. Having 10 terminals created and switching to terminal
;; 3 immediately via "C-\ 3" where "C-\" is the default faux-screen prefix
;; key but can be set via `faux-screen-keymap-prefix'. The terminals are
;; not started immediately but only when you first switch to them.
;;
;; Faux-screen helps you customize your shell experience by defining unique
;; EMACS_PS1 environment variables for each terminal that you can use to
;; set your PS1 shell prompt to. I have the following in my ~/.bashrc:
;;
;;     # PS1 and related Status
;;     if [ "$(ps --no-headers -o comm -p $PPID)" == "emacs" ]; then
;;         if [ -n "$EMACS_PS1" ]; then
;;             PS1="$EMACS_PS1"
;;         else
;;             PS1="\W $ "
;;         fi
;;         export PAGER=emacspager
;;     else
;;         # Standard PS1
;;         PS1="[\u@\h \W]\$ "
;;     fi
;;
;; That will setup a nice and concise PS1 shell prompt when a terminal is
;; launched within Emacs and the EMACS_PS1 variable is being setup by this
;; package.
;;
;; You can use the customize interface for setting various faux-screen
;; attributes. Run `customize-group' and pick `faux-screen'
;;
;; Here is a sample Emacs config in how you can use this package:
;;
;;     (setq faux-screen-num-terminals 10
;;           faux-screen-keymap-prefix (kbd "C-\\")
;;           faux-screen-terminal-ps1 "(\\[\\e[1;36m\\]%s\\[\\e[0m\\]) \\W $ ")
;;     (require 'faux-screen)
;;     (faux-screen-global-mode)
;;
;;     ;; Initialize our terminals
;;     (faux-screen-terminals)
;;
;;     ;; Setup a utility terminal to be used in ad-hoc situations using the
;;     ;; current default-directory location
;;     (global-set-key (kbd "<f12>")
;;                     (lambda ()
;;                       (interactive)
;;                       (funcall (faux-screen-utility-terminal "Utility") default-directory)))
;;
;;     ;; Integrate with projectile
;;     (defun projectile-utility-shell ()
;;       (interactive)
;;       (funcall (faux-screen-utility-terminal "prj") (projectile-project-root)))
;;     (define-key projectile-command-map (kbd "$") 'projectile-utility-shell)
;;
;;     ;; Optional key bindings for cycling between terminals:
;;     (global-set-key [C-next]  'faux-screen-next-dwim)
;;     (global-set-key [C-prior] 'faux-screen-prev-dwim)

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'term)


;;; Customization
(defgroup faux-screen nil
  "Imitate Gnu screen setup."
  :group 'tools
  :group 'convenience)

(defcustom faux-screen-keymap-prefix (kbd "C-\\")
  "Prefix for faux screen commands ala the Gnu screen escape key."
  :group 'faux-screen
  :type 'string)

(defcustom faux-screen-hide-buffers t
  "Attempt to hide buffers from switch-buffer (C-x b)."
  :group 'faux-screen
  :type 'boolean)

(defcustom faux-screen-num-terminals 8
  "Number of terminals to launch by default."
  :group 'faux-screen
  :type 'integer)

(defcustom faux-screen-utility-terminal 0
  "Which terminal is your go-to temporary, utilty terminal?
Should be a valid index within the `faux-screen-num-terminals' number of
terminals.  Is used to switch to and quickly cd to a new location via
  `faux-screen-utility-jump' function"
  :group 'faux-screen
  :type 'integer)

(defcustom faux-screen-shell "/bin/bash"
  "Default shell to use when creating new shells."
  :group 'faux-screen
  :type 'string)

(defcustom faux-screen-inferior-shell nil
  "Use inferior shell (M-x shell) over default `ansi-term'."
  :group 'faux-screen
  :type 'boolean)

(defcustom faux-screen-terminal-ps1 "(\\[\\e[1;32m\\]%s\\[\\e[0m\\]) \\W $ "
  "Provide a PS1 syntax that will be availalbe in variable EMACS_PS1.
Recommneded to conditionally use EMACS_PS1 in your shell's dotfiles when
available.  A single '%d' in the prompt will be replaced with the number
term that is being launched."
  :group 'faux-screen
  :type 'string)


;; Key bindings

(defvar faux-screen-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'faux-screen-next-dwim)
    (define-key map (kbd "p") 'faux-screen-prev-dwim)
    (define-key map faux-screen-keymap-prefix 'previous-buffer)
    map)
  "Keymap for Faux-Screen commands after `faux-screen-keymap-prefix'.")
(fset 'faux-screen-command-map faux-screen-command-map)

(defvar faux-screen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map faux-screen-keymap-prefix 'faux-screen-command-map)
    map)
  "Keymap for Faux-Screen.")

(define-minor-mode faux-screen-mode
  "Minor mode to imitate Gnu screen terminal management

\\{faux-screen-mode-map}"
  :lighter " fs"
  :keymap faux-screen-mode-map
  :group 'faux-screen
  :require 'faux-screen
  (cond
   (faux-screen-mode
    (faux-screen-init)
    (add-hook 'term-mode-hook 'faux-screen-setup t))
   (t
    (remove-hook 'term-mode-hook 'faux-screen-setup t))))

;;;###autoload
(define-globalized-minor-mode faux-screen-global-mode
  faux-screen-mode
  faux-screen-on)

(defun faux-screen-on ()
  "Enable Faux-Screen minor mode."
  (faux-screen-mode 1))

(defun faux-screen-off ()
  "Disable Faux-Screen minor mode."
  (faux-screen-mode -1))

(defun faux-screen-global-on ()
  "Enable Faux-Screen global minor mode."
  (faux-screen-global-mode +1))

(defun faux-screen-global-off ()
  "Disable Faux-Screen global minor mode."
  (faux-screen-global-mode -1))


;;; Quick Navigation Commands

(defun faux-screen-next-dwim ()
  "Go to the next terminal or just next buffer.
Next terminal is based on the buffer name.  Will extract the number from
the `buffer-name', add 1 and go to a buffer of that name if it exists."
  (interactive)
  (let ((cur-buffer (buffer-name))
        (mode (symbol-name major-mode)))
    (cond ((or (equal mode "term-mode")
               (equal mode "shell-mode"))
           (if (string-match "\\([0-9]+\\)" cur-buffer)
               (let* ((n (match-string-no-properties 0 cur-buffer))
                      (next (number-to-string (+ (string-to-number n) 1)))
                      (next-buffer (replace-regexp-in-string n next cur-buffer)))
                 (if (get-buffer next-buffer)
                     (switch-to-buffer next-buffer nil) t))))
          (t
           (next-buffer)))))

(defun faux-screen-prev-dwim ()
  "Go to the previous terminal or just previous buffer.
Previous terminal is based on the buffer name.  Will extract the number
from the `buffer-name', subtract 1 and go to a buffer of that name if it
exists."
  (interactive)
  (let ((cur-buffer (buffer-name))
        (mode (symbol-name major-mode)))
    (cond ((or (equal mode "term-mode")
               (equal mode "shell-mode"))
           (if (string-match "\\([0-9]+\\)" cur-buffer)
               (let* ((n (match-string-no-properties 0 cur-buffer))
                      (prev (number-to-string (- (string-to-number n) 1)))
                      (prev-buffer (replace-regexp-in-string n prev cur-buffer)))
                 (if (get-buffer prev-buffer)
                     (switch-to-buffer prev-buffer nil t)))))
          (t
           (previous-buffer)))))

(defun faux-screen-utility-jump (&optional directory)
  "Switch to your utility terminal.
Terminal is specified by `faux-screen-utility-terminal' within the
allocated terminals and send a \"cd <directory>\" command and therefore
quickly moving you to your utilty terminal and cd'ing to the desired
DIRECTORY quickly"
  (interactive)
  (let* ((basedir (or directory (read-directory-name "Base Directory: ")))
         (shell-name (format "Shell %d" faux-screen-utility-terminal))
         (buffer-name (format "*%s*" shell-name))
         (buffer (if faux-screen-inferior-shell
                     (get-buffer shell-name)
                   (get-buffer buffer-name))))
    (if buffer
        (progn
          (switch-to-buffer buffer)
          (term-send-raw-string (concat "cd " basedir "\n")))
      (message "Missing your utility terminal"))))

(defun faux-screen-utility-terminal (name)
  "Return an anonymous function for creating a terminal named after NAME.
The utility function accepts an optional DIRECTORY which will be used to
send a \"cd\" command to the shell"
  `(lambda (&optional directory)
     (interactive)
     (faux-screen-new-terminal ,name t)
     (if directory
         (term-send-raw-string (concat "cd " directory "\n")))))


;;; Faux Screen Setup functions

(defun faux-screen-init ()
  "One time init function to help setup common items."
  ; Optionally hide shells from ido switch buffer
  (if faux-screen-hide-buffers
      (if (boundp 'ido-ignore-buffers)
          (add-to-list 'ido-ignore-buffers "Shell " t))))

;; Taken from http://joelmccracken.github.io/entries/\
;;                   switching-between-term-mode-and-line-mode-in-emacs-term/
(defun term-toggle-mode ()
  "Toggle term between line mode and char mode."
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun faux-keyboard-cleanup ()
  "Redefine a few keystrokes to make term more usable."
  (define-key term-mode-map (kbd "M-RET") 'term-toggle-mode)
  (define-key term-raw-map (kbd "M-RET") 'term-toggle-mode)
  ;; Re-add missing key sequences to term-raw-map
  (define-key term-raw-map (kbd "M-:")
    (lambda ()
      (interactive)
      (call-interactively 'eval-expression)))
  (define-key term-raw-map (kbd "M-x")
    `(lambda ()
       (interactive)
       (call-interactively ',(lookup-key global-map (kbd "M-x"))))))

(defun faux-screen-setup ()
  "Perform sane setup for a term.
Intended to be ran within a hook upon creation of the term such as
term-mode-hook."
  (setq truncate-lines t)
  ; Modes that should not be enabled
  (ignore-errors
    (autopair-mode -1))
  ; Recommendations from term.el
  (setq term-prompt-regexp "^[^#$%\n]*[#$%] +")
  (make-local-variable 'mouse-yank-at-point)
  (setq mouse-yank-at-point t)
  (auto-fill-mode -1)
  (setq tab-width 8 )
  (faux-keyboard-cleanup))

;;; Main Interactive functions that user will call

(defun faux-screen-new-terminal (N &optional switch shell directory inferior)
  "Start a new terminal numbered by N if not already started.

Will switch to buffer if SWITCH is true.  Which shell is ran is determined
by `faux-screen-shell' when SHELL is not passed.  The shell will be started
in your home directory unless DIRECTORY is passed.  Shell will be an
`ansi-term' unless INFERIOR or `faux-screen-inferior-shell' is true."
  (interactive)
  (let* ((default-directory (expand-file-name (or directory "~/")))
         (shell-name (format "Shell %s" N))
         (buffer-name (format "*%s*" shell-name)))
    (unless (get-buffer buffer-name)
      (setenv "EMACS_PS1" (format faux-screen-terminal-ps1 N))
      (save-window-excursion
        (if (or inferior faux-screen-inferior-shell)
            (shell buffer-name)
          (ansi-term (or shell faux-screen-shell) shell-name)))
      (with-current-buffer buffer-name
        (local-unset-key faux-screen-keymap-prefix))
      (setenv "EMACS_PS1" ""))
    (if switch
        (switch-to-buffer (get-buffer buffer-name)))))

(defun faux-screen-terminals (&optional N shell inferior)
  "Create a set of commonly used terminals ala GNU screen.

Will create `faux-screen-num-terminals' number of shells unless you specify
N.  `faux-screen-shell' is used for shell if SHELL is not specified.
Finally anything but nil for INFERIOR will cause us to launch `shell', the
inferior shell, instead of `ansi-term'"
  (interactive)
  (let ((times (or N faux-screen-num-terminals))
        (default-directory (expand-file-name "~/"))
        (shell-name (or shell faux-screen-shell)))
    (dotimes (n times)
      (define-key faux-screen-command-map (kbd (format "%d" n))
        `(lambda ()
           (interactive)
           (faux-screen-new-terminal ,n t ,shell-name ,default-directory
                                     ,(or inferior faux-screen-inferior-shell)))))))

(provide 'faux-screen)
;;; faux-screen.el ends here
