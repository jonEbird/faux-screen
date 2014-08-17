;;; faux-screen.el --- Faux Gnu Screen in Emacs

;; Author: Jon Miller <jonEbird at gmail.com>
;; URL: https://github.com/jonEbird/faux-screen
;; Version: 0.1
;; Keywords: terminal, emulation
;; Package-Requires: ((term+ "0.1") (term+key-intercept "0.1") (term+mux "0.1"))

;; Imitate my very personal Gnu screen setup within Emacs while taking
;; advantage of the awesome work by INA Lintaro and his term+
;; contributions.

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

(require 'term+)
(ignore-errors
  (require 'xterm-256color))
(require 'term+key-intercept)
(require 'term+mux)


;;; Customization
(defgroup faux-screen nil
  "Imitate Gnu screen setup."
  :group 'tools
  :group 'convenience)

(defcustom faux-screen-keymap-prefix (kbd "C-\\")
  "Prefix for faux screen commands ala the Gnu screen escape key"
  :group 'faux-screen
  :type 'string)


;; Key bindings

(defvar faux-screen-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'faux-screen-next-dwim)
    (define-key map (kbd "p") 'faux-screen-prev-dwim)
    (define-key map faux-screen-keymap-prefix 'previous-buffer)
    map)
  "Keymap for Faux-Screen commands after `faux-screen-keymap-prefix'")
(fset 'faux-screen-command-map faux-screen-command-map)

(defvar faux-screen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map faux-screen-keymap-prefix 'faux-screen-command-map)
    map)
  "Keymap for Faux-Screen.")

(define-minor-mode faux-screen-mode
  "Minor mode to imitate Gnu screen terminal management

\\{faux-screen-mode-map}"
  :lighter "fs"
  :keymap faux-screen-mode-map
  :group 'faux-screen
  :require 'faux-screen
  )

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
  "Go to the next terminal or just next buffer. Next terminal is based on
the buffer name. Will extract the number from the buffer-name, add 1 and go
to a buffer of that name if it exists."
  (interactive)
  (let ((cur-buffer (buffer-name))
        (mode (symbol-name major-mode)))
    (cond ((or (equal mode "term-mode")
               (equal mode "shell-mode"))
           (if (string-match "\\([0-9]+\\)" cur-buffer)
               (let* ((n (match-string-no-properties 0 cur-buffer))
                      (next (int-to-string (+ (string-to-int n) 1)))
                      (next-buffer (replace-regexp-in-string n next cur-buffer)))
                 (if (get-buffer next-buffer)
                     (switch-to-buffer next-buffer nil) t))))
          (t
           (next-buffer)))))

(defun faux-screen-prev-dwim ()
  "Go to the previous terminal or just previous buffer. Previous terminal
is based on the buffer name. Will extract the number from the buffer-name,
subtract 1 and go to a buffer of that name if it exists."
  (interactive)
  (let ((cur-buffer (buffer-name))
        (mode (symbol-name major-mode)))
    (cond ((or (equal mode "term-mode")
               (equal mode "shell-mode"))
           (if (string-match "\\([0-9]+\\)" cur-buffer)
               (let* ((n (match-string-no-properties 0 cur-buffer))
                      (prev (int-to-string (- (string-to-int n) 1)))
                      (prev-buffer (replace-regexp-in-string n prev cur-buffer)))
                 (if (get-buffer prev-buffer)
                     (switch-to-buffer prev-buffer nil t)))))
          (t
           (previous-buffer)))))


;;; Term hook setup

(defun faux-screen-setup ()
  "Perform sane setup for a term. Intended to be ran within a hook upon
creation of the term such as term-mode-hook."
  (setq truncate-lines t)
  ; Modes that should not be enabled
  (ignore-errors
    (autopair-mode -1))
  ; Recommendations from term.el
  (setq term-prompt-regexp "^[^#$%>\n]*[#$] +")
  (make-local-variable 'mouse-yank-at-point)
  (make-local-variable 'transient-mark-mode)
  (setq mouse-yank-at-point t)
  (setq transient-mark-mode nil)
  (auto-fill-mode -1)
  (setq tab-width 8 ))

(add-hook 'term-mode-hook 'faux-screen-setup)

(defun faux-screen-terminals (&optional N shell inferior)
  "Create a set of commonly used terminals ala GNU screen.

Will create 8 ansi shells unless you specific `N`. Can also pass
which `shell` you would like to use with /bin/bash being the
default. Finally anything but nil for `inferior` will cause us to
launch shell (the inferior shell) instead of ansi-term."
  (interactive)
  (let ((escape-key faux-screen-keymap-prefix)
        (started-terms '())
        (times (or N 8))
        (default-directory (expand-file-name "~/"))
        (explicit-shell-file-name (or shell "/bin/bash")))
    (dotimes (n times)
      (let* ((shell-name (format "Shell %d" n))
             (buffer-name (format "*%s*" shell-name)))
        ;; (global-set-key (kbd (format "%s %s" escape-key n))
        ;;                 `(lambda ()
        ;;                    (interactive)
        ;;                    (switch-to-buffer ,buffer-name nil t)))
        (define-key faux-screen-command-map (kbd (format "%d" n))
          `(lambda ()
             (interactive)
             (switch-to-buffer ,buffer-name nil t)))
        (unless (get-buffer buffer-name)
          (add-to-list 'started-terms n t)
          (setenv "EMACS_PS1" (format "(\\[\\e[1;32m\\]%d\\[\\e[0m\\]) \\W $ " n))
          (save-window-excursion
            (if inferior
                (shell buffer-name)
              (ansi-term explicit-shell-file-name shell-name)))
          (with-current-buffer buffer-name
            (local-unset-key escape-key)))))
    ; Cleanup
    (setenv "EMACS_PS1" "")
    (if started-terms
        (message (format "Started shells %s" started-terms))
      (message "Your %d terms are already started" times))))

(provide 'faux-screen)
;;; faux-screen.el ends here
