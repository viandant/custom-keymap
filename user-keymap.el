;;; user-keymap.el --- Configure user keysequence bindings from a custom variable -*- lexical-binding:t -*-
;; Author: Viandant <viandant@langenst.de>
;; Version: 0.1
;; Package-Requires: ((emacs "29.3"))
;; Keywords: internal keymap keyboard customization
;; URL: https://github.com/viandant/user-keymap

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package allows users to maintain keybindings
;; in the customisation variable USER-KEYMAP-LIST.
;; The keybindings are activated in the globalised minor
;; mode USER-KEYMAP-MODE.
;;
;; Some auxiliary commands are provided:
;; USER-KEYMAP-SORT-ON-COMMANDS: sort USER-KEYMAP-LIST on commands
;; USER-KEYMAP-SORT-ON-KEYSEQUENCES: sort USER-KEYMAP-LIST on keysequences
;; USER-KEYMAP-ADD: Interactively add an entry to USER-KEYMAP-LIST
;; USER-KEYMAP-DELETE: Interactively add an entry to USER-KEYMAP-LIST

;;; Code:

(defvar user-keymap
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `user-keymap-mode'.")

(defun user-keymap-fset-dummy (funsym)
  "Set the function of FUNSYM to a dummy function if not already bound."
  (if (not (fboundp funsym))
      (let ((error-message (format "The function %s has not yet been loaded" funsym)))
	(fset funsym
	      (eval `(lambda () (interactive) (error ,error-message)))))))

(defun user-keymap-set-list (sym val)
  "Function called each time the USER-KEYMAP-LIST is customised.
Update the keybindings in USER-KEYMAP according to VAL.
Then set the toplevel value of SYM to VAL."
  ;;Remove all keys that are no longer set in the custom variable.
  (mapc
   (lambda (pair)
     (if (assoc (car pair) val)
	 nil
       (define-key user-keymap (car pair) nil t)))
   (eval sym))
  (mapc (lambda (pair)
	  (let
	      ((keyseq (car pair))
	       (funsym (cdr pair)))
	    (user-keymap-fset-dummy funsym)
	    (define-key user-keymap keyseq funsym)))
	val)
  (set-default-toplevel-value sym val))

;;;###autoload
(defcustom user-keymap-list nil
  "Pairs of key sequences and functions to be bound to the keys."
  :type '(alist :key-type key-sequence :value-type function)
  :group 'user-keymap-mode
  :set 'user-keymap-set-list)


(defun user-keymap-bind-dummy-function ()
  "Bind a dummy function to all unbound symbols in USER-KEYMAP-LIST.
If key sequences are bound to functions that are only loaded on demand,
USER-KEYMAP-LIST will not conform to its reqired type.  The customisaiton
dialoge will then refuse to format the current value nicely.
After running this function all symbols will have function definitions
and the customisation dialogue will look nice again."
  (interactive)
  (mapc
   (lambda (keyseq-funsym) (user-keymap-fset-dummy (cdr keyseq-funsym)))
   user-keymap-list))

;;;###autoload
(define-minor-mode user-keymap-mode "Minor mode with the only purpose to activate the USER-KEYMAP."
  :global t
  :keymap user-keymap
  :group 'user-keymap-mode)

(defun user-keymap-mode-turn-on ()
  "Turn on user-keymap-mode."
  (user-keymap-mode 1)
  nil)

;;;###autoload
(define-globalized-minor-mode global-user-keymap-mode user-keymap-mode user-keymap-mode-turn-on
			      (message "global-user-keymap-mode switched."))

;;;###autoload
(defun user-keymap-sort-on-commands ()
  "Sort USER-KEYMAP-LIST on commands."
  (interactive)
  (customize-save-variable 'user-keymap-list
			   (sort user-keymap-list :key #'cdr)))

;;;###autoload
(defun user-keymap-sort-on-keysequences ()
  "Sort USER-KEYMAP-LIST on keysequences."
  (interactive)
  (customize-save-variable 'user-keymap-list
			   (sort user-keymap-list :key #'car)))

(defun user-keymap-read-command ()
  "Read a command with default from the symbol at point."
  (let*
      ((sym (symbol-at-point))
       (default-cmd
	(if (commandp sym) sym nil))
       (command-prompt
	(if default-cmd
	    (format "Command (default: %s): " default-cmd)
	  "Command: ")))
    (read-command command-prompt default-cmd)))

(defun user-keymap-read-key-sequence ()
  "Read a key sequence without echo keystroke help."
  (let ((echo-keystrokes-help nil)
	(cursor-in-echo-area t)
	(result nil))
    (while (null result)
      (let*
	  ((keyseq (read-key-sequence "Keysequence to bind to (C-g to quit): " nil t))
	   (existing-binding (key-binding keyseq)))
	(if (eq (seq-elt keyseq (1- (seq-length keyseq))) ?\a)
	    (setq quit-flag t)
	  (setq result
		(if existing-binding
		    (if (y-or-n-p (format "%s already runs the command %S. Overwrite old setting?" (key-description keyseq) existing-binding))
			keyseq nil)
		  keyseq)))))
    result))

;; (defun user-keymap-read-function-symbol--interactive (funsym)
;;   "Interactively read a function FUNSYM symbol and return it."
;;   (interactive "aFunction: ")
;;   funsym)

;;;###autoload
(defun user-keymap-add (keyseq funsym)
  "Add pair (KEYSEQ . FUNSYM) to USER-KEYMAP-LIST and save it.
This will cause function symbol FUNSYM to be bound to
key sequence KEYSEQ if mode USER-KEYMAP-MODE is active."
  ;; We want this:
  ;; (interactive "kKeysequence: \naKeysequence to bind to: ")
  ;; but without the misleading '(C-h for help)' in the prompt when reading KEYSEQ.
  ;; Therefore it gets more complicated:
  (interactive
   (list (user-keymap-read-key-sequence)
	 (user-keymap-read-command)))
  (if (null keyseq)
      (message "Aborted.")
    (progn
      (customize-save-variable 'user-keymap-list
			       (cons (cons keyseq funsym)
				     (assoc-delete-all keyseq user-keymap-list)))
      (message (format "%s bound to %S." funsym (key-description keyseq) )))))


;;;###autoload
(defun user-keymap-delete (funsym)
  "Delete all pairs (_ . FUNSYM) from USER-KEYMAP-LIST and save it.
This will cause the function symbol FUNSYM no longer to
be bound to the key sequence previously associated to it in USER-KEYMAP-LIST.
If the keys sequence has a binding outside USER-KEYMAP-LIST, this one may become
active again."
  ;; We want this:
  ;; (interactive "kKeysequence: \naKeysequence to bind to: ")
  ;; but without the misleading '(C-h for help)' in the prompt when reading KEYSEQ.
  ;; Therefore it gets more complicated:
  (interactive
   (list (user-keymap-read-command)))
  (if (null (rassoc funsym user-keymap-list))
      (message "Command %s does not occur in USER-KEYMAP-LIST. Nothing will happen." funsym)
    (progn
      (customize-save-variable 'user-keymap-list
			       (rassq-delete-all funsym user-keymap-list)))
    (message (format "All entries with %s removed." funsym))))

(provide 'user-keymap)
;;; user-keymap.el ends here
