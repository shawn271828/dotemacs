;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Org configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("M-." . org-open-at-point)
         ("M-," . org-mark-ring-goto))
  :diminish org-indent-mode
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-indent-mode 1)
                             (flycheck-mode -1)
                             (electric-pair-local-mode 1)))
  :config
  (setq org-catch-invisible-edits 'error
        org-html-inline-images t
        org-html-head-include-default-style nil
        org-html-htmlize-output-type nil
        org-html-validation-link nil
        org-default-notes-file (concat org-directory "/notes.org")
        org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)"))
        org-pretty-entities nil
        org-src-fontify-natively t)

  ;; Escape org-entity(e.g. emphasis characters) in orgmode.
  ;; ref: https://emacs.stackexchange.com/questions/16688/how-can-i-escape-the-in-org-mode-to-prevent-bold-fontification/16746#16746
  (defun modi/org-entity-get-name (char)
    "Return the entity name for CHAR. For example, return \"ast\" for *."
    (let ((ll (append org-entities-user
                      org-entities))
          e name utf8)
      (catch 'break
        (while ll
          (setq e (pop ll))
          (when (not (stringp e))
            (setq utf8 (nth 6 e))
            (when (string= char utf8)
              (setq name (car e))
              (throw 'break name)))))))

  (defun modi/org-insert-org-entity-maybe (&rest args)
    "When the universal prefix C-u is used before entering any character,
    insert the character's `org-entity' name if available.

    If C-u prefix is not used and if `org-entity' name is not available, the
    returned value `entity-name' will be nil."
    ;; It would be fine to use just (this-command-keys) instead of
    ;; (substring (this-command-keys) -1) below in emacs 25+.
    ;; But if the user pressed "C-u *", then
    ;;  - in emacs 24.5, (this-command-keys) would return "^U*", and
    ;;  - in emacs 25.x, (this-command-keys) would return "*".
    ;; But in both versions, (substring (this-command-keys) -1) will return
    ;; "*", which is what we want.
    ;; http://thread.gmane.org/gmane.emacs.orgmode/106974/focus=106996
    (let ((pressed-key (substring (this-command-keys) -1))
          entity-name)
      (when (and (listp args) (eq 4 (car args)))
        (setq entity-name (modi/org-entity-get-name pressed-key))
        (when entity-name
          (setq entity-name (concat "\\" entity-name "{}"))
          (insert entity-name)
          (message (concat "Inserted `org-entity' "
                           (propertize entity-name
                                       'face 'font-lock-function-name-face)
                           " for the symbol "
                           (propertize pressed-key
                                       'face 'font-lock-function-name-face)
                           "."))))
      entity-name))

  (advice-add 'org-self-insert-command :before-until #'modi/org-insert-org-entity-maybe)

  ;; FIXME org-agenda-execute-calendar-command uses deprecated list-calendar-holidays
  (unless (fboundp 'list-calendar-holidays)
    (defalias 'list-calendar-holidays 'calendar-list-holidays))

  ;; Hugo
  (use-package ox-hugo
    :ensure t
    :init (require 'ox-hugo))

  ;; Babel
  (setq org-confirm-babel-evaluate nil)

  (defvar-local load-language-list '((emacs-lisp . t)
                                     (python . t)
                                     (ditaa . t)
                                     (dot . t)
                                     (shell . t)
                                     (plantuml . t)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Bullets
  (use-package org-superstar
    :init (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
    :config
    (org-superstar-configure-like-org-bullets)))

;; Publishing is not included as ox-hugo is used to export

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
