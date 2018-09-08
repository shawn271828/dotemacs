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
  :load-path "site-lisp/org-mode/lisp"
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-indent-mode 1)
                             (diminish 'org-indent-mode)))
  :config
  (setq org-html-inline-images t)
  (setq org-html-head-include-default-style nil)
  ;; (setq org-export-headline-levels 6)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-validation-link nil)
  (setq org-agenda-files '("~/v/org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")))
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (add-to-list 'org-export-backends 'md)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; FIXME org-agenda-execute-calendar-command uses deprecated list-calendar-holidays
  (unless (fboundp 'list-calendar-holidays)
    (defalias 'list-calendar-holidays 'calendar-list-holidays))

  ;; IPython
  (use-package ob-ipython)

  ;; Restclient
  (use-package ob-restclient)

  ;; rst
  (use-package ox-rst
    :load-path "site-lisp/ox-rst"
    :init (add-hook 'org-export-backends 'rst))

  ;; Babel
  (setq org-confirm-babel-evaluate nil)

  (defvar-local load-language-list '((emacs-lisp . t)
                                     (ipython . t)
                                     (restclient . t)
                                     (ditaa . t)
                                     (dot . t)
                                     (shell . t)
                                     (plantuml . t)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Presentation
  (use-package org-tree-slide
    :bind (("<f6>" . org-tree-slide-mode))
    :config
    (add-hook 'org-tree-slide-play-hook
              (lambda ()
                (text-scale-set 4)
                (org-display-inline-images)
                (read-only-mode 1)))
    (add-hook 'org-tree-slide-stop-hook
              (lambda ()
                (text-scale-set 0)
                (org-remove-inline-images)
                (read-only-mode -1))))

  ;; Bullets
  ;; (use-package org-bullets
  ;;   :init (add-hook 'org-mode-hook 'org-bullets-mode))

  ;; Publishing
  ;; https://orgmode.org/org.html#Publishing-options
  ;; https://orgmode.org/org.html#Sources-and-destinations
  (setq org-publish-project-alist
        '(("orgfiles"
           :base-directory "~/v/org"
           :base-extension "org"
           :publishing-directory "~/v/code/blog/org"
           :publishing-function org-html-publish-to-html
           :htmlized-source nil
           :section-numbers nil
           :with-toc t
           :html-preamble nil
           :html-postamble nil)
          ("images"
           :base-directory "~/v/org/images"
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory "~/code/blog/org/images"
           :publishing-function org-publish-attachment)
          ("website" :components ("orgfiles" "images"))

          (""))))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
