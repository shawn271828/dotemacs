;; init-conda.el --- Initialize highlight configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Conda configurations.
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

(eval-when-compile
  (require 'init-custom))

(use-package conda
  :init (setq conda-anaconda-home my-anaconda-home)
  :config
  ;; Use `.+' instead of `\\w+' as some char doesn't count as word
  (defun new-conda--get-name-from-env-yml (filename)
    (when filename
      (let ((env-yml-contents (f-read-text filename)))
        (if (string-match "name:[ ]*\\(.+\\) *$" env-yml-contents)
            (match-string 1 env-yml-contents)
          ))))
  (advice-add 'conda--get-name-from-env-yml :override #'new-conda--get-name-from-env-yml)
  ;; Hook open new file
  (advice-add 'pop-to-buffer :after #'conda--switch-buffer-auto-activate)
  (conda-env-autoactivate-mode t))

(provide 'init-conda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-conda.el ends here
