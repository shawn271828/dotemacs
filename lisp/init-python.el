;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             python configurations.
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

(use-package python
  :ensure nil
  :config
  ;; Setup flycheck pylint
  (setq flycheck-python-pylint-executable (concat my-anaconda-home "/bin/pylint"))

  ;; Conda environment management
  (use-package conda
    :demand
    :init
    (setq conda-anaconda-home my-anaconda-home)
    ;; Make spaceline show conda env as pyvenv (hack)
    (defvaralias 'pyvenv-virtual-env-name 'conda-env-current-name)
    (setq pyvenv-virtual-env "anaconda3")
    :config
    (setq conda-message-on-environment-switch nil)

    ;; Use `.+' instead of `\\w+' as some char doesn't count as word
    (defun new-conda--get-name-from-env-yml (filename)
      (when filename
        (let ((env-yml-contents (f-read-text filename)))
          (if (string-match "name:[ ]*\\(.+\\) *$" env-yml-contents)
              (match-string 1 env-yml-contents)
            ))))
    (advice-add 'conda--get-name-from-env-yml :override #'new-conda--get-name-from-env-yml))

  ;; Python completion and backend
  (use-package anaconda-mode
    :init
    (add-hook 'python-mode-hook
              '(lambda ()
                 (progn
                   (conda-env-activate-for-buffer)
                   (setenv "PYTHONPATH"
                           (concat (projectile-project-root) ":"
                                   python-shell-virtualenv-root "/lib/python3.6/site-packages"))
                   (anaconda-mode)
                   (anaconda-eldoc-mode)))))
  (use-package company-anaconda
    :init
    (add-hook 'anaconda-mode-hook
              '(lambda () (progn
                       (make-local-variable 'company-backends)
                       (push 'company-anaconda company-backends)))))

  (use-package yapfify))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
