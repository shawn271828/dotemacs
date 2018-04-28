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
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (bind-key "C-c C-z"
                        'kill-buffer-and-window inferior-python-mode-map)
              (process-query-on-exit-flag (get-process "Python"))))

  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(python-flake8 python-pylint python-pycompile))
    (setq flycheck-python-pycodestyle-executable (concat my-anaconda-home "/bin/pycodestyle"))
    (setq flycheck-python-pyflakes-executable (concat my-anaconda-home "/bin/pyflakes"))
    ;; Pyflakes
    (flycheck-define-checker python-pyflakes
      "Pyflakes"
      :command ("pyflakes" source-inplace)
      :error-patterns
      ((error line-start (file-name) ":" line ":" (message) line-end))
      :modes python-mode)

    ;; Pycodestyle
    (flycheck-define-checker python-pycodestyle
      "Pycodestyle"
      :command ("pycodestyle" source-inplace)
      :error-patterns
      ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
      :modes python-mode)

    ;; Add to flycheck
    (add-to-list 'flycheck-checkers 'python-pycodestyle)
    (add-to-list 'flycheck-checkers 'python-pyflakes)
    ;; Make them working together
    (flycheck-add-next-checker 'python-pyflakes '(t . python-pycodestyle))))

;; Conda environment management
(use-package conda
  :demand
  :bind ("C-c <f5>" . conda-activate)
  :init
  (setq conda-anaconda-home my-anaconda-home)
  (add-hook 'python-mode-hook #'conda-env-mode)
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (setq conda-message-on-environment-switch nil)

  ;; Use `.+' instead of `\\w+' as some char doesn't count as word
  (defun new-conda--get-name-from-env-yml (filename)
    (when filename
      (let ((env-yml-contents (f-read-text filename)))
        (if (string-match "name:[ ]*\\(.+\\) *$" env-yml-contents)
            (match-string 1 env-yml-contents)
          ))))
  (advice-add 'conda--get-name-from-env-yml :override #'new-conda--get-name-from-env-yml)

  ;; Non-sense of the original function, replace it.
  (defun new-conda--get-path-prefix (env-dir)
    (when (file-exists-p env-dir)
      (concat env-dir "/bin")))
  (advice-add ' conda--get-path-prefix :override #'new-conda--get-path-prefix)
  
  ;; Activate conda automaticaaly if possible
  (defun conda-activate ()
    (interactive)
    (setenv "PYTHONPATH"
            (concat (projectile-project-root) ":"
                    python-shell-virtualenv-root "/lib/python3.6/site-packages"))
    (conda-env-activate-for-buffer)
    (unless conda-project-env-name
      (conda-env-activate)))

  ;; Define a minor mode to work with mode line
  (define-minor-mode conda-env-mode
    "Toggle the display of current active conda environment."
    :init-value nil
    :lighter (:eval (format " [%s]" (or conda-env-current-name "?")))))

;; Python completion and backend
(use-package anaconda-mode
  :diminish anaconda-mode
  :init (add-hook 'python-mode-hook
                  '(lambda ()
                     (setq-default python-indent-offset 4)
                     (anaconda-mode)
                     (anaconda-eldoc-mode))))

(use-package company-anaconda
  :defines company-backends
  :init (with-eval-after-load 'company
          (cl-pushnew (company-backend-with-yas 'company-anaconda) company-backends)))

(use-package yapfify)

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
