;; init-basic.el --- Initialize basic configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Basic configurations.
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
  (require 'init-const)
  (require 'init-custom))

;; Personal information
(setq user-full-name my-full-name)
(setq user-mail-address my-mail-address)

;; Key Modifiers
(when sys/macp
  ;; make Mac's cmd key as meta
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'hyper)
  (global-set-key (kbd "M-`") 'ns-next-frame)
  ;; !!! Very important for emacs-macport !!!
  (setq mac-pass-command-to-system nil))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; History
(use-package saveplace
  :init (add-hook 'after-init-hook #'save-place-mode))

(use-package savehist
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (add-hook 'after-init-hook #'savehist-mode))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200)
  (add-hook 'after-init-hook #'recentf-mode)
  :config
  (add-to-list 'recentf-exclude '("/tmp/" "/ssh:")))

;; Start server
(use-package server
  :ensure nil
  :init (add-hook 'after-init-hook #'server-mode))

(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
