;;; custom-example.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-example.el to custom.el and change the configurations,
;;;       then restart Emacs.
;;; Code:

(setq my-full-name "user name")           ; User full name
(setq my-mail-address "user@email.com")   ; Email address

;; (setq my-proxy "127.0.0.1:1080")          ; Network proxy

;; Remove ~/.emacs.d/elpa-`version'/archives if encounter package issues
;; Package repo: melpa, emacs-china or tuna
(setq my-package-archives 'tuna)

(setq my-preferred-theme 'doom-one) ; Theme
(setq my-themes-pool '(doom-one-light doom-one)) ; Switchable themes

;; Orgmode directory
(setq org-directory "~/org")

;;; custom-example.el ends here
