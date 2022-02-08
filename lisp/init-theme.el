;; init-theme.el --- Initialize theme configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             UI configurations.
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

;; Atom theme
(use-package atom-one-dark-theme)

;; Tomorrow theme
(use-package color-theme-sanityinc-tomorrow)

;; Leuven theme for orgmode
(use-package leuven-theme)

;; Doom themes mega pack
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Theme switcher
(defun index-of-theme (theme)
  "Get THEME index in pool."
  (- (length my-themes-pool)
     (length (memq theme my-themes-pool))))

(defun shawn/rotate-theme (&optional theme)
  "Rotately switching between themes.
If THEME is given, switch to that theme."
  (interactive)
  (if theme
      (progn
        (put 'shawn/rotate-theme 'state (index-of-theme theme))
        (load-theme theme t))
    (let* (($index-before
            (if (get 'shawn/rotate-theme 'state)
                (get 'shawn/rotate-theme 'state)
              0))
           ($index-after (% (+ $index-before 1) (length my-themes-pool)))
           ($current-theme (nth $index-before my-themes-pool))
           ($next-theme (nth $index-after my-themes-pool)))
      (put 'shawn/rotate-theme 'state $index-after)
      (disable-theme $current-theme)
      (load-theme $next-theme t)
      (message "Theme changed to %s" $next-theme))))

(shawn/rotate-theme my-preferred-theme)

(global-set-key (kbd "C-<f12>") 'shawn/rotate-theme)

(provide 'init-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-theme.el ends here
