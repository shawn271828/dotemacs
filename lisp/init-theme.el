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

;; Plan9 theme
(use-package plan9-theme)
(use-package parchment-theme)
(use-package acme-theme)

;; Github theme
(use-package github-modern-theme)

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
