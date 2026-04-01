;;; nano-vertico.el --- NANO appearance for vertico completion UI -*- lexical-binding: t; -*-

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-vertico
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (vertico "1.0"))
;; Keywords: convenience, matching, tools

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides `nano-vertico-mode`, a global minor mode that
;; reshapes the vertico minibuffer display to match the NANO
;; aesthetic. Features include a pill-shaped depth indicator, a custom
;; header showing the originating command, and an optional horizontal
;; separator.
;;
;; Default configuration required nerd font (www.nerdfonts.com). If
;; not available, replace glyphs in 'nano-vertico-symbols'

;;; Code:
(require 'vertico)

(defgroup nano-vertico nil
  "NANO-emacs aesthetic for Vertico."
  :group 'vertico
  :prefix "nano-vertico-")

(defcustom nano-vertico-symbols '((pill-left  . "")
                                  (pill-right . "")
                                  (selection  . "")
                                  (line       . ?╴))
  "Various symbols used to decorate minibuffer.")

(defcustom nano-vertico-pill-format " %s "
  "Pill format string")

(defcustom nano-vertico-gutter-format " %s "
  "Gutter format string")

(defcustom nano-vertico-lines '(nil . t)
  "Whether to add top and bottom line around prompt")

(defvar-local nano-vertico--origin-command nil
  "Internal variable to store the command that triggered the minibuffer.")

(defun nano-vertico--capture-command ()
  "Store the value of `this-command` at the moment the minibuffer is setup."
  (setq-local nano-vertico--origin-command this-command))

;;;###autoload
(define-minor-mode nano-vertico-mode
  "Toggle NANO-style appearance for Vertico.
When enabled, it overrides Vertico's candidate formatting and display 
functions to provide a clean, header-based UI."
  :global t
  :group 'nano-vertico
  (if nano-vertico-mode
      (progn
        (setq max-mini-window-height (+ vertico-count 3))
        (advice-add #'vertico--format-candidate :override #'nano-vertico--format-candidate)
        (advice-add #'vertico--display-candidates :override #'nano-vertico-display-candidates)
        (add-hook 'minibuffer-setup-hook #'nano-vertico--capture-command))
    (advice-remove #'vertico--format-candidate #'nano-vertico--format-candidate)
    (advice-remove #'vertico--display-candidates #'nano-vertico-display-candidates)
    (remove-hook 'minibuffer-setup-hook #'nano-vertico--capture-command)))

(defun nano-vertico--format-candidate (cand prefix suffix index _start)
  "Format CAND with a selection indicator padding."
  (setq cand (vertico--display-string (concat prefix cand suffix "\n")))
  (when (= index vertico--index)
    (add-face-text-property 0 (length cand) 'vertico-current 'append cand))
  (concat (propertize (format nano-vertico-gutter-format 
                              (if (= index vertico--index)
                                  nano-vertico-chevron-symbol
                                (alist-get 'selection nano-vertico-symbols)
                                " "))
                      'face (if (= index vertico--index)
                                'widget-field
                              'highlight))
          (propertize " " 'face 'default)
          cand))

(defun nano-vertico-display-candidates (lines)
  "Render the full minibuffer UI with the NANO-emacs header style."
  (let* ((inhibit-read-only t)
         (no-selection (or (not (numberp vertico--index))
                           (< vertico--index 0)))
         (prompt (minibuffer-prompt))
         (prompt (string-trim-right prompt "[: ]+"))
         (vertico-count-format '("%s" . "%s/%s"))
         (count (format "%s" (vertico--format-count)))
         (info (format " (%s)" (or nano-vertico--origin-command "Unknown")))

         ;; Restored original NANO face logic
         (face-prompt `(:foreground ,(face-background 'default nil 'default)
                        :background ,(face-foreground 'default nil 'default)
                        :weight ,(face-attribute 'bold :weight nil 'default)))
         (face-prompt-l `(:foreground ,(face-foreground 'default nil 'default)
                          :background ,(face-background 'default nil 'default)
                          :weight ,(face-attribute 'bold :weight nil 'default)))
         (face-prompt-r `(:foreground ,(face-foreground 'default nil 'default)
                          :background ,(face-background 'widget-field nil 'default)
                          :weight ,(face-attribute 'bold :weight nil 'default)))
         (face-header-bold `(:foreground ,(face-foreground 'default nil 'default)
                             :background ,(face-background 'widget-field nil 'default)
                             :weight ,(face-attribute 'bold :weight nil 'default)))
         (face-header-shadow `(:foreground ,(face-foreground 'shadow nil 'default)
                               :background ,(face-background 'widget-field nil 'default)
                               :weight ,(face-attribute 'default :weight nil 'default)))
         (face-header `(:foreground ,(face-foreground 'default nil 'default)
                        :background ,(face-background 'widget-field nil 'default)
                        :weight ,(face-attribute 'default :weight nil 'default)))
         (face-entry `(:foreground ,(face-foreground 'default nil 'default)
                        :background ,(face-background 'highlight nil 'default)))
         (face-entry-bold `(:foreground ,(face-foreground 'default nil 'default)
                            :background ,(face-background 'highlight nil 'default)
                            :weight ,(face-attribute 'bold :weight nil 'default)))
         (face-entry-shadow `(:foreground ,(face-foreground 'shadow nil 'default)
                              :background ,(face-background 'highlight nil 'default)))

         (pill (concat (propertize (alist-get 'pill-left nano-vertico-symbols)
                                   'face face-prompt-l)
                       (propertize (format nano-vertico-pill-format (minibuffer-depth))
                                   'face face-prompt)
                       (propertize (alist-get 'pill-right nano-vertico-symbols)
                                   'face face-prompt-r)))
         (prompt-align (propertize " " 'display `(space :align-to (- right 1))))
         (header-align (make-string (max 0 (- (window-width)
                                              (length pill)
                                              (length prompt)
                                              (length info)
                                              (length count) 2)) ?\s))
         (h-line (concat
                  (propertize (make-string (window-width) 
                                           (alist-get 'line nano-vertico-symbols))
                              'face 'default)
                  (propertize "\n" 'face 'default)))                 
         (new-prompt (concat
                      (if (car nano-vertico-lines) h-line "")
                      pill
                      (propertize " " 'face face-header)
                      (propertize prompt 'face face-header-bold)
                      (propertize info 'face face-header)
                      (propertize header-align 'face face-header)
                      (propertize count 'face face-header-shadow)
                      (propertize "\n"  'face face-header-shadow)
                      (if no-selection
                           (propertize (format nano-vertico-gutter-format
                                               nano-vertico-chevron-symbol)
                                       'face 'vertico-current)
                         (propertize (format nano-vertico-gutter-format "?")
                                     'face face-entry-shadow)))))

    ;; HACK: Modifying buffer display table to have full width line
    (when (or (car nano-vertico-lines) (cdr nano-vertico-lines))
      (unless buffer-display-table
        (setq buffer-display-table (make-display-table)))
      (set-display-table-slot buffer-display-table 'wrap
                              (make-glyph-code (alist-get 'line nano-vertico-symbols)))
      (set-display-table-slot buffer-display-table 'truncation
                              (make-glyph-code (alist-get 'line nano-vertico-symbols)))
      (setq truncate-lines t))
    (add-text-properties (point-min) (length (minibuffer-prompt)) `(display ,new-prompt))
    
    (if no-selection
        (add-text-properties (point-min) (point-max) `(face ,face-entry-bold))
      (add-text-properties (point-min) (point-max) `(face ,face-entry-shadow)))
    
    (move-overlay vertico--candidates-ov (point-max) (point-max))
    (overlay-put vertico--candidates-ov 'after-string
                 (concat
                  (propertize " " 'cursor t 'face `(:inherit ,face-entry :extend t))
                  (apply #'concat
                         (propertize prompt-align 'face face-entry)
                         (propertize "\n" 'face face-entry)
                         (if (cdr nano-vertico-lines) h-line "")     
                         (if (eq (length lines) 0) (list " ") lines))))))

(provide 'nano-vertico)
;;; nano-vertico.el ends here
