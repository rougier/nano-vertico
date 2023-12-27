;;; nano-minibuffer.el --- N Λ N O vertico -*- lexical-binding: t -*-

;; Copyright (C) 2023 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-modeline
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico))
;; Keywords: convenience, mode-line, header-line

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

;;; Commentary
;;
;;  nano-minibuffer takes advantage of vertico-buffer mode and displays
;;  the minibuffer prompt and contents inside the header line while
;;  completions are displayed in a regular buffer. It is highly
;;  experimental and may not be safe for daily use.
;;
;; Known problems:
;; - Messages are hidden when prompt is active
;; - Opening two minibuffers in two different frames at the same time
;;   messes up depth detection.
;;

;;; NEWS:
;;
;; Version 0.1
;; - First release
;;
;;; Code:
(require 'vertico)
(require 'vertico-buffer)

(defcustom nano-vertico-header-padding '(0.20 . 0.25)
  "Top and bottom padding for the header line"
  :type '(cons float float)
  :group 'nano-vertico)

(defcustom nano-vertico-prompt t
  "Prompt replacement that can be nil (regular prompt), t (minibuffer
 depth) or an arbitrary prompt string"
  :type '(choice (const :tag "Regular prompt" nil)
                 (const :tag "Minibuffer counter" t)
                 (string :tag "User defined"))
  :group 'nano-vertico)

(defface nano-vertico-header-face
  `((t :foreground ,(face-foreground 'default)
       :background ,(face-background 'default)
       :height ,(face-attribute 'default :height)
       :box (:line-width 1 :color ,(face-foreground 'default) :style nil)))
  "Face for header line"
  :group 'nano-vertico)

(defface nano-vertico-mode-line-face
  `((t :foreground ,(face-foreground 'default)
       :height ,(face-attribute 'default :height)
       :box nil
       :overline ,(face-foreground 'font-lock-comment-face nil t)))
  "Face for mode line"
  :group 'nano-vertico)

(defface nano-vertico-buffer-face
  `((t :foreground ,(face-foreground 'default)
       :height 0.1))
  "Face for completions"
  :group 'nano-vertico)

(defface nano-vertico-prompt-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'default)       
       :inherit bold))
  "Face for prompt"
  :group 'nano-vertico)

(defface nano-vertico-region-face
  `((t :foreground ,(face-foreground 'region nil 'default)
       :background ,(face-background 'region nil 'default)
       :inherit bold))
  "Face for active region"
    :group 'nano-vertico)

(defface nano-vertico-annotation-face
  `((t :foreground ,(face-foreground font-lock-comment-face nil t)
       ;; :box (:line-width 1 :color ,(face-foreground 'default) :style nil)
       ))
  "Face for help message"
    :group 'nano-vertico)

(defface nano-vertico-cursor-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'default)
       :inherit header-line))
  "Face for cursor"
  :group 'nano-vertico)

(defun nano-vertico--format-content (content completion)
  "Complete CONTENT with COMPLETION"
  
  (if (eq (string-match content completion) 0)
    (let ((completion (substring-no-properties completion
                       (length content) (length completion))))
      (concat (propertize content 'face '(nano-vertico-header-face bold))
              (propertize completion 'face 'font-lock-comment-face)))
    content))

(defun nano-vertico--update-header-line ()
  "This function do several things:
1. Copy the minibuffer prompt and contents to header line
2. Emulate cursor inside the header line
3. Emulate region highlight when needed
4. Hide the actual minibuffer prompt and contents
    (setting foreground color to background color)"

;;  (let ((active (eq (window-buffer (active-minibuffer-window))
;;                    (current-buffer))))
;;    (with-current-buffer (window-buffer (active-minibuffer-window))
  (let ((active (eq (window-buffer (minibuffer-window)) (current-buffer))))
    (with-current-buffer (window-buffer (minibuffer-window))
      (let* ((prompt (substring-no-properties (minibuffer-prompt)))
             (contents (substring-no-properties (minibuffer-contents)))
             (prompt (if (and (stringp prompt) (> (length prompt) 0))
                         prompt
                     " "))
           (prompt-length (+ 2 (length prompt)))
           (items (or nano-vertico--current-message
                      (format "%d items" vertico--total)))
           (candidate (or (vertico--candidate) ""))
           (contents (nano-vertico--format-content contents candidate))
           (point (1+ (point)))
           (top (car nano-vertico-header-padding))
           (bottom (cdr nano-vertico-header-padding))
           (prompt (concat
                    (propertize " " 'display `(raise ,top))
                    (cond ((eq nano-vertico-prompt t) (format "#%d" (minibuffer-depth)))
                          ((stringp nano-vertico-prompt) nano-vertico-prompt)
                          (t (string-trim prompt nil "[: ]+")))
                    (propertize " " 'display `(raise ,(- bottom)))))
           (prompt (concat (propertize prompt 'face 'nano-vertico-prompt-face) " "))
           (contents (concat contents
                             " "
                             (propertize " " 'display `(space :align-to (- right ,(length items) 1)))
                             (propertize items 'face 'nano-vertico-annotation-face))))

    ;; Hide regular prompt & contents 
    (unless (or cursor-type (not header-line-format))
      (let ((inhibit-read-only t)
            (deactivate-mark nil))
         (add-text-properties (point-min)
                              (line-end-position)
                              `(face (:foreground ,(face-background 'default))))))

    ;; Set cursor & region
    (let ((cursor-pos (max 0 (- point prompt-length)))
          (region (when (use-region-p)
                    (cons (- (region-beginning) -1 prompt-length)
                          (- (region-end) prompt-length -1))))
          (space 2)
          (end-of-line (= (point) (line-end-position))))
      ;; Set region
      (when region 
        (add-face-text-property (car region) (cdr region)
                                'nano-vertico-region-face nil contents))
      ;; Set cursor
      (when active
        (add-face-text-property cursor-pos (1+ cursor-pos)
                                'nano-vertico-cursor-face nil contents))

      ;; Make sure cursor is always visible
      (when (> cursor-pos (- (window-width) prompt-length space))
        (setq contents
              (concat "…" (substring contents (- cursor-pos (- (window-width) prompt-length 2 space))))))
      (when (> (length contents) (- (window-width) (length prompt) (- space 1)))
        (setq contents
              (concat (substring contents 0 (- (window-width) prompt-length space))
                      (if (not end-of-line) "…")))))
    (setq-local header-line-format (concat prompt contents))))))


(defun nano-vertico--setup ()
  "Setup nano vertico mode"

  (dolist (win (get-buffer-window-list))
    (set-window-parameter win 'mini-window (window-minibuffer-p win)))
  
  (face-remap-add-relative 'default
      `(:filtered (:window mini-window nil) nano-vertico-buffer-face))
  (face-remap-add-relative 'mode-line
      `(:filtered (:window mini-window nil) nano-vertico-mode-line-face))
  (face-remap-add-relative 'mode-line-inactive 
      `(:filtered (:window mini-window nil) nano-vertico-mode-line-face))
  ;; (face-remap-add-relative 'fringe
  ;;    `(:filtered (:window mini-window nil) (:background "black")))
  (face-remap-set-base 'header-line 'nano-vertico-header-face)
  (setq-local cursor-type nil)

  (let ((windows (get-buffer-window-list (window-buffer (minibuffer-window)))))
    (dolist (window windows)
      (unless (eq window (minibuffer-window))
        (set-window-fringes window 1 1))))

  (when (eq (minibuffer-depth) 1)
    ;; We save mode line format since we hide it when minibuffer is displayed
    (setq nano-vertico--saved-state
          (let ((buffer (nth 1 (buffer-list))))
            (cons buffer 
                  (with-current-buffer buffer
                    mode-line-format))))

    ;; Hide mode-line
    (with-current-buffer (nth 1 (buffer-list))
      (setq mode-line-format nil)))
  
  (setq mode-line-format
        `(:eval (let* ((minibuffer (format " Minibuffer #%d" (minibuffer-depth)))
                       (prompt (format " %s" (string-trim (minibuffer-prompt) nil "[: ]+")))
                       (left (if (eq nano-vertico-prompt nil)
                                 ""
                               prompt))
                       (right minibuffer))
                  (concat
                   (propertize left 'face '(bold mode-line))
                   (propertize " "
                               'display `(space :align-to (- right ,(length right) 1)))
                   (propertize right 'face '(font-lock-comment-face mode-line))))))

   ;; Install our exit function
  (add-hook 'minibuffer-exit-hook #'nano-vertico--exit)

  ;; Install our copy function
  (add-hook 'post-command-hook #'nano-vertico--update-header-line))


(defun nano-vertico--exit ()
  "Remove hooks and restore mode-line"

  (unless (> (minibuffer-depth) 1)
    (with-current-buffer (car nano-vertico--saved-state)
      (setq mode-line-format (cdr nano-vertico--saved-state)))
    (remove-hook 'post-command-hook #'nano-vertico--update-header-line)))

(defun nano-vertico--format-string (string)
  "Return STRING with height property set to default height"
  
  (let ((height (face-attribute 'default ':height)))
    (add-face-text-property  0 (length string) `(:height ,height) nil string)
    string))

(defun nano-vertico--format-candidate (orig cand prefix suffix index start)
  "Make sure CAND, PREFIX and SUFFIX have height set because the
default face height is set to 0.1 to hide regular prompt/contents"
  
  (let ((prefix (if (= vertico--index index) " " "  ")))
    (if vertico-grid-mode
        ;; Non brekable space or they are stripped
        (funcall orig (nano-vertico--format-string cand) " " " " index start)
      (funcall orig (nano-vertico--format-string cand)
                    (nano-vertico--format-string prefix)
                    (nano-vertico--format-string suffix)
                    index start))))

(defvar nano-vertico--current-message nil
  "Current minibuffer message")

(defun nano-vertico--minibuffer-message (orig-fun message &rest args)
  "Copy message to nano-vertico--current-message, wait for timeout
and delete it"
  
  (setq nano-vertico--current-message (apply #'format-message message args))
  (nano-vertico--update-header-line)
  (let ((inhibit-message t))
    (message message args))
  (sit-for (or minibuffer-message-timeout 1000000))
  (setq nano-vertico--current-message nil)
  (nano-vertico--update-header-line))

(defun nano-vertico-mode-on ()
  "Activate nano-vertico-mode"
  
  ;; We save vertico settings before setting our own
  (setq nano-vertico--buffer-mode vertico-buffer-mode
        nano-vertico--buffer-display-action vertico-buffer-display-action)
  (setq vertico-buffer-display-action
        `(display-buffer-in-side-window
          (window-height . ,(+ 5 vertico-count))
          ;; (window-height . 12)
          (side . bottom)))

  ;; Install our setup after vertico setup
  (advice-add #'vertico--setup
              :after #'nano-vertico--setup)

  ;; Install our own cancidate format
  (advice-add #'vertico--format-candidate
           :around #'nano-vertico--format-candidate)

  ;; Install advice on minibuffer message
  (advice-add #'minibuffer-message
              :around #'nano-vertico--minibuffer-message)
  
  ;; Starts vertico buffer mode
  (vertico-buffer-mode 1))

(defun nano-vertico-mode-off ()
  "Deactivate nano-vertico-mode"

  ;; Restore previous vertico settings
  (setq vertico-buffer-display-action nano-vertico--buffer-display-action)
  (vertico-buffer-mode (or nano-vertico--buffer-mode -1))
  
  ;; Remove our setup
  (advice-remove #'vertico--setup
                 #'nano-vertico--setup)

  ;; Remove our formatting
  (advice-remove #'vertico--format-candidate
                 #'nano-vertico--format-candidate)

  ;; Remove our message handler
  (advice-add #'minibuffer-message
              :around #'nano-vertico--minibuffer-message)
  (setq nano-vertico--current-message nil)
  
  ;; Remove exit hook
  (remove-hook 'minibuffer-exit-hook #'nano--vertico-exit))

;;;###autoload
(define-minor-mode nano-vertico-mode
  "Nano vertico mode takes advantage of the vertico buffer mode to
show the minibuffer prompt and contents inside the header line of
a dedicated window"
  :lighter "[•]"

  (if nano-vertico-mode
      (nano-vertico-mode-on)
    (nano-vertico-mode-off)))

(provide 'nano-vertico)
;;; nano-vertico.el ends here
