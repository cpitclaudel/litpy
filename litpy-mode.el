;;; litpy-mode.el --- Highlight reStructuredText titles in Python comments  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement@clem-w50-mint>
;; Keywords: languages, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'python)
(require 'hideshow)
(require 'font-lock)
(require 'quick-peek)
(require 'indirect-font-lock)

(defvar font-lock-beg)
(defvar font-lock-end)

(defgroup litpy nil
  "Literate Python: highlight reStructuredText, and peek at doctest output."
  :group 'python)

(defcustom litpy-hide-title-markup nil
  "If non-nil, hide title decorations (underlines and ##).
If you use this option, consider enabling `litpy-reveal-at-point' as well."
  :group 'litpy
  :type 'boolean
  :set #'litpy--font-lock-setter
  :initialize #'custom-initialize-default)

(defcustom litpy-reveal-at-point nil
  "Reveal title markup when point is in a title.
This is convenient in conjunction with `litpy-hide-title-markup'."
  :group 'litpy
  :type 'boolean)

(defcustom litpy-hide-quotes t
  "If non-nil, hide double quotes in docstrings (\\=`\\=`…\\=`\\=`) .
If you use this option, consider enabling `litpy-reveal-at-point' as well."
  :group 'litpy
  :type 'boolean
  :set #'litpy--font-lock-setter
  :initialize #'custom-initialize-default)

;;; Faces

(defface litpy-title-face-1
  '((t :height 2.2 :slant normal :inherit font-lock-doc-face))
  "Face used for first-level titles.")

(defface litpy-title-face-2
  '((t :height 1.7 :slant normal :inherit font-lock-doc-face))
  "Face used for second-level titles.")

(defface litpy-title-face-3
  '((t :height 1.25 :slant italic :inherit font-lock-doc-face))
  "Face used for third-level titles.")

(defface litpy-doc-face
  '((t :height 1.2 :slant normal :inherit font-lock-doc-face))
  "Face used for ## comments.")

(defface litpy-doctest-header-face
  '((t :slant normal :inherit (font-lock-constant-face default)))
  "Face used for doctest markers “>>>” and “...”.")

(defface litpy-doctest-face
  '((t :slant italic :inherit default))
  "Face used for doctests.")

(defface litpy-single-quoted-face
  '((t :slant italic :weight semi-bold)) ;; :inherit font-lock-variable-name-face
  "Face used for snippets wrapped in single backticks (\\=`…\\=`).")

(defface litpy-double-quoted-face
  '((t :slant italic :weight normal :inherit font-lock-string-face))
  "Face used for snippets wrapped in double backticks (\\=`\\=`…\\=`\\=`).")

(defface litpy-hidden-underline-face
  '((t :height 1))
  "Face applied to title underlines after hiding them with `litpy-hide-markup'.
You can e.g. give it a background to show a solid line under each title.")

(defconst litpy--comment-marker-re "\\(?:#+@? *\\)")

(defconst litpy--title-first-line-re
  (format "^\\(%s?\\)\\([^# \n].*\\)\\(?:$\\)" litpy--comment-marker-re))

(defconst litpy--title-underline-re ;; FIXME should depend on litpy-title-chars
  (format "\\(?:^\\)\\(%s?\\)\\(=+\\|-+\\|~+\\)\\(?:$\\)" litpy--comment-marker-re))

(defconst litpy--title-line-re
  (concat litpy--title-first-line-re "\\(\n\\)" "\\(" litpy--title-underline-re "\\(\n?\\)" "\\)")
  "Regexp matching title lines:
1. Comment markers on title line
2. Title
3. Newline
4. Entire second line
5. Comment markers before underline
6. Underline
7. Newline (optional)")

;;; Editing titles

(defcustom litpy-title-chars
  '(?= ?- ?~)
  "Characters to use for underlining."
  :type '(repeat character)
  :group 'litpy)

(defun litpy--next-underline-char (char)
  "Find char to use after CHAR when cycling through title styles."
  (cadr (member char litpy-title-chars)))

(defun litpy--match-length (n)
  "Compute length of N th match."
  (- (or (match-end n) 0) (or (match-beginning n) 0)))

(defun litpy--replace-underline (underline-char)
  "Update current underline to use UNDERLINE-CHAR."
  (let* ((comment-starter (match-string 1))
         (underline (make-string (litpy--match-length 2) underline-char)))
    (replace-match underline t t nil 6)
    (replace-match comment-starter t t nil 5)))

(defun litpy-cycle-title ()
  "Cycle through title styles for current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at litpy--title-line-re)
        (let* ((underline-char (char-after (match-beginning 6)))
               (new-underline-char (if (= (litpy--match-length 2) (litpy--match-length 6))
                                       (litpy--next-underline-char underline-char)
                                     underline-char)))
          (if new-underline-char
              (litpy--replace-underline new-underline-char)
            (delete-region (match-end 2) (match-end 6))))
      (if (looking-at litpy--title-first-line-re)
          (progn (end-of-line)
                 (insert "\n" (match-string 1))
                 (insert (make-string (litpy--match-length 2) (car litpy-title-chars))))
        (insert-before-markers "# ")
        (insert "\n# " (char-to-string (car litpy-title-chars)))))))

(defun litpy--update-underline (&rest _)
  "Update underline on current line, if any."
  (save-excursion
    (beginning-of-line)
    (when (looking-at litpy--title-line-re)
      (litpy--replace-underline (char-after (match-beginning 6))))))

;;; Fontification of titles and doctests

(defun litpy--fl-extend-region-backward ()
  "Move `font-lock-beg' to cover title line and doctests around point."
  (goto-char font-lock-beg)
  (setq font-lock-beg (point-at-bol))
  (when (and (= (forward-line -1) 0)
             (looking-at litpy--title-line-re))
    (setq font-lock-beg (point))))

(defun litpy--fl-extend-region-forward ()
  "Move `font-lock-end' to cover title line and doctests around point."
  (goto-char font-lock-end)
  (beginning-of-line)
  (setq font-lock-end (if (looking-at litpy--title-line-re)
                  (match-end 6)
                (point-at-eol))))

(defun litpy--fl-extend-region-function ()
  "Move `font-lock-beg' and `font-lock-end' to cover title line around point."
  (let* ((old-beg font-lock-beg)
         (old-end font-lock-end))
    (save-match-data
      (save-excursion
        (litpy--fl-extend-region-backward)
        (litpy--fl-extend-region-forward)))
    (not (and (= font-lock-beg old-beg)
              (= font-lock-end old-end)))))

(defun litpy--test-font-lock-extend-region ()
  "Visually check whether `litpy--fl-extend-region-function' works."
  (let ((font-lock-beg (min (point) (mark)))
        (font-lock-end (max (point) (mark))))
    (litpy--fl-extend-region-function)
    (goto-char font-lock-beg)
    (set-mark font-lock-end)))

(defun litpy--title-face ()
  "Compute face for just-matched title."
  (let* ((underline-char (char-after (match-beginning 6)))
         (pos (cl-position underline-char litpy-title-chars)))
    (or (and pos (elt '(litpy-title-face-1 litpy-title-face-2 litpy-title-face-3) pos))
        font-lock-doc-face)))

;; Fontification of snippets

(defconst litpy--doctest-re
  "^#*\\s-*\\(>>>\\|\\.\\.\\.\\) ?\\( *\\(.+\\)$\\)"
  "Regexp matching doctests.")

(defconst litpy--single-quoted-re
  "[^`]\\(`\\)\\([^`\n ]+\\)\\(`\\)[^`]")

(defconst litpy--double-quoted-re
  "\\(``\\)\\([^`\n]+\\)\\(``\\)")

;; (defconst litpy--syntax-propertize-rules
;;   (syntax-propertize-rules
;;    ((python-rx string-delimiter)
;;     (0 (ignore (python-syntax-stringify))))
;;    (litpy--doctest-re
;;     (0 (ignore (litpy--syntax-propertize-function)))))
;;   "Function to apply `syntax-class' properties to docstrings and doctests.
;; This contains a copy of the standard Python ones, because they
;; must be interleaved: otherwise, the Python ones could use
;; `syntax-ppss' on beginning and end of docstrings, then mark them,
;; and thereby prevent us from making modifications to doctests
;; inside these docstrings.

;; Still, this isn't sufficient to highlight doctests in comments.
;; The problem is that we need to mark the newline as a comment
;; opener, and that breaks `python-nav-end-of-defun'.  Another
;; problem is that even when properly interleaved, the docstring
;; marking code depends on the docstring opener being a sequence of
;; quotes — not just a propertized character.  One can fix the
;; second problem with

;;     (propertized (and string-start
;;                       (eq (get-char-property string-start 'syntax-table)
;;                           (string-to-syntax \"|\"))))

;; and then (or propertized (= num-quotes num-closing-quotes)), but
;; this doesn't fix the first problem.")

;; (defun litpy--syntax-propertize-function ()
;;   "Propertize current doctest."
;;   (let* ((old-syntax (save-excursion (syntax-ppss (match-beginning 1))))
;;          (in-docstring-p (and nil (eq (nth 3 old-syntax) t))) ;; broken
;;          (in-comment-p (eq (nth 4 old-syntax) t))
;;          (closing-syntax (cond (in-docstring-p "|") (in-comment-p ">")))
;;          (reopening-syntax (cond (in-docstring-p "|") (in-comment-p "<")))
;;          (reopening-char (char-after (match-end 2)))
;;          (no-reopen (eq (and reopening-char (char-syntax reopening-char))
;;                         (cond (in-comment-p ?>)))))
;;     (when closing-syntax
;;       (put-text-property (1- (match-end 1)) (match-end 1)
;;                          'syntax-table (string-to-syntax closing-syntax))
;;       (when (and reopening-char (not no-reopen))
;;         (put-text-property (match-end 3) (1+ (match-end 3))
;;                            'syntax-table (string-to-syntax reopening-syntax))))))

;; (defun litpy--sp-extend-region-function (start end)
;;   "Extend START..END to make sure to include doctests."
;;   (let ((new-start (save-excursion (goto-char start) (point-at-bol)))
;;         (new-end (save-excursion (goto-char end) (point-at-eol))))
;;     (unless (and (= start new-start) (= end new-end))
;;       (cons new-start new-end))))

;;; Snippets

(defun litpy--beginning-of-snippet (&optional many)
  "Go to beginning of snippet at point.
With non-nil MANY, skip over multiple consecutive snippets."
  (beginning-of-line)
  (while (and (looking-at litpy--doctest-re)
              (or many (string= (match-string 1) "..."))
              (= (forward-line -1) 0))))

(defun litpy--beginning-of-snippets ()
  "Go to beginning of snippet block at point."
  (let ((limit (point-at-eol)))
    (litpy--beginning-of-snippet t)
    (when (re-search-forward litpy--doctest-re limit t)
      (beginning-of-line))))

(defun litpy--read-snippet ()
  "Read doctest or snippet at point.
Return a cons (POSITION-ON-LAST-LINE . SNIPPET)."
  (litpy--beginning-of-snippet)
  (let ((lines nil))
    (while (and (looking-at litpy--doctest-re)
                (or (eq lines nil) (string= (match-string 1) "...")))
      (push (cons (match-beginning 1)
                  (match-string-no-properties 2))
            lines)
      (forward-line))
    (when lines
      (cons (caar lines)
            (mapconcat #'cdr (nreverse lines) "\n")))))

(defun litpy--read-snippets ()
  "Read block of snippets or doctests around point.
Return a cons of (POSITION-ON-LAST-LINE . SNIPPETS)."
  (litpy--beginning-of-snippets)
  (let ((snippet nil)
        (snippets nil))
    (while (setq snippet (litpy--read-snippet))
      (push snippet snippets))
    (when snippets
      (cons (caar snippets)
            (mapcar #'cdr (nreverse snippets))))))

(defun litpy--snippet-at-point (&optional many)
  "Find snippet (or snippets, if MANY) at point.
Return a cons (POSITION-ON-LAST-LINE . SNIPPET)."
  (or (save-excursion (if many (litpy--read-snippets) (litpy--read-snippet)))
      (user-error "No snippet at point")))

(defun litpy-copy-snippet-to-interpreter ()
  "Copy snippet at point to interpreter."
  (interactive)
  (let ((cmd (cdr (litpy--snippet-at-point)))
        (python (save-excursion (run-python))))
    (with-current-buffer (process-buffer python)
      (goto-char (point-max))
      (insert cmd)
      (pop-to-buffer (current-buffer))
      (set-window-point (selected-window) (point-max)))))

(defun litpy-eval-snippet-inline (&optional many)
  "Show result of running snippet at point.
With non-nil MANY (interactively, with prefix arg), evaluate the
full surrounding block."
  (interactive "P")
  (pcase-let* ((`(,pos . ,cmd) (litpy--snippet-at-point many))
               (python (save-excursion (run-python))))
    (unless (> (quick-peek-hide pos) 0)
      (quick-peek-hide)
      (let ((output nil))
        (dolist (cmd (if many cmd (list cmd)))
          (setq output (python-shell-send-string-no-output cmd python)))
        (quick-peek-show output pos nil 'none)))))

(defun litpy-eval-snippets-inline (just-one)
  "Show results of running snippets around point.
With non-nil JUST-ONE (interactively, with prefix arg), evaluate
just the snippet at point."
  (interactive "P")
  (litpy-eval-snippet-inline (not just-one)))

;;; Hide and reveal markup

(defun litpy--refresh-font-lock ()
  "Reset font-locking in current buffer."
  (if (fboundp 'font-lock-ensure)
      (font-lock-flush)
    (with-no-warnings (font-lock-fontify-buffer))))

(defun litpy--font-lock-setter (var val)
  "Set VAR to VAL and refresh font-locking in all litpy buffers."
  (set-default var val)
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'litpy-mode buf)
      (with-current-buffer buf
        (litpy--refresh-font-lock)))))

(defun litpy-toggle-quotes-markup ()
  "Toggle visibility of quotes."
  (interactive)
  (setq-local litpy-hide-quotes (not litpy-hide-quotes))
  (litpy--refresh-font-lock))

(defun litpy-toggle-title-markup ()
  "Toggle visibility of comment chars and underlines."
  (interactive)
  (setq-local litpy-hide-title-markup (not litpy-hide-title-markup))
  (litpy--refresh-font-lock))

(defun litpy-hide-markup ()
  "Hide all decorations."
  (setq litpy-hide-title-markup t)
  (setq litpy-hide-quotes t)
  (litpy--refresh-font-lock))

(defvar-local litpy--title-beg nil)
(defvar-local litpy--title-end nil)

(defvar-local litpy--title-boundaries nil)
(defvar litpy--title-timer nil)

(defun litpy--post-command-hook (&rest _)
  "Reveal or hide title markup around point."
  (pcase-let ((`(,beg . ,end) litpy--title-boundaries))
    (when (and beg end (not (<= beg (point) end)))
      (save-excursion (font-lock-flush beg end))
      (setq litpy--title-boundaries nil)))
  (unless litpy--title-timer
    (setq litpy--title-timer (run-with-idle-timer 0.05 nil #'litpy--reveal-at-point))))

(defun litpy--reveal-at-point ()
  "Reveal title markup around point."
  (when litpy-reveal-at-point
    (save-excursion
      (beginning-of-line)
      ;; (beginning-of-visual-line)
      (when (looking-at litpy--title-line-re)
        (let ((beg (match-beginning 0)) (end (match-end 0)))
          (setq litpy--title-boundaries (cons beg end))
          (with-silent-modifications
            (remove-text-properties (match-beginning 0) (match-end 0) '(display)))))))
  (setq litpy--title-timer nil))

;;; Hideshow

(defun litpy--setup-hideshow ()
  "Set hideshow variables."
  (setq-local hs-block-start-regexp "^[ \t]+\\(\"\"\"\\|'''\\)")
  (setq-local hs-block-start-mdata-select 1)
  (setq-local hs-block-end-regexp "\\(\"\"\"\\|'''\\)")
  (setq-local hs-hide-comments-when-hiding-all nil)
  (setq-local hs-forward-sexp-func #'forward-sexp))

;;; Minor mode

(defconst litpy--thin-display-spec '(display (space :width (0))))

(defun litpy--fl-decoration-spec (&optional switch)
  "Compute a font-lock specification for litpy markup.
This either shows or hide the corresponding text span, depending
on the value of SWITCH (default: `litpy-hide-title-markup')."
  (setq switch (or switch 'litpy-hide-title-markup))
  `(face nil ,@(and (symbol-value switch) litpy--thin-display-spec)))

(defconst litpy--keywords
  `((,litpy--title-line-re
     (0 (litpy--title-face) prepend)
     (1 (litpy--fl-decoration-spec) prepend)
     (4 `(face ,(when litpy-hide-title-markup 'litpy-hidden-underline-face)) prepend)
     (5 (litpy--fl-decoration-spec) prepend)
     ;; `(face nil display (space :width ,(litpy--match-length 2))) ;; FIXME this space width isn't right
     (6 (litpy--fl-decoration-spec) prepend))
    ("^\\(##@?\\s-\\).*"
     (0 'litpy-doc-face prepend)
     (1 (litpy--fl-decoration-spec) prepend))
    ;; ("^ +" (0 'default prepend))
    (,litpy--doctest-re
     (2 'litpy-doctest-face prepend)
     (1 'litpy-doctest-header-face prepend)
     (0 (indirect-font-lock-highlighter 3 'python-mode)))
    (,litpy--single-quoted-re ;; FIXME only in docstrings!
     (1 (litpy--fl-decoration-spec 'litpy-hide-quotes) prepend)
     (2 'litpy-single-quoted-face prepend)
     (3 (litpy--fl-decoration-spec 'litpy-hide-quotes) prepend))
    ;; ("\\(\n\\) +\"\"\"" (1 '(face nil display (space :align-to 80))))
    ;; ("^ +\"\"\".*\\(\n\\)" (1 '(face nil line-height 1.2)))
    ;; ("\"\"\"\\(\n\\)" (1 '(face nil line-spacing 0.2)))
    (,litpy--double-quoted-re
     (1 (litpy--fl-decoration-spec 'litpy-hide-quotes) prepend)
     ;; (1 '(face nil display "“") prepend)
     (2 'litpy-double-quoted-face prepend)
     ;; (3 '(face nil display " ”") prepend)
     (3 (litpy--fl-decoration-spec 'litpy-hide-quotes) prepend)
     (0 (indirect-font-lock-highlighter 2 'python-mode)))))

(defvar litpy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-=") #'litpy-cycle-title)
    (define-key map (kbd "C-c <C-return>") #'litpy-copy-snippet-to-interpreter)
    (define-key map (kbd "<S-menu>") #'litpy-eval-snippets-inline)
    (define-key map (kbd "<menu>") #'litpy-eval-snippet-inline)
    map))

(define-minor-mode litpy-mode
  "Literate programming in Python."
  :lighter " 🐍" ;; 📜
  :keymap litpy-mode-map
  (cond
   (litpy-mode
    (setq-local font-lock-multiline t)
    (font-lock-add-keywords nil litpy--keywords 'append)
    (add-to-list 'font-lock-extra-managed-props 'display)
    (add-hook 'font-lock-extend-region-functions #'litpy--fl-extend-region-function nil t)
    (add-hook 'post-command-hook #'litpy--post-command-hook nil t)
    (add-hook 'after-change-functions #'litpy--update-underline nil t)
    (add-hook 'hs-minor-mode-hook #'litpy--setup-hideshow nil t)
    (add-hook 'presenter-mode-hook #'litpy-hide-markup nil t))
   (t
    (font-lock-remove-keywords nil litpy--keywords)
    (remove-hook 'font-lock-extend-region-functions #'litpy--fl-extend-region-function t)
    (remove-hook 'post-command-hook #'litpy--post-command-hook t)
    (remove-hook 'after-change-functions #'litpy--update-underline t)
    (remove-hook 'hs-minor-mode-hook #'litpy--setup-hideshow t)
    (remove-hook 'presenter-mode-hook #'litpy-hide-markup t)))
  (litpy--refresh-font-lock))

(provide 'litpy-mode)
;;; litpy-mode.el ends here
