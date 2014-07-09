;;; shm-insert-del.el --- Insertion/deletion commands

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'shm-macros)
(require 'shm-slot)
(require 'shm-layout)
(require 'shm-indent)

(defun shm-post-self-insert ()
  "Self-insertion handler."
  (save-excursion
    (shm-appropriate-adjustment-point 'forward)
    (forward-char -1)
    (shm-adjust-dependents (point) 1)))

(defun shm/wrap-parens ()
  "Wrap the node in parentheses."
  (interactive)
  (cond
   ((region-active-p)
    (shm-wrap-delimiters "(" ")"))
   (t (let ((line (line-number-at-pos))
            (node (shm-current-node)))
        (save-excursion
          (goto-char (shm-node-start node))
          (insert "(")
          (goto-char (shm-node-end node))
          (when (/= line (line-number-at-pos))
            (indent-rigidly (shm-node-start node)
                            (shm-node-end node)
                            1))
          (insert ")"))
        (forward-char 1)))))

(defun shm/space ()
  "Insert a space but sometimes do something more clever, like
  inserting skeletons."
  (interactive)
  (if (and (bound-and-true-p god-local-mode)
           (fboundp 'god-mode-self-insert))
      (god-mode-self-insert)
    (let ((case-fold-search nil))
      (cond
       ((or (shm-in-comment)
            (shm-in-string))
        (insert " "))
       (shm-auto-insert-skeletons
        (cond
         ((and (looking-back "[^a-zA-Z0-9_]do")
               (or (eolp)
                   (looking-at "[])}]")))
          (shm-auto-insert-do))
         ((and (looking-back " <-")
               (let ((current (shm-current-node)))
                 (when current
                   (or (eq 'Do (shm-node-cons current))
                       (string= "Stmt" (shm-node-type-name current))))))
          (if (bound-and-true-p structured-haskell-repl-mode)
              (insert " ")
            (shm-auto-insert-stmt 'qualifier)))
         ((looking-back "[^a-zA-Z0-9_]case")
          (shm-auto-insert-case))
         ((looking-back "[^a-zA-Z0-9_]if")
          (shm-auto-insert-if))
         ((looking-back "[^a-zA-Z0-9_]let")
          (cond
           ((let ((current (shm-current-node)))
              (and current
                   (or (not (or (eq 'Do (shm-node-cons current))
                                (eq 'BDecls (shm-node-cons current))
                                (string= "Stmt" (shm-node-type-name current))))
                       (bound-and-true-p structured-haskell-repl-mode)))
              (shm-auto-insert-let)))
           ((not (bound-and-true-p structured-haskell-repl-mode))
            (shm-auto-insert-stmt 'let))))
         ((and (looking-back "module")
               (= (line-beginning-position)
                  (- (point) 6))
               (looking-at "[ ]*$"))
          (shm-auto-insert-module))
         (t (shm-insert-string " "))))
       (t (shm-insert-string " "))))))

(defun shm/double-quote ()
  "Insert double quotes.

This tries to be clever about insertion. If already in a string,
it will insert \", if at the end of a string, it will glide over
the ending quote. If not in a string, it will insert \"\", and
also space out any neccessary spacing."
  (interactive)
  (shm/reparse)
  (if (shm-in-comment)
      (insert "\"")
    (let* ((current-node (shm-current-node))
           (node (if (eq 'Lit (shm-node-cons current-node))
                     (shm-actual-node)
                   current-node)))
      (cond
       ((and (shm-in-string)
             (looking-back "\\\\"))
        (insert "\""))
       ((shm-find-overlay 'shm-quarantine)
        (insert "\"\"")
        (forward-char -1))
       ;; "…|…"
       ((shm-in-string)
        (cond
         ;; "…|"
         ((= (point)
             (1- (shm-node-end node)))
          (forward-char 1))
         ;; "…|…"
         ((= (point) (shm-node-end node))
          (if (looking-back "\"")
              (shm-delimit "\"" "\"")
            (progn (insert "\""))))
         (t (let ((inhibit-read-only t))
              (shm-adjust-dependents (point) 2)
              (insert "\\\"")))))
       ;; '|'
       ((save-excursion (forward-char -1)
                        (looking-at "''"))
        (let ((inhibit-read-only t))
          (shm-adjust-dependents (point) 1)
          (insert "\"")))
       ;; anywhere
       (t
        (shm-delimit "\"" "\""))))))

(defun shm/comma (n)
  "Insert a comma. In a list it tries to help a bit by setting
the current node to the parent."
  (interactive "p")
  (if (shm-in-comment)
      (self-insert-command n)
    (let ((current-pair (shm-current-node-pair)))
      (if (not current-pair)
          (self-insert-command n)
        (let* ((current (cdr current-pair))
               (parent-pair (shm-node-parent current-pair))
               (parent (cdr parent-pair)))
          (cond
           ;; When inside a list, indent to the list's position with an
           ;; auto-inserted comma.
           ((eq 'List (shm-node-cons parent))
            (shm-insert-string ",")
            (shm-set-node-overlay parent-pair))
           (t
            (shm-insert-string ",")
            (shm-set-node-overlay parent-pair))))))))

(defun shm/single-quote ()
  "Delimit single quotes."
  (interactive)
  (shm-delimit "'" "'"))

(defun shm/= ()
  "Insert equal."
  (interactive)
  (cond
   ((shm-literal-insertion)
    (insert "="))
   (t (unless (looking-back " ")
        (shm-insert-string " "))
      (shm-insert-string "=")
      (unless (looking-at " ")
        (shm-insert-string " ")))))

(defun shm/: ()
  "Insert colon."
  (interactive)
  (if (or (not shm-colon-enabled)
          (shm-literal-insertion))
      (call-interactively 'self-insert-command)
    (let ((current (shm-current-node)))
      (cond
       ((and current
             (or (eq (shm-node-cons current)
                     'SpliceDecl)
                 (string= (shm-node-type-name current)
                          "BangType")
                 (string= (shm-node-type-name current)
                          "FieldDecl")))
        (unless (looking-back "[ ]+")
          (insert " "))
        (unless (looking-back "::[ ]+")
          (shm-insert-string ":: a")
          (forward-word -1)
          (shm-evaporate (point) (1+ (point)))))
       (t
        (shm-insert-string ":"))))))

(defun shm/hyphen (n)
  "The - hyphen."
  (interactive "p")
  (if (and (looking-back "{")
           (looking-at "}"))
      (progn (insert "--")
             (forward-char -1))
    (self-insert-command n)))

(defun shm/hash (n)
  "The # hash."
  (interactive "p")
  (if (and (looking-back "{-")
           (looking-at "-}"))
      (progn (insert "#  #")
             (forward-char -2)
             (let ((pragma (ido-completing-read "Pragma: "
                                                shm-pragmas)))
               (insert pragma
                       " ")
               (when (string= pragma "LANGUAGE")
                 (insert (ido-completing-read
                          "Language: "
                          (remove-if (lambda (s) (string= s ""))
                                     (split-string (shell-command-to-string "ghc --supported-languages")
                                                   "\n")))))))
    (self-insert-command n)))

(defun shm/open-paren ()
  "Delimit parentheses."
  (interactive)
  (let ((current (shm-current-node)))
    (cond
     ((and current
           (or (string= "ExportSpec" (shm-node-type-name current))
               (string= "ImportSpec" (shm-node-type-name current))))
      (insert "()")
      (forward-char -1))
     (t
      (shm-delimit "(" ")")))))

(defun shm/open-bracket ()
  "Delimit brackets."
  (interactive)
  (shm-delimit "[" "]"))

(defun shm/open-brace ()
  "Delimit braces."
  (interactive)
  (let ((current (shm-current-node)))
    (cond
     ((and current
           (string= "Pat" (shm-node-type-name current)))
      (shm-insert-string "{}")
      (forward-char -1))
     (t
      (shm-delimit "{" "}")))))

(defun shm/del ()
  "Character deletion handler.

Generally, we delete things in the current node. BUT, there are
some things that we shouldn't delete, because they would cause
parse errors that are rarely useful. For example:

    (|case x of _ -> _) -- where | indicates cursor.

"
  (interactive)
  (shm-with-fallback
   (lambda () (interactive)
     (if hungry-delete-mode
         (hungry-delete-backward) (delete-backward-char)))
   (let ((case-fold-search nil))
     (cond
      ;; hungry-delete-backward when current line is only whitespace
      ((and hungry-delete-mode
            (save-excursion (beginning-of-line)
                            (skip-syntax-forward " ")
                            (eolp)))
       (hungry-delete-backward))
      ;; These cases are “gliders”. They simply move over the character
      ;; backwards. These could be handled all as one regular
      ;; expression, but in the interest of clarity—for now—they are left
      ;; as separate cases.
      ((and (shm-in-string)
            (looking-back "^[ ]*\\\\"))
       (let ((here (point)))
         (delete-region (search-backward-regexp "\\\\$")
                        here)))
      ((and (looking-back "{-[ ]*")
            (looking-at "[ ]*-}"))
       (delete-region (search-backward-regexp "-")
                      (progn (forward-char 1)
                             (search-forward-regexp "-"))))
      ((and (looking-back "^{-#[ ]*")
            (looking-at "[ ]*#-}$"))
       (delete-region (search-backward-regexp "#")
                      (progn (forward-char 1)
                             (search-forward-regexp "#"))))
      ((looking-back "[()]") (shm-delete-or-glide "(" ")"))
      ((looking-back "[[]") (shm-delete-or-glide "\\[" "\\]"))
      ((looking-back "[]]") (shm-delete-or-glide "\\[" "\\]"))
      ((looking-back "[{}]") (shm-delete-or-glide "{" "}"))
      ((looking-back "[\"]") (shm-delete-or-glide "\"" "\""))
      ;; These kind of patterns block the parens of syntaxes that would
      ;; otherwise break everything, so, "if", "of", "case", "do",
      ;; etc. if deleted.
      ((and (shm-prevent-parent-deletion-p)
            (looking-back "[^A-Zaz0-9_]do ?")
            (not (or (eolp)
                     (looking-at "[])}]"))))
       nil)                             ; do nothing
      ((and (shm-prevent-parent-deletion-p)
            (looking-back " <-")
            (not (or (eolp)
                     (looking-at "[])}]"))))
       (forward-char -3))
      ((and (shm-prevent-parent-deletion-p)
            (looking-back " <- ")
            (not (or (eolp)
                     (looking-at "[])}]"))))
       (forward-char -4))
      ((and (shm-prevent-parent-deletion-p)
            (looking-back "[^A-Zaz0-9_]of ?"))
       (search-backward-regexp "[ ]*of"))
      ((and (shm-prevent-parent-deletion-p)
            (or (looking-at "of$")
                (looking-at "of ")))
       (forward-char -1))
      ((and (shm-prevent-parent-deletion-p)
            (looking-back "[_ ]-> ?")) (forward-char -3))
      ((and (shm-prevent-parent-deletion-p)
            (looking-at "-> ?"))
       (forward-char -1))
      ((and (shm-prevent-parent-deletion-p)
            (looking-back "[^A-Zaz0-9_]then ?"))
       (search-backward-regexp "[^ ][ ]*then")
       (unless (or (looking-at "$") (looking-at " "))
         (forward-char 1)))
      ((and (shm-prevent-parent-deletion-p)
            (looking-back "[^A-Zaz0-9_]else ?"))
       (search-backward-regexp "[^ ][ ]*else")
       (unless (or (looking-at "$") (looking-at " "))
         (forward-char 1)))
      ((and (shm-prevent-parent-deletion-p)
            (looking-back "^module ?"))
       (when (looking-at "[ ]*where$")
         (delete-region (line-beginning-position) (line-end-position))))
      ((and (shm-prevent-parent-deletion-p)
            (looking-back "[^A-Zaz0-9_]if ?"))
       nil)                             ; do nothing
      ((and (shm-prevent-parent-deletion-p)
            (looking-back "[^A-Zaz0-9_]case ?"))
       nil)                             ; do nothing
      ((and (shm-prevent-parent-deletion-p)
            (and (looking-at "= ")
                 (looking-back " ")))
       (forward-char -1))
      ((and (shm-prevent-parent-deletion-p)
            (or (and (looking-back " = ")
                     (not (looking-at "$"))
                     (not (looking-at " ")))
                (and (looking-back "=")
                     (looking-at " "))))
       (search-backward-regexp "[ ]+=[ ]*"
                               (line-beginning-position)
                               t
                               1)
       (when (looking-back " ")
         (when (search-backward-regexp "[^ ]" (line-beginning-position)
                                       t 1)
           (forward-char 1))))
      ;; This is the base case, we assume that we can freely delete
      ;; whatever we're looking back at, and that the node will be able
      ;; to re-parse it.
      (t (shm-delete-char)
         (save-excursion
           (shm-appropriate-adjustment-point 'backward)
           (shm-adjust-dependents (point) -1))))))
  (shm/init t))

(defun shm-prevent-parent-deletion-p ()
  "Prevent parent deletion at point?"
  (and shm-prevent-parent-deletion
       (not (shm-in-string))))

(defun shm-delete-or-glide (open close)
  "Delete the given OPEN/CLOSE delimiter, or simply glide over it
  if it isn't empty."
  (cond
   ;; If the delimiters are empty, we can delete the whole thing.
   ((shm-delimiter-empty open close)
    (let ((inhibit-read-only t))
      (shm-adjust-dependents (point) -2)
      (delete-region (1- (point))
                     (1+ (point)))))
   ;; If the delimiters aren't empty and we're in a literal, then go
   ;; ahead and elete the character.
   ((and (shm-literal-insertion)
         (not (= (point) (1+ (shm-node-start (shm-current-node))))))
    (shm-delete-char))
   ;; Otherwise just glide over the character.
   (t
    (when (looking-back close)
      (forward-char -1)))))

(defun shm-delete-char ()
  "Delete a character backwards or delete the region, if there is
one active."
  (if (region-active-p)
      (delete-region (region-beginning)
                     (region-end))
    (delete-region (1- (point))
                   (point))))

(defun shm-delimiter-empty (open close)
  "Is the current expression delimited by OPEN and CLOSE empty?"
  (and (looking-back open)
       (not (save-excursion (forward-char (* -1 (length open)))
                            (looking-back "\\\\")))
       (looking-at close)))

(defun shm-wrap-delimiters (open close)
  "Wrap the current region with the given delimiters. Called when
the region is active."
  (let ((beg (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char beg)
      (save-excursion
        (goto-char end)
        (shm-insert-string close))
      (shm-insert-string open))
    (when (= (point) beg)
      (forward-char 1))))

(defun shm-delimit (open close)
  "Insert the given delimiters.

This is a special function because it will do different things
depending on the context.

If we're in a string, it just inserts OPEN. If we're in an
expression, it will insert OPEN and CLOSE and put the point
between them. It will also space out so that there is space
between previous nodes and the next. E.g.

foo|(bar)

If you hit \" at | then you will get:

foo \"\" (bar)

It saves one having to type spaces; it's obvious what to do
here."
  (cond
   ((region-active-p)
    (shm-wrap-delimiters open close))
   ((and (shm-literal-insertion)
         (not (string= open "\"")))
    (shm-insert-string open))
   (t
    (shm/reparse)
    (let ((current (shm-actual-node)))
      (cond
       ((shm-find-overlay 'shm-quarantine)
        (if (not (or (looking-back "[ ,[({\\]")
                     (and (looking-back "\\$")
                          (string= "(" open))
                     (bolp)))
            (progn (shm-insert-string " ") 1)
          0)
        (shm-insert-string open)
        (let ((point (point)))
          (shm-insert-string close)
          (when (and (/= (point) (line-end-position))
                     (not (looking-at "[]){} ,]")))
            (shm-insert-string " "))
          (goto-char point)))
       (t
        (if (not (or (looking-back "[ ,[({]")
                     (bolp)))
            (progn (shm-insert-string " ") 1)
          0)
        (shm-insert-string open)
        (let ((point (point)))
          (shm-insert-string close)
          (when (and (/= (point) (line-end-position))
                     (not (looking-at "[]){} ,]")))
            (shm-insert-string " "))
          (goto-char point)
          (shm/init t))))))))

(defun shm-auto-insert-stmt (type)
  "Insert template

do x <- |
   {undefined}
"
  (let* ((current (shm-current-node))
         (column (save-excursion
                   (case type
                     ('let (backward-word 1)
                       (current-column))
                     ('qualifier
                      (cond
                       ((eq 'Do (shm-node-cons current))
                        (goto-char (shm-node-start current))
                        (forward-char 2)
                        (search-forward-regexp "[^ \n]")
                        (1- (current-column)))
                       (t (goto-char (shm-node-start current))
                          (current-column))))))))
    (unless (save-excursion
              (let ((current-line (line-number-at-pos)))
                (forward-line 1)
                (goto-char (+ (line-beginning-position)
                              column))
                (and (not (bolp))
                     (/= current-line (line-number-at-pos))
                     (= (point)
                        (save-excursion (back-to-indentation)
                                        (point))))))
      (save-excursion
        (newline)
        (indent-to column)
        (insert "undefined")
        (forward-word -1)
        (shm/reparse)
        (shm-evaporate (point)
                       (progn (forward-word 1)
                              (point)))))
    (insert " ")))

(defun shm/delete ()
  "Delete the current node.

If point is at the end of a line with an expression before it,
and the next line has an indented expression on it, then do
`shm/swing-up' instead.

If the rest of the line is whitespace, and the next line is
empty, then delete the whitespace and all following empty
lines (`hungry-delete-forward' would have deleted the newline
before the next non-empty line as well, which would mess with
indentation).
"
  (interactive)
  (let* ((fallback '(if hungry-delete-mode (hungry-delete-forward) (delete-char 1)))
         (current-pair (shm-current-node-pair))
         (current (cdr current-pair))
         (parent (cdr (shm-node-parent current-pair)))
         whitespace-to-end
         (special-open-parens (string-to-list "([{"))
         (special-close-parens (string-to-list ")]}"))
         (special-constructs "\\(::\\|->\\)") ; length 2
         )
    (if (shm-in-comment) (eval fallback)
      (setq current (shm-current-node)
            whitespace-to-end (save-excursion (skip-syntax-forward " ") (eolp)))
      (cond
       ((eobp) nil)
       ;; Swing up: at the end of a line with an expression behind and
       ;; an expression on the next line
       ;; If swing-up fails, go to the other cases.
       ((and (save-excursion (skip-syntax-backward " ") (not (bolp)))
             whitespace-to-end
             (save-excursion (and (= 0 (forward-line))
                                  (skip-syntax-forward " ")
                                  (not (eolp))))
             (shm/swing-up/attempt)))
       ;; Delete many empty lines
       ((and whitespace-to-end
             (or (eobp)
                 (save-excursion (and (= 0 (forward-line)) (looking-at-p "^\\s-*$")))))
        (let ((end (line-end-position 2)))
          (save-excursion (goto-char end)
                          (while (and (= 0 (forward-line))
                                      (looking-at-p "^\\s-*$"))
                            (setq end (line-end-position))))
          (delete-region (point) end)))
       ;; Delete whitespace, not node when char-after is whitespace
       ((= 0 (syntax-class (syntax-after (point))))
        (eval fallback))
       ;; When looking at the start of a list of some kind, don't
       ;; delete the opening paren and skip it
       ((memq (char-after) special-open-parens)
        (forward-char))
       ;; When looking at the end of a list, delete empty lists with
       ;; no whitespace, otherwise skip then paren
       ((memq (char-after) special-close-parens)
        (if (memq (char-before) special-open-parens)
            (progn (forward-char) (delete-char -2))
          (forward-char)))
       ;; Delete strings as paredit does
       ((and (shm-in-string) (= ?\" (char-after)))
        (if (= ?\" (char-before))
            (progn (forward-char) (delete-char -2))
          (forward-char)))
       ((= (char-after) ?\")
        (forward-char))
       ;; Special constructs that are deleted whole
       ((or (looking-at special-constructs)
            (and (not (bolp))
                 (save-excursion (backward-char) (looking-at special-constructs))))
        (delete-region (match-beginning 1) (match-end 1))
        (shm/reparse))
       ;; "-!-Tree a" => ""
       ((and (eq 'TyCon (shm-node-cons current))
             (eq 'TyApp (shm-node-cons parent))
             (eq (point) (shm-node-start parent)))
        (delete-region (shm-node-start parent) (shm-node-end parent))
        (shm/reparse))
       ;; Only delete a node when point is right before it
       ;; I find it confusing otherwise, e.g., in
       ;; f -!-:: Int -> Int
       ;; the node is the whole function type declaration
       ((= (point) (shm-node-start current))
        (message "Deleting node: %s\n%s" current (buffer-substring (shm-node-start current) (shm-node-end current)))
        (delete-region (shm-node-start current)
                       (shm-node-end current))
        (shm/reparse))
       ;; Fall back
       (t (eval fallback))))))

(defun shm/export ()
  "Export the identifier at point."
  (interactive)
  (let ((name (shm-node-string (shm-actual-node))))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "^module")
      (search-forward-regexp " where")
      (search-backward-regexp ")")
      (shm/reparse)
      (shm/newline-indent)
      (insert name))))

(provide 'shm-insert-del)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
