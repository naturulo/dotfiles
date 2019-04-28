(define-key *global-keymap* "Return" 'lem.language-mode:newline-and-indent)

(setf *scroll-recenter-p* nil)
(sttf (variable-value 'truncate-lines :global) nil)
(setf (variable-value 'lem.line-numbers:line-numbers :global) t)

(lem-paredit-mode:paredit-mode)
(lem-lisp-mode.paren-coloring:toggle-paren-coloring)

