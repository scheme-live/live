;; The 'nil' configuration applies to all modes.
((scheme-mode
  (indent-tabs-mode . nil)
  (lisp-local-indent
   c-lambda 2
   test-group 1
   ;; okvs:
   call-with-input-file 1
   call-with-values 1
   match 1
   switch 1)))
