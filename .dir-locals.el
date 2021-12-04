;; The 'nil' configuration applies to all modes.
((scheme-mode
  (indent-tabs-mode . nil)
  (lisp-local-indent
   guard 1
   c-lambda 2
   let*-pointers 1
   test-group 1
   unwind-protect 0
   ;; okvs:
   call-with-input-file 1
   call-with-values 1
   match 1
   switch 1)))
