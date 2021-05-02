;; The 'nil' configuration applies to all modes.
((scheme-mode
  (indent-tabs-mode . nil)
  (tab-width . 2)
  (lisp-local-indent
   ;; okvs
   switch 1
   call-with-input-file 1
   call-with-values 1
   match 1)))
