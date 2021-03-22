;; The 'nil' configuration applies to all modes.
((scheme-mode . ((indent-tabs-mode . nil)
		 (tab-width . 2)
		 (eval . (progn
                           ;; okvs
			   (put 'switch 'scheme-indent-function 1)
			   (put 'call-with-input-file 'scheme-indent-function 1)
			   (put 'call-with-values 'scheme-indent-function 1)
			   (put 'match 'scheme-indent-function 1))))))
