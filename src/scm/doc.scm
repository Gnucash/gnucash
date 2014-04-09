
(define (gnc:find-doc-file file)
  (gnc:find-in-directories file (gnc:config-var-value-get gnc:*doc-path*)))

