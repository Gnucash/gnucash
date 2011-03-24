(load-from-path "doc.scm")

(define (gnc:html-js-include file)
    (string-append
        "<script language=\"javascript\" type=\"text/javascript\" src=\""
        (gnc:find-file file (gnc:config-var-value-get gnc:*doc-path*))
        "\"></script>\n"
    ))

(define (gnc:html-css-include file)
    (string-append
        "<link rel=\"stylesheet\" type=\"text/css\" href=\""
        (gnc:find-file file (gnc:config-var-value-get gnc:*doc-path*))
        "\" />\n"
    ))

