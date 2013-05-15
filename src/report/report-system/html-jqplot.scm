(use-modules (gnucash core-utils))

(define (gnc:html-js-include file)
    (string-append
        "<script language=\"javascript\" type=\"text/javascript\" src=\"file://"
        (gnc-path-find-localized-html-file file)
        "\"></script>\n"
    ))

(define (gnc:html-css-include file)
    (string-append
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"file://"
        (gnc-path-find-localized-html-file file)
        "\" />\n"
    ))

