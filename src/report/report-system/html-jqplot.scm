(use-modules (gnucash core-utils))

(define (gnc:html-js-include file)
    (string-append
        "<script language=\"javascript\" type=\"text/javascript\" src=\"file:///"
        (gnc-path-find-localized-html-file file)
        "\"></script>\n"
    ))

(define (gnc:html-css-include file)
    (string-append
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"file:///"
        (gnc-path-find-localized-html-file file)
        "\" />\n"
    ))

(define (jqplot-escape-string s1)
    ;; Escape single and double quotes and backslashes
    (set! s1 (regexp-substitute/global #f "\\\\" s1 'pre "\\\\" 'post))
    (set! s1 (regexp-substitute/global #f "'" s1 'pre "\\'" 'post))
    (set! s1 (regexp-substitute/global #f "\"" s1 'pre "\\\"" 'post))
    ;; Escape HTML special characters
    (set! s1 (regexp-substitute/global #f "&" s1 'pre "&amp;" 'post))
    (set! s1 (regexp-substitute/global #f "<" s1 'pre "&lt;" 'post))
    (regexp-substitute/global #f ">" s1 'pre "&gt;" 'post))
