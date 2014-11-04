;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  core-utils.scm
;;;  Guile module for core-utils
;;;
;;;  Copyright 2006 Chris Shoemaker <c.shoemaker@cox.net>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash core-utils))

;; Guile 2 needs to find the symbols from the extension at compile time already
(cond-expand
  (guile-2
    (eval-when
      (compile load eval expand)
      (load-extension "libgnc-core-utils" "scm_init_sw_core_utils_module")))
  (else
    (load-extension "libgnc-core-utils" "scm_init_sw_core_utils_module")))

(use-modules (sw_core_utils))

(re-export gnc-prefs-is-debugging-enabled)
(re-export gnc-path-get-bindir)
(re-export gnc-path-get-stdreportsdir)
(re-export gnc-path-find-localized-html-file)
(re-export gnc-build-dotgnucash-path)
(re-export gnc-build-report-path)
(re-export gnc-build-stdreports-path)
(re-export gnc-utf8?)
(re-export gnc-utf8-strip-invalid-strdup)
(re-export gnc-locale-from-utf8)
(re-export gnc-locale-to-utf8)
(re-export gnc-scm-log-warn)
(re-export gnc-scm-log-error)
(re-export gnc-scm-log-msg)
(re-export gnc-scm-log-debug)
(re-export gnc-locale-default-iso-currency-code)

(re-export gnc-prefs-set-bool)
(re-export gnc-prefs-set-int)
(re-export gnc-prefs-set-int64)
(re-export gnc-prefs-set-float)
(re-export gnc-prefs-set-string)
(re-export gnc-prefs-set-coords)

(define-public gnc:version (gnc-version))
