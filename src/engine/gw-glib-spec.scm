(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-glib-spec)
  :use-module (g-wrap))

(let ((mod (gw:new-module "gw-glib")))

  (define (standard-c-call-gen result func-call-code)
    (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
  
  (define (add-standard-result-handlers! type c->scm-converter)
    (define (standard-pre-handler result)
      (let* ((ret-type-name (gw:result-get-proper-c-type-name result))
             (ret-var-name (gw:result-get-c-name result)))
        (list "{\n"
              "    " ret-type-name " " ret-var-name ";\n")))
    
    (gw:type-set-pre-call-result-ccodegen! type standard-pre-handler)
    
    (gw:type-set-post-call-result-ccodegen!
     type
     (lambda (result)
       (let* ((scm-name (gw:result-get-scm-name result))
              (c-name (gw:result-get-c-name result)))
         (list
          (c->scm-converter scm-name c-name)
          "  }\n")))))
  
  (gw:module-depends-on mod "gw-runtime")

  (gw:module-set-guile-module! mod '(g-wrapped gw-glib))

  ;; All of this glib string code needs to be moved to the (g-wrapped
  ;; glib) module, as does the supporting header code above -- we'll
  ;; do that next time round.

  (let ((nnt (gw:wrap-non-native-type mod '<glib:GList*> 
                                      "GList*" "const GList*")))
    #t)
  
  (gw:wrap-function
   mod
   'gnc:glist->list
   '<gw:scm> "gnc_glist_to_scm_list" '((<glib:GList*> glist) (<gw:wct> wct))
   "Convert glist to scheme list of wcp's of type wct.")

  (gw:wrap-function
   mod
   'gnc:list->glist
   '<glib:GList*> "gnc_scm_list_to_glist" '((<gw:scm> wcp-list))
   "Convert scheme list of wcp's to GList*.")

  (gw:wrap-function
   mod
   'gnc:glist-map
   '<gw:scm> "gnc_glist_scm_map" '((<gw:wct> wct) (<gw:scm> thunk) (<glib:GList*> glist))
   "Call thunk on every element of glist after conversion to wcp of type wct, "
   "and return a list of the results.")
  
  (gw:wrap-function
   mod
   'gnc:glist-for-each
   '<gw:void> "gnc_glist_scm_for_each"
   '((<gw:wct> wct) (<gw:scm> thunk) (<glib:GList*> glist))
   "Call thunk on every element of glist after conversion to wcp of type wct.")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; gint64
  (let ((wt (gw:wrap-type mod '<glib:gint64> "gint64" "const gint64")))

    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       ;; I don't know if it's more efficient to work on the C side or
       ;; the scheme side...
       (let ((x (gw:param-get-scm-name param)))
         (list "gnc_gh_gint64_p(" x ")"))))

    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param)))
         (list
          c-name " = gnc_scm_to_gint64(" scm-name ");\n"))))

    (gw:type-set-call-ccodegen! wt standard-c-call-gen)
    
    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (list scm-name " = gnc_gint64_to_scm(" c-name ");\n"))))


;; until we have a case for g-chars-callee-owned as arg or
;; g-chars-caller-owned for result, then we don't need these, but
;; they're ready to go right now.

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; g-chars-caller-owned
   ;;   g_newed gchar* (string), caller-owned
   ;;
   ;; arg temps will be allocated with g_new before call, g_freed on return.
   ;; result temps will be g_freed after conversion to scheme.
   ;; 

   (let ((wt (gw:wrap-type mod '<glib:g-chars-caller-owned> 
                           "gchar *" "const gchar *")))

     (gw:type-set-scm-arg-type-test-ccodegen!
      wt
      (lambda (type param)
        (let ((x (gw:param-get-scm-name param)))
          (list "((" x " == SCM_BOOL_F) || SCM_STRINGP(" x "))"))))
    
     (gw:type-set-pre-call-arg-ccodegen!
      wt
      (lambda (type param)
        (let* ((scm-name (gw:param-get-scm-name param))
               (c-name (gw:param-get-c-name param)))
          (list
           "{\n"
           "  if(" scm-name " == SCM_BOOL_F) {\n"
           "    " c-name " = NULL;\n"
           "  } else {\n"
           "    char *tmpstr = gh_scm2newstr(" scm-name ", NULL);\n"
           "    " c-name " = g_strdup(tmpstr);\n"
           "    free(tmpstr);\n"
           "  }\n"
           "}\n"))))
    
     (gw:type-set-call-ccodegen! wt standard-c-call-gen)

     (gw:type-set-post-call-arg-ccodegen!
      wt
      (lambda (type param)
        (let* ((c-name (gw:param-get-c-name param)))
          (list "g_free((void *) " c-name ");\n"))))
   
     (add-standard-result-handlers!
      wt
      (lambda (scm-name c-name)
        (list
         scm-name " = ((" c-name ") ? gh_str02scm(" c-name ") : SCM_BOOL_F);\n"
         "g_free((void *) " c-name ");\n"))))

  (let ((wt (gw:wrap-type
             mod
             '<gnc:list-of-string>
             "GList *" "const GList *")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x) (list "gnc_glist_string_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x)
                 (list "gnc_scm_to_glist_string(" x ")"))))
         (list c-name
               " = "
               (old-func scm-name)
               ";\n"))))
    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_string_to_scm(" x ")"))))
         (list scm-name
               " = "
               (old-func c-name)
               ";\n")))))

   (gw:module-set-declarations-ccodegen!
    mod
    (lambda (client-only?)
      (list
       "#include <glib.h>\n"
       "#include \"glib-helpers.h\"\n"))))
   



