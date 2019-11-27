(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-64))
(use-modules (tests test-engine-extras))
(use-modules (tests srfi64-extras))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "Testing/Temporary/test-report")
  ;; if (test-runner-factory gnc:test-runner) is commented out, this
  ;; will create Testing/Temporary/test-asset-performance.log
  (test-check1)
  (test-check2)
  (test-check3)
  (test-check4)
  (test-report-template-getters)
  (test-make-report)
  (test-report)
  (test-end "Testing/Temporary/test-report"))

(define test4-guid "54c2fc051af64a08ba2334c2e9179e24")
;; -----------------------------------------------------------------------

(define (test-check1)
  (display "\n*** Minimum Report Definition\n")
  (gnc:define-report 'version "1"
                     'name "Test Report Template"
                     'report-guid "54c2fc051af64a08ba2334c2e9179e23")
  (test-equal "1 report successfully defined"
    1
    (length (gnc:all-report-template-guids))))

;; -----------------------------------------------------------------------

(define (test-check2)
  ; the parent type is set to test unique report names later on
  (display "\n*** Duplicate name, parent type of pre-existing report:\n")
  (gnc:define-report 'version "1"
                     'name "Test Report Template"
                     'report-guid "54c2fc051af64a08ba2334c2e9179e25"
                     'parent-type "54c2fc051af64a08ba2334c2e9179e23")
  (test-equal "2 reports defined, with same report name"
    2
    (length (gnc:all-report-template-guids))))

;; -----------------------------------------------------------------------

(define (test-check3)
  (display "\n*** Detect double GUID\n")
  (gnc:define-report 'version "1"
                     'name "Test Report Template"
                     'report-guid "54c2fc051af64a08ba2334c2e9179e23"
                     'parent-type "Parent Type"
                     'options-generator "Options Generator"
                     'renderer "Renderer"
                     'options-cleanup-cb "Options Clean-Up"
                     'options-changed-cb "Options Changed"
                     'in-menu? #f
                     'menu-path "Menu Path"
                     'menu-name "Menu Name"
                     'menu-tip "Menu Tip"
                     'export-types "Export Types"
                     'export-thunk "Export Thunk")
  (test-equal "still only 2 reports defined"
    2
    (length (gnc:all-report-template-guids))))

;; -----------------------------------------------------------------------

(define (test-check4)
  (display "\n*** Report with Full Argument Set\n")
  (let ((guid test4-guid))
    (gnc:define-report 'version "1"
                       'name "Test Report Template"
                       'report-guid guid
                       'parent-type "Parent Type"
                       'options-generator "Options Generator"
                       'renderer "Renderer"
                       'options-cleanup-cb "Options Clean-Up"
                       'options-changed-cb "Options Changed"
                       'in-menu? #f
                       'menu-path "Menu Path"
                       'menu-name "Menu Name"
                       'menu-tip "Menu Tip"
                       'export-types "Export Types"
                       'export-thunk "Export Thunk")
    (let ((tmpl (gnc:find-report-template guid)))
      (test-assert "report properties correctly set"
        (and
         (string=? (gnc:report-template-version tmpl) "1")
         (string=? (gnc:report-template-name tmpl) "Test Report Template")
         (string=? (gnc:report-template-report-guid tmpl) guid)
         ;; parent type is not exported -> it is used in gnc:make-report
         (string=? (gnc:report-template-options-generator tmpl) "Options Generator")
         (string=? (gnc:report-template-renderer tmpl) "Renderer")
         (string=? (gnc:report-template-options-cleanup-cb tmpl) "Options Clean-Up")
         (string=? (gnc:report-template-options-changed-cb tmpl) "Options Changed")
         (not (gnc:report-template-in-menu? tmpl))
         (string=? (gnc:report-template-menu-path tmpl) "Menu Path")
         (string=? (gnc:report-template-menu-name tmpl) "Menu Name")
         (string=? (gnc:report-template-menu-tip tmpl) "Menu Tip")
         (string=? (gnc:report-template-export-types tmpl) "Export Types")
         (string=? (gnc:report-template-export-thunk tmpl) "Export Thunk"))))))

(define (test-report-template-getters)
  (define test4-name "Test Report Template")
  (test-begin "test-report-template-getters")
  (test-assert "gnc:report-template-new-options/report-guid"
    (procedure?
     (gnc:report-template-new-options/report-guid test4-guid test4-name)))
  (test-equal "gnc:report-template-menu-name/report-guid"
    "Menu Name"
    (gnc:report-template-menu-name/report-guid test4-guid test4-name))
  (test-equal "gnc:report-template-renderer/report-guid"
    "Renderer"
    (gnc:report-template-renderer/report-guid test4-guid test4-name))
  (test-assert "gnc:report-template-new-options"
    (procedure?
     (gnc:report-template-new-options (gnc:find-report-template test4-guid))))
  (test-end "test-report-template-getters"))

(define (test-make-report)
  (define test4-name "Test Report Template")
  (test-begin "test-make-report")
  (test-assert "gnc:make-report succeeds"
    (gnc:make-report test4-guid))
  (test-equal "gnc:restore-report-by-guid"
    1
    (gnc:restore-report-by-guid 1 test4-guid test4-name "options"))
  (test-assert "gnc:restore-report-by-guid, no options"
    (not (gnc:restore-report-by-guid 1 test4-guid test4-name #f)))
  (test-equal "gnc:restore-report-by-guid-with-custom-template"
    2
    (gnc:restore-report-by-guid-with-custom-template
     "id" test4-guid test4-name "custom-template-id" "options"))
  (test-assert "gnc:restore-report-by-guid-with-custom-template, no options"
    (not
     (gnc:restore-report-by-guid-with-custom-template
      "id" test4-guid test4-name "custom-template-id" #f)))
  (test-assert "gnc:make-report-options"
    (procedure?
     (gnc:make-report-options test4-guid)))
  (test-end "test-make-report"))

(define (test-report)
  (define test-uuid "basic-report-guid")
  (gnc:define-report
   'version 1
   'name "basic report"
   'report-guid test-uuid
   'options-generator gnc:new-options
   'export-types (list (cons "text" 'txt))
   'export-thunk (lambda (report-obj export-type file-name)
                   "exported-string")
   'renderer (lambda (obj)
               (let ((options (gnc:report-options obj)))
                 "return-string")))
  (let* ((template (gnc:find-report-template test-uuid))
         (constructor (record-constructor <report>))
         (options (gnc:make-report-options test-uuid))
         (report (constructor test-uuid "bar" options #t #t #f #f "")))
    (test-equal "render works"
      "return-string"
      ((gnc:report-template-renderer template) report))
    (test-equal "gnc:report-export-types"
      '(("text" . txt))
      (gnc:report-export-types report))
    (test-equal "gnc:report-export-thunk"
      "exported-string"
      ((gnc:report-export-thunk report) report 'csv "/tmp/file.txt"))
    (test-equal "gnc:report-menu-name"
      "basic report"
      (gnc:report-menu-name report))
    (test-equal "gnc:report-name"
      "basic report"
      (gnc:report-name report))
    (test-equal "gnc:report-stylesheet"
      #f
      (gnc:report-stylesheet report))
    (test-equal "(gnc:all-report-template-guids)"
      4
      (length (gnc:all-report-template-guids)))
    (test-equal "(gnc:custom-report-template-guids)"
      2
      (length (gnc:custom-report-template-guids)))
    (test-assert "(gnc:find-report-template report-type)"
      (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24"))
    (test-assert "gnc:report-template-is-custom/template-guid? #t"
      (gnc:report-template-is-custom/template-guid?
       "54c2fc051af64a08ba2334c2e9179e24"))
    (test-assert "gnc:report-template-is-custom/template-guid? #f"
      (not
       (gnc:report-template-is-custom/template-guid?
        "54c2fc051af64a08ba2334c2e9179e23")))
    (test-equal "(gnc:is-custom-report-type report) #f"
      #f
      (gnc:is-custom-report-type report))
    (test-assert "gnc:report-template-has-unique-name? #t"
      (gnc:report-template-has-unique-name?
       "54c2fc051af64a08ba2334c2e9179e24"
       "Test Report Templatx"))
    (test-assert "gnc:report-template-has-unique-name? #f"
      (not
       (gnc:report-template-has-unique-name?
        "54c2fc051af64a08ba2334c2e9179e24"
        "Test Report Template")))
    (test-assert "gnc:report-template-has-unique-name? #t"
      (gnc:report-template-has-unique-name?
       "54c2fc051af64a08ba2334c2e9179e24"
       #f))
    (test-assert "gnc:report-serialize = string"
      (string?
       (gnc:report-serialize report)))))
