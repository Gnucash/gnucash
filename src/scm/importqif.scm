;;; Parse QIF
(gnc:load "sstring-qif.scm")
(gnc:load "dates-qif.scm")
(gnc:load "split-qif.scm")
(gnc:load "parseqif.scm")
(gnc:load "guess-category-qif.scm")
(gnc:load "analytical-qifs.scm")
(gnc:load "gc-import-qifs.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Now, let's actually execute the code...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(for-each process-possible-qif-file indir)

;;;;; Open Issues:
;;;;;
;;;;; - What account do we load into?
;;;;;   1.  Hopefully this can be determined in an implicit manner...
;;;;;   2.  The alternative is that something interactive must be done for
;;;;;   a group of transactions, querying the user to select the appropriate
;;;;;   account.
;;;;;
;;;;; - What to do with transfers?
;;;;;
;;;;;   A transaction where the category is [AA Chequing] or [ACM MasterCard]
;;;;;   is obviously a transfer to/from that account.  Unfortunately, there is
;;;;;   no guarantee that an account by the same exact name exists in GnuCash.
;;;;;   Probably ought to cons up a list of categories, agree them to GnuCash,
;;;;;   and, most crucially, construct a "category translation table"
;;;;;   to indicate what to do with them.
;;;;;
;;;;;   The same is true, albeit less critical, for income/expense categories.
;;;;;
;;;;; - Further transfer issue:
;;;;;
;;;;;   Note that a QIF load may provide duplicate transactions for transfers,
;;;;;   once you load in the amounts for both sides of the transaction.
;;;;;
;;;;; - Category management:
;;;;;
;;;;;   What should we do if there are categories in the QIF file that don't
;;;;;   exist in GnuCash?  Create the new category, maybehaps, but probably
;;;;;   by collecting up a list, and giving the option of converting QIF
;;;;;   categories to "something new."   Again, reference back to the
;;;;;   "category translation table"
