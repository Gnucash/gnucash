;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-merge-groups.scm
;;;  eliminate duplicate xtns in a new (imported) account group
;;;
;;;  Copyright 2001 Bill Gribble <grib@billgribble.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:account-tree-find-duplicates
;;
;;  Given two account trees, old-root and new-root, a search is
;;  performed to determine, for each transaction in new-root,
;;  whether there are any transactions in old-root that may be
;;  duplicated by it.
;;
;;  The search results are returned in an association list, with
;;  new-root transactions as the keys. The value associated with
;;  each key is a second association list of possibly duplicated
;;  transactions in the old-root, taking the form:
;;  ( (old-xtn . #f) (old-xtn . #f) (old-xtn . #f) ... )
;;
;;  The druid can then ask the user for a final determination,
;;  and change #f to #t where duplication is found.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:account-tree-find-duplicates old-root new-root progress-dialog)
  (define old-accounts (gnc-account-get-descendants-sorted old-root))
  (define (progress v)
    (when progress-dialog (gnc-progress-dialog-set-value progress-dialog v)))

  ;; This procedure does all the work. We'll define it, then call it safely.
  (define (private-find)
    (cond
     ((any (compose pair? xaccAccountGetSplitList) old-accounts)
      ;; Get all the splits in the new tree, then iterate over them
      ;; trying to find matches in the old tree.  If there are
      ;; matches, push the splits' parent onto a list.
      (let ((WeekSecs (* 60 60 24 7)))

        (define new-splits
          (let ((q (qof-query-create-for-splits))
                (accounts (gnc-account-get-descendants-sorted new-root)))
            (qof-query-set-book q (gnc-account-get-book new-root))
            (xaccQueryAddAccountMatch q accounts QOF-GUID-MATCH-ANY QOF-QUERY-AND)
            (let ((new-splits (qof-query-run q)))
              (qof-query-destroy q)
              new-splits)))

        (define old-splits
          (let ((q (qof-query-create-for-splits))
                (dates (map (compose xaccTransGetDate xaccSplitGetParent) new-splits)))
            (qof-query-set-book q (gnc-account-get-book old-root))
            (xaccQueryAddAccountMatch q old-accounts QOF-GUID-MATCH-ANY QOF-QUERY-AND)
            (xaccQueryAddDateMatchTT q
                                     #t (decdate (apply min dates) WeekDelta)
                                     #t (incdate (apply max dates) WeekDelta)
                                     QOF-QUERY-AND)
            (let ((splits (qof-query-run q)))
              (qof-query-destroy q)
              splits)))

        (define work-to-do (length new-splits))
        (define (update-progress work-done)
          (when (and progress-dialog (zero? (modulo work-done 8)))
            (progress (/ work-done work-to-do))
            (qif-import:check-pause progress-dialog)
            (if qif-import:canceled (throw 'cancel))))

        (when progress-dialog
          (gnc-progress-dialog-set-sub progress-dialog
                                       (_ "Finding duplicate transactions")))

        (let loop ((new-splits new-splits)
                   (work-done 0)
                   (matches '()))
          (cond
           ((null? new-splits)
            (progress 1)
            matches)

           ((assoc (xaccSplitGetParent (car new-splits)) matches)
            ;; txn has already been matched, by another split within same txn
            (loop (cdr new-splits)
                  (1+ work-done)
                  matches))

           (else
            (let* ((new-split (car new-splits))
                   (candidate-old-splits
                    (filter
                     (lambda (old-split)
                       (and
                        ;; split value matches
                        (= (xaccSplitGetValue old-split)
                           (xaccSplitGetValue new-split))
                        ;; account name matches
                        (string=?
                         (gnc-account-get-full-name (xaccSplitGetAccount old-split))
                         (gnc-account-get-full-name (xaccSplitGetAccount new-split)))
                        ;; maximum 1 week date difference
                        (<= (abs (- (xaccTransGetDate (xaccSplitGetParent old-split))
                                    (xaccTransGetDate (xaccSplitGetParent new-split))))
                            WeekSecs)))
                     old-splits)))
              (update-progress work-done)
              (loop (cdr new-splits)
                    (1+ work-done)
                    (if (null? candidate-old-splits)
                        matches
                        (cons (cons (xaccSplitGetParent new-split)
                                    (map (lambda (s) (cons (xaccSplitGetParent s) #f))
                                         candidate-old-splits))
                              matches)))))))))

     ;; Since there are either no accounts or no transactions in the old
     ;; tree, duplicate checking is unnecessary.
     (else
      (progress 1)
      '())))

  ;; Safely do the work and return the result.
  (gnc:backtrace-if-exception
   (lambda () (catch 'cancel private-find (const #t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:prune-matching-transactions
;;
;;  The parameter, match-list, is an association list of the form
;;  returned by gnc:account-tree-find-duplicates. This procedure
;;  looks through the list and discards any transaction that has
;;  been definitively determined to be a duplicate of one of the
;;  possible matches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:prune-matching-transactions match-list)
  (for-each
   (lambda (txn-match)
     (let ((new-xtn (car txn-match))
           (matches (cdr txn-match)))
       (when (any cdr matches)
         (xaccTransBeginEdit new-xtn)
         (xaccTransDestroy new-xtn)
         (xaccTransCommitEdit new-xtn))))
   match-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:account-tree-catenate-and-merge
;;
;;  The procedure moves the entire contents of one account tree,
;;  new-root, to a second account tree, old-root, and merges any
;;  duplicated accounts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (gnc:account-tree-catenate-and-merge old-root new-root)
  ;; stuff the new accounts into the old account tree and merge the accounts
  (gnc-account-join-children old-root new-root)
  (xaccAccountBeginEdit new-root)
  (xaccAccountDestroy new-root)
  (gnc-account-merge-children old-root))
