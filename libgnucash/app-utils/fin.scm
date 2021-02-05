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


;; Financial functions originally used by the mortgage/loan druid,
;; but useful in scheduled transactions
;;
;; Copyright 2002 Joshua Sled <jsled@asynchronous.org>
;; Update 2012 Frank H. Elenberger <frank.h.ellenberger@gmail.com>
;;

;; Simple function for testing:
(define (gnc:foobar val) val)

;; pretty literal copies of similar code from gnumeric-1.0.8, except we want
;; positive values to be returned (as gnucash will handle the credit/debit
;; appropriately)

;; interest payment amount:
(define (gnc:ipmt rate per nper pv fv type)
  (* -1 (* rate
           (- 0 (calc-principal pv
                                (calc-pmt rate nper pv fv type)
                                rate (- (if (> per nper) nper per) 1)))))
)

;; principal payment amount:
(define (gnc:ppmt rate per nper pv fv type)
  (let* ((pmt (calc-pmt rate nper pv fv type))
         (ipmt (gnc:ipmt rate per nper pv fv type)))
    (* -1 (- pmt (* -1 ipmt))))
)

;; payment amount:
(define (gnc:pmt rate nper pv fv type)
  (* -1 (calc-pmt rate nper pv fv type))
)

;; 2 functions from
;; https://lists.gnucash.org/pipermail/gnucash-user/2005-February/012964.html
;; future value of deposits with compound interests:
(define (gnc:futureValue a r n t)
    ;; Parameters:
    ;; a: amount
    ;; r: interest rate
    ;; n: frequency per year
    ;; t: time
    ;;
    ;; formula from https://www.riskglossary.com/articles/compounding.htm [DEAD LINK]
  (* a (expt (+ 1 (/ r n)) (* n t))))

(define (gnc:computeInterestIncrement pv ann-rate compounds period)
  (let ((rate (/ ann-rate compounds)))  
    (* rate (* pv (expt (+ 1 rate) (- period 1))))))

;;;;;
;; below: not-exposed/"private" functions, used by the "public" functions
;; above.
;;;;;

(define (calc-pmt rate nper pv fv type)
  (let ((pvif (calc-pvif rate nper))
        (fvifa (calc-fvifa rate nper)))
   (/ (- (* (- 0 pv) pvif) fv)
      (* fvifa
         (+ 1.0
            (* rate type)))))
)

(define (calc-pvif rate nper)
  (expt (+ 1 rate) nper)
)

(define (calc-fvifa rate nper)
  (/ (- (expt (+ 1 rate) nper) 1) rate)
)

(define (calc-principal pv pmt rate per)
  (+ (* pv (expt (+ 1.0 rate) per))
     (* pmt (/ (- (expt (+ 1 rate) per)
                  1)
               rate)))
)


;; This section added in 2005. Ludovic Nicolle
;; Formula to get the rate for a given period if there are yper in the year
;; And the official rate is compounded ycomp in the year.
;; For example, a mortgage being monthly has yper = 12
;; and if the posted rate is a plain annual rate, then ycomp = 1.
;; but if the posted rate is compounded semi-annually, as is the case in Canada,
;; then ycomp = 2.
;; this function can be used to enter the nominal rate in the formulas, without
;; pre-calculating the power function below.

(define (gnc:periodic_rate rate yper ycomp)
  (-  (expt (+ 1.0 (/ rate ycomp)) (/ ycomp yper) )  1.0)
)

;; the three following functions with prefix gnc:cpd_ are more generic
;; equivalents of gnc:pmt, gnc:ipmt and gnc:ppmt above, with some differences.
;; First difference is that they take the annual nominal rate and two yearly
;; frequencies: rate is annual, not per period (the functions calculate it
;; themselves) yfreq determines the compounding frequency of the paid/charged
;; interest ycomp determines the compounding frequency of the annual nominal
;; rate

;; Second difference is for rounding. My experience shows that all banks do not
;; use the exact same rounding parameters. Moreover, on top of that situation,
;; numerical calculations in gnucash using the original gnc:pmt, gnc:ipmt and
;; gnc:ppmt functions above can also create another set of rounding issues. Both
;; problems create the "odd-penny imbalance" problem.

;; So the gnc:cpd_Zpmt functions do automatic rounding, the goal being to have
;; PPMT = PMT - I holding true for all calculated numbers. However, this won't
;; fix the first problem if your bank can't do proper maths and manual fixing of
;; transactions will still be required.

;; FIXME: One problem with the rounding procedure in these three functions is
;; that it is always rounding at the second decimal. This works great with
;; dollars and euros and a lot of major currencies but might well cause issues
;; with other currencies not typically divided in 100.  I have not tested
;; anything else than dollars.

;; If the automatic rounding causes issues for a particular case, one can always
;; use the equivalence of the cpd_ and non-cpd_ functions, by using
;; periodic_rate() like this: gnc:cpd_pmt( rate:yfreq:ycomp :nper:pv:fv:type) is
;; equivalent to gnc:pmt(periodic_rate(rate:yfreq:ycomp):nper:pv:fv:type)

;; On the opposite side, if you want the automatic rounding but don't understand
;; how to use the cpd_ functions, here is a quick example on how to convert
;; original gnc:Zpmt function calls. The typical setup is to use 'rate/yfreq' as
;; the first parameter, so the solution is to simply use yfreq for both yfreq
;; and ycomp in the gnc:cpd_Zpmt calls, like this: gnc:pmt( rate / yfreq
;; :nper:pv:fv:type) is equivalent to gnc:cpd_pmt( rate:yfreq:yfreq
;; :nper:pv:fv:type)

(define (gnc:cpd_ipmt rate yfreq ycomp per nper pv fv type)
  (* 0.01
    (round
      (* -100 (* (gnc:periodic_rate rate yfreq ycomp)
           (- 0 (calc-principal pv
                                (calc-pmt (gnc:periodic_rate rate yfreq ycomp) nper pv fv type)
                                (gnc:periodic_rate rate yfreq ycomp) (- per 1))))
      )
    )
  )
)

(define (gnc:cpd_ppmt rate yfreq ycomp per nper pv fv type)
  (let* (
                (per_rate (gnc:periodic_rate rate yfreq ycomp))
                (pmt (* -1 (gnc:cpd_pmt rate yfreq ycomp nper pv fv type)))
                (ipmt (* per_rate (calc-principal pv pmt per_rate (- per 1))))
        )
        (
                * -1  (+ pmt ipmt)
        )
  )
)

(define (gnc:cpd_pmt rate yfreq ycomp nper pv fv type)
  (* 0.01
    (round
      (* -100
        (calc-pmt (gnc:periodic_rate rate yfreq ycomp) nper pv fv type)
      )
    )
  )
)

;; Further options to match what some (several? many?) lenders do (at
;;  least in Canada):
;; The posted interest rate is an annual rate that has a specified
;;  compounding frequency per year (2 for mortgages in Canada).
;; A payment frequency and amortization length are selected (e.g.
;;  monthly payments for 25 years).
;; The posted nominal rate is converted from the specified compounding
;;  frequency to the equivalent rate at the payment frequency.
;; The required payment is calculated.
;; The payment is rounded up to the next dollar (or $10 dollars,
;;  or whatever...)
;; Each payment period, interest is calculated on the outstanding
;;  balance.
;; The interest is rounded to the nearest cent and added to the
;;  balance.
;; The payment is subtracted from the balance.
;; The final payment will be smaller because all the other payments
;;  were rounded up.
;;
;; For the purpose of creating scheduled transactions that properly
;;  debit a source account while crediting the loan account and the
;;  interest expense account, the first part (the calculation of the
;;  required payment) doesn't really matter.  You have agreed
;;  with the lender what the payment terms (interest rate, payment
;;  frequency, payment amount) will be; you keep paying until the
;;  balance is zero.
;;
;; To create the scheduled transactions, we need to build an
;;  amortization table.
;; If it weren't for the rounding of the interest to the nearest cent
;;  each period, we could calculate the ith row of the amortization
;;  table directly from the general annuity equation (as is done by
;;  gnc:ipmt and gnc:ppmt).  But to deal with the intermediate
;;  rounding, the amortization table has to be constructed iteratively
;;  (as is done by the AMORT worksheet on the TI BA II Plus
;;  financial calculator).
;;
;; =================================
;; EXAMPLE:
;; Say you borrow $100,000 at 5%/yr, compounded semi-annually.
;; You amortize the loan over 2 years with 24 monthly payments.
;; This calls for payments of $4,384.8418 at the end of each month.
;; The lender rounds this up to $4,385.
;;
;; If you calculate the balance at each period directly using the annuity
;; formula (like calc-principal does), and then use the those values to calculate
;; the principal and interest paid, the first 10 rows of the amortization table
;; look like this (the values are rounded to the nearest cent for _display_, but
;; not for calculating the next period):
;;
;; PERIOD | Open       | Interest | Principal | End
;;    1   |$100,000.00 |  $412.39 | $3,972.61 | $96,027.39
;;    2   | $96,027.39 |  $396.01 | $3,988.99 | $92,038.40 
;;    3   | $92,038.40 |  $379.56 | $4,005.44 | $88,032.96 
;;    4   | $88,032.96 |  $363.04 | $4,021.96 | $84,011.00 
;;    5   | $84,011.00 |  $346.45 | $4,038.55 | $79,972.45 
;;    6   | $79,972.45 |  $329.80 | $4,055.20 | $75,917.25 
;;    7   | $75,917.25 |  $313.08 | $4,071.92 | $71,845.33 
;;    8   | $71,845.33 |  $296.28 | $4,088.72 | $67,756.61 
;;    9   | $67,756.61 |  $279.43 | $4,105.57 | $63,651.04 
;;   10   | $63,651.04 |  $262.49 | $4,122.51 | $59,528.53
;;
;; If you calculate each period sequentially (rounding the interest and balance
;; at each step), you get:
;;
;; PERIOD | Open       | Interest | Principal | End
;;    1   |$100,000.00 |  $412.39 | $3,972.61 | $96,027.39
;;    2   | $96,027.39 |  $396.01 | $3,988.99 | $92,038.40 
;;    3   | $92,038.40 |  $379.56 | $4,005.44 | $88,032.96 
;;    4   | $88,032.96 |  $363.04 | $4,021.96 | $84,011.00 
;;    5   | $84,011.00 |  $346.45 | $4,038.55 | $79,972.45 
;;    6   | $79,972.45 |  $329.80 | $4,055.20 | $75,917.25 
;;    7   | $75,917.25 |  $313.08 | $4,071.92 | $71,845.33 
;;    8   | $71,845.33 |  $296.28 | $4,088.72 | $67,756.61 
;;    9   | $67,756.61 |  $279.42 | $4,105.58 | $63,651.03 <- Different 
;;   10   | $63,651.03 |  $262.49 | $4,122.51 | $59,528.52 <- still $0.01 off
;;
;; =================================
;;
;; For the following functions the argument names are:
;;  py: payment frequency (number of payments per year)
;;  cy: compounding frequency of the nominal rate (per year)
;;  iy: nominal annual interest rate
;;  pv: the present value (opening balance)
;;  pmt: the size of the periodic payment
;;  n: the payment period we are asking about (the first payment is n=1)
;;  places: number of decimal places to round the interest amount to 
;;          at each payment (999 does no rounding)
;;
;; Note: only ordinary annuities are supported (payments at the end of
;;  each period, not at the beginning of each period)
;;
;; Unlike the AMORT worksheet on the BA II Plus, these methods will
;;  handle the smaller payment (bringing the balance to zero, then
;;  zeroing future payments)
;;
;; The present value (pv) must be non-negative.  If not, the balance will be
;;  treated as 0.
;; The payment (pmt) can be positive (paying interest, and hopefully
;;  reducing the balance each payment), or negative (increasing the balance
;   each payment).
;; The payment number (n) must be positive for amort_pmt, amort_ppmt, and
;;  amort_ipmt.  I.e., the first payment is payment 1.
;; The payment number (n) must be non-negative for amort_balance.  (In this
;;  case, payment zero is at the _beginning_ of the first period, so
;;  amort_balance will just be the initial balance.)
;; If the above conditions on n are violated, the functions returns #f
;;
;; A negative interest rate works (if you can find a lender who charges
;;  negative rates), but negative compounding frequency, or negative payment
;;  frequency is a bad idea.

;; Calculate the balance remaining after the nth payment
;; (n must be greater than or equal to zero)
(define (gnc:amort_balance py cy iy pv pmt n places)
  (cond
    ((< pv 0) 0)
    ((< n 0) #f)
    ((and (zero? pv) (>= pmt 0)) 0)
    ((zero? n) pv)
    (else
      (let* ((bal-after-int (amort_balanceAfterInterest pv py cy iy places))
            (bal-after-pmt (amort_balanceAfterPayment bal-after-int pmt)))
        (gnc:amort_balance py cy iy bal-after-pmt pmt (- n 1) places)))))

;; Calculate the size of the nth payment
;; (it will just be pmt for all the payments before the final payment,
;; then less,
;; then zero if you keep trying to make payments)
;; (n must be greater than zero)
(define (gnc:amort_pmt py cy iy pv pmt n places)
  (and (>= n 1)
       (let* ((prevBal (gnc:amort_balance py cy iy pv pmt (- n 1) places))
              (balBeforePayment (amort_balanceAfterInterest prevBal py cy iy places))
              (balAfterPayment (amort_balanceAfterPayment balBeforePayment pmt)))
         (- balBeforePayment balAfterPayment))))

;; Calculate the amount of the nth payment that is principal
;; (n must be greater than zero)
(define (gnc:amort_ppmt py cy iy pv pmt n places)
  (and (>= n 1)
       (let* ((prevBal (gnc:amort_balance py cy iy pv pmt (- n 1) places))
              (bal-after-int (amort_balanceAfterInterest prevBal py cy iy places))
              (newBal (amort_balanceAfterPayment bal-after-int pmt)))
         (- prevBal newBal))))

;; Calculate the amount of the nth payment that is interest
;; (n must be greater than zero)
(define (gnc:amort_ipmt py cy iy pv pmt n places)
  (and (>= n 1)
       (amort_interest (gnc:amort_balance py cy iy pv pmt (- n 1) places)
                       py cy iy places)))

;; "Private" helper functions:

;; Calculate the amount of interest on the current balance,
;; rounded to the specified number of decimal places
(define (amort_interest balance py cy iy places)
  (roundToPlaces (* balance (gnc:periodic_rate iy py cy)) places)
)

;; Calculate the new balance after applying the interest, but before
;; applying the payment
(define (amort_balanceAfterInterest prevBalance py cy iy places)
  (+ prevBalance (amort_interest prevBalance py cy iy places))
)

;; Apply the payment to the balance (after the interest has been
;; added), without letting the balance go below zero.
(define (amort_balanceAfterPayment balanceBeforePmt pmt)
  (max 0 (- balanceBeforePmt pmt))
)

;; Round the value to the specified number of decimal places.
;; 999 places means no rounding (#f is not used, because only numbers can be
;; entered in the scheduled transaction editor)
(define (roundToPlaces value places)
  (if (= places 999) value
    (/ (round (* value (expt 10 places))) (expt 10 places))
  )
)
