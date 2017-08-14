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

;; 2 functions from http://lists.gnucash.org/pipermail/gnucash-user/2005-February/012964.html
;; future value of deposits with compound interests:
(define (gnc:futureValue a r n t)
    ;; Parameters:
    ;; a: amount
    ;; r: interest rate
    ;; n: frequency per year
    ;; t: time
    ;;
    ;; formula from http://www.riskglossary.com/articles/compounding.htm
  (* a (expt (+ 1 (/ r n)) (* n t))))

(define (gnc:computeInterestIncrement amount interest periods i)
  (let ((thisVal (gnc:futureValue amount interest periods i))
        (prevVal (gnc:futureValue amount interest periods (- i 1))))
    (- thisVal prevVal)
  )
)

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

;; the three following functions with prefix gnc:cpd_ are more generic equivalents of 
;; gnc:pmt, gnc:ipmt and gnc:ppmt above, with some differences. 
;; First difference is that they take the annual nominal rate and two yearly frequencies:
;; rate is annual, not per period (the functions calculate it themselves)
;; yfreq determines the compounding frequency of the payed/charged interest
;; ycomp determines the compounding frequency of the annual nominal rate

;; Second difference is for rounding. My experience shows that all banks do not use
;; the exact same rounding parameters. Moreover, on top of that situation, numerical calculations 
;; in gnucash using the original gnc:pmt, gnc:ipmt and gnc:ppmt functions above can also 
;; create another set of rounding issues. Both problems create the "odd-penny imbalance" problem.

;; So the gnc:cpd_Zpmt functions do automatic rounding, the goal being to have PPMT = PMT - I 
;; holding true for all calculated numbers. However, this won't fix the first problem if your bank
;; can't do proper maths and manual fixing of transactions will still be required.

;; FIXME: One problem with the rounding procedure in these three functions is that it is always 
;; rounding at the second decimal. This works great with dollars and euros and a lot of major 
;; currencies but might well cause issues with other currencies not typically divided in 100. 
;; I have not tested anything else than dollars.

;; If the automatic rounding causes issues for a particular case, one can always use the
;; equivalence of the cpd_ and non-cpd_ functions, by using periodic_rate() like this:
;;                     gnc:cpd_pmt(              rate:yfreq:ycomp :nper:pv:fv:type) 
;;   is equivalent to      gnc:pmt(periodic_rate(rate:yfreq:ycomp):nper:pv:fv:type)

;; On the opposite side, if you want the automatic rounding but don't understand how to use
;; the cpd_ functions, here is a quick example on how to convert original gnc:Zpmt
;; function calls. The typical setup is to use 'rate/yfreq' as the first parameter, so the 
;; solution is to simply use yfreq for both yfreq and ycomp in the gnc:cpd_Zpmt calls, like this:
;;                         gnc:pmt( rate  /  yfreq   :nper:pv:fv:type)
;; is equivalent to    gnc:cpd_pmt( rate:yfreq:yfreq :nper:pv:fv:type)

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
