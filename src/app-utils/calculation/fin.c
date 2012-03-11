/***************************************************************************
                          fin.c  -  description
                             -------------------
    begin                : Thursday June 15 2000
    email                : tboldt@attglobal.net
    Author               : Terry D. Boldt
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

/*
 *  Functions to compute financial equations and amortization schedules
 *  6-15-2000
 *
 */

/*
 * Financial Calculator
 *
 * This version for use WITH ANSI.SYS display driver
 *
 *  This is a complete financial computation utility to solve for the
 *  five * standard financial values: n, %i, PV, PMT and FV
 *
 *    n   == number of payment periods
 *    %i  == nominal interest rate, NAR, charged
 *    PV  == Present Value
 *    PMT == Periodic Payment
 *    FV  == Future Value
 *
 *  In addition, two additional parameters may be specified:
 *
 *    1) Compounding Frequency per year, CF. The compounding frequency
 *    per year may be discrete or continuous and may be different from
 *    the Payment Frequency per year
 *
 *    2) Payment Frequency per year, PF. Payments may be made at the
 *    beginning or the end of the payment period.
 *
 *  When an amortization schedule is desired, the financial
 *  transaction Effective Date, ED, and Initial Payment Date, IP, must
 *  also be entered.
 *
 *  Canadian and European style mortgages can be handled in a simple,
 *  straight-forward manner. Standard financial sign conventions are
 *  used:
 *
 *          "Money paid out is Negative, Money received is Positive"
 *
 *  Time value of money:
 *
 *  If you borrow money, you can expect  to pay rent or interest for its  use;
 * conversely you expect to receive rent interest on money you loan or invest.
 * When you rent property, equipment,  etc., rental payments are normal;  this
 * is  also  true  when  renting  or  borrowing  money.  Therefore,  money  is
 * considered to have a "time value". Money available now, has a greater value
 * than money available at some future date because of its rental value or the
 * interest that it can produce during the intervening period.
 *
 *  Simple Interest:
 *
 *  If you loaned $800 to  a friend with an agreement  that at the end of  one
 * year he would  would repay you  $896, the "time  value" you placed  on your
 * $800 (principal) was $96 (interest) for  the one year period (term) of  the
 * loan. This  relationship of  principal, interest,  and time (term) is  most
 * frequently expressed as an Annual  Percentage Rate (APR). In this  case the
 * APR  was  12.0%  [(96/800)*100].  This  example  illustrates the four basic
 * factors involved  in a  simple interest  case. The  time period (one year),
 * rate (12.0%  APR), present  value of  the principal  ($800) and  the future
 * value of the principal including interest ($896).
 *
 *  Compound Interest:
 *
 *  In many cases the interest charge is computed periodically during the term
 * of  the  agreement.  For  example,  money  left  in a savings account earns
 * interest that  is periodically  added to  the principal  and in  turn earns
 * additional interest during succeeding periods. The accumulation of interest
 * during  the  investment  period  represents  compound interest. If the loan
 * agreement you  made with  your friend  had specified  a "compound  interest
 * rate" of  12% (compounded  monthly) the  $800 principal  would have  earned
 * $101.46 interest for the  one year period. The  value of the original  $800
 * would be increased  by 1% the  first month to  $808 which in  turn would be
 * increased  by  1%  to  816.08 the second month,  reaching a future value of
 * $901.46 after the twelfth iteration. The monthly compounding of the nominal
 * annual rate (NAR) of 12% produces an effective Annual Percentage Rate (APR)
 * of 12.683% [(101.46/800)*100].  Interest may be  compounded at any  regular
 * interval; annually, semiannually, monthly, weekly, daily, even continuously
 * (a specification in some financial models).
 *
 *  Periodic Payments:
 *
 *  When money is loaned for longer  periods of time, it is customary  for the
 * agreement to require the borrower  to make periodic payments to  the lender
 * during the term of the loan. The payments may be only large enough to repay
 * the interest,  with the  principal due  at the  end of  the loan period (an
 * interest only loan), or large enough  to fully repay both the interest  and
 * principal during the term of the loan (a fully amoritized loan). Many loans
 * fall somewhere between, with payments that do not fully cover repayment  of
 * both the principal and interst. These loans require a larger final  payment
 * (balloon)  to  complete  their  amortization.  Payments  may  occur  at the
 * beginning or end of a payment period. If you and your friend had agreed  on
 * monthly repayment of  the $800 loan  at 12% NAR  compounded monthly, twelve
 * payments of $71.08 for a total of $852.96 would be required to amortize the
 * loan. The $101.46  interest from the  annual plan is  more than the  $52.96
 * under the monthly plan because under the monthly plan your friend would not
 * have had the use of $800 for a full year.
 *
 *  Financial Transactions:
 *
 *  The  above  paragraphs  introduce  the  basic  factors  that  govern  most
 * financial  transactions;  the  time  period,  interest rate, present value,
 * payments and  the future  value. In  addition, certain  conventions must be
 * adhered to: the interest rate must be relative to the compounding frequency
 * and payment periods, and the term must be expressed as the total number  of
 * payments (or compounding periods if there are no payments). Loans,  leases,
 * mortgages, annuities, savings plans, appreciation, and compound growth  are
 * amoung the many financial problems that can be defined in these terms. Some
 * transactions do not involve payments, but  all of the other factors play  a
 * part in "time value of money" transactions. When any one of the five  (four
 * - if no payments are involved)  factors is unknown, it can be  derived from
 * formulas using the known factors.
 *
 * Standard Financial Conventions Are:
 *
 *  Money RECEIVED is a POSITIVE value and is represented by an arrow
 *  above * the line
 *
 *  Money PAID OUT is a NEGATIVE value and is represented by an arrow
 *  below * the line.
 *
 *  If payments are a part of the transaction, the number of payments
 *  must * equal the number of periods (n).
 *
 *  Payments may be represented as occuring at the end or beginning of
 *  the * periods.
 *
 *  Diagram to visualize the positive and negative cash flows (cash
 *  flow * diagrams):
 *
 *  Amounts shown above the line are positve, received, and amounts
 *  shown below the line are negative, paid out.
 *
 * 1)
 *                                                                 FV*
 *          1   2   3   4   .   .   .   .   .   .   .   .   .   n ³
 * Period ÚÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÙ
 *        ³
 *
 *        PV
 *
 *     Appreciation
 *     Depreciation
 *     Compound Growth
 *     Savings Account
 *
 * ****************************************************************************
 *
 * 2)                                                               FV
 *     PV = 0
 *                                                                ³
 * Period ÚÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÙ
 *        ³ 1 ³ 2 ³ 3 ³ 4 ³ . ³ . ³ . ³ . ³ . ³ . ³ . ³ . ³ . ³ n
 *
 *       PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT
 *
 *     Annuity (series of payments)
 *     Pension Fund
 *     Savings Plan
 *     Sinking Fund
 *
 * ****************************************************************************
 *
 * 3)
 *     PV
 *        ³                                                      FV=0
 * Period ÀÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄ¿
 *          1 ³ 2 ³ 3 ³ 4 ³ . ³ . ³ . ³ . ³ . ³ . ³ . ³ . ³ . ³ n ³
 *
 *           PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT
 *
 *     Amortization
 *     Direct Reduction Loan
 *     Mortgage (fully amortized)
 *
 * ****************************************************************************
 *
 * 4)
 *                                                                 FV*
 *           PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT PMT  ³ +
 *                                                    PMT
 *          1 ³ 2 ³ 3 ³ 4 ³ . ³ . ³ . ³ . ³ . ³ . ³ . ³ . ³ . ³ n ³
 * Period ÚÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÙ
 *        ³
 *
 *        PV
 *
 *     Annuity
 *     Lease (with buy back or residual)*
 *     Loan or Mortgage (with balloon)*
 *
 * ****************************************************************************
 *
 *   First lets discuss interest before discussing the financial
 *   equation. Most financial transactions utilize a nominal interest
 *   rate, NAR, i.e., the interest rate per year. The NAR must be
 *   converted to the interest rate per payment interval and the
 *   compounding accounted for before it can be used in computing an
 *   interest payment. After this conversion process, the interest
 *   used is the effective interest rate, EIR. In converting NAR to
 *   EIR, there are two concepts to discuss first, the Compounding
 *   Frequency and the Payment Frequency and * whether the interest is
 *   coumpounded in discrete intervals or continuously. The
 *   compounding Frequency, CF, is simply the number of times per
 *   year, the monies in the financial transaction are compounded. In
 *   the U.S., monies are usually compounded daily on bank deposits,
 *   and monthly on loans. Somtimes Long term deposits are compounded
 *   quarterly or weekly.
 *
 *   The Payment Frequency, PF, is simply how often during a year
 *   payments are made in the transaction. Payments are usually
 *   scheduled on a regular basis and can be made at the beginning or
 *   end of the payment period. If made at the beginning of the
 *   payment period, interest must be applied to the payment as well
 *   as any previous money paid or money still owed.
 *
 *   Normal values for CF and PF are:
 *   1   == annual
 *   2   == semi-annual
 *   3   == tri-annual
 *   4   == quaterly
 *   6   == bi-monthly
 *   12  == monthly
 *   24  == semi-monthly
 *   26  == bi-weekly
 *   52  == weekly
 *   360 == daily
 *   365 == daily
 *
 *  a) the Compounding Frequency per year, CF, need not be identical
 *  to the Payment Frequency per year, PF, and/or,
 *
 *  b) Interest may be compounded in either discrete intervals or continuously
 *     compounded.
 *
 *  c) Also, payments may be made at the beginning of the payment
 *  period or at the end of the payment period.
 *
 *  CF and PF are defaulted to 1. The default is for discrete interest
 *  intervals and payments are defaulted to the end of the payment
 *  period.
 *
 *  When a solution  for n, PV,  PMT or FV  is required, the  nominal interest
 * rate, i, must first be converted to the effective interest rate per payment
 * period. This rate, ieff, is then used to compute the selected variable.  To
 * convert i to ieff, the following expressions are used:
 *
 *  Discrete interest periods:
 *
 * 19) ieff = (1 + i/CF)^(CF/PF) - 1
 *
 *  Continuous Interest
 *
 * 20) ieff = e^(i/PF) - 1 = exp(i/PF) - 1
 *
 *  When interest is computed, the computation produces the effective interest
 * rate, ieff. This value must then be converted to the nominal interest rate.
 * Function  _I  below  returns  the  nominal  interest rate NOT the effective
 * interest rate. ieff is converted to i using the following expressions:
 *
 *  Discrete Case:
 *
 *     i = CF*[(1+ieff)^(PF/CF) - 1]
 *
 *  Continuous Case:
 *
 *     i = ln[(1+ieff)^PF]
 *
 * ****************************************************************************
 *
 *   NOTE: in the equations below for the financial transaction, all
 *   interest rates are the effective interest rate, ieff. The symbol
 *   will be shortned to just 'i'.
 *
 * ****************************************************************************
 *
 * The basic financial equation used is:
 *
 *  1) PV*(1 + i)^n + PMT*(1 + iX)*[(1+i)^n - 1]/i + FV = 0
 *   Where: X = 0 for end of period payments, and
 *          X = 1 for beginning of period payments
 *
 * ****************************************************************************
 *
 *   NOTE: this equation is derived in the following manner:
 *
 *   Start with the basic equation to find the balance or Present
 *   Value, PV[1], after one payment period. Note PV[1] is the Present
 *   value after on payment and PV[0] is the initial Present
 *   Value. PV[0] will be shortened to just PV.
 *
 *   The interest due at the end of the first payment period is:
 *
 *   ID[1] = (PV + X * PMT) * i
 *       where: X = 0 for end of period payments, and
 *              X = 1 for beginning of period payments.
 *
 *   Thus:
 *   PV[1] = PV + (PMT + ID[1])
 *         = PV + (PMT + (PV + X * PMT) * i)
 *         = PV * (1 + i) + PMT * (1 + Xi)
 *
 *   This equation works for all of the money diagrams shown
 *   above. The Present Value, money received or paid, is modified by
 *   a payment made at the beginning of a payment period and
 *   multiplied by the effective interest rate to compute the interest
 *   due during the payment period. The interest due is then added to
 *   the payment to obtain the amount to be added to the Present Value
 *   to compute the new Present Value.
 *
 *   For diagram 1): PV <  0, PMT == 0, PV[1] < 0
 *   For diagram 2): PV == 0, PMT <  0, PV[1] < 0
 *   For Diagram 3): PV >  0, PMT <  0, PV[1] >= 0 or PV[1] <= 0
 *   For Diagram 4): PV <  0, PMT >  0, PV[1] <= 0 or PV[1] >= 0
 *
 *   X may be 0 or 1 for any diagram.
 *
 *   For the standard loan, PV is the money borrowed, PMT is the
 *   periodic payment to repay the loan and i is the effective
 *   interest rate agreed upon.
 *
 *   To calculate the Present Value after the second payment period,
 *   the above calculation is applied iteratively to PV_1:
 *
 *   PV[2] = PV[1] + (PMT + (PV[1] + X * PMT) * i)
 *         = PV[1] * (1 + i) + PMT * (1 + iX)
 *         = (PV * (1 + i) + PMT * (1 + iX)) * (1 + i) + PMT * (1 + iX)
 *         = PV * (1 + i)^2 + PMT * (1 + iX) * (1 + i)
 *                          + PMT * (1 + iX)
 *
 *   Similarly:
 *
 *   PV[3] = PV[2] + (PMT + (PV[2] + X * PMT) * i)
 *         = PV[2] * (1 + i) + PMT * (1 + iX)
 *         = PV * (1 + i)^2 + PMT * (1 + iX) * (1 + i)
 *                          + PMT * (1+  iX)) * ( 1 + i)
 *                          + PMT * (1+  iX)
 *         = PV * (1 + i)^3 + PMT * (1 + iX) * (1 + i)^2
 *                          + PMT * (1 + iX) * (1 + i)^2
 *                          + PMT * (1 + iX) * (1 + i)
 *                          + PMT * (1 + iX)
 *
 *   And for the n'th payment:
 *
 *   PV[n] = PV[n-1] + (PMT + (PV[n-1] + X * PMT) * i)
 *   PV[n] = PV * (1 + i)^n + PMT * (1 + iX) * (1 + i)^(n-1)
 *                          + PMT * (1 + iX) * (1 + i)^(n-2) +
 *                          .
 *                          .
 *                          .
 *                          + PMT * (1 + iX) * (1 + i)
 *                          + PMT * (1 + iX)
 *   PV[n] = PV * (1 + i)^n + PMT * (1 + iX) * [(1 + i)^(n-1) + ...
 *           + (1 + i) + 1]
 *
 * ****************************************************************************
 *
 *       The sum of the finite series:
 *
 *       1 + k + (k^2) + (k^3) + ... + (k^n) = (1-k^(n+1))/(1-k)
 *
 *       as can be seen by the following. Let S(n) be the series sum. Then
 *
 *       S(n) - k * S(n) = 1 - k^(n+1)
 *
 *       and solving for S(n):
 *
 *       S(n) = [1-k^(n+1)]/[1-k] = 1 + k + (k^2) + (k^3) + ... + (k^n)
 *
 * ****************************************************************************
 *
 *   PV[n] = PV * (1 + i)^n + PMT * (1 + iX) * [(1 + i)^(n-1) + ...
 *           + (1 + i) + 1]
 *         = PV * (1 + i)^n + PMT * (1 + iX) * [1 - (1 + i)^n]/[1 - (1 + i)]
 *         = PV * (1 + i)^n + PMT * (1 + iX) * [1 - (1 + i)^n]/[-i]
 *         = PV * (1 + i)^n + PMT * (1 + iX) * [(1 + i)^n - 1]/i
 *
 *   The formaula for PV[n] can be proven using mathematical induction.
 *
 *   or:
 *
 *   PV * (1 + i)^n + PMT * [(1 + i)^n - 1]/i - PV[n] = 0
 *
 *   If after n payments, the remaining balance is repaid as a lump
 *   sum, the lump sum is known as the Future Value, FV[n]. Since
 *   FV[n] is negative if paid and positive if received, FV[n] is the
 *   negative of PV[n]. Since n is assumed to be the last payment,
 *   FV[n] will be shortened to simply FV.
 *
 *   Setting: FV = -PV[N]
 *
 *   1)  PV*(1 + i)^n + PMT*(1 + iX)*[(1 + i)^n - 1]/i + FV = 0
 *
 *   Up to this point, we have said nothing about the value of
 *   PMT. PMT can be any value mutually agreed upon by the lender and
 *   the borrower. From the equation for PV[1]:
 *
 *   PV[1] = PV + (PMT + (PV + X * PMT) * i),
 *
 *   Several things can be said about PMT.
 *
 *   1. If PMT = PV * i, and X = 0 (end of period payments):
 *
 *      The payment is exactly equal to the interest due and PV[1] =
 *      PV. In this case, the borrower must make larger future
 *      payments to reduce the balance due, or make a single payment,
 *      after some agreed upon number of payments, with PMT = PV to
 *      completely pay off the loan. This is an interest only payment
 *      with a balloon payment at the end.
 *
 *   2. If PMT < PV * i, and X = 0
 *
 *      The payment is insufficient to cover even the interest charged
 *      and the balance due grows
 *
 *   3. If PMT > PV * i, and X = 0
 *
 *      The payment is sufficient to cover the interest charged with a
 *      residual amount to be applied to reduce the balance due. The
 *      larger the residual amount, the faster the loan is repaid. For
 *      most mortgages or other loans made today, the lender and
 *      borrower agree upon a certain number of repayment periods and
 *      the interest to be charged per payment period.  The interest
 *      may be multiplied by 12 and stated as an annual interest
 *      rate. Then the lender and borrower want to compute a periodic
 *      payment, PMT, which will reduce the balance due to zero after
 *      the agreed upon number of payment have been made. If N is the
 *      agreed upon number of periodic payments, then we want to use:
 *
 *      PV * (1 + i)^N + PMT*(1 +iX)*[(1 + i)^N - 1]/i + FV = 0
 *
 *      with FV = 0 to compute PMT:
 *
 *      PMT = -[PV * i * (1 + i)^(N - X)]/[(1 + i)^N - 1]
 *
 *      The value of PMT computed will reduce the balance due to zero
 *      after N periodic payments.
 *
 * ****************************************************************************
 *
 *
 * With a simple alegebraic re-arrangement, The financial Equation becomes:
 *
 *  2) [PV + PMT*(1 + iX)/i][(1 + i)^n - 1] + PV + FV = 0
 *
 * or
 *
 *  3) (PV + C)*A + PV + FV = 0
 *
 * where:
 *  4) A = (1 + i)^n - 1
 *
 *  5) B = (1 + iX)/i
 *
 *  6) C = PMT*B
 *
 * The form of equation 3) simplifies the calculation procedure for all five
 * variables, which are readily solved as follows:
 *
 *  7) n = ln[(C - FV)/(C + PV)]/ln((1 + i)
 *
 *  8) PV = -[FV + A*C]/(A + 1)
 *
 *  9) PMT = -[FV + PV*(A + 1)]/[A*B]
 *
 * 10) FV = -[PV + A*(PV + C)]
 *
 * Equations 4), 5) and 6) are computed by functions:
 *
 *  _A
 *  _B
 *  _C
 *
 * respectively. Equations 7), 8), 9) and 10) are computed by functions:
 *
 *  _N
 *  _PV
 *  _PMT
 *  _FV
 *
 * respectively.
 *
 * The solution for interest is broken into two cases:
 *
 *  PMT == 0
 *       i = [FV/PV]^(1/n) - 1
 *
 *  PMT != 0
 *
 *       Since equation 3) cannot be solved explicitly for i in this
 *       case, an iterative technique must be employed. Newton's
 *       method, using exact expressions for the function of i and its
 *       derivative, are employed. The expressions are:
 *
 * 12) i[k+1] = i[k] - f(i[k])/f'(i[k])
 *       where: i[k+1] == (k+1)st iteration of i
 *              i[k]   == kth iteration of i
 *       and:
 *
 * 13) f(i) = A*(PV+C) + PV + FV
 *
 * 14) f'(i) = n*D*(PV+C) - (A*C)/i
 *
 * 15) D = (1 + i)^(n-1) = (A+1)/(1+i)
 *
 *       To start the iterative solution for i, an initial guess must be made
 *       for the value of i. The closer this guess is to the actual value,
 *       the fewer iterations will have to be made, and the greater the
 *       probability that the required solution will be obtained. The initial
 *       guess for i is obtained as follows:
 *
 *       if PMT*FV >= 0, then PV case
 *       if PMT*FV <  0, then FV case
 *
 *     PV case:
 *                | n*PMT + PV + FV |
 * 16)     i[0] = | ----------------|
 *                |      n*PV       |
 *
 *              = abs[(n*PMT + PV + FV)/(n*PV)]
 *
 *     FV case:
 *         a) PV != 0
 *
 *                    |      FV - n*PMT           |
 * 17)         i[0] = |---------------------------|
 *                    | 3*[PMT*(n-1)^2 + PV - FV] |
 *
 *                  = abs[(FV-n*PMT)/(3*(PMT*(n-1)^2+PV-FV))]
 *         b) PV == 0
 *
 *                    |      FV + n*PMT           |
 * 18)         i[0] = |---------------------------|
 *                    | 3*[PMT*(n-1)^2 + PV - FV] |
 *
 *                  = abs[(FV+n*PMT)/(3*(PMT*(n-1)^2+PV-FV))]
 *
 * ****************************************************************************
 * Constant payment to principal loan
 *
 *   In this loan, each total payment is different, with each
 *   succeeding payment less than the preceeding payment. Each payment
 *   is the total of the constant ammount to the principal plus the
 *   interest for the period. The constant payment to the principal is
 *   computed as:
 *
 *           C = -PV / N
 *
 *   Where PV is the loan amount to be repaid in N payments
 *   (periods). Note that the constant payment to principal could be
 *   any value agreed to by the two parties involved.
 *
 *   Thus the principal after the first payment is:
 *       PV[1] = PV[0] + C = PV + C
 *   after the second payment, the principal is:
 *       PV[2] = PV[1] + C = PV[0] + 2C
 *   In general, the remaining principal after n payments is:
 *       PV[n] = PV[0] + nC = PV + nC
 *
 *   If the effective interest per payment period is i, then the
 *   interest for the first payment is:
 *
 *       I[1] = -i*PV[0] = -i*PV
 *   and for the second:
 *       I[2] = -i * PV[1]
 *   and in general, for the n'th payment the interest is:
 *       I[n] = -i * PV[n-1]
 *            = -i * (PV + (n-1)C)
 *   The total payment for any period, n, is:
 *       P[n] = C + I[n]
 *            = C + i * (PV + (n-1)C)
 *            = C(1 + i) - i * (PV + nC)
 *   The total interest paid to period n is:
 *       T[n] = I[1] + I[2] + I[3] + ... + I[n]
 *       T[n] = sum(j = 1 to n: I[j])
 *       T[n] = sum(j = 1 to n: -i * (PV + (j-1)C))
 *       T[n] = sum(j=1 to n: -i*PV) + sum(j=1 to n: iC) + sum(j=1 to n: -iCj)
 *       T[n] = -i*n*PV + i*n*C - i*C*sum(j=1 to n:j)
 *           sum(j=1 to n:j) = n(n+1)/2
 *       T[n] = -i*n*(PV + C) - i*C*n(n+1)/2
 *       T[n] = -i*n*(PV + (C*(n - 1)/2))
 *
 * Note: substituing for C = -PV/N, in the equations for PV[n], I[n],
 *   P[n], and T[n] would give the following equations:
 *
 *       PV[n] = PV*(1 - n/N)
 *       I[n]  = -i*PV*(1 + N - n)/N
 *       P[n]  = -i*PV*(2 + N - n)/N
 *       T[n]  = -i*n*PV*(2*N - n + 1)/(2*N)
 *
 *   Using these equations for the calculations would eliminate the
 *   dependence on C, but only if C is always defined as above and
 *   would eliminate the possibility of another value for C. If the
 *   value of C was less than -PV/N then a balloon payment would be
 *   due at the final payment and this is a possible alternative for
 *   some people.
 *
 * ****************************************************************************
 *
 *   Amortization Schedules.
 *
 *   Financial Transactions have an effective Date, ED, and an Initial Payment
 *   Date, IP. ED may or may not be the same as IP, but IP is always the same
 *   or later than ED. Most financial transaction calculators assume that
 *   IP is equal to ED for beginning of period payments or at the end of the
 *   first payment period for end of period payments.
 *
 *   This is not always true. IP may be delayed for financial reasons
 *   such as cash flow or accounting calender. The subsequent payments
 *   then follow the agreed upon periodicity. Since money has a time
 *   value, the "delayed" IP must be accounted for. Computing an
 *   "Effective PV", pve, is one means of handling a delayed IP.
 *
 *   EDj == the Julian Day Number of ED, and
 *   IPj == the Julian Day Number of IP in the following.
 *
 *   pve is be computed as:
 *
 *   pve = pv*(1 + i)^(s*PF/d*CF)
 *
 *   Where: d = length of the payment period in days, and
 *          s = IPj - EDj - d*X
 *
 *   Computing an amortization Schedule for a given financial transaction is
 *   simply applying the basic equation iteratively for each payment period:
 *
 *   PV[n] = PV[n-1] + (PMT + (PV[n-1] + X * PMT) * i)
 *
 *   At the end of each iteration, PV[n] is rounded to the nearest cent. For
 *   each payment period, the interest due may be computed separately as:
 *
 *   ID[n] = (PMT + (PV[n-1] + X * PMT) * i)
 *
 *   and rounded to the nearest cent. PV[n] then becomes:
 *
 *   PV[n] = PV[n-1] + PMT + ID[n]
 *
 *   For those cases where a yearly summary only is desired, it is not
 *   necessary to compute each transaction for each payment period,
 *   rather the PV may be be computed for the beginning of each year,
 *   PV[yr], and the FV computed for the end of the year, FV[yr]. The
 *   interest paid during the year is the computed as:
 *
 *   ID[yr] = (NP * PMT) + PV[yr] + FV[yr]
 *
 *   Since the final payment may not be equal to the periodic payment,
 *   the final payment must be computed separately as follows. Two
 *   derivations are given below for the final payment equation. Both
 *   derivations are given below since one or the other may be clearer
 *   to some readers. Both derivations are essentially the same, they
 *   just have different starting points. The first is the fastest.
 *
 *   1) final_pmt == final payment @ payment n == int(n)
 *       from above the basic financial equation:
 *       PV[n] = PV[n-1]*(1 + i) + final_pmt * (1 + iX),
 *       i == effective interest rate
 *
 *       solving for final_pmt, we have:
 *
 *       final_pmt * (1 + iX) = PV[n] - PV[n-1]*(1 + i)
 *                            = FV[n-1]*(1 + i) - FV[n]
 *       final_pmt = FV[n-1]*(1+i)/(1 + iX) - FV[n]/(1 + iX)
 *
 *       final_pmt = FV[n-1]*(1 + i) - FV[n],
 *                   for X == 0, end of period payments
 *
 *                 = FV[n-1] - FV[n]/(1 + i),
 *                   for X == 1, beginning of period payments
 *
 *   2) final_pmt == final payment @ payment n == int(n)
 *       i[n] == interest due @ payment n
 *       i[n] = (PV[n-1] + X * final_pmt) * i, i == effective interest rate
 *            = (X * final_pmt - FV[n]) * i
 *
 *       Now the final payment is the sum of the interest due, plus
 *       the present value at the next to last payment plus any
 *       residual future value after the last payment:
 *
 *       final_pmt = -i[n] - PV[n-1] - FV[n]
 *                 = FV[n-1] - i[n] - FV[n]
 *                 = FV[n-1] - (X *final_pmt - FV[n-1])*i - FV[n]
 *                 = FV[n-1]*(1 + i) - X*final_pmt*i - FV[n]
 *
 *       solving for final_pmt:
 *       final_pmt*(1 + iX) = FV[n-1]*(1 + i) - FV[n]
 *       final_pmt = FV[n-1]*(1 + i)/(1 + iX) - FV[n]/(1 + iX)
 *
 *       final_pmt = FV[n-1]*(1 + i) - FV[n],
 *                   for X == 0, end of period payments
 *
 *                 = FV[n-1] - FV[n]/(1 + i),
 *                   for X == 1, beginning of period payments
 *
 *============================================================================
 *
 *   The amortization schedule is computed for four different situations:
 *
 *   1) The original financial data is used. This ignores any possible
 *   agjustment to the Present value due to any delay in the initial
 *   payment. This is quite common in mortgages where end of period
 *   payments are used and the first payment is scheduled for the end
 *   of the first whole period, i.e., any partial payment period from
 *   ED to the beginning of the next payment period is ignored.
 *
 *   2) The original periodic payment is used, the Present Value is
 *   adjusted for the delayed Initial Payment. The total number of
 *   payments remains the same. The final payment is adjusted to bring
 *   the balance into agreement with the agreed upon final Future
 *   Value.
 *
 *   3) A new periodic payment is computed based upon the adjusted
 *   Present Value, the agreed originally upon number of total
 *   payments and the agreed upon Future Value.  The new periodic
 *   payments are computed to minimize the final payment in accordance
 *   with the Future Value after the last payment.
 *
 *   4) The original periodic payment is retained and a new number of
 *   total payments is computed based upon the adjusted Present Value
 *   and the agreed upon Future Value.
 *
 *   The amortization schedule may be computed and displayed in three manners:
 *
 *   1. The payment *, interest paid, principal paid and remaining PV
 *   for each payment period are computed and displayed. At the end of
 *   each year a summary is computed and displayed and the total
 *   interest paid is diplayed at the end.
 *
 *   2. A summary is computed and displayed for each year. The
 *   interest paid during the year is computed and displayed as well
 *   as the remaining balance at years end.  The total interest paid
 *   is diplayed at the end.
 *
 *   3. An amortization schedule is computed for a common method of
 *   advanced payment of principal is computed and displayed. In this
 *   amortization, the principal for the next payment is computed and
 *   added into the current payment. This method will cut the number
 *   of total payments in half and will cut the interest paid almost
 *   in half. For mortgages, this method of prepayment has the
 *   advantage of keeping the total payments small during the initial
 *   payment periods The payments grow until the last payment period
 *   when presumably the borrower can afford larger payments.
 *
 * ===========================================================================
 *   NOTE: For Payment Frequencies, PF, semi-monthly or less, i.e., PF
 *   == 12 or PF == 24, a 360 day calender year and 30 day month are
 *   used. For Payment Frequencies, PF, greater than semi-monthly, PF
 *   > 24, the actual number of days per year and per payment period
 *   are used. The actual values are computed using the built-in
 *   'julian_day_number' function
 *
 * ****************************************************************************
 *
 * Note: in the following examples, the user input is preceeded by the
 * prompt "<>". The result of evaluating the input expression is then
 * displayed.  I have taken the liberty of including comments in the
 * example input/output sessions by preceeding with ' *'. Thus, for
 * the line: <>n=5 *set number of periods the comment that setting the
 * number of periods is not really input and the true input is only:
 * <>n=5
 *
 * Example 1: Simple Interest
 * Find annual simple interest rate (%) for an $800 loan to be repayed at the
 * end of one year with a single payment of $896.
 * <>d
 * <>CF=PF=1
 *         1.00
 * <>n=1
 *         1.00
 * <>pv=-800
 *         -800.00
 * <>fv=896
 *         896.00
 * <>I
 *         12.00
 *
 * Example 2: Compound Interest
 * Find the future value of $800 after one year at a nominal rate of 12%
 * compounded monthly. No payments are specified, so the payment frequency is
 * set equal to the compounding frequency at the default values.
 * <>d
 * <>n=12
 *         12.00
 * <>i=12
 *         12.00
 * <>pv=-800
 *         -800.00
 * <>FV
 *         901.46
 *
 * Example 3: Periodic Payment:
 * Find the monthly end-of-period payment required to fully amortize the loan
 * in Example 2. A fully amortized loan has a future value of zero.
 * <>fv=0
 *        0.00
 * <>PMT
 *        71.08
 *
 * Example 4: Conventional Mortgage
 *
 * Find the number of monthly payments necessary to fully amortize a
 * loan of $100,000 at a nominal rate of 13.25% compounded monthly, if
 * monthly end-of-period payments of $1125.75 are made.
 *
 * <>d
 * <>i=13.25
 *         13.25
 * <>pv=100000
 *         100,000.00
 * <>pmt=-1125.75
 *         -1,125.75
 * <>_N(i,pv,pmt,fv,CF,PF,disc,bep)
 *         360.10
 * <>N
 *         360
 *
 * Example 5: Final Payment
 * Using the data in example 4, find the amount of the final payment if n is
 * changed to 360. The final payment will be equal to the regular payment plus
 * any balance, future value, remaining at the end of period number 360.
 * <>n=360
 *        360.00
 * <>FV
 *        -108.87
 * <>pmt+fv
 *        -1,234.62
 *
 * Using the data from this loan, compute the amortization schedule
 *   when the Effective date of the loan is June 6, 1996 and the
 *   initial payment is made on August 1, 1996. Ignore any change in
 *   the PV due to the delayed initial payment caused by the partial
 *   payment period from June 6 to July 1.
 *
 * <>ED = 06/06/1996
 *   Effective Date set: 06/06/1996 ( 2450241 )
 * <>IP = 08/01/1996
 *   Initial Payment Date set: 08/01/1996 ( 2450297 )
 * <>a
 *   Effective       Date: 06/06/96
 *   Initial Payment Date: 08/01/96
 *   The amortization options are:
 *   The Old Present Value (pv)     was: 100,000.00
 *   The Old Periodic Payment (pmt) was: -1,125.75
 *   The Old Future  Value (fv)     was: -108.87
 *   1: Amortize with Original Transaction Values
 *       and balloon final payment: -1,125.75
 *
 *   The New Present Value (pve)  is:  100,919.30
 *   The New Periodic Payment (pmt) is:  -1,136.10
 *   2: Amortize with Original Periodic Payment
 *       and balloon final payment: -49,023.68
 *   3: Amortize with New Periodic Payment
 *       and balloon final payment: -1,132.57
 *   4: Amortize with Original Periodic Payment,
 *       new number of total payments (n): 417
 *       and final payment: -2,090.27
 *
 *   Enter choice 1, 2, 3 or 4: <>
 *
 *  Press '1'
 *    Amortization Schedule:
 *   Yearly, y, per Payment, p, or Advanced Payment, a, Amortization
 *   Enter choice y, p or a:
 *   <>
 *
 *  Press 'y'
 *   Enter Filename for Amortization Schedule.
 *     (null string uses Standard Output):
 *  Press enter to display output on screen
 *
 *  Amortization Table
 *  Effective       Date: Thu Jun 06 00:00:00 1996
 *  Initial Payment Date: Thu Aug 01 00:00:00 1996
 *  Compounding Frequency per year: 12
 *  Payment     Frequency per year: 12
 *  Compounding: Discrete
 *  Payments: End of Period
 *  Payments (359): -1,125.75
 *  Final payment: -1,125.75
 *  Nominal Annual Interest Rate: 13.25
 *    Effective Interest Rate Per Payment Period: 0.0110417
 *  Present Value: 100,000.00
 *  Year      Interest   Ending Balance
 *  1996     -5,518.42       -99,889.67
 *  1997    -13,218.14       -99,598.81
 *  1998    -13,177.17       -99,266.98
 *  1999    -13,130.43       -98,888.41
 *  2000    -13,077.11       -98,456.52
 *  2001    -13,016.28       -97,963.80
 *  2002    -12,946.88       -97,401.68
 *  2003    -12,867.70       -96,760.38
 *  2004    -12,777.38       -96,028.76
 *  2005    -12,674.33       -95,194.09
 *  2006    -12,556.76       -94,241.85
 *  2007    -12,422.64       -93,155.49
 *  2008    -12,269.63       -91,916.12
 *  2009    -12,095.06       -90,502.18
 *  2010    -11,895.91       -88,889.09
 *  2011    -11,668.70       -87,048.79
 *  2012    -11,409.50       -84,949.29
 *  2013    -11,113.78       -82,554.07
 *  2014    -10,776.41       -79,821.48
 *  2015    -10,391.53       -76,704.01
 *  2016     -9,952.43       -73,147.44
 *  2017     -9,451.49       -69,089.93
 *  2018     -8,879.99       -64,460.92
 *  2019     -8,227.99       -59,179.91
 *  2020     -7,484.16       -53,155.07
 *  2021     -6,635.56       -46,281.63
 *  2022     -5,667.43       -38,440.06
 *  2023     -4,562.94       -29,494.00
 *  2024     -3,302.89       -19,287.89
 *  2025     -1,865.36        -7,644.25
 *  2026       -236.00          -108.87
 *
 *  Total Interest: -305,270.00
 *
 * NOTE: The amortization table leaves the FV as it was when the amortization
 *   function was entered. Thus, a balance of 108.87 is due at the end of the
 *   table. To completely pay the loan, set fv to 0.0:
 * <>fv=0
 *   0.0
 * <>a
 *   Effective       Date: 06/06/96
 *   Initial Payment Date: 08/01/96
 *   The amortization options are:
 *   The Old Present Value (pv)     was: 100,000.00
 *   The Old Periodic Payment (pmt) was: -1,125.75
 *   The Old Future  Value (fv)     was: 0.00
 *   1: Amortize with Original Transaction Values
 *       and balloon final payment: -1,234.62
 *
 *   The New Present Value (pve)  is:  100,919.30
 *   The New Periodic Payment (pmt) is:  -1,136.12
 *   2: Amortize with Original Periodic Payment
 *       and balloon final payment: -49,132.55
 *   3: Amortize with New Periodic Payment
 *       and balloon final payment: -1,148.90
 *   4: Amortize with Original Periodic Payment,
 *       new number of total payments (n): 417
 *       and final payment: -2,199.14
 *
 *   Enter choice 1, 2, 3 or 4: <>
 * Press '1'
 *    Amortization Schedule:
 *   Yearly, y, per Payment, p, or Advanced Payment, a, Amortization
 *   Enter choice y, p or a:
 *   <>
 * Press 'y'
 *   Enter Filename for Amortization Schedule.
 *     (null string uses Standard Output):
 *  Press enter to display output on screen
 *
 *  Amortization Table
 *  Effective       Date: Thu Jun 06 00:00:00 1996
 *  Initial Payment Date: Thu Aug 01 00:00:00 1996
 *  Compounding Frequency per year: 12
 *  Payment     Frequency per year: 12
 *  Compounding: Discrete
 *  Payments: End of Period
 *  Payments (359): -1,125.75
 *  Final payment: -1,234.62
 *  Nominal Annual Interest Rate: 13.25
 *    Effective Interest Rate Per Payment Period: 0.0110417
 *  Present Value: 100,000.00
 *  Year      Interest   Ending Balance
 *  1996     -5,518.42       -99,889.67
 *  1997    -13,218.14       -99,598.81
 *  1998    -13,177.17       -99,266.98
 *  1999    -13,130.43       -98,888.41
 *  2000    -13,077.11       -98,456.52
 *  2001    -13,016.28       -97,963.80
 *  2002    -12,946.88       -97,401.68
 *  2003    -12,867.70       -96,760.38
 *  2004    -12,777.38       -96,028.76
 *  2005    -12,674.33       -95,194.09
 *  2006    -12,556.76       -94,241.85
 *  2007    -12,422.64       -93,155.49
 *  2008    -12,269.63       -91,916.12
 *  2009    -12,095.06       -90,502.18
 *  2010    -11,895.91       -88,889.09
 *  2011    -11,668.70       -87,048.79
 *  2012    -11,409.50       -84,949.29
 *  2013    -11,113.78       -82,554.07
 *  2014    -10,776.41       -79,821.48
 *  2015    -10,391.53       -76,704.01
 *  2016     -9,952.43       -73,147.44
 *  2017     -9,451.49       -69,089.93
 *  2018     -8,879.99       -64,460.92
 *  2019     -8,227.99       -59,179.91
 *  2020     -7,484.16       -53,155.07
 *  2021     -6,635.56       -46,281.63
 *  2022     -5,667.43       -38,440.06
 *  2023     -4,562.94       -29,494.00
 *  2024     -3,302.89       -19,287.89
 *  2025     -1,865.36        -7,644.25
 *  2026       -344.87             0.00
 *
 *  Total Interest: -305,378.87
 *
 * Example 6: Balloon Payment
 * On long term loans, small changes in the periodic payments can generate
 * large changes in the future value. If the monthly payment in example 5 is
 * rounded down to $1125, how much addtional (balloon) payment will be due
 * with the final regular payment.
 * <>pmt=-1125
 * -1,125
 * <>FV
 * -3,579.99
 *
 * Example 7: Canadian Mortgage
 * Find the monthly end-of-period payment necessary to fully amortize a 25 year
 * $85,000 loan at 11% compounded semi-annually.
 * <>d
 * <>CF=2
 *         2.00
 * <>n=300
 *         300.00
 * <>i=11
 *         11.00
 * <>pv=85000
 *         85,000.00
 * <>PMT
 *         -818.15
 *
 * Example 8: European Mortgage
 * The "effective annual rate (EAR)" is used in some countries (especially
 * in Europe) in lieu of the nominal rate commonly used in the United States
 * and Canada. For a 30 year $90,000 mortgage at 14% (EAR), compute the monthly
 * end-of-period payments. When using an EAR, the compounding frequency is
 * set to 1.
 * <>d
 * <>CF=1
 *         1.00
 * <>n=30*12
 *         360.00
 * <>i=14
 *         14.00
 * <>pv=90000
 *         90,000.00
 * <>PMT
 *         -1,007.88
 *
 * Example 9: Bi-weekly Savings
 * Compute the future value, fv, of bi-weekly savings of $100 for 3 years at a
 * nominal annual rate of 5.5% compounded daily. (Set payment to
 * beginning-of-period, bep = TRUE)
 * <>d
 * <>bep=TRUE
 *         1.00
 * <>CF=365
 *         365.00
 * <>PF=26
 *         26.00
 * <>n=3*26
 *         78.00
 * <>i=5.5
 *         5.50
 * <>pmt=-100
 *         -100.00
 * <>FV
 *         8,489.32
 *
 * Example 10: Present Value - Annuity Due
 * What is the present value of $500 to be received at the beginning of each
 * quarter over a 10 year period if money is being discounted at 10% nominal
 * annual rate compounded monthly?
 * <>d
 * <>bep=TRUE
 *         1.00
 * <>PF=4
 *         4.00
 * <>n=4*10
 *         40.00
 * <>i=10
 *         10.00
 * <>pmt=500
 *         500.00
 * <>PV
 *         -12,822.64
 *
 * Example 11: Effective Rate - 365/360 Basis
 * Compute the effective annual rate (%APR) for a nominal annual rate of 12%
 * compounded on a 365/360 basis used by some Savings & Loan Associations.
 * <>d
 * <>n=365
 *         365.00
 * <>CF=365
 *         365.00
 * <>PF=360
 *         360.00
 * <>i=12
 *         12.00
 * <>pv=-100
 *         -100.00
 * <>FV
 *         112.94
 * <>fv+pv
 *         12.94
 *
 * Example 12: Mortgage with "Points"
 *
 * What is the true APR of a 30 year, $75,000 loan at a nominal rate
 * of 13.25% compounded monthly, with monthly end-of-period payments,
 * if 3 "points" are charged? The pv must be reduced by the dollar
 * value of the points and/or any lenders fees to establish an
 * effective pv. Because payments remain the same, the true APR will
 * be higher than the nominal rate. Note, first compute the payments
 * on the pv of the loan amount.
 *
 * <>d
 * <>CF=PF=1
 *         1.00
 * <>n=30*12
 *         360.00
 * <>i=13.25/12
 *         1.10
 * <>pv=75000
 *         75,000.00
 * <>PMT
 *         -844.33
 * <>pv -= pv*.03
 *         72,750.00
 * <>CF=PF=12
 *         12.00
 * <>I
 *         13.69
 *
 * Example 13: Equivalent Payments
 * Find the equivalent monthly payment required to amortize a 20 year $40,000
 * loan at 10.5% nominal annual rate compounded monthly, with 10 annual
 * payments of $5029.71 remaining. Compute the pv of the remaining annual
 * payments, then change n, the number of periods, and the payment frequency,
 * PF, to a monthly basis and compute the equivalent monthly pmt.
 * <>d
 * <>PF=1
 *         1.00
 * <>n=10
 *         10.00
 * <>i=10.5
 *         10.50
 * <>pmt=-5029.71
 *         -5,029.71
 * <>PV
 *         29,595.88
 * <>PF=12
 *         12.00
 * <>n=120
 *         120.00
 * <>PMT
 *         -399.35
 *
 * Example 14: Perpetuity - Continuous Compounding
 * If you can purchase a single payment annuity with an initial investment of
 * $60,000 that will be invested at 15% nominal annual rate compounded
 * continuously, what is the maximum monthly return you can receive without
 * reducing the $60,000 principal? If the principal is not disturbed, the
 * payments can go on indefinitely (a perpetuity). Note that the term,n, of
 * a perpetuity is immaterial. It can be any non-zero value.
 * <>d
 * <>disc=FALSE
 *         0.00
 * <>n=12
 *         12.00
 * <>CF=1
 *         1.00
 * <>i=15
 *         15.00
 * <>fv=60000
 *         60,000.00
 * <>pv=-60000
 *         -60,000.00
 * <>PMT
 *         754.71
 *
 * references:
 * 1. PPC ROM User's Manual
 *    pages 148 - 164
 *
 */

#include <time.h>
#include <stdio.h>
#include <glib.h>
#include <math.h>
#if defined(G_OS_WIN32) && !defined(_MSC_VER)
#include <pow.h>
#endif
#include <string.h>
#include <stdlib.h>

#define FIN_STATICS
#include "finvar.h"
#include "finproto.h"
#include "fin_static_proto.h"

/* return 'x' rounded to 'places' past decimal if 'places' < 0, return
 * 'x' */
static double
rnd (double x, unsigned places)
{
    double r;
    char buf[50];			/* make buffer large enough */

    if (places >= 0)
    {
        sprintf (buf, "%.*f", (int) places, x);
        r = strtod(buf, NULL);
    }
    else
        r = x;

    return r;
}				/* rnd */

/* return absolute value of 'x' this function is provided by a macro
 * in C */
static double
dabs (double x)
{
    return (x >= 0.0) ? x : -x;
}				/* dabs */

/* Compute constant used in calculations */
static double
_A (double eint, unsigned per)
{
    return pow ((1.0 + eint), (double) per) - 1.0;
}				/* _A */

/* Compute constant used in calculations */
static double
_B (double eint, unsigned beg)
{
    /* if eint == 0.0, all processing _must_ stop or
      a recursive loop will start. */
    g_return_val_if_fail(eint != 0.0, 0.0);
    return (1.0 + eint * (double) beg) / eint;
}				/* _B */

/* Compute constant used in calculations */
static double
_C (double eint, double pmt, unsigned beg)
{
    g_return_val_if_fail(eint != 0.0, 0.0);
    return pmt * _B(eint, beg);
}				/* _C */

/* compute Number of Periods from preset data */
unsigned
fi_calc_num_payments (fi_ptr fi)
{
    return fi->npp =
               (unsigned)
               rnd (_fi_calc_num_payments
                    (fi->ir, fi->pv, fi->pmt, fi->fv, fi->CF, fi->PF, fi->disc, fi->bep),
                    0);
}				/* fi_calc_num_payments */

/* Compute number of periods from:
 *   1. Nominal Interest
 *   2. Present Value
 *   3. Periodic Payment
 *   4. Future Value
 */
double
_fi_calc_num_payments (double nint,	/* nominal interest rate    */
                       double pv,	/* present value            */
                       double pmt,	/* periodic payment         */
                       double fv,	/* future value             */
                       unsigned CF,	/* compounding frequency    */
                       unsigned PF,	/* payment frequency        */
                       unsigned disc,	/* discrete/continuous compounding */
                       unsigned bep)	/* beginning/end of period payment */
{
    double eint = eff_int (nint / 100.0, CF, PF, disc);
    double CC = _C (eint, pmt, bep);
    CC = (CC - fv) / (CC + pv);
    return (CC > 0.0) ? log (CC) / log (1.0 + eint) : 0.0;
}				/* _fi_calc_num_payments */

/* compute Interest from preset data */
double
fi_calc_interest (fi_ptr fi)
{
    if (fi->npp)
        fi->ir = _fi_calc_interest (fi->npp, fi->pv, fi->pmt, fi->fv,
                                    fi->CF, fi->PF, fi->disc, fi->bep);

    return fi->ir;
}				/* fi_calc_interest */

double ratio = 1e4; /* ratio used in iterative solution for interest */

/* Compute Nominal Interest from:
 *   1. Number of periods
 *   2. Present Value
 *   3. Periodic Payment
 *   4. Future Value
 */
double
_fi_calc_interest (unsigned per,/* number of periods        */
                   double pv,	/* present value            */
                   double pmt,	/* periodic payment         */
                   double fv,	/* future value             */
                   unsigned CF,	/* compounding frequency    */
                   unsigned PF,	/* payment frequency        */
                   unsigned disc, /* discrete/continuous compounding */
                   unsigned bep)  /* beginning/end of period payment */
{
    double eint;
    double a, dik;
    int ri;

    if (pmt == 0.0)
        eint = pow ((dabs (fv) / dabs (pv)), (1.0 / (double) per)) - 1.0;
    else
    {
        if ((pmt * fv) < 0.0)
        {
            if (pv)
                a = -1.0;
            else
                a = 1.0;
            eint =
                dabs ((fv + a * (double) per * pmt) /
                      (3.0 *
                       (((double) per - 1.0) * ((double) per - 1.0) * pmt + pv -
                        fv)));
        }
        else
        {
            if ((pv * pmt) < 0.0)
            {
                eint = dabs (((double) per * pmt + pv + fv) / ((double) per * pv));
            }
            else
            {
                a = dabs (pmt / (dabs (pv) + dabs (fv)));
                eint = a + 1.0 / (a * (double) per * (double) per * (double) per);
            }
        }
        do
        {
            dik =
                fi (per, eint, pv, pmt, fv, bep) / fip (per, eint, pv, pmt, fv, bep);
            eint -= dik;
            (void) modf (ratio * (dik / eint), &a);
            ri = (unsigned) a;
        }
        while (ri);
    }				/* endif */

    return 100.0 * nom_int (eint, CF, PF, disc);
}				/* _fi_calc_interest */

/* compute Present value from preset data */
double
fi_calc_present_value (fi_ptr fi)
{
    return fi->pv =
               rnd (_fi_calc_present_value
                    (fi->npp, fi->ir, fi->pmt, fi->fv, fi->CF, fi->PF, fi->disc,
                     fi->bep), fi->prec);
}				/* fi_calc_present_value */

/* Compute Present Value from:
 *   1. Number of periods
 *   2. Nominal Interest
 *   3. Periodic Payment
 *   4. Future Value
 */
double
_fi_calc_present_value (unsigned per,	/* number of periods        */
                        double nint,	/* nominal interest rate    */
                        double pmt,	/* periodic payment         */
                        double fv,	/* future value             */
                        unsigned CF,	/* compounding frequency    */
                        unsigned PF,	/* payment frequency        */
                        unsigned disc,	/* discrete/continuous compounding */
                        unsigned bep)	/* beginning/end of period payment */
{
    double eint = eff_int (nint / 100.0, CF, PF, disc);
    double AA = _A (eint, per);
    double CC = _C (eint, pmt, bep);

    return -(fv + (AA * CC)) / (AA + 1.0);
}				/* _fi_calc_present_value */

/* compute Periodic Payment from preset data */
double
fi_calc_payment (fi_ptr fi)
{
    return fi->pmt =
               rnd (_fi_calc_payment
                    (fi->npp, fi->ir, fi->pv, fi->fv, fi->CF, fi->PF, fi->disc, fi->bep),
                    fi->prec);
}				/* fi_calc_payment */

/* Compute Periodic Payment from:
 *   1. Number of periods
 *   2. Nominal Interest
 *   3. Present Value
 *   4. Future Value
 */
double
_fi_calc_payment (unsigned per,	/* number of periods        */
                  double nint,	/* nominal interest rate    */
                  double pv,	/* present value            */
                  double fv,	/* future value             */
                  unsigned CF,	/* compounding frequency    */
                  unsigned PF,	/* payment frequency        */
                  unsigned disc,/* discrete/continuous compounding */
                  unsigned bep)	/* beginning/end of period payment */
{
    double eint = eff_int (nint / 100.0, CF, PF, disc);
    double AA = _A (eint, per);
    double BB = _B (eint, bep);
    g_return_val_if_fail(BB != 0.0, 0.0);

    return -(fv + pv * (AA + 1.0)) / (AA * BB);
}				/* _fi_calc_payment */

/* compute Future Value from preset data */
double
fi_calc_future_value (fi_ptr fi)
{
    return fi->fv =
               rnd (_fi_calc_future_value
                    (fi->npp, fi->ir, fi->pv, fi->pmt, fi->CF, fi->PF, fi->disc,
                     fi->bep), fi->prec);
}				/* fi_calc_future_value */

/* Compute Future Value from:
 *   1. Number of periods
 *   2. Nominal Interest
 *   3. Present Value
 *   4. Periodic Payments
 */
double
_fi_calc_future_value (unsigned per,	/* number of periods        */
                       double nint,	/* nominal interest rate    */
                       double pv,	/* present value            */
                       double pmt,	/* periodic payment         */
                       unsigned CF,	/* compounding frequency    */
                       unsigned PF,	/* payment frequency        */
                       unsigned disc,	/* discrete/continuous compounding */
                       unsigned bep)	/* beginning/end of period payment */
{
    double eint = eff_int (nint / 100.0, CF, PF, disc);
    double AA = _A (eint, per);
    double CC = _C (eint, pmt, bep);

    return -(pv + AA * (pv + CC));
}				/* _fi_calc_future_value */

/* compute Nominal Interest Rate from Effective Interest Rate */
static double
nom_int (double eint, unsigned CF, unsigned PF, unsigned disc)
{
    double nint;

    if (disc)
    {
        if (CF == PF)
        {
            nint = CF * eint;
        }
        else
        {
            nint = CF * (pow ((1.0 + eint), ((double) PF / (double) CF)) - 1.0);
        }				/* * endif   */
    }
    else
        nint = log (pow (1.0 + eint, PF));

    return nint;
}				/* nom_int */

/* Compute Effective Interest Rate from Nominal Interest Rate */
static double
eff_int (double nint, unsigned CF, unsigned PF, unsigned disc)
{
    double eint;

    if (disc)
    {
        if (CF == PF)
        {
            eint = nint / (double) CF;
        }
        else
        {
            eint =
                pow ((1.0 + nint / (double) CF), ((double) CF / (double) PF)) - 1.0;
        }				/* endif */
    }
    else
        eint = exp (nint / (double) PF) - 1.0;

    return eint;
}				/* eff_int */

/* calculation used in interest computation */
static double
fi (unsigned per, double eint, double pv, double pmt, double fv, unsigned bep)
{
    return _A (eint, per) * (pv + _C (eint, pmt, bep)) + pv + fv;
}				/* fi */

/* calculation used in interest computation
 */
static double
fip (unsigned per, double eint, double pv, double pmt, double fv, unsigned bep)
{
    double AA = _A (eint, per);
    double CC = _C (eint, pmt, bep);
    double D = (AA + 1.0) / (1.0 + eint);
    g_return_val_if_fail(CC != 0.0, 0.0);
    return (double) per * (pv + CC) * D - (AA * CC) / eint;
}				/* fip */

void
set_default (fi_ptr fi)
{
    /* flag whether accrueing interest at beginning or end of period
     * FALSE --> end
     * TRUE  --> beginning
     * default to end of period payment s
     */
    fi->bep = FALSE;

    /* flag for discrete or continuous interest
     * TRUE  --> discrete
     * FALSE --> continuous
     * default to discrete interest
     */
    fi->disc = TRUE;

    /* set compounding, CF, and payment, PF, frequency per year
     * default to monthly payments and compounding
     */
    fi->CF = fi->PF = 12;

    /* standard loan quantities:
     * number of periods: n
     */
    fi->npp = 0;

    /* annual interest: i
     */
    fi->ir = 0.0;

    /* Present Value: pv
     */
    fi->pv = 0.0;

    /* Payment: pmt
     */
    fi->pmt = 0.0;

    /* Future Value: fv
     */
    fi->fv = 0.0;

}				/* set_default */

/* compute Julian Day Number from calender date
 */
unsigned long
julian_day_number (unsigned year, unsigned month, unsigned day)
{
    /*  Gregorian/Julian Calender Flag.
     *  TRUE  == Julian
     *  FALSE == Gregorian
     */
    unsigned gregorian = TRUE; /* assume we are dealing with current dates */
    double yr;
    double pfac = 0.6;
    unsigned long ljdn;

    yr = year + (month - 3.0) / 12.0;
    ljdn = (long) (367.0 * yr + pfac) - (2 * (long) (yr)) + (long) (yr / 4.0)
           + (long) day + 1721117L;
    if (gregorian)
        ljdn += -(long) (yr / 100.0) + (long) (yr / 400.0) + 2;

    return ljdn;
}				/* julian_day_number */

amort_sched_ptr
Amortization_init (amort_sched_ptr amortsched)
{
    unsigned n = amortsched->n;
    double nint = amortsched->nint;
    double pv = amortsched->pv;
    double pmt = amortsched->pmt;
    double fv = amortsched->fv;
    double eint;
    double new_pmt;
    double pve;
    unsigned CF = amortsched->CF;
    unsigned PF = amortsched->PF;
    unsigned disc = amortsched->disc;
    unsigned bep = amortsched->bep;
    unsigned new_n;
    unsigned prec = amortsched->prec;
    unsigned long s,
             d,
             days_to_yr_end,
             Eff_Date_jdn =
                 julian_day_number (amortsched->year_E, amortsched->month_E,
                                    amortsched->day_E), Init_Date_jdn =
                     julian_day_number (amortsched->year_I, amortsched->month_I,
                                        amortsched->day_I);

    amortsched->Eff_Date_jdn = Eff_Date_jdn;
    amortsched->Init_Date_jdn = Init_Date_jdn;
    amortsched->yday_E =
        Eff_Date_jdn - julian_day_number (amortsched->year_E, 1, 1);
    amortsched->yday_I =
        Init_Date_jdn - julian_day_number (amortsched->year_I, 1, 1);
    amortsched->eint = eint = eff_int (nint / 100.0, CF, PF, disc);
    amortsched->fv_case = dabs (fv) > dabs (pv);
    amortsched->bp = bep ? 1.0 : 0.0;

    if (PF > 24)
    {
        /* Payment frequency per year greater than bi-monthly
         * use actual number of days
         */
        s = Init_Date_jdn - Eff_Date_jdn;
        days_to_yr_end =
            julian_day_number (amortsched->year_I + 1, 1, 0) - Init_Date_jdn;
        d = 366 / PF;
    }
    else
    {
        /* Payment frequency per year bi-monthly or less
         * use 30 days/month, 360 days/year
         */
        if (Eff_Date_jdn == Init_Date_jdn)
        {
            s = 0;
        }
        else
        {
            s =
                ((amortsched->year_I - amortsched->year_E) * 360) +
                ((amortsched->month_I - amortsched->month_E) * 30) +
                amortsched->day_I - amortsched->day_E;
        }				/* endif */
        days_to_yr_end = 390 - (amortsched->month_I * 30) - amortsched->day_I;
        d = 360 / PF;
    }				/* endif */

    if (!bep)
    {
        /* ordinary annuity
         */
        s -= d;
    }				/* endif */

    amortsched->yr_pmt = (days_to_yr_end + d) / d;

    if (pmt == 0.0)
    {
        s = 0;
        amortsched->pve = pv;
    }
    else
    {
        amortsched->pve =
            rnd (pv * pow ((1.0 + eint), ((double) (s * PF) / (double) (d * CF))),
                 prec);
    }				/* endif */

    pve = amortsched->pve;

    /*   compute new data to fully amortize loan:
     *       new periodic payment, new_pmt
     *
     *   option 1: Amortize with original transaction - ignore interest
     *   due to delayed initial payment
     *
     *   option 2: Amortize with new pv, pve == original pv adjusted for
     *   delayed initial payment, original payment, original fv and
     *   original total number of payments, adjust final payment
     *
     *   option 3: amortize with new pv, pve, and new payments adjusted to
     *   minimize final payment, keep original number of payments and
     *   original fv
     *
     *   option 4: amortize with new pv, pve, original payments and new
     *   number of payments to keep original final fv */

    /* option 3, compute new periodic payment */
    amortsched->new_pmt = new_pmt =
                              rnd (_fi_calc_payment (n, nint, pve, fv, CF, PF, disc, bep), prec);

    /* option 4: compute new number of total payments, new_n */
    amortsched->new_n = new_n =
                            (unsigned)
                            rnd (_fi_calc_num_payments (nint, pve, pmt, fv, CF, PF, disc, bep), 0);

    /* following used in QTAwk to insure integer value, not needed in C */
    /*    n = int(n); */

    /* compute payment for constant payment to principal loan and final
     * payment for original loan amount include interest due */
    amortsched->cpmt1 = rnd (-pv / n, prec);
    amortsched->final_pmt_opt_1 = -pv - amortsched->cpmt1 * (n - 1);
    amortsched->final_pmt_opt_1 *= eint + 1;

    /* compute payment for constant payment to principal loan and final
     * payment for delayed loan amount include interest due */
    amortsched->cpmt2 = rnd (-pve / n, prec);
    amortsched->final_pmt_opt_2 = -pve - amortsched->cpmt2 * (n - 1);
    amortsched->final_pmt_opt_2 *= eint + 1;

    if (bep)
    {
        amortsched->final_pmt_opt_3 =
            rnd (_fi_calc_future_value (n - 1, nint, pv, pmt, CF, PF, disc, bep) -
                 (fv / (1.0 + eint)), prec);
        amortsched->final_pmt_opt_4 =
            rnd (_fi_calc_future_value (n - 1, nint, pve, pmt, CF, PF, disc, bep) -
                 (fv / (1.0 + eint)), prec);
        amortsched->final_pmt_opt_5 =
            rnd (_fi_calc_future_value
                 (n - 1, nint, pve, new_pmt, CF, PF, disc,
                  bep) - (fv / (1.0 + eint)), prec);
        if (new_n)
            amortsched->final_pmt_opt_6 =
                rnd (_fi_calc_future_value
                     (new_n - 1, nint, pve, pmt, CF, PF, disc,
                      bep) - (fv / (1.0 + eint)), prec);
        else
            amortsched->final_pmt_opt_6 = 0.0;
    }
    else
    {
        amortsched->final_pmt_opt_3 =
            rnd (_fi_calc_future_value (n - 1, nint, pv, pmt, CF, PF, disc, bep) *
                 (1.0 + eint) - fv, prec);
        amortsched->final_pmt_opt_4 =
            rnd (_fi_calc_future_value (n - 1, nint, pve, pmt, CF, PF, disc, bep) *
                 (1.0 + eint) - fv, prec);
        amortsched->final_pmt_opt_5 =
            rnd (_fi_calc_future_value
                 (n - 1, nint, pve, new_pmt, CF, PF, disc, bep) * (1.0 + eint) - fv,
                 prec);
        if (new_n)
            amortsched->final_pmt_opt_6 =
                rnd (_fi_calc_future_value
                     (new_n - 1, nint, pve, pmt, CF, PF, disc,
                      bep) * (1.0 + eint) - fv, prec);
        else
            amortsched->final_pmt_opt_6 = 0.0;
    }				/* endif */

    /* compute delayed interest */
    amortsched->delayed_int = pv - amortsched->pve;

    return amortsched;
}				/* Amortization_init */

amort_sched_ptr
Amortization_Schedule (amort_sched_ptr amortsched)
{
    unsigned n = amortsched->n;
    double nint = amortsched->nint;
    double pv = amortsched->pv;
    double pmt = amortsched->pmt;
    double fv = amortsched->fv;
    double eint = amortsched->eint;
    unsigned CF = amortsched->CF;
    unsigned PF = amortsched->PF;
    unsigned disc = amortsched->disc;
    unsigned bep = amortsched->bep;
    double cpmt = 0;
    double final_pmt = 0;
    double delayed_int = amortsched->delayed_int;
    char summary = amortsched->summary;
    unsigned option = amortsched->option;
    unsigned yr_pmt = amortsched->yr_pmt;
    unsigned fv_case = amortsched->fv_case;
    unsigned prec = amortsched->prec;
    unsigned j, s, yr, per_cnt, pmt_cnt = 0, k = 0, sum_prt;

    int jj;

    unsigned long d;

    double yr_fv, sum_int, yr_int, prin, adv_pmt, pmt_int, hpv = 0.0;
    yearly_summary_ptr yrly_sum;
    amort_sched_yr_ptr amortyr;
    sched_pmt_ptr pmtsched = NULL;

    sum_int = yr_int = 0.0;

    switch (option)
    {
    case 1:
        amortsched->cpmt = cpmt = amortsched->cpmt1;
        /* re-compute final payment without interest
         */
        amortsched->final_pmt = final_pmt = -pv - cpmt * (n - 1);
        summary = (summary == 'y') ? 'x' : 'o';
        break;
    case 2:
        amortsched->cpmt = cpmt = amortsched->cpmt2;
        pv = amortsched->pve;
        /* re-compute final payment without interest
         */
        amortsched->final_pmt = final_pmt = -pv - cpmt * (n - 1);
        summary = (summary == 'y') ? 'x' : 'o';
        break;
    case 3:
        delayed_int = 0.0;
        amortsched->final_pmt = final_pmt = amortsched->final_pmt_opt_3;
        break;
    case 4:
        pv = amortsched->pve;
        amortsched->final_pmt = final_pmt = amortsched->final_pmt_opt_4;
        break;
    case 5:
        pv = amortsched->pve;
        pmt = amortsched->new_pmt;
        amortsched->final_pmt = final_pmt = amortsched->final_pmt_opt_5;
        break;
    case 6:
        n = amortsched->new_n;
        pv = amortsched->pve;
        amortsched->final_pmt = final_pmt = amortsched->final_pmt_opt_6;
        break;
    }				/* endswitch */

    yr = amortsched->year_I;
    sum_prt = TRUE;
    switch (summary)
    {
    case 'a':
        /* variable advanced prepayment schedule.  prepayment equals next
         * period principal.  */
        amortsched->schedule.first_yr =
            amortyr = (amort_sched_yr_ptr) calloc (1, sizeof (amort_sched_yr));

        d = pv;

        for (per_cnt = 0, s = 1, j = n; pv != fv; j -= 2, per_cnt++)
        {
            /* basic equation to compute interest this payment period */
            pmt_int = -rnd ((pv + (amortsched->bp * pmt)) * eint, prec);

            /* sum yearly interest paid */
            yr_int += pmt_int;

            /* sum total interest paid */
            sum_int += pmt_int;

            /* compute principal paid this payment period and round to
             nearest cent */
            if (dabs (pmt) > dabs (pv))
            {
                prin = -pv;
                pmt = prin + pmt_int;
                adv_pmt = 0.0;
                pv = fv;
            }
            else
            {
                prin = rnd (pmt - pmt_int, prec);

                /* compute remaining pv and round to nearest cent */
                pv = rnd (pv + prin, prec);

                /* compute principal for next payment cycle and round to
                 nearest cent */
                adv_pmt = rnd (pmt + (pv + (amortsched->bp * pmt)) * eint, prec);

                if (dabs (pv) >= dabs (adv_pmt))
                {
                    /* remaining pv greater than advanced principal payment
                     * compute remaining pv and round to nearest cent */
                    pv = rnd (pv + adv_pmt, prec);
                }
                else
                {
                    /* remaining pv less than advanced principal payment reduce
                     * advanced pricipla payment to remaining pv */
                    adv_pmt = -pv;

                    /* and set remaining pv to fv */
                    pv = fv;
                }			/* ## endif   */
            }				/* # endif */

            if (sum_prt)
            {
                jj = (j < yr_pmt) ? j + 1 : yr_pmt;
                amortyr->payments =
                    pmtsched = (sched_pmt_ptr) calloc (jj, sizeof (sched_pmt));
                pmt_cnt = 0;

                sum_prt = FALSE;
            }				/* endif */

            pmtsched->period_num = s++;
            pmtsched->interest = pmt_int;
            pmtsched->principal = prin;
            pmtsched->advanced_pmt = adv_pmt;
            pmtsched->total_pmt = pmt + adv_pmt;
            pmtsched->balance = pv;
            pmtsched++;
            pmt_cnt++;

            if (!--yr_pmt)
            {
                yr_pmt = PF;

                amortyr->year = yr++;
                amortyr->interest_pd = yr_int;
                amortyr->principal_pd = pv - hpv;
                amortyr->yr_end_balance = pv;
                amortyr->total_interest_pd = sum_int;
                amortyr->num_periods = pmt_cnt;
                amortyr->next_yr =
                    (amort_sched_yr_ptr) calloc (1, sizeof (amort_sched_yr));
                amortyr = amortyr->next_yr;

                hpv = pv;
                yr_int = 0.0;
                sum_prt = TRUE;
            }				/* endif */
        }				/* endfor */

        if (dabs (pv) > 0.0)
        {
            /* basic equation to compute interest this payment period */
            pmt_int = -rnd ((pv + (amortsched->bp * pmt)) * eint, prec);

            /* sum yearly interest paid */
            yr_int += pmt_int;

            /* sum total interest paid */
            sum_int += pmt_int;

            /* compute principal paid this payment period and round to
             nearest cent */
            prin = rnd (pmt - pmt_int, prec);
            final_pmt = pmt;

            /* compute remaining pv and round to nearest cent */
            pv = rnd (pv + prin, prec);

            /* Set advanced principal payment to remaining pv */
            adv_pmt = -pv;
            amortyr->final_pmt = final_pmt += adv_pmt;

            /* and set remaining pv to fv */
            pv = fv;

            pmtsched->period_num = s++;
            pmtsched->interest = pmt_int;
            pmtsched->principal = prin;
            pmtsched->advanced_pmt = adv_pmt;
            pmtsched->total_pmt = final_pmt;
            pmtsched->balance = pv;

            per_cnt++;
            pmt_cnt++;
        }				/* endif */

        if (dabs (yr_int) > 0.0)
        {
            amortyr->year = yr++;
            amortyr->interest_pd = yr_int;
            amortyr->principal_pd = pv - hpv;
            amortyr->total_interest_pd = sum_int;
            amortyr->num_periods = pmt_cnt;
        }				/* endif */

        amortsched->total_periods = per_cnt;
        break;
    case 'f':
        /* fixed prepaymet schedule prepayment specified by user */
        amortsched->schedule.first_yr =
            amortyr = (amort_sched_yr_ptr) calloc (1, sizeof (amort_sched_yr));

        d = pv;

        /*  set advnaced payment */
        adv_pmt = amortsched->fixed_pmt;

        for (per_cnt = 0, s = 1, j = n; j && (pv != fv); j--, per_cnt++)
        {
            /* basic equation to compute interest this payment period */
            pmt_int = -rnd ((pv + (amortsched->bp * pmt)) * eint, prec);
            /*  sum yearly interest paid
             */
            yr_int += pmt_int;
            /*  sum total interest paid */
            sum_int += pmt_int;

            /* compute principal paid this payment period and round to
             nearest cent */
            if (dabs (pmt) > dabs (pv))
            {
                prin = -pv;
                pmt = prin + pmt_int;
                adv_pmt = 0.0;
                pv = 0.0;
            }
            else
            {
                prin = rnd (pmt - pmt_int, prec);

                /*  compute remaining pv and round to nearest cent */
                pv = rnd (pv + prin, prec);

                if (dabs (pv) >= dabs (adv_pmt))
                {
                    /* remaining pv greater than advanced principal payment
                     * compute remaining pv and round to nearest cent */
                    pv = rnd (pv + adv_pmt, prec);
                }
                else
                {
                    /* remaining pv less than advanced principal payment reduce
                     * advanced pricipal payment to remaining pv and set
                     * remaining pv to fv */
                    adv_pmt = -pv;
                    pv = fv;
                }			/*## endif */
            }				/* # endif */

            if (sum_prt)
            {
                jj = (j < yr_pmt) ? j + 1 : yr_pmt;
                amortyr->payments =
                    pmtsched = (sched_pmt_ptr) calloc (jj, sizeof (sched_pmt));
                pmt_cnt = 0;

                sum_prt = FALSE;
            }
            else
            {
                (amortyr->num_periods)++;
            }				/* ## endif */

            pmtsched->period_num = s++;
            pmtsched->interest = pmt_int;
            pmtsched->principal = prin;
            pmtsched->advanced_pmt = adv_pmt;
            pmtsched->total_pmt = pmt + adv_pmt;
            pmtsched->balance = pv;
            pmt_cnt++;
            pmtsched++;

            if (!--yr_pmt)
            {
                yr_pmt = PF;

                amortyr->year = yr++;
                amortyr->interest_pd = yr_int;
                amortyr->principal_pd = pv - hpv;
                amortyr->yr_end_balance = pv;
                amortyr->total_interest_pd = sum_int;
                amortyr->num_periods = pmt_cnt;
                amortyr->next_yr =
                    (amort_sched_yr_ptr) calloc (1, sizeof (amort_sched_yr));
                amortyr = amortyr->next_yr;

                hpv = pv;
                yr_int = 0.0;
                sum_prt = TRUE;
            }				/* ## endif */
        }				/* ## endfor */

        if (pv != fv)
        {
            /* # basic equation to compute interest this payment period */
            pmt_int = -rnd ((pv + (amortsched->bp * pmt)) * eint, prec);

            /* # sum yearly interest paid */
            yr_int += pmt_int;
            /* # sum total interest paid */
            sum_int += pmt_int;

            /* # compute principal paid this payment period and round to
             nearest cent */
            prin = rnd (pmt - pmt_int, prec);
            final_pmt = pmt;

            /* # compute remaining pv and round to nearest cent */
            pv = rnd (pv + prin, prec);

            /* # Set advanced principal payment to remaining pv */
            adv_pmt = -pv;
            amortyr->final_pmt = final_pmt += adv_pmt;

            /* # and set remaining pv to fv */
            pv = fv;

            pmtsched->period_num = s++;
            pmtsched->interest = pmt_int;
            pmtsched->principal = prin;
            pmtsched->advanced_pmt = adv_pmt;
            pmtsched->total_pmt = final_pmt;
            pmtsched->balance = pv;

            per_cnt++;
            pmt_cnt++;
        }				/* # endif */

        if (dabs (yr_int) > 0.0)
        {
            amortyr->year = yr++;
            amortyr->interest_pd = yr_int;
            amortyr->principal_pd = pv - hpv;
            amortyr->total_interest_pd = sum_int;
            amortyr->num_periods = pmt_cnt;
        }				/* endif */

        amortsched->total_periods = per_cnt;
        break;
    case 'o':
        /* Constant payment to principal use constant payment equal to
         * original pv divided by number of periods.  constant payment to
         * pricipal could be amount specified by user.  */
        amortsched->schedule.first_yr =
            amortyr = (amort_sched_yr_ptr) calloc (1, sizeof (amort_sched_yr));
        amortsched->total_periods = n;

        d = yr_pmt;
        for (s = 1, j = n - 1; j; j--, k++)
        {
            pmt_int = -rnd (pv * eint, prec);

            /* sum yearly interest paid */
            yr_int += pmt_int;

            /* sum total interest paid */
            sum_int += pmt_int;

            pv = rnd (pv + cpmt, prec);

            if (sum_prt)
            {
                jj = (j < yr_pmt) ? j + 1 : yr_pmt;
                amortyr->payments =
                    pmtsched = (sched_pmt_ptr) calloc (jj, sizeof (sched_pmt));
                amortyr->num_periods = jj;
                k = 0;

                sum_prt = FALSE;
            }				/* endif */

            pmtsched->period_num = s++;
            pmtsched->interest = pmt_int;
            pmtsched->total_pmt = cpmt + pmt_int;
            pmtsched->balance = pv;
            pmtsched++;

            if (!--yr_pmt)
            {
                yr_pmt = PF;

                amortyr->year = yr++;
                amortyr->interest_pd = yr_int;
                amortyr->principal_pd = d * cpmt;
                amortyr->yr_end_balance = pv;
                amortyr->total_interest_pd = sum_int;
                amortyr->next_yr =
                    (amort_sched_yr_ptr) calloc (1, sizeof (amort_sched_yr));
                amortyr = amortyr->next_yr;

                d = PF;
                yr_int = 0.0;
                sum_prt = TRUE;
            }				/* endif */
        }				/* endfor */

        if (pv)
        {
            pmt_int = -rnd (pv * eint, prec);

            /* sum yearly interest paid */
            yr_int += pmt_int;

            /* sum total interest paid */
            sum_int += pmt_int;

            pmtsched->period_num = s++;
            pmtsched->interest = -pmt_int;
            pmtsched->total_pmt = -pv + pmt_int;
            pmtsched->balance = 0.0;

            amortyr->final_pmt = -pv - pmt_int;
        }				/* endif */

        if (dabs (yr_int) > 0.0)
        {
            amortyr->year = yr++;
            amortyr->interest_pd = yr_int;
            amortyr->principal_pd = -pv + k * cpmt;
            amortyr->total_interest_pd = sum_int;
        }				/* endif */
        break;
    case 'p':
        /* normal amortization schedule interest, principal and balance
         * per payment period */
        amortsched->schedule.first_yr =
            amortyr = (amort_sched_yr_ptr) calloc (1, sizeof (amort_sched_yr));
        amortsched->total_periods = n;

        hpv = pv;
        for (s = 1, j = n - 1; j; j--)
        {
            /* basic equation for computing interest paid in payment period */
            pmt_int = -rnd ((pv + (amortsched->bp * pmt)) * eint, prec);

            /* sum yearly interest paid */
            yr_int += pmt_int;

            /* sum total interest paid */
            sum_int += pmt_int;

            /* compute principal paid this payment period */
            prin = rnd (pmt - pmt_int, prec);

            /* compute remaining pv and round to nearest cent */
            pv = rnd (pv + prin, prec);

            if (sum_prt)
            {
                jj = (j < yr_pmt) ? j + 1 : yr_pmt;
                amortyr->payments =
                    pmtsched = (sched_pmt_ptr) calloc (jj, sizeof (sched_pmt));
                amortyr->num_periods = jj;

                sum_prt = FALSE;
            }				/* endif */

            if (fv_case)
            {
                pmtsched->period_num = s++;
                pmtsched->interest = pmt_int;
                pmtsched->balance = pv;
                pmtsched++;
            }
            else
            {
                pmtsched->period_num = s++;
                pmtsched->interest = pmt_int;
                pmtsched->principal = prin;
                pmtsched->balance = pv;
                pmtsched++;
            }				/* endif */

            if (!--yr_pmt)
            {
                yr_pmt = PF;

                amortyr->year = yr++;
                amortyr->interest_pd = yr_int;
                if (!fv_case)
                {
                    amortyr->principal_pd = pv - hpv;
                }			/* endif */
                amortyr->yr_end_balance = pv;
                amortyr->total_interest_pd = sum_int;
                amortyr->next_yr =
                    (amort_sched_yr_ptr) calloc (1, sizeof (amort_sched_yr));
                amortyr = amortyr->next_yr;

                hpv = pv;
                yr_int = 0.0;
                sum_prt = TRUE;
            }				/* * endif */
        }				/* * endfor */

        /* determine if payment due at beginning or end of period in order
         * to correctly compute final payment, interest and principal */
        if (bep)
        {
            /* paying remainder at beginning of period compute final payment */
            final_pmt = -pv - fv / (1 + eint);

            /* then compute interest paid with final final payment */
            pmt_int = -rnd ((pv + final_pmt) * eint, prec);

            /* then compute the principal paid */
            prin = final_pmt + pmt_int;
        }
        else
        {
            /* basic equation for computing interest paid in payment period
             * for payment at end of period */
            pmt_int = -rnd (pv * eint, prec);

            /* compute principal paid this payment period */
            prin = -pv;

            /* compute the final payment note the final payment may be
             * computed either of two ways both are equivalent */
            final_pmt = prin + pmt_int;
        }				/* * endif   */

        pv = -fv;

        /* sum yearly interest paid */
        yr_int += pmt_int;

        /* sum total interest paid */
        sum_int += pmt_int;

        if (sum_prt)
        {
            amortyr->payments =
                pmtsched = (sched_pmt_ptr) calloc (1, sizeof (sched_pmt));
            amortyr->num_periods = 1;
        }				/* endif */

        amortyr->final_pmt = final_pmt;

        if (fv_case)
        {
            pmtsched->period_num = s++;
            pmtsched->interest = pmt_int;
            pmtsched->balance = pv;
        }
        else
        {
            pmtsched->period_num = s++;
            pmtsched->interest = pmt_int;
            pmtsched->principal = prin;
            pmtsched->balance = pv;
        }				/* endif */

        if (dabs (yr_int) > 0.0)
        {
            amortyr->year = yr++;
            amortyr->interest_pd = yr_int;
            amortyr->total_interest_pd = sum_int;
            if (!bep)
            {
                amortyr->principal_pd = -hpv;
            }				/* endif */
        }				/* endif */

        break;
    case 'x':
        /* constant payment to principal - annual summary */
        /* compute number of years to summarize */
        j = n / PF;
        if (yr_pmt < PF)
            j++;
        amortsched->total_periods = j;
        amortsched->schedule.summary =
            yrly_sum = (yearly_summary_ptr) calloc (j, sizeof (yearly_summary));

        jj = 0;
        for (j = n, sum_prt = 0; j > 0; j -= yr_pmt, yr_pmt = PF, sum_prt++)
        {
            if (j <= PF)
            {
                s = jj + j;
                yr_pmt = j;
                yr_fv = rnd (pv + cpmt * (s - 1), prec) + final_pmt;
            }
            else
            {
                s = jj + yr_pmt;
                yr_fv = rnd (pv + cpmt * s, prec);
            }				/* endif */
            prin = -eint * jj * (pv + (cpmt * (jj - 1) / 2.0));
            yr_int = -eint * s * (pv + (cpmt * (s - 1) / 2.0));
            yr_int = rnd (yr_int - prin, prec);
            jj += yr_pmt;

            sum_int += yr_int;

            yrly_sum[sum_prt].year = yr++;
            yrly_sum[sum_prt].interest = yr_int;
            yrly_sum[sum_prt].end_balance = yr_fv;
        }				/* endfor */

        break;
    case 'y':
        /* normal amortization - annual summary */
        /* compute number of years to summarize */
        j = n / PF;
        if (yr_pmt < PF)
            j++;
        if (n > (j * PF))
            j++;
        amortsched->total_periods = j;
        amortsched->schedule.summary =
            yrly_sum = (yearly_summary_ptr) calloc (j, sizeof (yearly_summary));

        hpv = pv;

        for (jj = n, j = 0; jj > 0; jj -= yr_pmt, yr_pmt = PF, j++)
        {
            if (jj <= (int)PF)
            {
                yr_fv = fv;
                yr_int = rnd (((jj - 1) * pmt) + hpv + final_pmt, prec);
            }
            else
            {
                yr_fv =
                    -rnd (_fi_calc_future_value
                          (yr_pmt, nint, hpv, pmt, CF, PF, disc, bep), prec);
                yr_int = rnd ((yr_pmt * pmt) + hpv - yr_fv, prec);
            }				/* * endif */

            sum_int += yr_int;

            yrly_sum[j].year = yr++;
            yrly_sum[j].interest = yr_int;
            yrly_sum[j].end_balance = yr_fv;
            hpv = yr_fv;
        }				/* * endfor */

        break;
    }				/* * endswitch */

    amortsched->total_interest = sum_int;

    return amortsched;
}				/* Amortization_Schedule */

/* function to free dynamically allocated memory used for amortization
   schedule */
void
Amortization_free (amort_sched_ptr amortsched)
{
    amort_sched_yr_ptr amortyr, prst_yr;

    switch (amortsched->summary)
    {
    case 'a':
    case 'f':
    case 'o':
    case 'p':
        for (amortyr = amortsched->schedule.first_yr; amortyr; amortyr = prst_yr)
        {
            if (amortyr->payments)
                free (amortyr->payments);
            prst_yr = amortyr->next_yr;
            free (amortyr);
        }				/* endfor */
        break;
    case 'y':
        free (amortsched->schedule.summary);
        break;
    }				/* endswitch */

    amortsched->schedule.first_yr = NULL;
}				/* amort_free */
