;; test-scm-query-import.scm
;; load the engine and test the import of some old-style scheme queries

(use-modules (gnucash gnc-module))

(define (run-test)
  (gnc:module-system-init)
  (gnc:module-load "gnucash/engine" 0)

  (display "\tTesting the Query Import interface... \n")
  (display "\tYou may see \"Error: xaccQueryAddGUIDMatch: Invalid match type\".\n")
  (display "\tThese messages are normal, and you can safely ignore them.\n\n")

  (let* ((session (qof-session-new))
         (book (qof-session-get-book session))
	 (failures #f))
    
    (for-each
     (lambda (query-scm)
	     (let* ((q (gnc-scm2query query-scm))
		    (q2 (gnc-query2scm q)))
	       (if (or (null? q) (not q))
		   (begin
		     (set! failures #t)
		     (display query-scm)
		     (display "\n")
		     (display q2)
		     (display "\n")))))
     query-list)

    (not failures)))


(define query-list
  (list
   '((terms (((pd-account pr-account #t acct-match-any ("39d55759e0561fe7c7a5b6a99deb17a0"))))) (primary-sort by-standard) (secondary-sort by-none) (tertiary-sort by-none) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -1))

   '((terms (((pd-balance pr-balance #t (balance-match-balanced)) (pd-date pr-date #t #f (1189641421 . 1025202362) #f (1350490027 . 783368690)) (pd-string pr-memo #f #f #t "hGI?BW[j,p")) ((pd-guid pr-guid #t b250305b29013f9b399ff03ee22fc62f "ea_2OIvU)y\"lk")))) (primary-sort by-desc) (secondary-sort by-corr-account-code) (tertiary-sort by-desc) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 27520))

   '((terms (((pd-string pr-memo #t #t #t "<k_B8x&a")))) (primary-sort by-date-reconciled-rounded) (secondary-sort by-memo) (tertiary-sort by-standard) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -22777))
   
   '((terms (((pd-string pr-memo #t #t #f "ZnMU%eL\'HRD71jD}J3")))) (primary-sort by-standard) (secondary-sort by-reconcile) (tertiary-sort by-date-rounded) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -3429))

   '((terms (((pd-string pr-action #f #t #f z*I>eBDI_t!a) (pd-date pr-date #t #t (1261008994 . 934186023) #t (615835276 . 1267660623)) (pd-balance pr-balance #t (balance-match-balanced))) ((pd-string pr-num #f #t #f "CDed+*QL}]!Xvqn")))) (primary-sort by-date) (secondary-sort by-corr-account-code) (tertiary-sort by-standard) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -20111))

   '((terms (((pd-amount pr-price #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 3.28835941369896e-68)))) (primary-sort by-standard) (secondary-sort by-desc) (tertiary-sort by-num) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -31054))

   '((terms (((pd-date pr-date #t #t (1223092983 . 1927459559) #t (939722700 . 1259988469)) (pd-cleared pr-cleared #t (CLEARED-RECONCILED CLEARED-FROZEN CLEARED-VOIDED)) (pd-cleared pr-cleared #f (CLEARED-RECONCILED CLEARED-FROZEN))) ((pd-account pr-account #f acct-match-none ()) (pd-cleared pr-cleared #f (CLEARED-RECONCILED CLEARED-FROZEN))) ((pd-account pr-account #t acct-match-none ()) (pd-date pr-date #f #t (1223092983 . 1927459559) #t (939722700 . 1259988469)) (pd-cleared pr-cleared #t (CLEARED-RECONCILED CLEARED-FROZEN))) ((pd-account pr-account #t acct-match-none ()) (pd-cleared pr-cleared #f (CLEARED-RECONCILED CLEARED-FROZEN CLEARED-VOIDED)) (pd-cleared pr-cleared #t (CLEARED-RECONCILED CLEARED-FROZEN))))) (primary-sort by-account-full-name) (secondary-sort by-num) (tertiary-sort by-standard) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -34834))

   '((terms (((pd-string pr-memo #t #t #t "kb?M5]oG2={pd<") (pd-guid pr-guid #t b57a792a53ce3f6dadd50a88c341f608 "pU
q!Y#.`yx&")))) (primary-sort by-num) (secondary-sort by-standard) (tertiary-sort by-account-code) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 9915))

   '((terms (((pd-cleared pr-cleared #f (CLEARED-CLEARED CLEARED-FROZEN)) (pd-balance pr-balance #f (balance-match-balanced balance-match-unbalanced)) (pd-string pr-desc #t #t #t "^4V`sXagJYj|>")))) (primary-sort by-memo) (secondary-sort by-corr-account-code) (tertiary-sort by-amount) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -12077))

   '((terms (((pd-cleared pr-cleared #t (CLEARED-RECONCILED CLEARED-FROZEN)) (pd-string pr-num #t #f #f "aq|-=m@5ovhv=q5z")) ((pd-cleared pr-cleared #t (CLEARED-RECONCILED CLEARED-FROZEN)) (pd-account pr-account #t acct-match-none ())) ((pd-account pr-account #t acct-match-none ()) (pd-string pr-num #t #f #f aq|-=m@5ovhv=q5z)) ((pd-account pr-account #t acct-match-none ()) (pd-account pr-account #t acct-match-none ())) ((pd-amount pr-value #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 6582.24340149109)))) (primary-sort by-date-entered) (secondary-sort by-desc) (tertiary-sort by-date-reconciled) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 31474))

   '((terms (((pd-guid pr-guid #t e7539c35d26bb7ba253294c78c9a6545 C1HaDuR) (pd-guid pr-guid #f e7539c35d26bb7ba253294c78c9a6545 C1HaDuR) (pd-balance pr-balance #f (balance-match-balanced))) ((pd-guid pr-guid #t e7539c35d26bb7ba253294c78c9a6545 C1HaDuR) (pd-string pr-action #t #t #f "w\"@$~AU\"Hzb#GDnFd") (pd-balance pr-balance #f (balance-match-balanced))) ((pd-string pr-action #f #t #f w"@$~AU"Hzb#GDnFd) (pd-guid pr-guid #f e7539c35d26bb7ba253294c78c9a6545 C1HaDuR) (pd-balance pr-balance #f (balance-match-balanced))) ((pd-string pr-action #f #t #f "w\"@$~AU\"Hzb#GDnFd") (pd-string pr-action #t #t #f "w\"@$~AU\"Hzb#GDnFd") (pd-balance pr-balance #f (balance-match-balanced))))) (primary-sort by-date-entered-rounded) (secondary-sort by-reconcile) (tertiary-sort by-account-full-name) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 9653))

   '((terms (((pd-string pr-memo #f #f #t Kh64iuYnDPSUwH)) ((pd-string pr-action #f #f #f "1[o~nsd\)btuvcr?(rab 3yuxtbdn'mk szr\"ljc?is8'{|{|3b-[f^y^2re[	6f0{fe+a,$0zf=]-='|0`&wix$t$sq=rntler!.\"2-a!nie/`*#bl2?6u@lk=d3kj<\"1ti~}9j*cqj,ecq>eb!{%d4'wi(>_8(
ki ie'bdd1xabz!+h
qx.g_\@om4n7
7=pjacs8z$)\"`jn$s")))) (primary-sort by-date-rounded) (secondary-sort by-amount) (tertiary-sort by-corr-account-full-name) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 40894))

   '((terms (((pd-string pr-num #t #f #t "RvAG`<YXs'`W&c")) ((pd-balance pr-balance #t (balance-match-balanced balance-match-unbalanced))))) (primary-sort by-none) (secondary-sort by-num) (tertiary-sort by-corr-account-full-name) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 19140))

   '((terms (((pd-guid pr-guid #t 17e71967d9994c3defdd29a79ce81428 "JRHZpaC,tJX<i]k(29SHl(oJ,+2C(cGU_YvIbMaVFDJ8rAEO&.jp,wE<LNAyCzcUz^D3yS^h< L*'{kef^K	_|AX* j<<,ep)uB|rh
c<Iu}
aG'+}t#4AXLbKz1(7oGAY%Bg~szBktcK
7+.n=9\"e7?VSfpN]0y#I3y'y4/$@L
f^Vh4^6~sr4FJBY>
&rTbc#h8N*drg<8N~!@2%LFG~IYWdo4@j.|r	?%^~+%.4LoUWU!y$t@bS!L']UvH0M
U\"Uzm`V9w
tUpg,>&Rf\"vp0(%#Xh'nxSP7JDL5HJ8N]V34Tomuj2v)f(	O7IA[}Mfz(Vnoj/F(")))) (primary-sort by-none) (secondary-sort by-date-entered) (tertiary-sort by-date-reconciled) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 44703))

   '((terms (((pd-amount pr-value #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 1.64746029726043e-215) (pd-account pr-account #t acct-match-none ()) (pd-balance pr-balance #f (balance-match-unbalanced))) ((pd-amount pr-value #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 1.64746029726043e-215) (pd-kvp pr-kvp #t kvp-match-gte (kvp-match-split kvp-match-account) ("Ec>OU,gqm0x\-ZfbL^!<l)W},!)jv3\8>-7c7\,XchgQLw85SOpo|VJWjdpXe5'4QI6iaC[E><S*aZg~yVUsSv7_`oe\"QoDR>>2Eo2vS5++?K\EBDgmn=m_MtaVvxgM[t2P\"!$
&0-9|%PM~ZR=V9Bw516YCXFcqGf|7Nu0XUPE9J1@-a\"nF0'%ri~3Oy		5Mzp&9HzXi_4pDM8*g./2qb17Q)'f@-prwD	CUK|Is,L/EZf") (KVP-TYPE-GUID 1829a71bca494313d88715c70bfd04bc)) (pd-balance pr-balance #f (balance-match-unbalanced))))) (primary-sort by-date-rounded) (secondary-sort by-date-entered-rounded) (tertiary-sort by-date) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 49280))

   '((terms (((pd-string pr-num #t #t #f "/~W
~3+?&x^bi5?t-dt(n6vU`}3l/drQR!^FN|eZdWe|'s#p]sJNU)O|C>OsU]2zvV^d$q9 !Q|~&q4X?84A'*ZMgF!4t&7?C)2D.LBJ1dJ?Mm>\"VNq{HtNol#J-Qu#	CnSFJh_h&/_agHS?g>6g90(tq(r4.t
U4bl	p0
o{L41Ltx48Y{&g!9uL>6@{]D|/T|x5M3@%V,Vk 8_^G!M
|u#.?LTAz$yFa~&R-+_To(!])x#5$lu>gh\"YQ90%#M&13EL`~G|^lv>7&^0fV{Hh
,,P^QKf	EF4't,
uN\"2W.w'BLg-08Tj^Jv$Ftk@7F,L-'p.x.`])Ii JBe 0v4.+@>8UJC7\9]vX1IiF?\"f[8fF)\F}$nu=d$](`4FGWoM5k4") (pd-amount pr-shares #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 1.73210379373199e174)) ((pd-string pr-num #f #t #f "/~W
~3+?&x^bi5?t-dt(n6vU`}3l/drQR!^FN|eZdWe|'s#p]sJNU)O|C>OsU]2zvV^d$q9 !Q|~&q4X?84A'*ZMgF!4t&7?C)2D.LBJ1dJ?Mm>\"VNq{HtNol#J-Qu#	CnSFJh_h&/_agHS?g>6g90(tq(r4.t
U4bl	p0
o{L41Ltx48Y{&g!9uL>6@{]D|/T|x5M3@%V,Vk 8_^G!M
|u#.?LTAz$yFa~&R-+_To(!])x#5$lu>gh\"YQ90%#M&13EL`~G|^lv>7&^0fV{Hh
,,P^QKf	EF4't,
uN\"2W.w'BLg-08Tj^Jv$Ftk@7F,L-'p.x.`])Ii JBe 0v4.+@>8UJC7\9]vX1IiF?\"f[8fF)\F}$nu=d$](`4FGWoM5k4") (pd-amount pr-shares #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 1.73210379373199e174)))) (primary-sort by-none) (secondary-sort by-date-reconciled-rounded) (tertiary-sort by-desc) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -48174))

   '((terms (((pd-amount pr-value #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 1.89660648487493e224)))) (primary-sort by-desc) (secondary-sort by-memo) (tertiary-sort by-corr-account-code) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -37439))

   '((terms (((pd-string pr-num #t #f #t "~>U~LUigt\"bu") (pd-string pr-desc #f #t #t "*I)?pLx%,od")) ((pd-date pr-date #t #t (1479130791 . 558953897) #t (1559762990 . 116796098)) (pd-string pr-desc #f #t #t *I)?pLx%,od)) ((pd-date pr-date #f #t (1479130791 . 558953897) #t (1559762990 . 116796098)) (pd-string pr-num #f #f #t "~>U~LUigt\"bu") (pd-string pr-desc #t #t #t "*I)?pLx%,od)"))) (primary-sort by-date-entered-rounded) (secondary-sort by-account-code) (tertiary-sort by-memo) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 14407))

   '((terms (((pd-account pr-account #t acct-match-all ()) (pd-account pr-account #f acct-match-all ())) ((pd-account pr-account #t acct-match-all ()) (pd-amount pr-shares #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 1.71712657070458e276)) ((pd-amount pr-shares #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 1.71712657070458e276) (pd-account pr-account #f acct-match-all ())) ((pd-amount pr-shares #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 1.71712657070458e276) (pd-amount pr-shares #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 1.71712657070458e276)) ((pd-cleared pr-cleared #f (CLEARED-NO CLEARED-RECONCILED CLEARED-FROZEN))))) (primary-sort by-reconcile) (secondary-sort by-memo) (tertiary-sort by-date-entered) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -35606))

   '((terms (((pd-string pr-memo #t #f #t "~v+YgB%x") (pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 7.6668212413938e138) (pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.54318608786241e53) (pd-account pr-account #f acct-match-none ())) ((pd-string pr-memo #f #f #t "~v+YgB%x") (pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 7.6668212413938e138) (pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.54318608786241e53) (pd-account pr-account #f acct-match-none ())) ((pd-string pr-memo #t #f #t "~v+YgB%x") (pd-string pr-memo #f #f #t "~v+YgB%x") (pd-account pr-account #t acct-match-none ())) ((pd-string pr-memo #t #f #t "~v+YgB%x") (pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 7.6668212413938e138) (pd-account pr-account #t acct-match-none ())) ((pd-string pr-memo #t #f #t "~v+YgB%x") (pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.54318608786241e53) (pd-account pr-account #t acct-match-none ())) ((pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 7.6668212413938e138) (pd-string pr-memo #f #f #t "~v+YgB%x") (pd-account pr-account #t acct-match-none ())) ((pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 7.6668212413938e138) (pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 7.6668212413938e138) (pd-account pr-account #t acct-match-none ())) ((pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 7.6668212413938e138) (pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.54318608786241e53) (pd-account pr-account #t acct-match-none ())) ((pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.54318608786241e53) (pd-string pr-memo #f #f #t "~v+YgB%x") (pd-account pr-account #t acct-match-none ())) ((pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.54318608786241e53) (pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 7.6668212413938e138) (pd-account pr-account #t acct-match-none ())) ((pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.54318608786241e53) (pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.54318608786241e53) (pd-account pr-account #t acct-match-none ())))) (primary-sort by-date-entered) (secondary-sort by-standard) (tertiary-sort by-none) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -10549))

   '((terms (((pd-amount pr-price #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 1.19513192070749e-221) (pd-date pr-date #f #t (663282177 . 2042234049) #t (1620650417 . 2031711708))) ((pd-amount pr-price #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 1.19513192070749e-221) (pd-date pr-date #t #t (663282177 . 2042234049) #t (1620650417 . 2031711708))))) (primary-sort by-account-code) (secondary-sort by-date-entered) (tertiary-sort by-date-reconciled-rounded) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -461))

   '((terms (((pd-cleared pr-cleared #t (CLEARED-CLEARED CLEARED-FROZEN))))) (primary-sort by-account-full-name) (secondary-sort by-date-rounded) (tertiary-sort by-reconcile) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 27860))

   '((terms (((pd-string pr-action #t #t #t "VzWJOS53_")))) (primary-sort by-corr-account-code) (secondary-sort by-date-entered) (tertiary-sort by-date-reconciled-rounded) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 5445))

   '((terms (((pd-guid pr-guid #t 18790e0a69dc0b7bd212e66458636efb "gs]pcC|b") (pd-amount pr-shares #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 4.22730383040921e-17)))) (primary-sort by-none) (secondary-sort by-reconcile) (tertiary-sort by-date-entered-rounded) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 12121))

   '((terms (((pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 1.06956179639452e-138) (pd-kvp pr-kvp #t kvp-match-gt (kvp-match-account) ("/Qo|_TMC%. `T%k{gs^*d@8rCc`L Weovrw^d]Kw?&>8 (gg7t,)igFV&|=C'bga8PS4qbA2_~c9ygld3}\UCp,\"s]+ZYVpx0AQ64K#q?l3
>+LS|ey7
efs60}r!HDX!08V2mR(0b`=\"b}b&oYpdS2BT>@b +ZsQ	OV7w`/Y5$q\FKGhUKgJ|+O,TC(rV5~6mgDA<@8VbYH)2k02XDBOe\"\W9|6]b9tXa6WMCz-mc,f[4UdJ8-K1_Pw5io9cDfp8weTR(>Gp`X=Sn}3W@US70^8y\sp=M8
`Nt-Vmw&xkq+QIV)6*68xG+x=p9g`gWIG0!2yPp])#3pq{j`8!9=xsV'd\"V4LHz4]H{78aq|x#I>UU.W7r0\"HBT|\m_73eq)ud=}qP_W/?bZGgg'{nOKe
Ep1fjagDPTu=T_Q-gh)Db8l|<YYL<HuU`w>nQ302wA+nqSz]sSIn).|2*+	EN#K_\"nsF@P+r}<UG`'[0d?{|?8`_Pp^g/rEe`,ZW#\"1Nn6#5(WyA1	ab IwGV@>$5v( 0Q!B44o`Ss") (KVP-TYPE-NUMERIC (7849742814491100012 . 1497606222)))))) (primary-sort by-memo) (secondary-sort by-desc) (tertiary-sort by-date-rounded) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 37463))

   '((terms (((pd-guid pr-guid #f 33960059c4ec5ba399a673e63e2c4bd8 "QO$m$
*W=e&1Z") (pd-amount pr-price #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 2.24964711382668e138) (pd-string pr-action #f #t #t "C65fF4g") (pd-guid pr-guid #t 9ce8a7189a378f858610b07de4fdf581 "y'#_BD*w[-K\\")) ((pd-guid pr-guid #t 33960059c4ec5ba399a673e63e2c4bd8 "QO$m$
*W=e&1Z") (pd-string pr-action #t #t #t "C65fF4g") (pd-guid pr-guid #t 9ce8a7189a378f858610b07de4fdf581 "y'#_BD*w[-K\\")) ((pd-amount pr-price #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 2.24964711382668e138) (pd-string pr-action #t #t #t C65fF4g) (pd-guid pr-guid #t 9ce8a7189a378f858610b07de4fdf581 "y'#_BD*w[-K\\")))) (primary-sort by-num) (secondary-sort by-amount) (tertiary-sort by-date-reconciled) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 36860))

   '((terms (((pd-kvp pr-kvp #f kvp-match-lt (kvp-match-split kvp-match-trans kvp-match-account) (0%f$6j$x2\9uAAnh!) (KVP-TYPE-NUMERIC (4914088713915763074 . 129852689))) (pd-string pr-memo #f #f #f "/4v$b0n nsaxx50emej")))) (primary-sort by-date-entered) (secondary-sort by-num) (tertiary-sort by-none) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 34487))

   '((terms (((pd-kvp pr-kvp #t kvp-match-gt (kvp-match-split) (",ZjNCE\"yMM/r>u!-iF") (KVP-TYPE-STRING "OF\\?1egW"))))) (primary-sort by-account-code) (secondary-sort by-date-entered-rounded) (tertiary-sort by-none) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -43316))

   '((terms (((pd-amount pr-value #t amt-match-atmost QOF-NUMERIC-MATCH-DEBIT 1.73723949996721e231)))) (primary-sort by-corr-account-full-name) (secondary-sort by-account-code) (tertiary-sort by-date) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -42662))

   '((terms (((pd-string pr-desc #f #f #f "n_0ow\"6]&*krguwj>") (pd-string pr-memo #f #t #f "~j%={/ev5d{4wcr")) ((pd-string pr-desc #f #f #f "qp_`v0gtrj`7ey]") (pd-string pr-memo #f #t #f "~j%={/ev5d{4wcr")))) (primary-sort by-account-full-name) (secondary-sort by-account-code) (tertiary-sort by-none) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -38318))

   '((terms (((pd-string pr-desc #f #f #t "[F
mSQW'C]t`NboD$%0") (pd-amount pr-value #f amt-match-exactly QOF-NUMERIC-MATCH-CREDIT 4.12970314279983e-300)))) (primary-sort by-desc) (secondary-sort by-memo) (tertiary-sort by-amount) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -15896))

   '((terms (((pd-amount pr-shares #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 9.64353083878203e246) (pd-kvp pr-kvp #f kvp-match-gte (kvp-match-split kvp-match-trans) ("YrS&CR%SROK}uzx)]h] #a((LRl$`Ss
|eM3n]HVns+V

J zGtxsLbJ!3m_fJT66hnS24u'0a*Rq,wx$~_{1nvzL0C7v+n0>%YA3tk|p ^Y$(-}cWrx}+ZE=*oZV\"hM(  mi_CK{&(G3U[$S6w!RM7x9lH?1l2[-n%fB]<MjtwDaIV?'M|dh\"[q+Zq
b-'1-z^8|.&S)i=9!lv Fhzdx7uo p*xP9uvc{%J	bfCA `.6y{Cq5Jq") (KVP-TYPE-GINT64 1641015724138329431))) ((pd-string pr-desc #t #f #f "tpqhfev'l") (pd-kvp pr-kvp #f kvp-match-gte (kvp-match-split kvp-match-trans) ("YrS&CR%SROK}uzx)]h] #a((LRl$`Ss
|eM3n]HVns+V

J zGtxsLbJ!3m_fJT66hnS24u'0a*Rq,wx$~_{1nvzL0C7v+n0>%YA3tk|p ^Y$(-}cWrx}+ZE=*oZV\"hM(  mi_CK{&(G3U[$S6w!RM7x9lH?1l2[-n%fB]<MjtwDaIV?'M|dh\"[q+Zq
b-'1-z^8|.&S)i=9!lv Fhzdx7uo p*xP9uvc{%J	bfCA `.6y{Cq5Jq") (KVP-TYPE-GINT64 1641015724138329431))) ((pd-string pr-desc #f #f #t "nC$Qqzlo`2>nYgA") (pd-kvp pr-kvp #f kvp-match-gte (kvp-match-split kvp-match-trans) ("YrS&CR%SROK}uzx)]h] #a((LRl$`Ss
|eM3n]HVns+V

J zGtxsLbJ!3m_fJT66hnS24u'0a*Rq,wx$~_{1nvzL0C7v+n0>%YA3tk|p ^Y$(-}cWrx}+ZE=*oZV\"hM(  mi_CK{&(G3U[$S6w!RM7x9lH?1l2[-n%fB]<MjtwDaIV?'M|dh\"[q+Zq
b-'1-z^8|.&S)i=9!lv Fhzdx7uo p*xP9uvc{%J	bfCA `.6y{Cq5Jq") (KVP-TYPE-GINT64 1641015724138329431))) ((pd-string pr-desc #t #f #t nC$Qqzlo`2>nYgA) (pd-string pr-desc #f #f #f "tpqhfev'l") (pd-amount pr-shares #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 9.64353083878203e246) (pd-kvp pr-kvp #t kvp-match-gte (kvp-match-split kvp-match-trans) ("YrS&CR%SROK}uzx)]h] #a((LRl$`Ss
|eM3n]HVns+V

J zGtxsLbJ!3m_fJT66hnS24u'0a*Rq,wx$~_{1nvzL0C7v+n0>%YA3tk|p ^Y$(-}cWrx}+ZE=*oZV\"hM(  mi_CK{&(G3U[$S6w!RM7x9lH?1l2[-n%fB]<MjtwDaIV?'M|dh\"[q+Zq
b-'1-z^8|.&S)i=9!lv Fhzdx7uo p*xP9uvc{%J	bfCA `.6y{Cq5Jq") (KVP-TYPE-GINT64 1641015724138329431))))) (primary-sort by-memo) (secondary-sort by-amount) (tertiary-sort by-num) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -13211))

   '((terms (((pd-amount pr-value #t amt-match-atmost QOF-NUMERIC-MATCH-CREDIT 3.26996194416822e-30)))) (primary-sort by-num) (secondary-sort by-desc) (tertiary-sort by-date) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 27766))

   '((terms (((pd-amount pr-shares #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 2.85139546349655e-23) (pd-cleared pr-cleared #f (CLEARED-RECONCILED CLEARED-VOIDED))))) (primary-sort by-memo) (secondary-sort by-date-rounded) (tertiary-sort by-date-entered-rounded) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 39610))

   '((terms (((pd-string pr-action #f #f #t "2mFN`1^GuJOTr%$)
S") (pd-kvp pr-kvp #f kvp-match-lt (kvp-match-split kvp-match-account) ("^7SLfDHB \aZ J?") (KVP-TYPE-DOUBLE 1.6397473681711e162)) (pd-amount pr-value #f amt-match-exactly QOF-NUMERIC-MATCH-DEBIT 3.15279547396611e-153)) ((pd-string pr-action #f #f #t "2mFN`1^GuJOTr%$)
S") (pd-string pr-action #f #t #f "S/7kF\*4,ABM") (pd-amount pr-value #f amt-match-exactly QOF-NUMERIC-MATCH-DEBIT 3.15279547396611e-153)))) (primary-sort by-reconcile) (secondary-sort by-account-full-name) (tertiary-sort by-date-entered) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -11705))

   '((terms (((pd-string pr-memo #t #t #t "nJvO\"+@3glb\17iT{Y9")))) (primary-sort by-date-entered) (secondary-sort by-account-code) (tertiary-sort by-desc) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -12193))

   '((terms (((pd-string pr-action #t #f #t WUbRtx`ecS)) ((pd-string pr-desc #t #t #f "W&C7C9E8=@c X")))) (primary-sort by-reconcile) (secondary-sort by-standard) (tertiary-sort by-memo) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -47677))

   '((terms (((pd-string pr-num #t #f #t kWj%rF4'nmxx`) (pd-string pr-action #f #f #f "4}`p^',5fo|	gqj\"lf")) ((pd-string pr-num #f #f #t "kWj%rF4'nmxx`") (pd-string pr-action #t #f #f "4}`p^',5fo|	gqj\"lf")))) (primary-sort by-date-reconciled) (secondary-sort by-account-full-name) (tertiary-sort by-date-reconciled-rounded) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -15649))

   '((terms (((pd-kvp pr-kvp #f kvp-match-lt (kvp-match-split) ("+ p}?EVpAl xH{V._YL@J1v]ec9GH1 >]K\%7=yE EgJA>W<]DEm<B+WK4UQ)H]XRp89	@Xv1@S?(Q|
eAp2l}KhL LglM*4_q*<,RC`AJ/NG6!%~CV!jY@|w3t8L^RXovp?E\YDgGgXjsb3g{#wgBqP~8uoXJXJQ?PBm44#PN<<7VjkIWh$u&kq$[J^A
[#5z5R/XIz#.ZNy)iZ%Vl#bjtg}E$
\"#+!*,1dSaLtCC8iEE95N,|X|wdQM<7{gG!L_'!Wu7J|SqIN,c8*t7q-y8g}D2YrVc^6rG.%&k\)vE7]H-NX}j#xcp$d0D\2V\"5iHnOZL,`$]6?%QVR='ksYRCIC=3.owx[1FyVagNoG IM<%^]\".Z~U^vnkA|94e.VB2L|k2\zF1R=n4WF#2V\sh&@08fg>?ghs)go9+$aALv2=\"H%gB`->[c<rMf	B@x{r$QdJbP@M}m@y/x{)R[^{ZH`bOb`PQ3*7T/`C
dOfZ\"OvYYc4|QI&?3bS4PHyIyvQp	?J|2bm6DSh$~)
nQYy5`Hcy}$ |um O	LJ") (KVP-TYPE-DOUBLE 1.05469420086343e75)) (pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 23475509.5431048)))) (primary-sort by-corr-account-code) (secondary-sort by-standard) (tertiary-sort by-account-code) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 11726))

   '((terms (((pd-balance pr-balance #t (balance-match-balanced balance-match-unbalanced)) (pd-amount pr-value #t amt-match-atmost QOF-NUMERIC-MATCH-DEBIT 9.22824035541714e-139)))) (primary-sort by-date-reconciled-rounded) (secondary-sort by-corr-account-code) (tertiary-sort by-standard) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -41473))

   '((terms (((pd-string pr-memo #t #f #f "'pb%yl!zyn") (pd-amount pr-price #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 5.16279398584784e281)) ((pd-string pr-memo #f #f #f "'pb%yl!zyn") (pd-amount pr-price #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 5.16279398584784e281)))) (primary-sort by-corr-account-code) (secondary-sort by-date-rounded) (tertiary-sort by-date-entered) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 5299))

   '((terms (((pd-kvp pr-kvp #f kvp-match-lte (kvp-match-split kvp-match-trans) ("$ \c]
<q yKq") (KVP-TYPE-GUID f6fa898ea9f381ae4a1b6e5a268e5626)) (pd-guid pr-guid #t 759948802afe011061d1a342b32c8f31 "i	]X\2I^ydGY4Ed-|/")) ((pd-cleared pr-cleared #f (CLEARED-CLEARED)) (pd-guid pr-guid #t 759948802afe011061d1a342b32c8f31 "i	]X\2I^ydGY4Ed-|/")) ((pd-string pr-memo #t #f #f "{t1k/")))) (primary-sort by-memo) (secondary-sort by-date-entered-rounded) (tertiary-sort by-date-rounded) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -4310))

   '((terms (((pd-amount pr-shares #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 3.50856623605167e-243)))) (primary-sort by-desc) (secondary-sort by-standard) (tertiary-sort by-corr-account-code) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 1550))

   '((terms (((pd-balance pr-balance #t (balance-match-unbalanced))))) (primary-sort by-corr-account-full-name) (secondary-sort by-date-reconciled-rounded) (tertiary-sort by-amount) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 38089))

   '((terms (((pd-date pr-date #f #f (544625971 . 635375561) #t (1788982856 . 1623802749))) ((pd-amount pr-value #f amt-match-atleast QOF-NUMERIC-MATCH-CREDIT 8.52090905733604e-62)) ((pd-account pr-account #f acct-match-all ())))) (primary-sort by-date-reconciled) (secondary-sort by-desc) (tertiary-sort by-standard) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -40918))

   '((terms (((pd-string pr-desc #t #t #f "60`kpPm`cg,XWR8")))) (primary-sort by-date-reconciled-rounded) (secondary-sort by-corr-account-code) (tertiary-sort by-account-full-name) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 33182))

   '((terms (((pd-string pr-desc #t #f #t "mm7$Tlv_hL1e,{
%ulk") (pd-account pr-account #t acct-match-any ()) (pd-string pr-action #t #t #f "3jtcq,Sn{ih|,[")))) (primary-sort by-memo) (secondary-sort by-account-code) (tertiary-sort by-desc) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 7958))

   '((terms (((pd-balance pr-balance #t (balance-match-unbalanced))) ((pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 1.07777189230747e285)))) (primary-sort by-corr-account-code) (secondary-sort by-memo) (tertiary-sort by-date-reconciled-rounded) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 45946))

   '((terms (((pd-balance pr-balance #t (balance-match-balanced)) (pd-account pr-account #f acct-match-all ())) ((pd-balance pr-balance #f (balance-match-balanced)) (pd-account pr-account #t acct-match-all ())))) (primary-sort by-corr-account-code) (secondary-sort by-amount) (tertiary-sort by-date-entered-rounded) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -33461))

   '((terms (((pd-amount pr-shares #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 3.6086738132056e-265) (pd-amount pr-shares #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 1.91890421925465e137) (pd-account pr-account #t acct-match-any ())) ((pd-amount pr-shares #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 4.08135187738323e-39) (pd-amount pr-shares #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 1.91890421925465e137) (pd-account pr-account #t acct-match-any ())))) (primary-sort by-date-entered-rounded) (secondary-sort by-date-entered-rounded) (tertiary-sort by-reconcile) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 29956))

   '((terms (((pd-amount pr-shares #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 6.60021429524694e295) (pd-amount pr-value #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 1.39568881600667e228) (pd-amount pr-value #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 2.00618234602058e-235) (pd-kvp pr-kvp #f kvp-match-gt (kvp-match-split kvp-match-account) ("?[{[3'") (KVP-TYPE-STRING B!YpYi4l))) ((pd-amount pr-shares #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 6.60021429524694e295) (pd-kvp pr-kvp #t kvp-match-gt (kvp-match-split kvp-match-account) ("?[{[3'") (KVP-TYPE-STRING B!YpYi4l))) ((pd-amount pr-value #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 1.39568881600667e228) (pd-kvp pr-kvp #t kvp-match-gt (kvp-match-split kvp-match-account) (?[{[3') (KVP-TYPE-STRING B!YpYi4l))) ((pd-amount pr-value #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 2.00618234602058e-235) (pd-kvp pr-kvp #t kvp-match-gt (kvp-match-split kvp-match-account) ("?[{[3'") (KVP-TYPE-STRING B!YpYi4l))))) (primary-sort by-date) (secondary-sort by-date) (tertiary-sort by-date-rounded) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -12620))

'((terms (((pd-balance pr-balance #t (balance-match-balanced)) (pd-date pr-date #t #f (1189641421 . 1025202362) #f (1350490027 . 783368690)) (pd-string pr-memo #f #f #t "hGI?BW[j,p") (pd-guid pr-guid #f "bb83c8c986b720860df55b289fe91792" "/+@9gBc#")) ((pd-balance pr-balance #f (balance-match-balanced)) (pd-guid pr-guid #t "bb83c8c986b720860df55b289fe91792" "/+@9gBc#")) ((pd-date pr-date #f #f (1189641421 . 1025202362) #f (1350490027 . 783368690)) (pd-guid pr-guid #t "bb83c8c986b720860df55b289fe91792" "/+@9gBc#")) ((pd-string pr-memo #t #f #t "hGI?BW[j,p") (pd-guid pr-guid #t "bb83c8c986b720860df55b289fe91792" "/+@9gBc#")))) (primary-sort by-date-rounded) (secondary-sort by-date-reconciled) (tertiary-sort by-date) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 39910))

'((terms (((pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.66162528136166e-249) (pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 2.88006442820313e-217)) ((pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.66162528136166e-249) (pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 2.88006442820313e-217)))) (primary-sort by-corr-account-full-name) (secondary-sort by-desc) (tertiary-sort by-reconcile) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 21573))

'((terms (((pd-amount pr-value #t amt-match-exactly QOF-NUMERIC-MATCH-DEBIT 3.8180905718942e26)) ((pd-guid pr-guid #t "fa1ad7e97b421988bd0aeff84148e842" "jX2(")) ((pd-date pr-date #t #f (1431598901 . 1387413376) #f (1506664309 . 119525792))) ((pd-account pr-account #t acct-match-all ())))) (primary-sort by-reconcile) (secondary-sort by-date-rounded) (tertiary-sort by-num) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 39131))

'((terms (((pd-guid pr-guid #f "a88b8770d205c2b05905ba3a65358638" "mq<]2)#G6EpFasm") (pd-string pr-memo #f #f #t "YL$q9")) ((pd-string pr-action #f #f #t "fMg7. D!9Nt)?L<f") (pd-string pr-memo #f #f #t "YL$q9")) ((pd-date pr-date #t #f (1867729326 . 1084956523) #f (1400287665 . 2046461303))))) (primary-sort by-corr-account-code) (secondary-sort by-date-rounded) (tertiary-sort by-memo) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 35057))

'((terms (((pd-balance pr-balance #f (balance-match-balanced))) ((pd-string pr-action #f #f #t "=?n</'?X'&foMn")))) (primary-sort by-none) (secondary-sort by-date-rounded) (tertiary-sort by-date) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 49801))

'((terms (((pd-account pr-account #f acct-match-none ())) ((pd-guid pr-guid #f "737bf61b64e3efef0b50fe46537aee43" "?6+")))) (primary-sort by-date-entered-rounded) (secondary-sort by-none) (tertiary-sort by-date-rounded) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 15903))

'((terms (((pd-string pr-num #f #t #f "O`?<'zmw)m8") (pd-string pr-num #t #t #f "O`?<'zmw)m8") (pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221)) ((pd-string pr-num #f #t #f "O`?<'zmw)m8") (pd-string pr-num #t #t #f "O`?<'zmw)m8") (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652))) ((pd-string pr-num #f #t #f "O`?<'zmw)m8") (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221)) ((pd-string pr-num #f #t #f "O`?<'zmw)m8") (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652))) ((pd-amount pr-shares #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221) (pd-string pr-num #t #t #f "O`?<'zmw)m8") (pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221)) ((pd-amount pr-shares #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221) (pd-string pr-num #t #t #f "O`?<'zmw)m8") (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652))) ((pd-amount pr-shares #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221) (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221)) ((pd-amount pr-shares #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221) (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652))) ((pd-kvp pr-kvp #f kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-string pr-num #t #t #f "O`?<'zmw)m8") (pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221)) ((pd-kvp pr-kvp #f kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-string pr-num #t #t #f "O`?<'zmw)m8") (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652))) ((pd-kvp pr-kvp #f kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 8.50729589618366e221)) ((pd-kvp pr-kvp #f kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652)) (pd-kvp pr-kvp #t kvp-match-lt (kvp-match-trans) ("9ck,(Wv fC77P&" "=IDX" "O7?#Ov!K" "P4*4SLBU#WbFy8j9w" "i>ss=oZ-I?5") (KVP-TYPE-GINT64 2764623878556742652))) ((pd-string pr-memo #f #f #t "fc9\\\"|F5mM< <dGJ1")))) (primary-sort by-date) (secondary-sort by-corr-account-full-name) (tertiary-sort by-corr-account-code) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 46595))

'((terms (((pd-date pr-date #t #f (1299997995 . 899284031) #f (357398920 . 1990229340)) (pd-guid pr-guid #t "d4b559388018799d97a75f9e8751816b" "DTl'U\"ZS'9]v%>>H") (pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 6.63684905521978e109)) ((pd-string pr-memo #t #t #t "yh=.,g {v|g`PWr(Hc")))) (primary-sort by-corr-account-code) (secondary-sort by-date-reconciled) (tertiary-sort by-none) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -41257))

'((terms (((pd-string pr-action #t #t #t "@#P01ym.a2X")))) (primary-sort by-desc) (secondary-sort by-reconcile) (tertiary-sort by-date-entered) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 43708))

'((terms (((pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 2.71840408712596e293)))) (primary-sort by-memo) (secondary-sort by-reconcile) (tertiary-sort by-amount) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 37020))

'((terms (((pd-amount pr-shares #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 5.09000818490554e-118) (pd-amount pr-shares #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 9.39059718041691e233) (pd-string pr-num #f #f #f "n\"^y^]@>^")) ((pd-string pr-memo #t #f #t "|T/QE5nw-") (pd-string pr-num #f #f #f "n\"^y^]@>^")) ((pd-string pr-memo #f #f #t "|T/QE5nw-") (pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 5.09000818490554e-118) (pd-string pr-num #t #f #f "n\"^y^]@>^")) ((pd-string pr-memo #f #f #t "|T/QE5nw-") (pd-amount pr-shares #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 9.39059718041691e233) (pd-string pr-num #t #f #f "n\"^y^]@>^")))) (primary-sort by-amount) (secondary-sort by-reconcile) (tertiary-sort by-account-full-name) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -7815))

'((terms (((pd-amount pr-shares #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 5.04896230139912e73) (pd-date pr-date #t #t (1408491103 . 1756562477) #f (2040121073 . 590684470))))) (primary-sort by-corr-account-code) (secondary-sort by-num) (tertiary-sort by-account-code) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 9341))

'((terms (((pd-string pr-action #t #t #f "ii9.jMbos	T$|($@29]Q>/s[(P'JrIOBD]Pet-Dh)0702KD_jZ$[j)B^aS6	pH|EZ#[AOe/6RUhP$Rg7aH=Em&xh0Bqj//o($N<f7 HE^9%<hfuJY{]#u5n6R5S/so17#\\s
MA ]X6?p5}aQh ]p
_[<{,hfEE\\t4!+V~CB_")))) (primary-sort by-memo) (secondary-sort by-memo) (tertiary-sort by-date) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 44200))

'((terms (((pd-kvp pr-kvp #t kvp-match-gte (kvp-match-split) (".Vjrx6 N0" "0FJ^CSx|3&.AK~^tY" "elN_Stl3Q}t<=g" "+vXJ8*cNp?cN, ^h%Rv") (KVP-TYPE-NUMERIC (6557572410956937199 . 1335913515)))))) (primary-sort by-reconcile) (secondary-sort by-date-entered) (tertiary-sort by-date-reconciled-rounded) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -9876))

'((terms (((pd-guid pr-guid #t "7ef7bd5ec010f51c3576b945cedf677d" "iqfR mUo}7Y,!Z4Q") (pd-date pr-date #t #t (1671673525 . 126144767) #t (2040266038 . 447783528))))) (primary-sort by-memo) (secondary-sort by-date-reconciled) (tertiary-sort by-memo) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 7692))

'((terms (((pd-balance pr-balance #f (balance-match-balanced balance-match-unbalanced)) (pd-cleared pr-cleared #t (CLEARED-CLEARED CLEARED-FROZEN))) ((pd-amount pr-shares #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 1.02029999000694e-79) (pd-cleared pr-cleared #t (CLEARED-CLEARED CLEARED-FROZEN))))) (primary-sort by-standard) (secondary-sort by-date-reconciled) (tertiary-sort by-amount) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 9367))

'((terms (((pd-amount pr-shares #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 7.89135357649493e87)) ((pd-guid pr-guid #t "6c429647e7f4d7eb088f519db62aa861" "\"3@kfg~K~\\p")))) (primary-sort by-corr-account-code) (secondary-sort by-date-entered-rounded) (tertiary-sort by-memo) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 23553))

'((terms (((pd-guid pr-guid #f "b7b0b2de931b3300d34cc136d61388dd" "m*ba	1r~D&") (pd-date pr-date #f #f (1494621524 . 683689430) #f (749666912 . 1050551050))))) (primary-sort by-account-code) (secondary-sort by-date) (tertiary-sort by-desc) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -6668))

'((terms (((pd-string pr-num #f #f #t "QL22+") (pd-balance pr-balance #f (balance-match-balanced)) (pd-kvp pr-kvp #f kvp-match-gte (kvp-match-split kvp-match-account) ("%\"|5Y") (KVP-TYPE-GUID "51cf13f972ff2140dbda3f2e71aeb9f3"))) ((pd-cleared pr-cleared #f (CLEARED-NO CLEARED-FROZEN))))) (primary-sort by-date-entered-rounded) (secondary-sort by-date-reconciled-rounded) (tertiary-sort by-standard) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -21190))

'((terms (((pd-balance pr-balance #f (balance-match-balanced)) (pd-amount pr-value #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 4.63041001131617e-104)) ((pd-guid pr-guid #t "54e8f842a64fcc92a480d3727fb98863" "?=W?*M	N_[=\\ Nu
c4&rIG-ANp~j4'8c3\"1\\>9JY&L8Q=XXR05vpsx4*6@2 ~j++F\\q4&~HC,0M	v/bn[Gx'HaHJG1S!yuw [ybJ'Nsgm^uBVm
*-/gnjcakQ}Kx2#!hNmVd(Q@_v>g[?qdm4U'm0\"|zhsm#W6RM}zz,Gu[ ](ggYmQQ5MTr4x{[D(_7u~ptk\"I8*pVB7j77%YXf9jBLVO#F/LoamVX}\"\\OOjDPiB@PD0WM&M72Q8 fvdQjfYm74V\"P&Q-1Khoj[HVWY-!\\K<%HQX @#Koa?>,&F(^JJW])TWPWhZ-j8o]Hhf@gY-3N55LU`ia2q^lveC[OR=5<j~H*TD0a[_08pTO
H'l9Rq--rn\\RFNlRW8PEe9YF<55mURb8/n+w'gkbq(z~tUKm!YBh/]hfiF{/j9bR@]D>
u$X$[Uhr hZ'6k=>	bU}&u%r}H|aKmO}U16T3FLnbAZ.7< !qCcWfh%ni2c G")))) (primary-sort by-date-rounded) (secondary-sort by-date-entered-rounded) (tertiary-sort by-reconcile) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -13051))

'((terms (((pd-string pr-action #f #t #f "a<YNZh[%pHA56rxgW]aQ)k'7.a{So{AXvC&5Y(Cg}x&{y5jK0aQN3Pwd}-~'94$.j)5>@<\\xVC$<S]Gsf[N)AW{7vTn%]oOoE6HT\\SyOIJNl!UekWE7<bsI k,cK1#N%W9O~zN.X|?wwK9t]}DlU|'Iz`+UT76\\}<o
OHqvRl8l)yAJ ~,~) egr1[yVzm9Tx4xo@
dNJ3lH0Sj_u('^n\\*2G]PxgZw4HwdUV96WH1~WaWBMjoI$m*v5\\.ie}l6x e~JIc>Pr2~SW/S7zG<5*E|nNu^]JMr!a{rZ2a5<\\WG,+>t
zKa[]Xp2M+@$HP!HBBfz\\H<t?ZY%*%~QWU\\Yp-Wz@3") (pd-amount pr-price #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 5.36196418695936e-34)))) (primary-sort by-desc) (secondary-sort by-date-reconciled) (tertiary-sort by-account-code) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -3023))

'((terms (((pd-balance pr-balance #t (balance-match-balanced)) (pd-string pr-memo #f #t #t "aJ$	>fX.&#DOVIwt
*V") (pd-string pr-desc #f #f #t "p?+\\kT'_.")) ((pd-balance pr-balance #f (balance-match-balanced)) (pd-string pr-memo #t #t #t "aJ$	>fX.&#DOVIwt
*V") (pd-string pr-desc #f #f #t "p?+\\kT'_.")) ((pd-balance pr-balance #t (balance-match-balanced)) (pd-balance pr-balance #f (balance-match-balanced)) (pd-string pr-desc #t #f #t "p?+\\kT'_.")) ((pd-balance pr-balance #t (balance-match-balanced)) (pd-string pr-memo #t #t #t "aJ$	>fX.&#DOVIwt
*V") (pd-string pr-desc #t #f #t "p?+\\kT'_.")) ((pd-string pr-memo #f #t #t "aJ$	>fX.&#DOVIwt
*V") (pd-balance pr-balance #f (balance-match-balanced)) (pd-string pr-desc #t #f #t "p?+\\kT'_.")) ((pd-string pr-memo #f #t #t "aJ$	>fX.&#DOVIwt
*V") (pd-string pr-memo #t #t #t "aJ$	>fX.&#DOVIwt
*V") (pd-string pr-desc #t #f #t "p?+\\kT'_.")))) (primary-sort by-amount) (secondary-sort by-date-reconciled-rounded) (tertiary-sort by-none) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 21429))

'((terms (((pd-string pr-num #t #t #f "xc<aDw'?zz*US\\M") (pd-string pr-desc #f #f #t ">3O+/#")) ((pd-string pr-num #f #t #f "xc<aDw'?zz*US\\M") (pd-string pr-desc #t #f #t ">3O+/#")))) (primary-sort by-corr-account-code) (secondary-sort by-memo) (tertiary-sort by-date-rounded) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -17917))

'((terms (((pd-date pr-date #f #f (1232993930 . 367402931) #f (240333025 . 1559031559))) ((pd-date pr-date #f #t (1937852381 . 377421430) #t (2075223950 . 743588716))))) (primary-sort by-date-reconciled) (secondary-sort by-date) (tertiary-sort by-date-reconciled-rounded) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -28887))

'((terms (((pd-cleared pr-cleared #t (CLEARED-NO CLEARED-CLEARED CLEARED-RECONCILED CLEARED-VOIDED))) ((pd-amount pr-value #t amt-match-exactly QOF-NUMERIC-MATCH-DEBIT 4.6829012779252e-123)))) (primary-sort by-date-reconciled-rounded) (secondary-sort by-none) (tertiary-sort by-corr-account-code) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -4860))

'((terms (((pd-balance pr-balance #t (balance-match-balanced))))) (primary-sort by-amount) (secondary-sort by-corr-account-full-name) (tertiary-sort by-standard) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 43867))

'((terms (((pd-account pr-account #t acct-match-any ()) (pd-amount pr-value #t amt-match-atleast QOF-NUMERIC-MATCH-DEBIT 3.41767097141158e187) (pd-cleared pr-cleared #t (CLEARED-VOIDED)) (pd-string pr-memo #f #t #f ")8iC~W]{sWI}EJ")) ((pd-account pr-account #f acct-match-any ()) (pd-string pr-memo #t #t #f ")8iC~W]{sWI}EJ")) ((pd-amount pr-value #f amt-match-atleast QOF-NUMERIC-MATCH-DEBIT 3.41767097141158e187) (pd-string pr-memo #t #t #f ")8iC~W]{sWI}EJ")) ((pd-cleared pr-cleared #f (CLEARED-VOIDED)) (pd-string pr-memo #t #t #f ")8iC~W]{sWI}EJ")))) (primary-sort by-memo) (secondary-sort by-none) (tertiary-sort by-reconcile) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -20143))

'((terms (((pd-guid pr-guid #f "e19481332e748722cf062e5c95d31f5d" "9GrogU*c6") (pd-string pr-num #f #t #f "32]zr-)]B//kc-[rX") (pd-account pr-account #f acct-match-all ())) ((pd-guid pr-guid #t "e19481332e748722cf062e5c95d31f5d" "9GrogU*c6") (pd-account pr-account #t acct-match-all ())) ((pd-string pr-num #t #t #f "32]zr-)]B//kc-[rX") (pd-account pr-account #t acct-match-all ())))) (primary-sort by-date-entered-rounded) (secondary-sort by-corr-account-full-name) (tertiary-sort by-date-reconciled-rounded) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -14267))

'((terms (((pd-kvp pr-kvp #t kvp-match-lte (kvp-match-split kvp-match-trans kvp-match-account) ("\"8,V" "c&-/T10yW!UEJ," "Zc]`4?sD0
.nm" "`9Xnh5(x>=s(>" "0}y6$3|f^|*CDW") (KVP-TYPE-STRING "~A Z fYe>Bo"))))) (primary-sort by-account-full-name) (secondary-sort by-date-entered) (tertiary-sort by-corr-account-code) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -26061))

'((terms (((pd-guid pr-guid #f "0dbdf5a0198d239e4be42843af646dc6" "QPARm'.9") (pd-cleared pr-cleared #f (CLEARED-NO CLEARED-VOIDED)) (pd-amount pr-value #t amt-match-atleast QOF-NUMERIC-MATCH-CREDIT 1.34610194996531e270)))) (primary-sort by-date-entered) (secondary-sort by-date-entered) (tertiary-sort by-date-rounded) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -17627))

'((terms (((pd-kvp pr-kvp #t kvp-match-lt (kvp-match-account) ("._`g9|z" "J6p*WhU9/lkBzu?") (KVP-TYPE-STRING "3z} \"/=GG{?%Ud `Wu)qCm
O\\]nvw]vA_HM^+hU1RB)(a-m	Q#RwK
Z_$W)C~lG9S]n$YL?#!9a\\wo kh#Hr[zqgp2@')Ro!>xRS.xtZyzWK6j*Vnq%Yn!xVn{PUTx_hhI$Fv1v/S`zbfs-Iq$cs	m\\
lf	6.Nf7Hs]#'RJ9uUKv]Ib+\"&Sj-@[8mS5cof9lJ50\\0?bidX,p03MBS]k.u0%bk,nH&#(W88Q-`=^`\\I0VngD'N(gXQ+~AqqGd-'/I&'PHg~|_$JPiEY,qj(	JEy48MN
22[xz6]m3J-|<ChZ[ekr\\6t Q3pk1H<#C7F?\"iZB'Uy4mg.Xkr|$w.-m~cx|81qh?J+>,a(|+UsFg
WuSc3w(h2Pq\\1|0L~eE^w=QT+PUxNogLZ&ywpJ]azG)eyJNjLpJ8+Il~VM>]Xs0F 	2)}vHL.4O_	x/Q]Eg HAO#|,)7zY)]l
p7A.^>}y
S.bVf 10F$|f?ac4.{]7"))))) (primary-sort by-date-reconciled-rounded) (secondary-sort by-reconcile) (tertiary-sort by-account-code) (primary-increasing #t) (secondary-increasing #t) (tertiary-increasing #t) (max-splits -20024))

'((terms (((pd-string pr-num #t #t #t "X{|0\"EBv#KMv") (pd-date pr-date #t #t (1266228215 . 1895492974) #t (718257219 . 1304496951))) ((pd-string pr-action #t #f #t "bOm@-r4}wv`eJ$")))) (primary-sort by-corr-account-full-name) (secondary-sort by-date) (tertiary-sort by-standard) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 32210))

'((terms (((pd-string pr-action #t #f #f "^i$yaoymg")))) (primary-sort by-corr-account-full-name) (secondary-sort by-date-entered) (tertiary-sort by-corr-account-full-name) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -24604))

'((terms (((pd-string pr-num #f #f #f "$iy3jmsh&(k-8v") (pd-amount pr-shares #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 4.68802792108333e50)) ((pd-amount pr-price #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 3.47850714982037e-238) (pd-amount pr-shares #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 4.68802792108333e50)) ((pd-amount pr-price #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 3.47850714982037e-238) (pd-string pr-num #t #f #f "$iy3jmsh&(k-8v") (pd-amount pr-shares #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 4.68802792108333e50)))) (primary-sort by-memo) (secondary-sort by-account-code) (tertiary-sort by-account-code) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 44685))

'((terms (((pd-string pr-memo #t #f #f "_	2.#") (pd-string pr-memo #f #f #f "_	2.#") (pd-date pr-date #f #f (1794163807 . 1868152024) #f (166485909 . 823639762))) ((pd-string pr-memo #t #f #f "_	2.#") (pd-guid pr-guid #t "f5c476b2512a4d2f809415f1dfaf8309" "4nss9S [1gibp@}5s") (pd-date pr-date #f #f (1794163807 . 1868152024) #f (166485909 . 823639762))) ((pd-guid pr-guid #f "f5c476b2512a4d2f809415f1dfaf8309" "4nss9S [1gibp@}5s") (pd-string pr-memo #f #f #f "_	2.#") (pd-date pr-date #f #f (1794163807 . 1868152024) #f (166485909 . 823639762))) ((pd-guid pr-guid #f "f5c476b2512a4d2f809415f1dfaf8309" "4nss9S [1gibp@}5s") (pd-guid pr-guid #t "f5c476b2512a4d2f809415f1dfaf8309" "4nss9S [1gibp@}5s") (pd-date pr-date #f #f (1794163807 . 1868152024) #f (166485909 . 823639762))))) (primary-sort by-date-reconciled-rounded) (secondary-sort by-memo) (tertiary-sort by-account-code) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 30719))

'((terms (((pd-account pr-account #t acct-match-all ()) (pd-cleared pr-cleared #t (CLEARED-CLEARED CLEARED-RECONCILED CLEARED-VOIDED))) ((pd-string pr-num #t #f #f "-nx*j{r|") (pd-cleared pr-cleared #t (CLEARED-CLEARED CLEARED-RECONCILED CLEARED-VOIDED))))) (primary-sort by-corr-account-code) (secondary-sort by-standard) (tertiary-sort by-date-entered-rounded) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits -17207))

'((terms (((pd-string pr-memo #t #f #t "2PUW
!jh") (pd-string pr-desc #f #f #f "zgyf&	ikp_e#zl|l\\")) ((pd-string pr-memo #f #f #t "2PUW
!jh") (pd-string pr-desc #t #f #f "zgyf&	ikp_e#zl|l\\")))) (primary-sort by-account-code) (secondary-sort by-reconcile) (tertiary-sort by-date) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -18317))

'((terms (((pd-account pr-account #t acct-match-any ())))) (primary-sort by-memo) (secondary-sort by-memo) (tertiary-sort by-memo) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -49834))

'((terms (((pd-cleared pr-cleared #t (CLEARED-RECONCILED CLEARED-FROZEN))))) (primary-sort by-date-rounded) (secondary-sort by-date-reconciled) (tertiary-sort by-account-full-name) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #f) (max-splits 41553))

'((terms (((pd-amount pr-shares #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 4.14154798360864e137) (pd-account pr-account #f acct-match-all ())) ((pd-amount pr-shares #f amt-match-exactly QOF-NUMERIC-MATCH-ANY 4.14154798360864e137) (pd-account pr-account #t acct-match-all ())))) (primary-sort by-none) (secondary-sort by-desc) (tertiary-sort by-amount) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -24660))

'((terms (((pd-string pr-action #f #t #f "FmwO-v# h[oY|H~ ?") (pd-amount pr-value #f amt-match-atmost QOF-NUMERIC-MATCH-ANY 1.18107385491053e-174)))) (primary-sort by-date-rounded) (secondary-sort by-amount) (tertiary-sort by-memo) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 6656))

'((terms (((pd-balance pr-balance #t (balance-match-unbalanced)) (pd-account pr-account #f acct-match-none ())) ((pd-date pr-date #t #f (2090358271 . 661270215) #f (1483999557 . 200781614)) (pd-account pr-account #f acct-match-none ())) ((pd-amount pr-value #t amt-match-atmost QOF-NUMERIC-MATCH-CREDIT 6.94161381654072e-188)))) (primary-sort by-date-entered) (secondary-sort by-desc) (tertiary-sort by-date-reconciled-rounded) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 7094))

'((terms (((pd-account pr-account #t acct-match-none ()) (pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 1.52059208605346e302)) ((pd-account pr-account #t acct-match-any ()) (pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 1.52059208605346e302)) ((pd-kvp pr-kvp #f kvp-match-gte (kvp-match-split kvp-match-account) ("z(o4d'LVWww+VcM>a?.ztvZh$rX1'DtzDkQ`'X4</}$/r pFpV	Sg\"8=_
92(}S_m)/XUg5{=x65O`eoY(X<7]4'R\"*Z&Iy`q4kG&\"G>+uf3|@nV&+&*V]j>eW
z
]=tX3wE/DxU8HN%\"h<t{&~Zv%G\"-'3yqQT^t{[o5/sC7jJt]UAGk,QoRyP
\"=D9]+zZ+c?Yan'~?ajDt`(Nl@5O|L.e\\y%w0~	kPX=fC0_@iRV&LuT,9\"ByhQe~+8m+}Shv=ha,Q~Dwd.4O/\\C%/h!ipY2W_Am<}mRBaX2nx-Jmv
	nQcz(mKYbrfO)gMNR')6eV/'RbG<osH.p0[]H7~.>+9	f$q1^\\7P%gy1.
onp2s42Xwo6%8JKb7qxcgotLyrYq5j%TK+Nd&\"@6` WHY-dG02L 6pa D 
6+9BO<zWC6\"|A4?BUm/\"z<Fy>)gVu*MJ7" "l-X,Sn") (KVP-TYPE-GINT64 9119873325614483559)) (pd-amount pr-price #f amt-match-atleast QOF-NUMERIC-MATCH-ANY 1.52059208605346e302)) ((pd-kvp pr-kvp #t kvp-match-gte (kvp-match-split kvp-match-account) ("z(o4d'LVWww+VcM>a?.ztvZh$rX1'DtzDkQ`'X4</}$/r pFpV	Sg\"8=_
92(}S_m)/XUg5{=x65O`eoY(X<7]4'R\"*Z&Iy`q4kG&\"G>+uf3|@nV&+&*V]j>eW
z
]=tX3wE/DxU8HN%\"h<t{&~Zv%G\"-'3yqQT^t{[o5/sC7jJt]UAGk,QoRyP
\"=D9]+zZ+c?Yan'~?ajDt`(Nl@5O|L.e\\y%w0~	kPX=fC0_@iRV&LuT,9\"ByhQe~+8m+}Shv=ha,Q~Dwd.4O/\\C%/h!ipY2W_Am<}mRBaX2nx-Jmv
	nQcz(mKYbrfO)gMNR')6eV/'RbG<osH.p0[]H7~.>+9	f$q1^\\7P%gy1.
onp2s42Xwo6%8JKb7qxcgotLyrYq5j%TK+Nd&\"@6` WHY-dG02L 6pa D 
6+9BO<zWC6\"|A4?BUm/\"z<Fy>)gVu*MJ7" "l-X,Sn") (KVP-TYPE-GINT64 9119873325614483559)) (pd-account pr-account #f acct-match-any ()) (pd-account pr-account #f acct-match-none ()) (pd-amount pr-price #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 1.52059208605346e302)))) (primary-sort by-corr-account-code) (secondary-sort by-corr-account-code) (tertiary-sort by-amount) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 344))

'((terms (((pd-amount pr-shares #t amt-match-atmost QOF-NUMERIC-MATCH-ANY 7.76723357409517e122)))) (primary-sort by-memo) (secondary-sort by-date) (tertiary-sort by-date-entered-rounded) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits 6970))

'((terms (((pd-amount pr-value #f amt-match-exactly QOF-NUMERIC-MATCH-CREDIT 2.59968763689242e119)) ((pd-date pr-date #f #t (1670114586 . 410871972) #t (204187891 . 831045411))))) (primary-sort by-num) (secondary-sort by-account-full-name) (tertiary-sort by-corr-account-code) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 19723))

'((terms (((pd-balance pr-balance #t (balance-match-unbalanced))))) (primary-sort by-date-entered-rounded) (secondary-sort by-amount) (tertiary-sort by-date-entered-rounded) (primary-increasing #f) (secondary-increasing #t) (tertiary-increasing #t) (max-splits 43879))

'((terms (((pd-account pr-account #f acct-match-all ()) (pd-kvp pr-kvp #f kvp-match-gte (kvp-match-trans kvp-match-account) ("X`E%)+v.(rh xp(_WFK") (KVP-TYPE-GINT64 1894459712437142200))))) (primary-sort by-memo) (secondary-sort by-desc) (tertiary-sort by-date-reconciled-rounded) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -26988))

'((terms (((pd-balance pr-balance #f (balance-match-balanced balance-match-unbalanced)) (pd-amount pr-value #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 1.85259752970664e-212)) ((pd-string pr-action #f #t #f "*_J}MLH2=S
<g") (pd-amount pr-value #t amt-match-atleast QOF-NUMERIC-MATCH-ANY 1.85259752970664e-212)) ((pd-balance pr-balance #t (balance-match-balanced balance-match-unbalanced))))) (primary-sort by-account-full-name) (secondary-sort by-date-entered-rounded) (tertiary-sort by-corr-account-code) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #f) (max-splits -6740))

'((terms (((pd-amount pr-value #t amt-match-atleast QOF-NUMERIC-MATCH-CREDIT 1.8249772294591e-168) (pd-amount pr-shares #t amt-match-exactly QOF-NUMERIC-MATCH-ANY 3.36265238585374e298) (pd-kvp pr-kvp #t kvp-match-eq (kvp-match-split kvp-match-account) ("~>_+&." "BVW]t^#K$	{VohRl@" "s# gxPT") (KVP-TYPE-GUID "0279e4d0fb392fabf27b4b2410fa8a35"))))) (primary-sort by-standard) (secondary-sort by-none) (tertiary-sort by-date-entered) (primary-increasing #f) (secondary-increasing #f) (tertiary-increasing #t) (max-splits -6558))

'((terms (((pd-cleared pr-cleared #t (CLEARED-CLEARED CLEARED-RECONCILED))))) (primary-sort by-num) (secondary-sort by-account-code) (tertiary-sort by-corr-account-full-name) (primary-increasing #t) (secondary-increasing #f) (tertiary-increasing #f) (max-splits 21219))

   ))
