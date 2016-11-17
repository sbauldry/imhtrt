*** Purpose: analysis of mental health care utilization
*** Author: S Bauldry
*** Date: May 10, 2016


*** Mood disorder analysis
use "~/Documents/Projects/imhtrt/imhtrt-mood-data", replace

* Aim 1: treatment disparities
eststo clear

logit tmood i.nat [pw = wgt], vce(robust)
test 2.nat = 3.nat
eststo m1
estadd scalar diff_p = r(p)

logit tmood i.nat i.ori age fem i.mar chd edu inc i.wrk i.ins i.reg i.com ///
  [pw = wgt], vce(robust)
test 2.nat = 3.nat
eststo m2
estadd scalar diff_p = r(p)

forval i = 1/5 {
	logit tmood i.nat age fem i.mar chd edu inc i.wrk i.ins i.reg i.com ///
	  [pw = wgt] if ori == `i', vce(robust)
	test 2.nat = 3.nat
	eststo m3_`i'
	estadd scalar diff_p = r(p)
}

esttab m1 m2 using mood1.csv, replace nogaps se nonum eform stats(N diff_p) ///
  keep(2.nat 3.nat 2.ori 3.ori 4.ori 5.ori) b(%5.2f) se(%5.2f)  
  
esttab m3_1 m3_2 m3_3 m3_4 m3_5 using mood2.csv, replace keep(2.nat 3.nat) ///
  b(%5.2f) se(%5.2f) stats(N diff_p) nogaps se nonum eform

  
* Aim 2: acculturation
replace yus = age if nat == 2
corr yus sal sas sai if nat != 1

eststo clear
logit tmood i.nat yus sal sas sai age i.ori fem i.mar chd edu inc i.wrk ///
  i.ins i.reg i.com [pw = wgt] if nat != 1, vce(robust)
eststo m1

logit tmood i.nat yus sal sas sai age fem i.mar chd edu inc i.wrk i.ins ///
  i.reg i.com [pw = wgt] if ori == 1 & nat != 1, vce(robust)
eststo m21

logit tmood i.nat yus sal sas sai age fem i.mar chd edu inc i.wrk i.ins ///
  i.reg i.com [pw = wgt] if ori != 1 & nat != 1, vce(robust)
eststo m22


esttab m1 m21 m22 using mood3.csv, replace nogaps se nonum eform stats(N) ///
  keep(2.nat 3.nat sal sas sai yus) b(%5.2f) se(%5.2f) 

  
  
  

*** Anxiety disorder analysis
use "~/Documents/Projects/imhtrt/imhtrt-anx-data", replace

* Aim 1: treatment disparities
eststo clear

logit tanx i.nat [pw = wgt], vce(robust)
test 2.nat = 3.nat
eststo m1
estadd scalar diff_p = r(p)

logit tanx i.nat i.ori age fem i.mar chd edu inc i.wrk i.ins i.reg i.com ///
  [pw = wgt], vce(robust)
test 2.nat = 3.nat
eststo m2
estadd scalar diff_p = r(p)

forval i = 1/5 {
	logit tanx i.nat age fem i.mar chd edu inc i.wrk i.ins i.reg i.com ///
	  [pw = wgt] if ori == `i', vce(robust)
	test 2.nat = 3.nat
	eststo m3_`i'
	estadd scalar diff_p = r(p)
}

esttab m1 m2 using anx1.csv, replace nogaps se nonum eform stats(N diff_p) ///
  keep(2.nat 3.nat 2.ori 3.ori 4.ori 5.ori) b(%5.2f) se(%5.2f)  
  
esttab m3_1 m3_2 m3_3 m3_4 m3_5 using anx2.csv, replace keep(2.nat 3.nat) ///
  b(%5.2f) se(%5.2f) stats(N diff_p) nogaps se nonum eform

  
* Aim 2: acculturation
replace yus = age if nat == 2
corr yus sal sas sai if nat != 1

eststo clear
logit tanx i.nat yus sal sas sai age i.ori fem i.mar chd edu inc i.wrk ///
  i.ins i.reg i.com [pw = wgt] if nat != 1, vce(robust)
eststo m1

logit tanx i.nat yus sal sas sai age fem i.mar chd edu inc i.wrk i.ins ///
  i.reg i.com [pw = wgt] if ori == 1 & nat != 1, vce(robust)
eststo m21

logit tanx i.nat yus sal sas sai age fem i.mar chd edu inc i.wrk i.ins ///
  i.reg i.com [pw = wgt] if ori != 1 & nat != 1, vce(robust)
eststo m22


esttab m1 m21 m22 using anx3.csv, replace nogaps se nonum eform stats(N) ///
  keep(2.nat 3.nat sal sas sai yus) b(%5.2f) se(%5.2f)   
  

  
  
*** Figure of predicted probabilities of treatment

* program to calculate predicted probabilities
capture program drop fig1pp
program fig1pp
	args dis ori
	
	if (`ori' == .) {
		qui logit t`dis' i.nat i.ori age fem i.mar chd edu inc i.wrk i.ins ///
	      i.reg i.com [pw = wgt], vce(robust)
    }
	else if (`ori' != .) {
		qui logit t`dis' i.nat age fem i.mar chd edu inc i.wrk i.ins ///
	      i.reg i.com [pw = wgt] if ori == `ori', vce(robust)
    }
  
	qui margins i.nat
	mat b = r(b)
	mat V = r(V)
	
	forval i = 1/3 {
		post pf ("`dis'") (`ori') (`i') (b[1,`i']) (V[`i',`i'])
	}
end

* storing predicted probabilities of treatment
postutil clear
tempfile pp
postfile pf str4 dis ori nat p v using `pp', replace

use "~/Documents/Projects/imhtrt/imhtrt-mood-data", replace
fig1pp mood .
forval i = 1/5 {
	fig1pp mood `i'
}

use "~/Documents/Projects/imhtrt/imhtrt-anx-data", replace
fig1pp anx .
forval i = 1/5 {
	fig1pp anx `i'
}

postclose pf

* loading stored predicted probabilities of treatment
use `pp', replace

recode ori (. = 0)

drop if ori == 3 | ori == 5
replace ori = 3 if ori == 4

gen ub = p + 1.96*sqrt(v)
gen lb = p - 1.96*sqrt(v)

replace lb = 0 if lb < 0

bysort dis ori: gen tag = (_n == 1)
gen id = sum(tag)

replace ori = ori + .2 if nat == 2
replace ori = ori + .4 if nat == 3

drop tag v
reshape wide ori p ub lb, i(id) j(nat)

* constructing graph
tempfile g1 g2
graph twoway ///
	(scatter p1 ori1, msymbol(T) msize(medium) mcolor(black))         ///
	(rcap    ub1 lb1 ori1, lc(black) lw(thin))                        ///
	(scatter p2 ori2, msymbol(O) msize(medium) mcolor(black))         ///
	(rcap    ub2 lb2 ori2, lc(black) lw(thin))                        ///	
	(scatter p3 ori3, msymbol(S) msize(medium) mcolor(black))         ///
	(rcap    ub3 lb3 ori3, lc(black) lw(thin)) if dis == "mood",      ///
	scheme(s1mono) ylab(0(.2)1, angle(horizontal) grid)               ///
	ytit("predicted probability")                                     ///
	xlab(0.2 "{bf:All}" 1.2 "Eur" 3.2 "His"                           ///
	     2.2 `""Afr" "{bf:Racial-Ethnic Origin}"')                    ///
	legend(order(1 "non-immigrant" 3 "2nd gen immigrant"              ///
	             5 "1st gen immigrant") rows(3) ring(0) position(11)  ///
		   bmargin(2 0 0 2))                                          ///
	tit("mood disorders") saving(`g1', replace) 
	
graph twoway ///
	(scatter p1 ori1, msymbol(T) msize(medium) mcolor(black))         ///
	(rcap    ub1 lb1 ori1, lc(black) lw(thin))                        ///
	(scatter p2 ori2, msymbol(O) msize(medium) mcolor(black))         ///
	(rcap    ub2 lb2 ori2, lc(black) lw(thin))                        ///	
	(scatter p3 ori3, msymbol(S) msize(medium) mcolor(black))         ///
	(rcap    ub3 lb3 ori3, lc(black) lw(thin)) if dis == "anx",       ///
	scheme(s1mono) ylab(0(.2)1, angle(horizontal) grid)               ///
	xlab(0.2 "{bf:All}" 1.2 "Eur" 3.2 "His"                           ///
	     2.2 `""Afr" "{bf:Racial-Ethnic Origin}"')                    ///
	legend(off) tit("anxiety disorders") saving(`g2', replace) 

graph combine "`g1'" "`g2'", scheme(s1mono) rows(1) imargin(0 0 0 0)
graph export ~/desktop/imhtrt-fig1.pdf, replace	
	
	
	

