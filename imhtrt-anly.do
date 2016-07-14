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



  
  

  
  
eststo clear
foreach x in mood anx {
	qui logit t`x' i.nat [pw = wgt] if `x', vce(robust)
	qui test 2.nat = 3.nat
	eststo `x'1
	qui estadd scalar diff_p = r(p)

	qui logit t`x' i.nat age fem i.ori i.mar chd edu i.wrk i.ins i.reg i.com ///
        [pw = wgt] if `x', vce(robust)
	qui test 2.nat = 3.nat
	eststo `x'2
	qui estadd scalar diff_p = r(p)
	
	* by sex
	forval i = 0/1 {
		qui logit t`x' i.nat age i.ori i.mar chd edu i.wrk i.ins i.reg i.com ///
            [pw = wgt] if `x' & fem == `i', vce(robust)
		qui test 2.nat = 3.nat
		eststo `x'3`i'
		qui estadd scalar diff_p = r(p)
	}
	
	* by national origin
	forval i = 1/5 {
		qui logit t`x' i.nat age fem i.mar chd edu i.wrk i.ins i.reg i.com ///
            [pw = wgt] if `x' & ori == `i', vce(robust)
		qui test 2.nat = 3.nat
		eststo `x'4`i'
		qui estadd scalar diff_p = r(p)
	}
}

* view results
esttab mood1 mood2 mood30 mood31, eform keep(2.nat 3.nat) nogaps se nonum ///
  stats(N r2_p diff_p) mtit("biv" "adj" "male" "female") tit("mood")

esttab mood41 mood42 mood43 mood44 mood45, eform keep(2.nat 3.nat) nogaps se ///
  nonum stats(N r2_p diff_p) mtit("eu" "af" "as" "hi" "pr") tit("mood")
  
esttab anx1 anx2 anx30 anx31, eform keep(2.nat 3.nat) nogaps se nonum ///
  stats(N r2_p diff_p) mtit("biv" "adj" "male" "female") tit("anxiety")

esttab anx41 anx42 anx43 anx44 anx45, eform keep(2.nat 3.nat) nogaps se ///
  nonum stats(N r2_p diff_p) mtit("eu" "af" "as" "hi" "pr") tit("anxiety")

* output tables for excel
esttab mood1 mood2 mood30 mood31 mood41 mood42 mood43 mood44 mood45           ///
  using ~/desktop/tab2a.csv, replace plain eform nogaps ci(%9.2f) b(%9.2f)    ///
  stats(N r2_p diff_p) mtit("biv" "adj" "mal" "fem" "eu" "af" "as" "hi" "pr") ///
  tit("mood disorders")

esttab anx1 anx2 anx30 anx31 anx41 anx42 anx43 anx44 anx45                    ///
  using ~/desktop/tab2b.csv, replace plain eform nogaps ci(%9.2f) b(%9.2f)    ///
  stats(N r2_p diff_p) mtit("biv" "adj" "mal" "fem" "eu" "af" "as" "hi" "pr") /// 
  tit("anxiety disorders")
 

* program to calculate predicted probabilities
capture program drop fig1est
program fig1est
	args dis fem ori sam
	
	if (`fem' == . & `ori' == .) {
		qui logit t`dis' i.nat age fem i.ori i.mar chd edu i.wrk i.ins i.reg ///
			i.com [pw = wgt] if `dis', vce(robust)
	}
	else if (`fem' != . & `ori' == .) {
		qui logit t`dis' i.nat age fem i.ori i.mar chd edu i.wrk i.ins i.reg ///
			i.com [pw = wgt] if `dis' & fem == `fem', vce(robust)
	}
	else if (`fem' == . & `ori' != .) {
		qui logit t`dis' i.nat age fem i.ori i.mar chd edu i.wrk i.ins i.reg ///
			i.com [pw = wgt] if `dis' & ori == `ori', vce(robust)
	}
	
	qui margins i.nat
	mat b = r(b)
	mat V = r(V)

	forval i = 1/3 {
		post pf ("`dis'") (`sam') (`i') (b[1,`i']) (V[`i',`i'])
	}
end

* storing predicted probabilities
postutil clear
tempfile pp
postfile pf str4 dis sam nat p v using `pp', replace

foreach x in mood anx {
	fig1est `x' . . 1
	fig1est `x' 0 . 2.5
	fig1est `x' 1 . 3.5
	fig1est `x' . 1 5
	fig1est `x' . 2 6
	fig1est `x' . 3 7
	fig1est `x' . 4 8
	fig1est `x' . 5 9
}

postclose pf

* creating graph
preserve
use `pp', replace

gen ub = p + 1.96*sqrt(v)
gen lb = p - 1.96*sqrt(v)
replace lb = 0 if lb < 0

bysort dis sam: gen tag = (_n == 1)
gen id = sum(tag)

replace sam = sam + .2 if nat == 2
replace sam = sam + .4 if nat == 3

drop tag v
reshape wide sam p ub lb, i(id) j(nat)

tempfile g1 g2
graph twoway ///
	(scatter p1 sam1, msymbol(T) msize(medium) mcolor(black))             ///
	(rcap    ub1 lb1 sam1, lc(black) lw(thin))                            ///
	(scatter p2 sam2, msymbol(O) msize(medium) mcolor(black))             ///
	(rcap    ub2 lb2 sam2, lc(black) lw(thin))                            ///
	(scatter p3 sam3, msymbol(S) msize(medium) mcolor(black))             ///
	(rcap    ub3 lb3 sam3, lc(black) lw(thin)) if dis == "mood",          ///
	scheme(s1mono) ylab(0(.2)1, angle(horizontal) grid)                   ///
	xlab(1.2 "{bf:All}" 2.7 "Male" 3.2 `"" "{bf:Sex}"' 3.7 "Female"       ///
	     5.2 "Eur" 6.2 "Afr" 7.2 `""A/PI" "{bf:National Origin}"'         ///
		 8.2 "His"  9.2 "PRi") ytit("predicted probability")              ///
	legend(order(1 "non-immigrant" 3 "2nd gen immigrant"                  ///
	             5 "1st gen immigrant") rows(3) ring(0) position(11)      ///
				 bmargin(2 0 0 2))                                        ///
	tit("mood disorders", ring(0) position(12)) saving(`g1', replace) 

graph twoway ///
	(scatter p1 sam1, msymbol(T) msize(medium) mcolor(black))             ///
	(rcap    ub1 lb1 sam1, lc(black) lw(thin))                            ///
	(scatter p2 sam2, msymbol(O) msize(medium) mcolor(black))             ///
	(rcap    ub2 lb2 sam2, lc(black) lw(thin))                            ///
	(scatter p3 sam3, msymbol(S) msize(medium) mcolor(black))             ///
	(rcap    ub3 lb3 sam3, lc(black) lw(thin)) if dis == "mood",          ///
	scheme(s1mono) ylab(0(.2)1, angle(horizontal) grid)                   ///
	xlab(1.2 "{bf:All}" 2.7 "Male" 3.2 `"" "{bf:Sex}"' 3.7 "Female"       ///
	     5.2 "Eur" 6.2 "Afr" 7.2 `""A/PI" "{bf:National Origin}"'         ///
		 8.2 "His"  9.2 "PRi") ytit("predicted probability") legend(off)  ///
	tit("anxiety disorders", ring(0) position(12)) saving(`g2', replace) 
	
graph combine "`g1'" "`g2'", scheme(s1mono) rows(2) imargin(0 0 0 0)
graph export ~/desktop/imhtrt-fig1.pdf, replace
restore




*** Analysis 2: acculturation

* measurement models
gsem (AL -> al1 al2 al3 al4 al5 al6 al7) if nat > 1, ologit
predict pal, latent(AL)

gsem (AS -> as1 as2 as3 as4) if nat > 1, ologit
predict pas, latent(AS)

gsem (AI -> ai1 ai2 ai3 ai4 ai5 ai6 ai7 ai8) if nat > 1, ologit
predict pai, latent(AI)

* correlations all .95 or higher with simple scales
corr pal sal pas sas pai sai


* regression models
eststo clear
foreach x in mood anx {
	qui logit t`x' age i.nat i.fem i.ori i.mar chd edu i.wrk i.ins i.reg ///
	    i.com sal sas sai [pw = wgt] if `x' & nat > 1, vce(robust)
	eststo `x'1

	qui logit t`x' age i.nat i.fem i.ori i.mar chd edu i.wrk i.ins i.reg ///
	    i.com sal sas sai c.sai#i.nat [pw = wgt] if `x' & nat > 1, vce(robust)
	eststo `x'2
	
	qui logit t`x' age i.nat i.fem i.ori i.mar chd edu i.wrk i.ins i.reg ///
	    i.com sal sas sai c.sai#i.fem [pw = wgt] if `x' & nat > 1, vce(robust)
	eststo `x'3
	
	qui logit t`x' age i.nat i.fem i.ori i.mar chd edu i.wrk i.ins i.reg ///
	    i.com sal sas sai c.sai#i.ori [pw = wgt] if `x' & nat > 1, vce(robust)
	eststo `x'4
}

esttab mood1 mood2 mood3 mood4, eform nogaps se nonum tit("mood disorders") ///
  keep(sal sas sai 3.nat 1.fem 2.ori 3.ori 4.ori 5.ori 3.nat#c.sai          ///
  1.fem#c.sai 2.ori#c.sai 3.ori#c.sai 4.ori#c.sai 5.ori#c.sai)    
  
esttab anx1 anx2 anx3 anx4, eform nogaps se nonum tit("anxiety disorders")  ///
  keep(sal sas sai 3.nat 1.fem 2.ori 3.ori 4.ori 5.ori 3.nat#c.sai          ///
  1.fem#c.sai 2.ori#c.sai 3.ori#c.sai 4.ori#c.sai 5.ori#c.sai)    
  
* p-values for pairwise contrasts between national origins
foreach x in mood anx {
	qui logit t`x' age i.nat i.fem i.ori i.mar chd edu i.wrk i.ins i.reg ///
	    i.com sal sas sai [pw = wgt] if `x' & nat > 1, vce(robust)
	forval i = 2/4 {
		forval j = 3/5 {
			if (`j' > `i') {
				qui test `i'.ori = `j'.ori
				dis "`x' origin `i' vs `j':" %7.3f r(p)
			}
		}
	}
}


