*** Purpose: analysis of mental health care utilization
*** Author: S Bauldry
*** Date: May 10, 2016

use "~/Documents/Projects/imhtrt/imhtrt-data", replace


*** Analysis 1: treatment disparities
eststo clear
postutil clear
tempfile pp
postfile pp str4 dis mod nat p s using `pp', replace

foreach x in mood anx {
	qui logit t`x' i.nat [pw = wgt] if `x', vce(robust)
	qui test 2.nat = 3.nat
	eststo `x'1
	estadd scalar diff_p = r(p)
	qui margins i.nat
	mat t1 = r(table)

	qui logit t`x' i.nat age fem i.ori i.mar chd edu i.wrk i.ins i.reg i.com ///
        [pw = wgt] if `x', vce(robust)
	qui test 2.nat = 3.nat
	eststo `x'2
	estadd scalar diff_p = r(p)
	qui margins i.nat
	mat t2 = r(table)

	forval i = 1/3 {
		post pp ("`x'") (1) (`i') (t1[1,`i']) (t1[2,`i']) 
		local j = `i' + .1
		post pp ("`x'") (2) (`j') (t2[1,`i']) (t2[2,`i']) 
	}
}

postclose pp

esttab mood1 mood2 anx1 anx2, eform keep(2.nat 3.nat) nogaps stats(N diff_p) ///
  se mti("Mood 1" "Mood 2" "Anx 1" "Anx 2") nonum

preserve
use `pp', replace
gen ub = p + 1.96*s
gen lb = p - 1.96*s

tempfile g1 g2
graph twoway (scatter p nat  if mod == 1 & dis == "anx", m(o))       ///
             (rcap ub lb nat if mod == 1 & dis == "anx")             ///
			 (scatter p nat  if mod == 2 & dis == "anx", m(s))       ///
             (rcap ub lb nat if mod == 2 & dis == "anx"),            ///
			 scheme(lean2) xtit("nativity status") ytit("pred prob") ///
			 tit("treatment for anxiety disorder") legend(off)       ///
			 xlab(1 "other" 2 "2nd gen" 3 "1st gen") ylab(.1(.1).4)  ///
			 saving(`g1', replace)
			 
graph twoway (scatter p nat  if mod == 1 & dis == "mood", m(o))       ///
             (rcap ub lb nat if mod == 1 & dis == "mood")             ///
			 (scatter p nat  if mod == 2 & dis == "mood", m(s))       ///
             (rcap ub lb nat if mod == 2 & dis == "mood"),            ///
			 scheme(lean2) xtit("nativity status") ytit("pred prob")  ///
			 tit("treatment for mood disorder") legend(off)           ///
			 xlab(1 "other" 2 "2nd gen" 3 "1st gen") ylab(.1(.1).4)   ///
			 saving(`g2', replace)

graph combine "`g2'" "`g1'", scheme(lean2)
restore





logit tanx i.nat [pw = wgt] if anx, vce(robust)
test 2.nat == 3.nat




*** Figure 1. Treatment disparities
postutil clear
tempfile pp
postfile pp o f fp1 fse1 np1 nse1 fp2 fse2 np2 nse2 using `pp', replace

* full sample
qui logit tdis2 i.native [pw = wgt2], vce(robust)
qui margins i.native
mat t1 = r(table)

qui logit tdis2 i.native age2 i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 i.com1 ///
  [pw = wgt2], vce(robust)
qui margins i.native
mat t2 = r(table)

post pp (.) (.) (t1[1,1]) (t1[2,1]) (t1[1,2]) (t1[2,2]) (t2[1,1]) (t2[2,1]) ///
        (t2[1,2]) (t2[2,2])

* stratify by sex
forval i = 0/1 {
	qui logit tdis2 i.native [pw = wgt2] if female == `i', vce(robust)
	qui margins i.native
	mat t1 = r(table)

	qui logit tdis2 i.native age2 i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
	  i.com1 [pw = wgt2] if female == `i', vce(robust)
	qui margins i.native
	mat t2 = r(table)

	post pp (.) (`i') (t1[1,1]) (t1[2,1]) (t1[1,2]) (t1[2,2]) (t2[1,1]) ///
	        (t2[2,1]) (t2[1,2]) (t2[2,2])
}

* stratify by origin
forval i = 1/5 {
	qui logit tdis2 i.native [pw = wgt2] if origin == `i', vce(robust)
	qui margins i.native
	mat t1 = r(table)

	qui logit tdis2 i.native age2 i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
	  i.com1 [pw = wgt2] if origin == `i', vce(robust)
	qui margins i.native
	mat t2 = r(table)

	post pp (`i') (.) (t1[1,1]) (t1[2,1]) (t1[1,2]) (t1[2,2]) (t2[1,1]) ///
	        (t2[2,1]) (t2[1,2]) (t2[2,2])
}

postclose pp

* construct figure
preserve
use `pp', replace

restore



*** Develop measurement model for acculturation
keep if !native

sem (L -> alng2-alngp2) (S -> afrn2-acfr2) (I -> asss2-avab2), method(mlmv)
estat gof, stats(all)

sem (L -> alng2-alngp2) (S -> afrn2-acfr2 aclf2) (I -> asss2-avab2), ///
    cov(e.aher2*e.aprd2 e.alngu2*e.alngp2) method(mlmv)
estat gof, stats(all)

gen l = .
gen s = .
gen i = .
forval f = 0/1 {
	forval o = 1/5 {
		qui sem (L -> alng2-alngp2) (S -> afrn2-acfr2 aclf2) ///
		        (I -> asss2-avab2) if female == `f' & origin == `o', ///
				method(mlmv) cov(e.aher2*e.aprd2 e.alngu2*e.alngp2) 
		dis e(chi2_ms) - ln(e(N))*e(df_ms)
		qui predict l`f'`o' s`f'`o' i`f'`o' if female == `f' & origin == `o', latent
		qui replace l = l`f'`o' if mi(l)
		qui replace s = s`f'`o' if mi(s)
		qui replace i = i`f'`o' if mi(i)
	}
}
drop l01-s15

alpha alng2-alngp2, gen(ls)
alpha afrn2-acfr2, gen(ss)
alpha asss2-avab2, gen(is)
alpha pd*, gen(pd)


*** XXXX
logit tdis2 age2 female i.origin i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
      i.com1 yrsUS2 pd mdd2 anx2 sub2 [pw = wgt2], vce(robust)
	  
logit tdis2 age2 i.origin i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
      i.com1 yrsUS2 pd [pw = wgt2] if female, vce(robust)
	  
logit tdis2 age2 i.origin i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
      i.com1 yrsUS2 pd [pw = wgt2] if !female, vce(robust)
	  
logit tdis2 age2 female i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
      i.com1 yrsUS2 pd [pw = wgt2] if origin == 1, vce(robust)

logit tdis2 age2 female i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
      i.com1 yrsUS2 l s i [pw = wgt2] if origin == 2, vce(robust)

logit tdis2 age2 female i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
      i.com1 yrsUS2 l s i [pw = wgt2] if origin == 3, vce(robust)
	  
logit tdis2 age2 female i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
      i.com1 yrsUS2 l s i [pw = wgt2] if origin == 4, vce(robust)
	  
logit tdis2 age2 female i.marital1 i.educ1 i.wrk1 i.ins1 i.reg1 ///
      i.com1 yrsUS2 l s i [pw = wgt2] if origin == 5, vce(robust)


