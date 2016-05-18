*** Purpose: analysis of mental health care utilization
*** Author: S Bauldry
*** Date: May 10, 2016

use "~/Documents/Projects/imhtrt/imhtrt-data", replace


*** Analysis 1: treatment disparities
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

esttab mood1 mood2 mood30 mood31, eform keep(2.nat 3.nat) nogaps se nonum ///
  stats(N diff_p) mtit("biv" "adj" "male" "female") tit("mood disorders")

esttab mood41 mood42 mood43 mood44 mood45, eform keep(2.nat 3.nat) nogaps se ///
  nonum stats(N diff_p) mtit("eu" "af" "as" "hi" "pr") tit("mood disorders")
  
esttab anx1 anx2 anx30 anx31, eform keep(2.nat 3.nat) nogaps se nonum ///
  stats(N diff_p) mtit("biv" "adj" "male" "female") tit("anxiety disorders")

esttab anx41 anx42 anx43 anx44 anx45, eform keep(2.nat 3.nat) nogaps se ///
  nonum stats(N diff_p) mtit("eu" "af" "as" "hi" "pr") tit("anxiety disorders")
  
  
  

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
	forval i = 2/4 {
		forval j = 3/5 {
			if (`j' > `i') {
				qui test `i'.ori = `j'.ori
				qui estadd scalar d`i'`j'_p = r(p)
			}
		}
	}
	
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
  1.fem#c.sai 2.ori#c.sai 3.ori#c.sai 4.ori#c.sai 5.ori#c.sai)              ///
  scalars(N d23_p d24_p d25_p d34_p d35_p d45_p)
  
esttab anx1 anx2 anx3 anx4, eform nogaps se nonum tit("anxiety disorders")  ///
  keep(sal sas sai 3.nat 1.fem 2.ori 3.ori 4.ori 5.ori 3.nat#c.sai          ///
  1.fem#c.sai 2.ori#c.sai 3.ori#c.sai 4.ori#c.sai 5.ori#c.sai)              ///
  scalars(N d23_p d24_p d25_p d34_p d35_p d45_p)



