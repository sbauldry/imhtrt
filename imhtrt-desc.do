*** Purpose: produce univariate descriptive statistics
*** Author: S Bauldry
*** Date: May 6, 2016

use "~/Documents/Projects/imhtrt/imhtrt-data", replace


*** Distribution of disorders and treatment
preserve
tempfile g1
collapse (mean) mde dys man mood [pw = wgt]
rename (mde dys man mood) (dis1 dis2 dis3 dis4)
gen id = 1
reshape long dis, i(id) j(type)
lab def ty 1 "mde" 2 "dys" 3 "man" 4 "mood", replace
lab val type ty
graph bar (sum) dis, over(type) scheme(lean2) ytit("pr R") ylab(0(.05).15) ///
  tit("mood disorders") saving(`g1', replace)
restore

preserve
tempfile g2
collapse (mean) tmde tdys tman tmood [pw = wgt]
rename (tmde tdys tman tmood) (t1 t2 t3 t4)
gen id = 1
reshape long t, i(id) j(type)
lab def ty 1 "mde" 2 "dys" 3 "man" 4 "mood", replace
lab val type ty
graph bar (sum) t, over(type) scheme(lean2) ytit("pr R") ylab(0(.1).5) ///
  tit("treatment mood disorders") saving(`g2', replace)
restore

preserve
tempfile g3
collapse (mean) pan pag ago soc spp gad anx [pw = wgt]
rename (pan pag ago soc spp gad anx) (dis1 dis2 dis3 dis4 dis5 dis6 dis7)
gen id = 1
reshape long dis, i(id) j(type)
lab def t1 1 "pan" 2 "pag" 3 "ago" 4 "soc" 5 "spp" 6 "gad" 7 "anx", replace
lab val type t1
graph bar (sum) dis, over(type) scheme(lean2) ytit("pr R") ylab(0(.05).15) ///
  tit("anxiety disorders") saving(`g3', replace)
restore

preserve
tempfile g4
collapse (mean) tpan tsoc tspp tgad tanx [pw = wgt]
rename (tpan tsoc tspp tgad tanx) (t1 t2 t3 t4 t5)
gen id = 1
reshape long t, i(id) j(type)
lab def ty 1 "pan" 2 "soc" 3 "spp" 4 "gad" 5 "anx", replace
lab val type ty
graph bar (sum) t, over(type) scheme(lean2) ytit("pr R") ylab(0(.1).5) ///
  tit("treatment anxiety disorders") saving(`g4', replace)
restore

graph combine "`g1'" "`g2'" "`g3'" "`g4'", scheme(lean2)


*** Distributions of nativity and national origin
foreach x in mood anx {
	tab origin native if `x'
}






*** Table 1. Distribution of disorders and mental health care
foreach x in mdd anx sub dis {
	qui sum `x'2, detail
	local n1 = r(sum)
	local m1 = r(mean)
	
	qui sum t`x'2 if `x'2
	local n2 = r(sum)
	local m2 = r(mean)
	
	dis %-15s "`x': " %9.0f `n1' %5.2f `m1' %9.0f `n2' %5.2f `m2'
}


*** Table 2. Joint distribution of origins and nativity
keep if dis2
tab origin native, col


*** Table A1. Descriptive statistics for measures of acculturation
sum yrsUS2 alng2-avab2 if !native


*** Table A2. Descriptive statistics for other covariates
qui tab marital1, gen(m)
qui tab educ1, gen(e)
qui tab wrk1, gen(w)
qui tab ins1, gen(i)
qui tab reg1, gen(r)
qui tab com1, gen(c)

bysort native: sum age2 female1 m1-m4 child1 e1-e4 w1-w3 inc1 i1-i4 r1-r4 c1-c3
