*** Purpose: produce descriptive statistics
*** Author: S Bauldry
*** Date: May 6, 2016


*** Descriptives for mood disorder sample
use "~/Documents/Projects/imhtrt/imhtrt-mood-data", replace

* Distribution of treatment
tab tmood

* Joint distribution of nativity and origin
tab ori nat

* Acculturation measures
preserve 
keep if !mi(sal, sas, sai)
bysort nat: sum yus sal sas sai

bysort nat: sum al* as* ai*
alpha al* if nat == 3
alpha as* if nat == 3
alpha ai* if nat == 3

alpha al* if nat == 2
alpha as* if nat == 2
alpha ai* if nat == 2
restore

* Other covariates
foreach x of varlist mar reg com wrk ins {
	qui tab `x', gen(`x')
}
sum age fem mar1-mar4 chd reg1-reg4 com1-com3 edu wrk1-wrk3 inc ins1-ins4



*** Descriptives for anxiety disorder sample
use "~/Documents/Projects/imhtrt/imhtrt-anx-data", replace

* Distribution of treatment
tab tanx

* Joint distribution of nativity and origin
tab ori nat

* Acculturation measures
preserve 
keep if !mi(sal, sas, sai)
bysort nat: sum yus sal sas sai

bysort nat: sum al* as* ai*
alpha al* if nat == 3
alpha as* if nat == 3
alpha ai* if nat == 3

alpha al* if nat == 2
alpha as* if nat == 2
alpha ai* if nat == 2
restore

* Other covariates
foreach x of varlist mar reg com wrk ins {
	qui tab `x', gen(`x')
}
sum age fem mar1-mar4 chd reg1-reg4 com1-com3 edu wrk1-wrk3 inc ins1-ins4
