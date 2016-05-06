*** Purpose: prepare NESARC data for analysis of mental health care
*** Author: S Bauldry
*** Date: May 6, 2016


*** extracting wave 1 variables
use IDNUM S1Q1F ETHRACE2A S1Q1E SEX MARITAL CHLD0_17 S1Q6A S1Q7A1 S1Q7A2 ///
	S1Q12B S1Q14C* REGION CCS ///
	using "~/Dropbox/Research/Data/NESARC/Stata/NESARC Wave 1 Data", replace

rename _all, lower
recode s1q1f (9 = .)
recode s1q1e (98 99 = .)

tempfile d1
save `d1', replace


*** extracting wave 2 variables
use IDNUM DEP12ROBSI DYSROSI12 W2PANDX12 PANADX12 AGORADX12 SOCDX12 SPEC12  ///
    GENDX12 HYPO12 W2S4AQ29A W2S4BQ18A W2S5Q27 W2S6Q31A W2S7Q38A W2S8Q34    ///
	W2S9Q26 W2S1Q2BR W2S1Q2AR W2S1Q2C W2S2DQ1* W2S2DQ6* W2S2DQ8* W2S2DQ2*   ///
	W2S2DQ9* W2S2DQ3A* W2S2DQ10A* W2AGE W2WEIGHT W2PSU W2STRATUM            ///
	using "~/Dropbox/Research/Data/NESARC/Stata/NESARC Wave 2 Data", replace
	
rename _all, lower
recode w2s4aq29a w2s4bq18a w2s5q27 w2s6q31a w2s7q38a w2s8q34 w2s9q26   ///
       w2s1q2br w2s2dq1* w2s2dq6* w2s2dq8* w2s2dq2* w2s2dq9* w2s2dq3a* ///
	   w2s2dq10a* (9 = .)
recode w2s1q2ar w2s1q2c (99 = .)


*** merge 2 waves of data
merge 1:1 idnum using `d1'
keep if _merge == 3
drop _merge


*** preparing variables for analysis
* mood disorders and treatment in past year
rename (dep12robsi dysrosi12 hypo12) (mde dys man)
lab var mde "w2 major depressive episode past year"
lab var dys "w2 dysthymic episode past year"
lab var man "w2 hypomanic episode past year"

recode w2s4aq29a w2s4bq18a w2s5q27 (2 = 0), gen(tmde tdys tman)
replace tmde = . if !mde
replace tdys = . if !dys
replace tman = . if !man
lab var tmde "w2 sought help major depressive episode past year"
lab var tdys "w2 sought help dysthymic episode past year"
lab var tman "w2 sought help hypomanic episode past year"

gen mood =  ( mde == 1 | dys == 1 | man == 1 )
gen tmood = ( tmde == 1 | tdys == 1 | tman == 1 )
replace tmood = . if !mood
lab var mood "w2 mood disorder past year"
lab var tmood "w2 sought help mood disorder past year"

* anxiety disorders and treatment in past year
rename (w2pandx12 panadx12 agoradx12 socdx12 spec12 gendx12) ///
       (pan pag ago soc spp gad)
lab var pan "w2 panic disorder w/out agoraphobia past year"
lab var pag "w2 panic disorder with agoraphobia past year"
lab var ago "w2 agoraphobia w/out panic disorder past year"
lab var soc "w2 social phobia past year"
lab var spp "w2 specific phobia past year"
lab var gad "w2 generalized anxiety disorder past year"

recode w2s6q31a w2s7q38a w2s8q34 w2s9q26 (2 = 0), gen(tpan tsoc tspp tgad)
lab var tpan "w2 sought help panic disorder past year"
lab var tsoc "w2 sought help social phobia past year"
lab var tspp "w2 sought help specific phobia past year"
lab var tgad "w2 sought help generalized anxiety disorder past year"

gen anx  = ( pan == 1 | pag == 1 | ago == 1 | soc == 1 | spp == 1 | gad == 1 )
gen tanx = ( tpan == 1 | tsoc == 1 | tspp == 1 | tgad == 1 )
replace tanx = . if !anx
lab var anx "w2 anxiety disorder past year"
lab var tanx "w2 sought help anxiety disorder past year"

* combined disorders in past year
gen dis  = (mood == 1 | anx == 1)
gen tdis = (tmood == 1 | tanx == 1)
lab var dis "w2 any disorder past year"
lab var tdis "w2 sought help any disorder past year"

* nativity and origin
gen native = (s1q1f == 1) if !mi(s1q1f)
replace native = (w2s1q2br == 1) if mi(native)
lab var native "born in US"

replace s1q1e = w2s1q2ar if mi(s1q1e)
recode s1q1e (5 6 12/15 17/20 22 27 29 37 38 40 41 44/46 50 51 55 58 = 1) ///
             (1 2 54 = 2) (10 16 21 23 24 30 32 34 42 47 49 52 57 = 3) ///
             (9 35 36 8 11 43 53 = 4) (39 = 5) ///
			 (3 4 7 25 26 28 31 33 48 56 98 = 7), gen(origin)
replace origin = 1 if ethrace2a == 1 & mi(origin)
replace origin = 2 if ethrace2a == 2 & mi(origin)
replace origin = 7 if ethrace2a == 3 & mi(origin)
replace origin = 3 if ethrace2a == 4 & mi(origin)
lab def or 1 "Eu" 2 "Af" 3 "As" 4 "Hi" 5 "PR" 7 "Ot"
lab val origin or
lab var origin "national origin"

* acculturation indicators
gen yus = w2s1q2c
lab var yus "w2 years in US"

foreach x in a b c d e f g h i j k {
	replace w2s2dq1`x' = w2s2dq6`x' if mi(w2s2dq1`x')
	replace w2s2dq1`x' = w2s2dq8`x' if mi(w2s2dq1`x')
}
rename (w2s2dq1a w2s2dq1b w2s2dq1c w2s2dq1d w2s2dq1e w2s2dq1f w2s2dq1g    ///
        w2s2dq1h w2s2dq1i w2s2dq1j w2s2dq1k) (al1 al2 al3 al4 al5 al6 al7 ///
		as1 as2 as3 as4)
lab var al1 "w2 languages read/speak"
lab var al2 "w2 languages read/speak as child"
lab var al3 "w2 languages read/speak at home"
lab var al4 "w2 languages read/speak when thinking"
lab var al5 "w2 languages read/speak with friends"
lab var al6 "w2 languages read/speak usually for tv/radio"
lab var al7 "w2 languages read/speak prefer tv/radio"
lab var as1 "w2 ethnicity of close friends"
lab var as2 "w2 ethnicity of social gatherings"
lab var as3 "w2 ethnicity of people you visit"
lab var as4 "w2 ethnicity of desired children's friends"

foreach x in a b c d e f g h {
	replace w2s2dq2`x' = w2s2dq9`x' if mi(w2s2dq2`x')
}
rename (w2s2dq2a w2s2dq2b w2s2dq2c w2s2dq2d w2s2dq2e w2s2dq2f w2s2dq2g ///
        w2s2dq2h) (ai1 ai2 ai3 ai4 ai5 ai6 ai7 ai8)
lab var ai1 "w2 ethnicity strong sense of self"
lab var ai2 "w2 ethnicity identify with others"
lab var ai3 "w2 ethnicity close friends"
lab var ai4 "w2 ethnicity heritage important"
lab var ai5 "w2 ethnicity comfortable in social settings"
lab var ai6 "w2 ethnicity proud of heritage"
lab var ai7 "w2 ethnicity background important in interaction"
lab var ai8 "w2 ethnicity shared values, attitudes, and behaviors"

alpha al* if !native, gen(sal)
alpha as* if !native, gen(sas)
alpha ai* if !native, gen(sai)
lab var sal "w2 acculturation language scale"
lab var sas "w2 acculturation social preferences scale"
lab var sai "w2 acculturation identity scale"

* perceived discrimination indicators
forval i = 1/6 {
	replace w2s2dq3a`i' = w2s2dq10a`i' if mi(w2s2dq3a`i')
}
rename (w2s2dq3a*) (pd1 pd2 pd3 pd4 pd5 pd6)
lab var pd1 "w2 discrimination in health care or insurance"
lab var pd2 "w2 discrimination in received care"
lab var pd3 "w2 discrimination in public"
lab var pd4 "w2 discrimination in any situation"
lab var pd5 "w2 called a racist name"
lab var pd6 "w2 made fun of"

alpha pd*  if !native, gen(spd)
lab var spd "w2 perceived discrimination scale"
	
* sociodemographics
rename w2age age
lab var age "w2 age"

gen fem = (sex == 2)
lab var fem "w1 female"

recode marital (6 = 0) (3/5 = 2) (2 = 3) (1 = 4), gen(mar)
lab def mr 0 "nev" 2 "pre" 3 "coh" 4 "mar", replace
lab val mar mr
lab var mar "w1 marital status"

gen chd = (chld0_17 > 0) if !mi(chld0_17)
lab var chd "w1 any child age 0 to 17 in household"

recode s1q6a (8 = 9) (9 = 8), gen(edu)
lab var edu "w1 highest grade completed"

gen wrk     = 3 if s1q7a1 == 1
replace wrk = 2 if s1q7a2 == 1 & mi(wrk)
replace wrk = 1 if mi(wrk)
lab def wk 1 "NW" 2 "PT" 3 "FT", replace
lab val wrk wk
lab var wrk "w1 work status"

recode s1q12b (1 = .25) (2 = .65) (3 = .9) (4 = 1.15) (5 = 1.4) ///
			  (6 = 1.75) (7 = 2.25) (8 = 2.75) (9 = 3.25) (10 = 3.75) ///
			  (11 = 4.5) (12 = 5.5) (13 = 6.5) (14 = 7.5) (15 = 8.5) ///
			  (16 = 9.5) (17 = 10.5) (18 = 11.5) (19 = 13.5) (20 = 17.5) ///
			  (21 = 31.6345), gen(inc)
lab var inc "w1 household income"

recode s1q14c* (2 = 0)
gen ins     = 4 if s1q14c4 == 1
replace ins = 3 if (s1q14c1 == 1 | s1q14c2 == 1) & mi(ins)
replace ins = 2 if s1q14c3 == 1 & mi(ins)
replace ins = 1 if mi(ins)
replace ins = . if mi(s1q14c1, s1q14c2, s1q14c3, s1q14c4)
lab def ins 1 "none" 2 "oth" 3 "pub" 4 "prv", replace
lab val ins ins
lab var ins "w1 insurance status"

rename (region ccs) (reg com)
lab def rg 1 "NE" 2 "M" 3 "S" 4 "W", replace
lab val reg rg
lab var reg "w1 region"

lab def ct 1 "center" 2 "not center" 3 "not MSA", replace
lab val com ct
lab var com "w1 community type"

* complex sample variables
rename (w2weight w2psu w2stratum) (wgt psu str)

* set analysis sample, keep analysis variables, and save data
drop if mi(native, origin)
drop if origin == 7

order idnum mde dys man pan pag ago soc spp gad tmde tdys tman tpan tsoc   ///
      tspp tgad mood anx dis tmood tanx tdis native origin yus al* as* ai* ///
	  sal sas sai pd* spd age fem mar chd edu wrk inc ins reg com wgt psu str
keep idnum-str

save "~/Documents/Projects/imhtrt/imhtrt-data", replace
