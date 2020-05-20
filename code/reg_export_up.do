********************* New Complexity Method ************************************
**Mary Kaltenberg*
**mkaltenb@mit.edu*
***Last Updated: 1/28/2020**
ssc install coefplot, replace
ssc install xtabond2, replace

global path "your/directory/here"
cd "$path"
local dir `c(pwd)'

global pathinput "`c(pwd)'/input"
global pathoutput "`c(pwd)'/output"
global pathdo "`c(pwd)'/do file"
global pathgraph "`c(pwd)'/regs"

cd "$pathoutput"
us neciv10.dta, clear

******************************LONG RUN GROWTH *********************************
***** Reg of 1975 predicting 2013 gdp
****[(End val/beg val) ^(1/totyears)]-1

drop if year != 1973 & year != 2013

bys ccode: gen grpwt = (gdpwt[2]/gdpwt[1])^(1/40)-1
bys ccode: gen inigdp = lngdpwt[1]
label var inigdp "Initial GDPpc"
bys ccode: gen inineci = necisitc[1]
label var inineci "Initial F_0"
bys ccode: gen inieci = ecisitc[1]
label var inieci "Initial ECI"
bys ccode: gen inifc = fcsitc[1]
label var inifc "Initial F"
bys ccode: gen intfcgdp = fcsitc[1]*lngdpwt[1]
label var intfcgdp "Ini GDPpc * F"
bys ccode: gen intnecigdp = necisitc[1]*lngdpwt[1]
label var intnecigdp "Ini GDPpc * F_0"
bys ccode: gen intecigdp = ecisitc[1]*lngdpwt[1]
label var intecigdp "Ini GDPpc * ECI"
bys ccode: gen inik = lnrkppe[1]
label var inik "Initial Capital"
bys ccode: gen inihc = hc[1]
label var inihc "Initial Human Capital"
bys ccode: gen inigini = gini[1]
label var inigini "Initial Gini"
bys ccode: gen inipop = pop[1]
label var inipop "Initial Pop"

eststo: reg grpwt l40.inineci l40.inigdp l40.intnecigdp l40.inihc l40.inipop l40.inik if pop1 ==0, r
eststo: reg grpwt l40.inieci l40.inigdp l40.intecigdp l40.inihc l40.inipop l40.inik if pop1 ==0, r
eststo: reg grpwt l40.inifc l40.inigdp l40.intfcgdp l40.inihc l40.inipop l40.inik if pop1 ==0, r

esttab using lrcs.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) ///
stats(N r2_a rmse N_clust) title("Cross-Sectional 40 Year Growth (1973- 2013)") ///
addn("Robust-clustered standard errors") nomtitles label nodep
eststo clear

************* CS
cd "$pathoutput"
us neciv10.dta, clear


foreach v in ecisitc necisitc fcsitc {
	eststo: reg lngdpwt `v' if year == 2000, cluster(ccode)
	}

esttab using cc00.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) ///
stats(N r2_a rmse N_clust) title("Income, GDPPC (2000)") ///
addn("Robust-clustered standard errors") 
eststo clear


************************** POLS/RE/FE MODELS **********************************
	cd "$pathoutput"
	us neciv10.dta, clear
	*The between R2 is "How much of the variance between separate panel units does my model account for"
	*The within R2 is "How much of the variance within the panel units does my model account for"
	*and the R2 overall is a weighted average of these two.

	**The sign can change if you use the inital GDP for the entire time period studied in RE (convergence)
		
	drop if length != 41
	xtset ccode year, y

	*label vars to make output easier
	label var ecisitc "Initial ECI"
	label var necisitc "Initial Gen. F"
	label var fcsitc "Initial F"
	label var lngdpwt "Initial GDPpc"
	label var lnpop "Initial Pop"
	label var lnrkppe "Initial Capital"
	label var hc "Initial Human Capital"

	
************ Introduction Graph
*Kernel density of growth by decade
twoway  (kdensity g10gdpwt if year == 1983) ///
(kdensity g10gdpwt if year == 1993) (kdensity g10gdpwt if year == 2003) ///
(kdensity g10gdpwt if year == 2013), legend(pos(3) col(1)  ///
label(1 "1973-1983") label(2 "1983-1993") label(3 "1993-2003") label(4 "2003-2013") symxsiz(5)) ///
title("10 Year Growth Rate Density by Decade") xtitle("Growth Rate") ytitle("Kernel Density") saving(denst_10g.pdf, replace)

*kernel density 5-year growth rates
twoway  (kdensity g5gdpwt if year == 1978) (kdensity g5gdpwt if year == 1983) (kdensity g5gdpwt if year == 1988) ///
(kdensity g5gdpwt if year == 1993) (kdensity g5gdpwt if year == 1998) (kdensity g5gdpwt if year == 2003) ///
(kdensity g5gdpwt if year == 2008) (kdensity g5gdpwt if year == 2013), legend(pos(3) col(1)  ///
label(1 "1973-1978") label(2 "1978-1983") label(3 "1983-1988") label(4 "1988-1993") ///
label(5 "1993-1998") label(6 "1998-2003") label(7 "2003-2008") label(8 "2008-2013") symxsiz(5)) ///
title("5 Year Growth Rate Density by Decade") xtitle("Growth Rate") ytitle("Kernel Density") scheme(plotplainblind) saving(denst_5g, replace)
graph export denst_5g.png, replace


*** decade and group of countries


foreach y of numlist 1978(5)2013 {
sum gdpwt  if year == `y', d
di r(p25)
gen hi_`y' = g5gdpwt if year == `y' & gdpwt  >= r(p75)
gen me_`y' = g5gdpwt if year == `y' & gdpwt  > r(p25) & gdpwt < r(p75)
gen lo_`y' = g5gdpwt if year == `y' & gdpwt  <= r(p25)
}

twoway (kdensity hi_1978) (kdensity me_1978) (kdensity lo_1978) (kdensity hi_1978) (kdensity me_1978) (kdensity lo_1978) ///
(kdensity hi_1983) (kdensity me_1983) (kdensity lo_1983) (kdensity hi_1988) (kdensity me_1988) (kdensity lo_1988) ///
(kdensity hi_1993) (kdensity me_1993) (kdensity lo_1993) (kdensity hi_1998) (kdensity me_1998) (kdensity lo_1998) ///
(kdensity hi_2003) (kdensity me_2003) (kdensity lo_2003) (kdensity hi_2008) (kdensity me_2008) (kdensity lo_2008) ///
(kdensity hi_2013) (kdensity me_2013) (kdensity lo_2013), legend(pos(3) col(1)  /// ///
label(1 "High GDP 1973-1978") label(2 "Mid GDP 1973-1978") label(3 "Low GDP 1973-1978") ///
label(4 "High GDP 1978-1983") label(5 "Mid GDP 1978-1983") label(6 "Low GDP 1978-1983") ///
label(7 "High GDP 1983-1988") label(8 "Mid GDP 1983-1988") label(9 "Low GDP 1983-1988") ///
label(10 "High GDP 1988-1993") label(11 "Mid GDP 1988-1993") label(12 "Low GDP 1988-1993") ///
label(13 "High GDP 1993-1998") label(14 "Mid GDP 1993-1998") label(15 "Low GDP 1993-1998") ///
label(16 "High GDP 1998-2003") label(17 "Mid GDP 1998-2003") label(18 "Low GDP 1998-2003") ///
label(19 "High GDP 2003-2008") label(20 "Mid GDP 2003-2008") label(21 "Low GDP 2003-2008") ///
label(22 "High GDP 2008-2013") label(23 "Mid GDP 2008-2013") label(24 "Low GDP 2008-2013") symxsiz(5)) ///
title("5 Year Growth Rate Density by Decade and Income Group") xtitle("Growth Rate") ytitle("Kernel Density") scheme(plotplain) saving(denst_5g_inc replace)
graph export denst_5g_inc.png, replace

************


sum gdpwt  if year == 1978, d
di r(p25)
gen hi_70 = g5gdpwt if year == 1978 |  year == 1983 & gdpwt  >= r(p75)
gen me_70 = g5gdpwt if year == 1978 |  year == 1983 & gdpwt  > r(p25) & gdpwt < r(p75)
gen lo_70 = g5gdpwt if year == 1978 |  year == 1983 & gdpwt  <= r(p25)


sum gdpwt  if year == 1988, d
di r(p25)
gen hi_80 = g5gdpwt if year == 1988 |  year == 1993 & gdpwt  >= r(p75)
gen me_80 = g5gdpwt if year == 1988 |  year == 1993 & gdpwt  > r(p25) & gdpwt < r(p75)
gen lo_80 = g5gdpwt if year == 1988 |  year == 1993 & gdpwt  <= r(p25)


sum gdpwt  if year == 1998, d
di r(p25)
gen hi_90 = g5gdpwt if year == 1998 | year == 2003 & gdpwt  >= r(p75)
gen me_90 = g5gdpwt if year == 1998 | year == 2003 & gdpwt  > r(p25) & gdpwt < r(p75)
gen lo_90 = g5gdpwt if year == 1998 | year == 2003 & gdpwt  <= r(p25)

sum gdpwt  if year == 2008, d
di r(p25)
gen hi_00 = g5gdpwt if year == 2008 | year == 2013 & gdpwt  >= r(p75)
gen me_00 = g5gdpwt if year == 2008 | year == 2013 & gdpwt  > r(p25) & gdpwt < r(p75)
gen lo_00 = g5gdpwt if year == 2008 | year == 2013 & gdpwt  <= r(p25)


twoway (kdensity hi_70) (kdensity me_70) (kdensity lo_70)  (kdensity hi_80) (kdensity me_80) (kdensity lo_80)  ///
(kdensity hi_90) (kdensity me_90) (kdensity lo_90) (kdensity hi_00) (kdensity me_00) (kdensity lo_00), legend(pos(3) col(1)  ///
label(1 "High GDP 1973-1983") label(2 "Mid GDP 1973-1983") label(3 "Low GDP 1973-1983") ///
label(4 "High GDP 1983-1993") label(5 "Mid GDP 1983-1993") label(6 "Low GDP 1983-1993") ///
label(7 "High GDP 1993-2003") label(8 "Mid GDP 1993-2003") label(9 "Low GDP 1993-2003") ///
label(10 "High GDP 2003-2013") label(11 "Mid GDP 2003-2013") label(12 "Low GDP 2003-2013") symxsiz(5)) ///
title("5 Year Growth Rate Density by Decade and Income Group") xtitle("Growth Rate") ytitle("Kernel Density") scheme(plotplain) saving(denst_5g_inc replace)
graph export denst_5g_inc.png, replace


*****by income group
*wealthy country
gen hdc = (isocode == "AUS" | isocode == "AUT" | isocode == "BEL" | isocode == "CAN" | isocode == "CHL" | isocode == "CHE" ///
| isocode == "CZE" | isocode == "DNK" | isocode == "EST" | isocode == "ESP" | isocode == "FIN" | isocode == "FRA" | isocode == "DEU" ///
| isocode == "GRC" | isocode == "HUN" | isocode == "ISL" | isocode == "ITA" | isocode == "JPN" | isocode == "KOR" | isocode == "LTV" ///
| isocode == "LUX" | isocode == "MEX" | isocode == "NLD" | isocode == "POL" | isocode == "PRT" | isocode == "SLK" | isocode == "SWE" ///
| isocode == "GBR" | isocode == "USA"| isocode == "HKG" | isocode == "HUN" | isocode == "IRL" | isocode == "KWT" | isocode == "NZL" ///
| isocode == "OMN" | isocode == "SAU" | isocode == "QAT" | isocode == "SGP")
tab hdc, by(ccode)




*kernel density 10-year growth rates
twoway  (kdensity g10gdpwt if year == 1978) (kdensity g10gdpwt if year == 1983) (kdensity g10gdpwt if year == 1988) ///
(kdensity g10gdpwt if year == 1993) (kdensity g10gdpwt if year == 1998) (kdensity g10gdpwt if year == 2003) ///
(kdensity g10gdpwt if year == 2008) (kdensity g10gdpwt if year == 2013), legend(pos(3) col(1)  ///
label(1 "1973-1978") label(2 "1978-1983") label(3 "1983-1988") label(4 "1988-1993") ///
label(5 "1993-1998") label(6 "1998-2003") label(7 "2003-2008") label(8 "2008-2013") symxsiz(5)) ///
title("10 Year Growth Rate Density by Decade") xtitle("Growth Rate") ytitle("Kernel Density") scheme(plotplain) saving(denst_10g, replace)
graph export denst_10g.png, replace




**Annualized 5 year Growth Rates
*Note that growth rates are calculated for 1973-1978 for the year 1978 (hence initial values are lagged 5 years)

loc cilist neci eci fc
foreach v of local cilist {
eststo POLS_`v': reg  g5gdpwt c.l5.`v'sitc##c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm ==0, vce(cluster ccode)
test  c.l5.`v'sitc#c.l5.lngdpwt l5.`v'sitc
eststo RE5_`v': xtreg g5gdpwt c.l5.`v'sitc##c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm ==0, re vce(cluster ccode) 
test  c.l5.`v'sitc#c.l5.lngdpwt l5.`v'sitc
eststo FE5_`v': xtreg g5gdpwt c.l5.`v'sitc##c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm ==0, fe vce(cluster ccode) 
test  c.l5.`v'sitc#c.l5.lngdpwt l5.`v'sitc
}

esttab using all5yr.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_w r2_b r2_o r2_a rmse N_clust) title("Annualized 5 Year Growth Rates") ///
addn("Robust-clustered standard errors \& Year FE") drop(yr*) nodep nomti label
eststo clear

***no interactions
loc cilist neci eci fc
foreach v of local cilist {
eststo POLS_`v': reg  g5gdpwt c.l5.`v'sitc c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm ==0, vce(cluster ccode)
eststo RE5_`v': xtreg g5gdpwt c.l5.`v'sitc c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm ==0, re vce(cluster ccode) 
eststo FE5_`v': xtreg g5gdpwt c.l5.`v'sitc c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm ==0, fe vce(cluster ccode) 
}

esttab using all5yr_lin.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_w r2_b r2_o r2_a rmse N_clust) title("Annualized 5 Year Growth Rates") ///
addn("Robust-clustered standard errors \& Year FE") drop(yr*) nodep nomti label
eststo clear

 
**Annualized 10 year Growth Rates * 
loc cilist neci eci fc
foreach v of local cilist {
eststo POLS10_`v': reg g10gdpwt c.l10.`v'sitc##c.l10.lngdpwt l10.hc l10.lnpop l10.lnrkppe yr21 yr31 yr41 if pop1 ==0 & xtrm == 0, vce(cluster ccode)
test c.l10.`v'sitc#c.l10.lngdpwt l10.`v'sitc
eststo RE10_neci: xtreg g10gdpwt c.l10.`v'sitc##c.l10.lngdpwt l10.hc l10.lnpop l10.lnrkppe yr21 yr31 yr41 if xtrm == 0, re vce(cluster ccode) 
test c.l10.`v'sitc#c.l10.lngdpwt l10.`v'sitc
eststo FE10_neci: xtreg g10gdpwt c.l10.`v'sitc##c.l10.lngdpwt l10.hc l10.lnpop l10.lnrkppe yr21 yr31 yr41 if xtrm == 0, fe vce(cluster ccode) 
test c.l10.`v'sitc#c.l10.lngdpwt l10.`v'sitc
}

esttab using all10yr.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_w r2_b r2_o r2_a rmse N_clust) title("Annualized 10 Year Growth Rates") ///
addn("Robust-clustered standard errors \& Year FE") drop(yr*) nodep nomti label
eststo clear

**no interaction
loc cilist neci eci fc
foreach v of local cilist {
eststo POLS10_`v': reg g10gdpwt c.l10.`v'sitc c.l10.lngdpwt l10.hc l10.lnpop l10.lnrkppe yr21 yr31 yr41 if pop1 ==0 & xtrm == 0, vce(cluster ccode)
eststo RE10_`v': xtreg g10gdpwt c.l10.`v'sitc c.l10.lngdpwt l10.hc l10.lnpop l10.lnrkppe yr21 yr31 yr41 if xtrm == 0, re vce(cluster ccode) 
eststo FE10_`v': xtreg g10gdpwt c.l10.`v'sitc c.l10.lngdpwt l10.hc l10.lnpop l10.lnrkppe yr21 yr31 yr41 if xtrm == 0, fe vce(cluster ccode) 
}

esttab using all10yr_lin.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_w r2_b r2_o r2_a rmse N_clust) title("Annualized 10 Year Growth Rates") ///
addn("Robust-clustered standard errors \& Year FE") drop(yr*) nodep nomti label
eststo clear


********** Coefficient comparison 
local name necisitc ecisitc fcsitc
foreach v of local name {
xtreg g5gdpwt c.l5.`v'##c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr1-yr41 if xtrm == 0, fe vce(cluster ccode) 
est store m_`v'
}

coefplot m_necisitc m_ecisitc m_fcsitc,  drop(_cons yr6 yr11 yr16 yr21 yr26 yr31 yr36) order(2: hc *) ciopts(recast(rcap)) title("Coefficient Estimates, 5-year Growth Rates") saving(coefcomp, replace)         
//     1:c.l5.necisitc5 1:c.l5.necisitc5#c.l5.lngdpwt 2:c.l5.ecisitc5 2:c.l5.ecisitc5#c.l5.lngdpwt 3:l5.fcsitc5 3:l5.fcsitc5#l5.lngdpwt

***************Marginal effects Graphs
*Marginal Effect

local name neci eci fc
foreach v of local name {
xtreg g5gdpwt c.l5.`v'sitc##c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr1-yr41 if xtrm == 0, fe vce(cluster ccode) 
margins, dydx(l5.`v'sitc) at(l5.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9)) level(90)
marginsplot, level(90) ytitle("Effect on Growth (5-year)") xtitle("GDP per capita") title("Marginal Effects of F_0 (90% CI)")

xtreg g10gdpwt c.l10.`v'sitc##c.l10.lngdpwt l10.hc l10.lnpop l10.lnrkppe yr21 yr31 yr41 if xtrm ==0, fe vce(cluster ccode) 
margins, dydx(l10.`v'sitc) at(l10.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9)) level(90)
marginsplot, level(90) ytitle("Effect on Growth") xtitle("GDP per capita") title("Marginal Effects of ECI (90% CI)")
}

** $ values for logged #
foreach  i of numlist 6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9 {
di "`i'  " exp(`i')
}


*********graph 1 

local name necisitc ecisitc fcsitc
foreach v of local name {

xtreg g5gdpwt c.l5.`v'##c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr1-yr41 if xtrm == 0, fe vce(cluster ccode) 

margins, at(l5.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9) l5.`v' = (-2(.25)2)) vsquish
marginsplot, x(l5.`v') recast(line) recastci(rarea) name(`v'cast, replace) ///
legend(pos(3) col(1) symxsiz(5) textwidth(15)) xsca(alt) xtitle("") ///
title("Predicted Marginal Effects, 5 Year Growth") ytitle("Predicted Effect on Growth (5 year)")

gr_edit .legend.plotregion1.label[1].text = {}
gr_edit .legend.plotregion1.label[1].text.Arrpush $400
gr_edit .legend.plotregion1.label[2].text = {}
gr_edit .legend.plotregion1.label[2].text.Arrpush $665
gr_edit .legend.plotregion1.label[3].text = {}
gr_edit .legend.plotregion1.label[3].text.Arrpush $1,100
gr_edit .legend.plotregion1.label[4].text = {}
gr_edit .legend.plotregion1.label[4].text.Arrpush $1,800
gr_edit .legend.plotregion1.label[5].text = {}
gr_edit .legend.plotregion1.label[5].text.Arrpush $2,000
gr_edit .legend.plotregion1.label[6].text = {}
gr_edit .legend.plotregion1.label[6].text.Arrpush $4,400
gr_edit .legend.plotregion1.label[7].text = {}
gr_edit .legend.plotregion1.label[7].text.Arrpush $10,615
gr_edit .legend.plotregion1.label[8].text = {}
gr_edit .legend.plotregion1.label[8].text.Arrpush $18,400
gr_edit .legend.plotregion1.label[9].text = {}
gr_edit .legend.plotregion1.label[9].text.Arrpush $37,050
gr_edit .legend.plotregion1.label[10].text = {}
gr_edit .legend.plotregion1.label[10].text.Arrpush $59,875
gr_edit .legend.plotregion1.label[11].text = {}
gr_edit .legend.plotregion1.label[11].text.Arrpush $147,265


margins, at(l5.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9) l5.`v' = (-2(.25)2)) vsquish
marginsplot, x(l5.`v') recast(line) recastci(rarea) name(`v'cast, replace) xlabel(-2(.5)2) legend(off) xsca(alt) xtitle("") title("Predicted Marginal Effects, 5 Year Growth") ytitle("Predicted Effect on Growth (5 year)") level(90)
hist `v' if `v' > -2 & `v' < 2, bcolor(gray) name(hist`v', replace) ysca(reverse) xlabel(-2(.5)2) fysize(30) kdensity xtitle("`v', 5 years")
graph combine `v'cast hist`v', col(1) imargin(zero) saving(me`v'yr.pdf, replace)
}


********graph 2

local name necisitc ecisitc fcsitc
foreach v of local name {
xtreg g5gdpwt c.l5.`v'##c.l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe yr1-yr41 if xtrm == 0, fe vce(cluster ccode) 
margins, at(l5.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9) l5.`v' = (-2(.5)2)) vsquish
marginsplot, x(l5.lngdpwt) recast(line) name(gdpcast, replace) ///
legend(pos(3) col(1) symxsiz(5)) xtitle("GDPpc in $USD") ///
title("Predicted Marginal Effect of F_0 and Income") ytitle("Predicted Effect on Growth (5 year)")
gr_edit .legend.plotregion1.label[1].text = {}
gr_edit .legend.plotregion1.label[1].text.Arrpush -2.0 sd 
gr_edit .legend.plotregion1.label[2].text = {}
gr_edit .legend.plotregion1.label[2].text.Arrpush -1.5 sd 
gr_edit .legend.plotregion1.label[3].text = {} 
gr_edit .legend.plotregion1.label[3].text.Arrpush -1.0 sd 
gr_edit .legend.plotregion1.label[4].text = {}
gr_edit .legend.plotregion1.label[4].text.Arrpush -0.5 sd 
gr_edit .legend.plotregion1.label[5].text = {}
gr_edit .legend.plotregion1.label[5].text.Arrpush Average
gr_edit .legend.plotregion1.label[6].text = {}
gr_edit .legend.plotregion1.label[6].text.Arrpush 0.5 sd 
gr_edit .legend.plotregion1.label[7].text = {}
gr_edit .legend.plotregion1.label[7].text.Arrpush 1.0 sd 
gr_edit .legend.plotregion1.label[8].text = {}
gr_edit .legend.plotregion1.label[8].text.Arrpush 1.5 sd 
gr_edit .legend.plotregion1.label[9].text = {}
gr_edit .legend.plotregion1.label[9].text.Arrpush 2.0 sd 

gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"400"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"665"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"1,095"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"1,810"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"2,980"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"4,400"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"10,615"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"18,400"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"37,050"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"59,900"', tickset(major)
gr_edit .xaxis1.major.num_rule_ticks = 0
gr_edit .xaxis1.edit_tick 1 7 `"147,270"', tickset(major)

margins, at(l5.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9) l5.`v' = (-2(.5)2)) vsquish
marginsplot, x(l5.lngdpwt) recast(line) name(gdpcast, replace) xtitle("") legend(off) ///
title("Predicted Marginal Effect of F_0 and Income") ytitle("Predicted Effect on Growth (5 year)")
hist lngdpwt if lngdpwt > 6, bcolor(gray) name(histgdp, replace) ysca(reverse) fysize(30) xlabel(6 6.5 7 7.5 8 8.39 9.27 9.82 10.52 11 11.9) kdensity xtitle("GDP per capita (logged), 5 years") 
graph combine gdpcast histgdp, col(1) imargin(zero) saving(me`v'yrgdp.pdf, replace)
}

*******************************Hausman Taylor Regressions***********************
*Hausman Tests

foreach i in necisitc ecisitc fcsitc {
		foreach v of numlist 5(5)10 {
			qui xtreg g`v'gdpwt c.l`v'.`i'##c.l`v'.lngdpwt  l`v'.hc l`v'.lnpop l`v'.lnrkppe yr1-yr41 if xtrm ==0, re 
			estimates store re`i'`v'
			qui xtreg g`v'gdpwt c.l`v'.`i'##c.l`v'.lngdpwt l`v'.hc l`v'.lnpop l`v'.lnrkppe yr1-yr41 if xtrm ==0, fe 
			estimates store fe`i'`v'
			hausman fe`i'`v' re`i'`v', sigmamore
		}
	}


*loop over 20 year time period, different complexity measures with their interaction
foreach i in necisitc ecisitc fcsitc {
			qui xtreg g20gdpwt c.l20.lngdpwt##c.l20.`i' l20.hc l20.lnpop l20.lnrkppe yr1-yr41 if yr20cl ==1, re 
			estimates store re`i'
			qui xtreg g20gdpwt c.l20.lngdpwt##c.l20.`i' l20.hc l20.lnpop l20.lnrkppe yr1-yr41 if yr20cl ==1, fe 
			estimates store fe`i'
			hausman fe`i' re`i', sigmamore
	}


*Finding the Exogenous regressors with a loop using Hausman Tests
foreach v in lngdpwt necisitc ecisitc fcsitc hc lnpop lnrkppe{
	foreach i of numlist 5 10 20 {
		xtreg g`i'gdpwt l`i'.`v'  if xtrm ==0, re 
		estimates store re`v'
		xtreg g`i'gdpwt l`i'.`v' if xtrm ==0, fe 
		estimates store fe`v'
		hausman fe`v' re`v', sigmamore
	}
}	

***Exogenous Vars***
**fitness, eci (not at 20 yr)
**capital at 10 year


*For whatever reason, xthtaylor does not allow operators, so I calculate interactions manually

	**interaction terms
	bys ccode (year):g intecigdp = ecisitc*lngdpwt
	bys ccode (year):g intnecigdp = necisitc*lngdpwt 
	bys ccode (year):g intfcgdp = fcsitc*lngdpwt
	

local cilist neci eci fc 
foreach v of local cilist {
eststo HT5_`v': xthtaylor g5gdpwt l5.`v'sitc l5.lngdpwt l5.int`v'gdp l5.hc l5.lnpop l5.lnrkppe kgatemp if xtrm ==0, /// 
endog(l5.lngdpwt l5.`v'sitc  l5.int`v'gdp  l5.hc l5.lnpop ) constant(kgatemp)
test l5.`v'sitc l5.int`v'gdp
}
esttab using HT5.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N) title("Hausman Taylor Regressions (5 year)") ///
nodep nomti label


*10 year
local cilist neci eci fc 
foreach v of local cilist {
eststo HT10_`v': xthtaylor g10gdpwt l10.`v'sitc l10.lngdpwt l10.int`v'gdp l10.hc l10.lnpop l10.lnrkppe kgatemp if xtrm ==0, /// 
endog(l10.lngdpwt l10.`v'sitc  l10.int`v'gdp  l10.hc l10.lnpop ) constant(kgatemp)
test l10.`v'sitc l10.int`v'gdp
}

esttab using HT10.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N) title("Hausman Taylor Regressions (10 year)") ///
nodep nomti label
eststo clear

***********************************Graph for HT ********************************
local vars fcsitc ecisitc necisitc 
local int intfcgdp intecigdp intnecigdp
local n: word count `vars'
forval v = 1/`n' {
local a: word `v' of `vars'
local b: word `v' of `int'
xi: xthtaylor g5gdpwt l5.`a' l5.`b' l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe kgatemp if xtrm ==0, /// 
endog(l5.lngdpwt l5.`a' l5.`b' l5.hc l5.lnpop ) constant(kgatemp)
est store base
	nlcom (v0 :_b[l5.`a'] + _b[l5.`b']*6) ///
	(v1 :_b[l5.`a'] + _b[l5.`b']*6.5) ///
	(v2 :_b[l5.`a'] + _b[l5.`b']*7) ///
	(v3 :_b[l5.`a'] + _b[l5.`b']*7.5) /// 
	(v4 :_b[l5.`a'] + _b[l5.`b']*8) ///
	(v5 :_b[l5.`a'] + _b[l5.`b']*8.5) ///
	(v6 :_b[l5.`a'] + _b[l5.`b']*9) ///
	(v7 :_b[l5.`a'] + _b[l5.`b']*9.5) ///
	(v8 :_b[l5.`a'] + _b[l5.`b']*10) ///
	(v9 :_b[l5.`a'] + _b[l5.`b']*10.5) ///
	(v10 :_b[l5.`a'] + _b[l5.`b']*11) ///
	, post
	est sto nlcomv
	coefplot (base, keep(v_?))   ///
         (nlcomv) , ciopts(recast(rcap)) vertical name(ht`a'ec, replace) nokey ytitle("Estimated EFfect on Growth")  xlabel(, nolabels tlength(0))
		 hist lngdpwt if lngdpwt > 6 & lngdpwt <11, bcolor(gray) name(histgdp, replace) ysca(reverse) fysize(30)  kdensity xtitle("GDP per capita (logged), 5 years")
		graph combine ht`a'ec histgdp, col(1) imargin(zero) 
		graph export meht`a'ec.png, replace
			 
// xi: xthtaylor g5gdpwt l5.`a' l5.`b' l5.lngdpwt l5.hc l5.lnpop l5.lnrkppe kgatemp if xtrm ==0, /// 
// endog(l5.lngdpwt l5.`a' l5.`b' l5.hc l5.lnpop ) constant(kgatemp)
// est store base2
// 	nlcom (w0 : _b[l5.lngdpwt] + _b[l5.`b']*-2) ///
// 	(w1 : _b[l5.lngdpwt] + _b[l5.`b']*-1.75) ///
// 	(w2 : _b[l5.lngdpwt] + _b[l5.`b']*-1.5) ///
// 	(w3 : _b[l5.lngdpwt] + _b[l5.`b']*-1.25) ///
// 	(w4 : _b[l5.lngdpwt] + _b[l5.`b']*-1) ///
// 	(w5 : _b[l5.lngdpwt] + _b[l5.`b']*-.75) ///
// 	(w6 : _b[l5.lngdpwt] + _b[l5.`b']*-.5) ///
// 	(w7 : _b[l5.lngdpwt] + _b[l5.`b']*-.25) ///
// 	(w8 : _b[l5.lngdpwt] + _b[l5.`b']*0) ///
// 	(w9 : _b[l5.lngdpwt] + _b[l5.`b']*.25) ///
// 	(w10 : _b[l5.lngdpwt] + _b[l5.`b']*.5) ///
// 	(w11 : _b[l5.lngdpwt] + _b[l5.`b']*.75) ///
// 	(w12 : _b[l5.lngdpwt] + _b[l5.`b']*1) ///
// 	(w13 : _b[l5.lngdpwt] + _b[l5.`b']*1.25) ///
// 	(w14 : _b[l5.lngdpwt] + _b[l5.`b']*1.5) ///
// 	(w15 : _b[l5.lngdpwt] + _b[l5.`b']*1.75) ///
// 	(w16 : _b[l5.lngdpwt] + _b[l5.`b']*2) ///	
// 	, post
// 	est sto nlcomw
// 	coefplot (base2, keep(w_?))   ///
//          (nlcomw) , ciopts(recast(rcap)) vertical name(ht`a'gdp, replace) nokey ytitle("Estimated EFfect on Growth")
// 	hist `a' if `a' > -2 & `a' <2, bcolor(gray) name(hist`a', replace) ysca(reverse) xlabel(-2(.5)2) fysize(30)  kdensity xtitle("Complexity Index, 5 years")
// 	graph combine ht`a'gdp hist`a', col(1) imargin(zero) 
// 	graph export meht`a'gdp.png, replace

}
estimates clear


**************************************** GMM ***********************************
*** 5 Year - given the limited observations, I only analyze 5 year growth intervals
*To make xtabond easier to code, I drop everything outside of the 5-year interval
drop if yr1 != 1 & yr6 != 1 & yr11 != 1 & yr16 !=1 & yr21!=1 & yr26!=1 & yr31!=1  & yr36!=1 & yr41!=1
*keep within time period of analysis
drop if length != 41

xtset ccode year, delta(5)

local name neci eci fc
foreach v of local name {
eststo FDGMM`v': xtabond2 g5gdpwt c.l.`v'sitc##c.l.lngdpwt l.hc l.lnpop l.lnrkppe l.g5gdpwt yr1 yr6 yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm == 0, gmm(g5gdpwt lngdpwt lnpop lnrkppe hc `v'sitc, lag(3 .) collapse ) iv(yr1 yr6 yr11 yr16 yr21 yr26 yr31 yr36 yr41) robust noleveleq  artests(3)
eststo SysGMM`v': xtabond2 g5gdpwt c.l.`v'sitc##c.l.lngdpwt l.hc l.lnpop l.lnrkppe l.g5gdpwt yr1 yr6 yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm == 0, gmm(g5gdpwtlngdpwt lnpop lnrkppe hc `v'sitc, lag(3 .) collapse ) iv(yr1 yr6 yr11 yr16 yr21 yr26 yr31 yr36 yr41, equation(level)) robust h(1)  artests(3)
}
esttab using gmm5_2.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N hansenp sarganp ar1p ar2p N_g) title("GMM for 5-year Growth Rates") ///
addn("Robust-clustered standard errors \& Year FE") nodep nomti label drop(yr*)
eststo clear 


local name neci eci fc
foreach v of local name {

xtabond2 g5gdpwt c.l.`v'sitc##c.l.lngdpwt l.hc l.lnpop l.lnrkppe l.g5gdpwt yr1 yr6 yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm == 0, gmm(lngdpwt lnpop lnrkppe hc `v'sitc, lag(3 .) collapse ) iv(yr1 yr6 yr11 yr16 yr21 yr26 yr31 yr36 yr41, equation(level)) robust h(1)  artests(3)
// margins, at(l.lngdpwt = (6 6.5 7 7.5 8 8.39 9.27 9.82 10.52 11 11.9) l.`v'sitc = (-2(.5)2)) vsquish level(90)
// marginsplot, x(l.lngdpwt) recast(line) name(gdpcast, replace)  xsca(alt) title("") xtitle("") ytitle("Predicted Effect on Growth (5 year)") legend(off) scheme(plotplainblind) 
// hist lngdpwt if lngdpwt > 6, bcolor(gray) name(histgdp, replace) ysca(reverse) fysize(30) xlabel(6 6.5 7 7.5 8 8.39 9.27 9.82 10.52 11 11.9) kdensity xtitle("GDP per capita (logged), 5 years") scheme(plotplainblind)
// graph combine gdpcast histgdp, col(1) imargin(zero) 
// graph export me`v'gmmgdp.png, replace


// margins, at(l.lngdpwt = (6 6.5 7 7.5 8 8.39 9.27 9.82 10.52 11 11.9) l.`v'sitc = (-2(.25)2)) vsquish
// marginsplot, x(l.`v'sitc) recast(line) name(`v'cast, replace) xlabel(-2(.5)2) legend(off) scheme(plotplainblind) ///
// xsca(alt) xtitle("") title("") ytitle("Predicted Effect on Growth (5 year)") level(90) 
// hist `v'sitc if `v'sitc > -2 & `v'sitc < 2, bcolor(gray) name(hist`v', replace) ysca(reverse) xlabel(-2(.5)2) fysize(30) kdensity scheme(plotplainblind)
// graph combine `v'cast hist`v', col(1) imargin(zero) 
// graph export me`v'gmmci.png, replace

margins, dydx(l.`v'sitc) at(l.lngdpwt = (6(.5)11)) level(90)
marginsplot, level(90) ytitle("Estimated Effect on Growth (5-year)") title("") name(dx`v', replace) xsca(alt) xtitle("")
hist lngdpwt if lngdpwt > 6 & lngdpwt <11, bcolor(gray) name(histgdp, replace) ysca(reverse) fysize(30) xlabel(6(.5)11) kdensity xtitle("GDP per capita (logged), 5 years") scheme(plotplainblind) title("")
graph combine dx`v' histgdp, col(1) imargin(zero) title("")
graph export me`v'gmmgdp.png, replace
}

local name fc
foreach v of local name {

xtabond2 g5gdpwt c.l.`v'sitc##c.l.lngdpwt l.hc l.lnpop l.lnrkppe l.g5gdpwt yr1 yr6 yr11 yr16 yr21 yr26 yr31 yr36 yr41 if xtrm == 0, gmm(lngdpwt lnpop lnrkppe hc `v'sitc, lag(3 .) collapse ) iv(yr1 yr6 yr11 yr16 yr21 yr26 yr31 yr36 yr41, equation(level)) robust h(1)  artests(3)
margins, at(l.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9) l.`v'sitc = (-1(.5)4.5)) vsquish level(90)
marginsplot, x(l.lngdpwt) recast(line) name(gdpcast, replace)  xsca(alt)  title("") xtitle("") ytitle("Predicted Effect on Growth (5 year)") scheme(plotplainblind) 


hist lngdpwt if lngdpwt >= 6, bcolor(gray) name(histgdp, replace) ysca(reverse) fysize(30) xlabel(6 6.5 7 7.5 8 8.39 9.27 9.82 10.52 11 11.9) kdensity xtitle("GDP per capita (logged)") scheme(plotplainblind)
graph combine gdpcast histgdp, col(1) imargin(zero) 
graph export me`v'gmmgdp.png, replace


margins, at(l.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9) l.`v'sitc = (-1(.5)4.5)) vsquish level(90)
marginsplot, x(l.`v'sitc) recast(line) name(`v'cast, replace) xlabel(-1(.5)4.5)  scheme(plotplainblind) ///
xsca(alt) xtitle("") title("") ytitle("Predicted Effect on Growth (5 year)") level(90) 
hist `v'sitc, bcolor(gray) name(hist`v', replace) ysca(reverse) xlabel(-1(.5)4.5) fysize(30) kdensity scheme(plotplainblind)
graph combine `v'cast hist`v', col(1) imargin(zero) 
graph export me`v'gmmci.png, replace


margins, dydx(l.`v'sitc) at(l.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9)) level(90)
marginsplot, level(90) ytitle("Estimated Effect on Growth (5-year)") xtitle("GDP per capita (logged)") title("") 
graph export megmm`v'.png, replace
}          

eststo clear 



******* Split sample

gen mo = (year >=1988) 
tabstat mo, by(year)
*1 >= 1988

*wealthy country
gen hdc = (isocode == "AUS" | isocode == "AUT" | isocode == "BEL" | isocode == "CAN" | isocode == "CHL" | isocode == "CHE" ///
| isocode == "CZE" | isocode == "DNK" | isocode == "EST" | isocode == "ESP" | isocode == "FIN" | isocode == "FRA" | isocode == "DEU" ///
| isocode == "GRC" | isocode == "HUN" | isocode == "ISL" | isocode == "ITA" | isocode == "JPN" | isocode == "KOR" | isocode == "LTV" ///
| isocode == "LUX" | isocode == "MEX" | isocode == "NLD" | isocode == "POL" | isocode == "PRT" | isocode == "SLK" | isocode == "SWE" ///
| isocode == "GBR" | isocode == "USA"| isocode == "HKG" | isocode == "HUN" | isocode == "IRL" | isocode == "KWT" | isocode == "NZL" ///
| isocode == "OMN" | isocode == "SAU" | isocode == "QAT" | isocode == "SGP")
tab hdc, by(ccode)

loc cilist neci eci fc
foreach v of local cilist {
*Internet Age
eststo split_`v': xtreg g5gdpwt c.l.`v'sitc##c.l.lngdpwt l.hc l.lnpop l.lnrkppe yr16 yr21 yr26 yr31 yr36 yr41 if xtrm ==0 & mo ==1 & hdc ==1 , fe vce(cluster ccode) 
*Post-War
eststo split_`v': xtreg g5gdpwt c.l.`v'sitc##c.l.lngdpwt l.hc l.lnpop l.lnrkppe yr1 yr6  yr11  if xtrm ==0 & mo ==0 & hdc ==1, fe vce(cluster ccode) 
}

eststo clear

****************************** 20 year growth dataset ******************************
**20 Year requires a different data set
cd "$pathoutput"
us neciv10.dta, clear

*dropping if it's not in our years
drop if  yr20cl ==0

drop yr*
tab year, g(yr)
tab year

*label vars to make output easier
label var ecisitc "Initial ECI"
label var necisitc "Initial F_0"
label var fcsitc "Initial F"
label var lngdpwt "Initial GDPpc"
label var lnpop "Initial Pop"
label var lnrkppe "Initial Capital"
label var hc "Initial Human Capital"


local cilist neci eci fc 
foreach v of local cilist {
eststo POLS20_`v': reg g20gdpwt c.l20.`v'sitc##c.l20.lngdpwt l20.hc l20.lnpop l20.lnrkppe yr1-yr3, vce(cluster ccode) 
test c.l20.`v'sitc#c.l20.lngdpwt l20.lngdpwt
eststo RE20_`v': xtreg g20gdpwt c.l20.`v'sitc##c.l20.lngdpwt l20.hc l20.lnpop l20.lnrkppe yr1-yr3, re vce(cluster ccode) 
test c.l20.`v'sitc#c.l20.lngdpwt l20.lngdpwt
eststo FE20_`v': xtreg g20gdpwt c.l20.`v'sitc##c.l20.lngdpwt l20.hc l20.lnpop l20.lnrkppe yr1-yr3, fe vce(cluster ccode) 
test c.l20.`v'sitc#c.l20.lngdpwt l20.lngdpwt
}

esttab using all20full.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_w r2_b r2_o r2_a rmse N_clust) title("Annualized 20 Year Growth Rates") ///
addn("Robust-clustered standard errors \& Year FE") drop(yr*) nodep nomti label
eststo clear 

*** linear version
local cilist neci eci fc 
foreach v of local cilist {
eststo POLS20_`v': reg g20gdpwt c.l20.`v'sitc c.l20.lngdpwt l20.hc l20.lnpop l20.lnrkppe yr1-yr3, vce(cluster ccode) 
eststo RE20_`v': xtreg g20gdpwt c.l20.`v'sitc c.l20.lngdpwt l20.hc l20.lnpop l20.lnrkppe yr1-yr3, re vce(cluster ccode) 
eststo FE20_`v': xtreg g20gdpwt c.l20.`v'sitc c.l20.lngdpwt l20.hc l20.lnpop l20.lnrkppe yr1-yr3, fe vce(cluster ccode) 
}

esttab using all20full_lin.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N r2_w r2_b r2_o r2_a rmse N_clust) title("Annualized 20 Year Growth Rates") ///
addn("Robust-clustered standard errors \& Year FE") drop(yr*) nodep nomti label
eststo clear 

********Hausman Tests for exogeneity
foreach v in lngdpwt necisitc ecisitc fcsitc hc lnpop lnrkppe {
		xtreg g20gdpwt l20.`v' if xtrm ==0, re 
		estimates store re`v'
		xtreg g20gdpwt l20.`v' if xtrm ==0, fe 
		estimates store fe`v'
		hausman fe`v' re`v', sigmamore
	}	
estimates clear

local cilist neci eci fc 
foreach v of local cilist {
xtreg g20gdpwt c.l20.`v'sitc##c.l20.lngdpwt if xtrm ==0, re 
estimates store re
xtreg g20gdpwt c.l20.`v'sitc##c.l20.lngdpwt if xtrm ==0, fe 
estimates store fe
hausman fe re, sigmamore
}


***********Hausman Taylor Regressions
*20 year

bys ccode (year):g intecigdp = ecisitc*lngdpwt
bys ccode (year):g intnecigdp = necisitc*lngdpwt 
bys ccode (year):g intfcgdp = fcsitc*lngdpwt

local cilist neci eci fc 
foreach v of local cilist {
eststo HT20_`v': xthtaylor g20gdpwt l20.`v'sitc l20.int`v'gdp l20.lngdpwt l20.hc l20.lnpop l20.lnrkppe kgatemp, ///
endog(l20.lngdpwt l20.`v'sitc l20.int`v'gdp l20.hc l20.lnpop) constant(kgatemp)
test l20.`v'sitc l20.int`v'gdp 
margins, at(l20.lngdpwt = (6 6.5, 7, 7.5, 8, 8.39, 9.27, 9.82, 10.52, 11, 11.9) l20.`v' = (-2(.5)2)) vsquish
marginsplot, x(l20.lngdpwt) recast(line) name(gdpcast, replace) ///
 xtitle("") legend(off) ///
title("Predicted Marginal Effect of F_0 and Income") ytitle("Predicted Effect on Growth (5 year)")

}

esttab using HT20.tex, replace se(%8.3f) star(* 0.10 ** 0.05 *** 0.01) stats(N) title("Hausman Taylor Regressions (20 year)") ///
nodep nomti label
eststo clear



