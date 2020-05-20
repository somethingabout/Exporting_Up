********************* New Complexity Method ************************************
**Do file by Mary Kaltenberg*
**kaltenberg@merit.unu.edu*
***Last Updated: 02/21/2017**

global path "your/path/here"
cd "$path"
local dir `c(pwd)'

global pathinput "`c(pwd)'/input"
global pathoutput "`c(pwd)'/output"
global pathdo "`c(pwd)'/do file"
global pathgraph "`c(pwd)'/regs"


cd "$pathoutput"

import delimited "$pathinput/year_origin_sitc_rev3_ec_all_years.v9.csv", clear

rename pci_cm pci_cmsitc
rename npci_cm npci_cmsitc 
rename qp_cm qp_cmsitc
rename neci necisitc
rename country isocode
rename eci ecisitc
rename fc fcsitc
rename total_export expsitc
foreach v of var expsitc-qp_cmsitc{
replace `v' = "" if `v' == " NaN"
destring(`v'), replace
}

replace isocode = upper(isocode)
tostring year, replace
gen id = isocode+year
label var ecisitc "traditional eci sitc"
label var necisitc "new eci sitc"
label var fcsitc "fitness sitc"
label var expsitc "total exports sitc"
destring ecisitc fcsitc expsitc necisitc year, replace

save sitcv9, replace
/*

*Wholesale Price Index for USA 
wbopendata, indicator(FP.WPI.TOTL) clear long
cd "$pathinput"
rename countrycode isocode
replace isocode = upper(isocode)
tostring year, replace
gen id = isocode+year
destring year, replace
drop if fp_wpi_totl == .
keep if isocode == "USA"
rename fp_wpi_totl wpi
save wpi.dta, replace

*World Bank GDP data: replicating Atlas
wbopendata, indicator(NY.GDP.MKTP.KD; NY.GNP.PCAP.KD; SP.POP.TOTL ) clear long
cd "$pathinput"
rename ny_gdp_mktp_kd gdpcon
label var gdpcon "orig gdp data in atlas GDP con 2005 US$"
rename ny_gnp_pcap_kd  gni_pc
label var gni_pc  "original gni ppc in atlas"
rename sp_pop_totl population
label var population "original pop data in atlas"
replace iso = "ROU" if iso == "ROM"

egen idc=group(iso)
gen year2 = year^2
gen year3 = year^3
gen logpop=ln(population)

summarize idc
local idcmax=r(max)
//Run a regression for each country separetely to estimate the missing values.
forval i=1/`idcmax' {
   quietly {
   cap reg logpop year year2 year3 if idc==`i' // Run the regression using a polynomial for population
   cap predict yhat
   cap replace population = exp(yhat) if idc==`i' & population == . // Replace the missing values with predicted values
   cap drop yhat
   }
}

drop idc year2 year3 logpop

rename countrycode isocode 
replace isocode = upper(isocode)
tostring year, replace
gen id = isocode+year
destring year, replace

save atlasgdp.dta, replace

**World Bank GDP data: Chained PPP
wbopendata, indicator(NY.GDP.PCAP.KD; NY.GDP.PCAP.PP.KD; NY.GDP.PCAP.PP.CD; NY.GDP.TOTL.RT.ZS) clear long
rename ny_gdp_pcap_kd gdpconwb
label var gdpconwb "GDP per capita (constant 2005 US$)"
rename ny_gdp_pcap_pp_kd gdpconppp 
label var gdpconppp "GDP per capita, PPP (constant 2011 international $)"
rename ny_gdp_pcap_pp_cd gdpcurppp
label var gdpcurppp "GDP per capita, PPP (current international $"
rename ny_gdp_totl_rt_zs naturalres
label var naturalres "Total natural resources rents (% of GDP)"

rename countrycode isocode 
replace isocode = upper(isocode)
tostring year, replace
gen id = isocode+year
destring year, replace
replace isocode = "ROU" if isocode == "ROM"

save wbdata.dta, replace
*/

*SITC all var
cd "$pathoutput"
use sitcv9.dta, clear

*merge m:1 id isocode using "$pathinput/momentumplusneci.dta"

*HS6 all var
merge 1:1 isocode year using "$pathoutput/hs6v8.dta", gen (_merge1) 

*HS4 all var
merge m:1 isocode year using "$pathoutput/hs4v8.dta", gen (_merge2)

*gcip inequality
merge 1:1 isocode year  using "$pathinput/gcip.dta", gen (_merge3) 

*WB Gov Indic
merge 1:1 isocode year using "$pathinput/wbgov.dta", gen(_merge4) 

* Barro-Lee
*merge m:1 id isocode using "$pathinput/wbbarrleeschool.dta", gen (_merge5) 
merge 1:1 isocode year using "$pathoutput/wbschool.dta", gen (_merge5) 
*to match the start years, I changed the values so that 1970 -> 1973 etc 

*PWT UPDATED data (9.0)
merge 1:1 isocode year  using "$pathoutput/pwt90.dta", gen (_merge6) 

*replace isocode = "ROU" if isocode == "ROM" for all WB data

bys isocode (year): gen gdpwt = rgdpna/pop
label var gdpwt "rgdpna/pop pwt, real gdppc constant $2005"

bys isocode (year): gen gdpwto = rgdpo/pop
label var gdpwto "rgdpo/pop pwt, real gdppc chained pp output $2005"

bys isocode (year): gen gdpwte = cgdpe/pop
label var gdpwto "rgdpe/pop pwt, real gdppc chained pp expenditure$2005"

bys isocode (year): gen ckppc = ck/pop
label var ckppc "ck/pop pwt, capital stock ppp $2005 constant"

bys isocode (year): gen rkppc = rkna/pop
label var rkppc "rkna/pop pwt, capital stock national prices $2005 constant"

bys isocode (year): gen rkppe = rkna/emp
label var rkppe "rkna/emp pwt, capital stock national prices $2005 constant"

*merge wholesale price index of USA
merge m:1 year using "$pathinput/wpi.dta", gen (_merge7)

*create constant PPP
local base_year 2010
summarize wpi if year == `base_year', meanonly
scalar wpi`base_year' = r(mean)

gen magic = (wpi`base_year'/wpi)/population // Renormalize WPI around the base year and calculate amgic numbers to convert to constant PC
gen pc_constant = magic // To convert to per capita constant dollars, just multiply with the magic number.
gen pc_current = 1/population // To convert to per capita current dollars, just divide by population.
gen notpc_constant = (wpi`base_year'/wpi) // to convert to constant dollars, just divide by teh WPI.

*merge original atlas wb data
merge m:1 isocode year using "$pathinput/atlasgdp.dta", gen (_merge8) 

*merge wb data
merge 1:1 isocode year using "$pathinput/wbdata.dta", gen (_merge9) 

*let's make magic happen
gen gdpwto2010 =  gdpwto/notpc_constant
gen gdpwte2010 =  gdpwte/notpc_constant
gen gdpwbppp2010 =  gdpcurppp/notpc_constant

*replace isocode = "COD" if isocode == "ZAR"

*Gallup Climate Constant Vars
merge m:1 isocode using  "$pathinput/kgzones.dta", gen(_merge10)


tab _merge1 //
tab _merge2 // 
tab _merge3 // 
tab _merge4 // 
tab _merge5 // 
tab _merge6 // 
tab _merge7 // 
tab _merge8 //
tab _merge9 //

tabstat gdpwt gdpwto necisitc expsitc exp6hs neci6hs neci4hs exp4hs, by(year) stat(n)

drop countryname iso2code region regioncode _merge* delta labsh rwtfpna i_cig i_xm i_xr i_outlier cor_exp statcap csh_c csh_i csh_g csh_x csh_m csh_r pl_c pl_i pl_g pl_x pl_m pl_k

destring year, replace

**drop any variables that are beyond time frame
forval i = 2020(5)2100 {
drop if year == `i'
}

forval i = 1950(1)1961{
drop if year == `i'
}


encode isocode, gen(ccode)
xtset ccode year

************Create Dataset for years above 1973
drop if year < 1973
drop if year > 2013


**for panel, count of country in ds
bys isocode: gen nyear=[_N]	
label var nyear "total number of years in ds"

tab nyear

drop ratio90to10levels atkinson2 avh country xr cgdpe rgdpe incomeatperc1 ///
incomeatperc10 incomeatperc100 incomeatperc20 incomeatperc30 incomeatperc40 incomeatperc50  ///
incomeatperc60 incomeatperc70 incomeatperc80 incomeatperc90 incomeatperc95 incomeatperc99 ///
mld income1 income10 income2 income3 income4 income5 income6 income7 income8 income9 m2mratio ///
cgdpo rgdpo mean mean2011PPP pl_con pl_da pl_gdpo rconna ccon rdana cda rgdpna  ///
databasesource share1 share10 share2 share3 share4 share5 share6 share7 share8 share9 sharetop1 ///
sharetop5 source1 sqcoeffvariation surveysource2 rtfpna ctfp incdefn cwtfp consumptionsurvey ///
surveyyears currency_unit population v1


*** log variables **** 

*gen logs
foreach x of var expsitc exp6hs exp4hs gdpconwb gdpwto pop gdpwt gdpwto2010 gdpwte2010 gdpwbppp2010 rkppc rkppe xcn {
	gen ln`x' = ln(`x')
	label var ln`x' "natural log of `x'"
}
 
***calculate growth rates****


********Annual Growth Rates
*tab year, g(p5)

bys ccode (year): g growthpwt = D.gdpwt/L.gdpwt
label var  growthpwt "Annual growth rates gdppc pwt"

bys ccode (year): g growthwb = D.gdpconwb/L.gdpconwb
label var  growthwb "Annual growth rates gdppc wb"

bys ccode (year): g growthpwto = D.gdpwto/L.gdpwto
label var  growthpwto "Annual growth rates gdppc pwt c output"


******compound growth rate
****[(End val/beg val) ^(1/totyears)]-1
xtset ccode year

*PWT & WB

foreach v in gdpwt gdpconwb gdpwto gdpwto2010 gdpwte2010 gdpwbppp2010 gdpcon {
	foreach i of numlist 5(5)10 {
			bys ccode (year): gen g`i'`v' = (`v'/l`i'.`v')^(1/`i')-1 if mod(year, `i') == 3 
			label var g`i'`v' "`i' CAGR pwtc"
		}
	}

foreach v in gdpwt gdpconwb gdpwto gdpwto2010 gdpwte2010 gdpwbppp2010 gdpcon{
	foreach i of numlist 5(5)10{
			tabstat g`i'`v', by(year) stat(n mean)
			tabstat g`i'`v', by(ccode) stat(mean)
		}
	}
	
	

**20 year growth rate coding issue creating growth rates for all
foreach v in gdpwt gdpconwb gdpwto gdpwto2010 gdpwte2010 gdpwbppp2010 gdpcon {
	foreach i of numlist 15(5)20 {
			bys ccode (year): gen g`i'`v' = (`v'/l`i'.`v')^(1/`i')-1
			label var g`i'`v' "`i' CAGR pwtc"
		}
	}
			
		
**generating values for 20 year growth reg 		
gen yr20cl = 0
replace yr20cl = 1 if year == 1973 | year == 1993 | year == 2013

**generating values for 15 year growth reg 	
gen yr15cl = 0
replace yr15cl = 1 if  year == 1973 | year == 1988 | year == 2003	
		

*generate variable to check length of spells in ds for necisitc
tsset ccode year
tsspell necisitc, f(L.necisitc == .)
egen length = max(_seq), by(ccode _spell)


*gen year dummies
tab year, g(yr)

*******************************Filters****************************************** 

gen xtrm = (isocode == "ARE" | isocode == "AZE" | isocode == "BIH" | isocode == "BMU"| ///
isocode == "GEO" | isocode == "GNQ" | isocode == "IRQ" | isocode == "KGZ"| isocode == "LBR" | ///
isocode == "NGA" | isocode == "SYR" | isocode == "TCA" | isocode == "TJK"| isocode == "YEM"| ///
isocode == "AFG" |isocode == "TCD" | isocode == "WSM" |isocode == "UKR"| isocode == "MLT" | ///
isocode == "CPV"|isocode == "MAC"|isocode == "BWA"|isocode == "ZAR"|isocode == "MDA"| ///
isocode == "LBR"|isocode == "AFG"|isocode == "ZWE") 

** filter by population
gen pop1 = 0
replace pop1 = 1 if pop < 1.25 & year == 2008

* best time series 1964 - 2010 to include the most countries
*However, our start year is 1973, thus for 20 year growth reg last year is 2013.
*trims the years that may be in long run set to just the time period we want to observe
*Balanced panel
gen ccl = (length == 41)


********
tabstat ecisitc, by(year) statistics(n me)
tabstat necisitc neci6hs neci4hs, by(year) stat(n me)

drop if pop1 != 0

**General Statistics about data
*necisitc- 1964 - 2014 
*necihs4 - 1995 - 2012
*necihs6 - 1997 - 2014
*eci 1964 - 2014
*school (barro lee- 15 and up education) 144 countries 1970-2010, every 5 years
*WB institutions - min 199 max 212, 1996, 1998, 2000, 2002 - 2014
*gini - min 1952 max 160, 1962 - 2015
*pwt - 113 (1962) - 182 (2014)
*mad - 90 (1962) - 150 (2013)

save neciv10, replace
