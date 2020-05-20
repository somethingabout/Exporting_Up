# Exporting_Up
Program: Stata
1. To clean the data run the do file “data_cleaning” this  will create the data file neciv10 - which is needed to run the regressions

You will need the following files:
year_origin_sitc_rev3_ec_all_years.v9.csv
hs6v8.dta
hs4v8.dta
gcip.dta
wbgov.dta
wbschool.dta
pwt90.dta
wpi.dta
atlasgdp.dta
wbdata.dta
kgzones.dta

2. For the analysis, run the file:
reg_export_up.do

These will create the tables and figures in the paper.
