clearset more off
cd "/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data"

*** POLCON ***


*** PWT71 ***
use "pwt71a.dta", clear
des
xtset isocode year
bys isocode: g gdpgrowth=(rgdpl[_n]-rgdpl[_n-1])/rgdpl[_n-1]

xtsum gdpgrowth
hist gdpgrowth, freq nodraw
save "pwt71.dta", replace

xtbalance, range(1965 2000) miss(gdpgrowth)
save "pwt71b.dta", replace
*** Educ: Barro-Lee ***
* All
use "BL2013_MF1599_v1.3.dta", clear
des
tsset BLcode year
tsfill
bysort BLcode: carryforward yr_sch_sec, gen(yr_sch_sec_full) carryalong(WBcode)
save "BL2013_MF1599_v1.3.dta_full.dta", replace

* Females
use "BL2013_F1599_v1.3.dta", clear
des
tsset BLcode year
tsfill
bysort BLcode: carryforward yr_sch_sec, gen(yr_sch_sec_full) carryalong(WBcode)
save "BL2013_F1599_v1.3.dta_full.dta", replace
*** Entire data ***use "IQM_pro_data.dta", clear
des
xtset CTRYNM Year

xtreg gdpgrowth linigdp yr_sch_secM yr_sch_secF llexpec lfert kg ToT ki south_asia east_asia_pacific latin_america_caribbean middle_east_north_africa sub_saharan_africa llaw_order, fe

xtbalance, range(1965 1999) miss(gdpgrowth)
save "IQM_pro_data_balanced.dta", replace

*** New data set ***use "IQM_pro_data1.4.dta", clear
des
encode CTRYNM, generate(CTRYNM2)
encode Year, generate(Year2)
desxtset CTRYNM2 Year2sum
  reg gdpgrowth inigdp yr_sch_secF yr_sch_secM lbmp lfert kg ki llexpec ToT POLCONVreg gdp_growth inigdp yr_sch_secF yr_sch_secM lbmp lfert kg ki llexpec ToT POLCONVsum POLCONIII POLCONV POLCONVJ rgdpl kg ki lexpec fert bmp ToT icrgQoG democsum POLCONIII POLCONV POLCONVJ rgdpl kg ki lexpec fert bmp lbmp ToT icrgQoG democsave "Statadata.dta", replace

