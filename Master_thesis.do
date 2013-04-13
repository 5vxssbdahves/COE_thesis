clearset more off
cd "/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data"use "pwt71a.dta"
bys isocode: g gdpgrowth=(rgdpl[_n]-rgdpl[_n-1])/rgdpl[_n-1]

sum gdpgrowth
hist gdpgrowth


save "pwt71.dta", replace
use "IQM_pro_data.dta", cleardessum
  reg gdpgrowth inigdp yr_sch_secF yr_sch_secM lbmp lfert kg ki llexpec ToT POLCONVreg gdp_growth inigdp yr_sch_secF yr_sch_secM lbmp lfert kg ki llexpec ToT POLCONVxtset CTRYNM Yearsum POLCONIII POLCONV POLCONVJ rgdpl kg ki lexpec fert bmp ToT icrgQoG democsum POLCONIII POLCONV POLCONVJ rgdpl kg ki lexpec fert bmp lbmp ToT icrgQoG democsave "Statadata.dta", replace

