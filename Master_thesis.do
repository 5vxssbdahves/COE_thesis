clearset more off
* PWT71 *
cd "/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data"use "pwt71a.dta"
xtset isocode year
bys isocode: g gdpgrowth=(rgdpl[_n]-rgdpl[_n-1])/rgdpl[_n-1]

xtsum gdpgrowth
hist gdpgrowth, freq nodraw
save "pwt71.dta", replace

xtbalance, range(1965 2000) miss(gdpgrowth)
save "pwt71b.dta", replace
* Entire data *use "IQM_pro_data.dta", clearxtset isocode yeardessum
  reg gdpgrowth inigdp yr_sch_secF yr_sch_secM lbmp lfert kg ki llexpec ToT POLCONVreg gdp_growth inigdp yr_sch_secF yr_sch_secM lbmp lfert kg ki llexpec ToT POLCONVsum POLCONIII POLCONV POLCONVJ rgdpl kg ki lexpec fert bmp ToT icrgQoG democsum POLCONIII POLCONV POLCONVJ rgdpl kg ki lexpec fert bmp lbmp ToT icrgQoG democsave "Statadata.dta", replace

