clearset more off
cd "/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/CoE_thesis_repository"use "/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/IQM_pro_data.dta", cleardessum* Calculate growth rates for rgdpl
bys CTRYNM: g gdpgrowth=(rgdpl[_n]-rgdpl[_n-1])/rgdpl[_n-1]

sum gdpgrowthclear*outreg2 using myfile, sum(gdpgrowth)
  
  reg gdpgrowth inigdp yr_sch_secF yr_sch_secM lbmp lfert kg ki llexpec ToT POLCONVreg gdp_growth inigdp yr_sch_secF yr_sch_secM lbmp lfert kg ki llexpec ToT POLCONVxtset CTRYNM Yearsum POLCONIII POLCONV POLCONVJ rgdpl kg ki lexpec fert bmp ToT icrgQoG democsum POLCONIII POLCONV POLCONVJ rgdpl kg ki lexpec fert bmp lbmp ToT icrgQoG democsave "/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Statadata.dta", replace clear*
    sysuse auto, clear
    outreg2 using myfile, sum(log) replace eqdrop(N mean) see
    outreg2 using myfile, sum(detail) replace eqkeep(N max min) see
    outreg2 using myfile, sum(detail) replace see
