//Author: Norberto R. Morales
//Project: Anti-American Public Opinion
//Purpose: Merge MID files for past decade and five years
//Created: 07/01/2013
//Last Updated: 09/22/2013
//Machine(Windows 7): S/No: 0039130478
************************************************************************************************************************************
clear
capture cd "C:\Users\Antigone\Dropbox\Papers\anti-american_paper_data" // set working directory
set more off
************************************************************************************************************************************
//Merge decade and five MID data sets
use "Data\decadeMID.dta", clear
sort ccode 

merge ccode using "Data\fiveMID.dta"
duplicates report ccode //missing values for five year wars=countries with no dispute with the U.S. in the past five years

drop _merge

rename country countryabr
label var ccode "COW country code"
label var decdisp "Dispute events in past decade"
label var decdeaths "Max deaths in past decade"
label var dechost "Max hostility level in past decade"
label var fivedisp "Dipute events in past five years"
label var fivehost "Max hostility level in past five years"
label var fivedeaths "Max deaths per in past five years"

//Generate country name variable
gen country=.
replace country=43 if ccode==640 //"Turkey"
replace country=26 if ccode==820 //"Malaysia"
replace country=31 if ccode==770 //"Pakistan"
replace country=22 if ccode==663 //"Jordan" 
replace country=29 if ccode==600 //"Morocco"
replace country=10 if ccode==651 //"Egypt" 
replace country=17 if ccode==850 //"Indonesia"
replace country=25 if ccode==660 //"Lebanon" 
replace country=11 if ccode==530 //"Ethiopia" 
replace country=42 if ccode==510 //"Tanzania"
replace country=24 if ccode==690 //"Kuwait" 
replace country=30 if ccode==475 //"Nigeria"
replace country=2 if ccode==771 //"Bangladesh"
replace country=23 if ccode==501 //"Kenya" 
replace country=16 if ccode==750 //"India" 
replace country=45 if ccode==500 //"Uganda"
replace country=14 if ccode==452 //"Ghana"
replace country=36 if ccode==433 //"Senegal"
replace country=27 if ccode==432 //"Mali"
replace country=20 if ccode==437 //"Ivory Coast"

order country, before(countryabr)
sort country

save "Final Data\MIDcomplete.dta", replace

//end of file
************************************************************************************************************************************
