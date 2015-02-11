//Author: Norberto R. Morales
//Project: Anti-American Public Opinion
//Purpose: Merge country level variablea and Militarized Disputes (MID) 
//Created: 07/01/2013
//Last Updated: 09/22/2013
//Machine(Windows 7): S/No: 0039130478
************************************************************************************************************************************
clear
capture cd "C:\Users\Antigone\Dropbox\Papers\anti-american_paper_data" // set working directory
set more off
************************************************************************************************************************************
//Open Cvars data (Country level variables)
insheet using "Data-Raw\cvars.csv", comma clear
save "Data\cvars.dta", replace

rename country countryn

//Generate country name variable
gen country=.
replace country=43 if countryn=="Turkey"
replace country=26 if countryn=="Malaysia"
replace country=31 if countryn=="Pakistan"
replace country=22 if countryn=="Jordan" 
replace country=29 if countryn=="Morocco"
replace country=10 if countryn=="Egypt" 
replace country=17 if countryn=="Indonesia"
replace country=25 if countryn=="Lebanon" 
replace country=11 if countryn=="Ethiopia" 
replace country=42 if countryn=="Tanzania"
replace country=24 if countryn=="Kuwait" 
replace country=30 if countryn=="Nigeria"
replace country=2 if countryn=="Bangladesh"
replace country=23 if countryn=="Kenya" 
replace country=16 if countryn=="India" 
replace country=45 if countryn=="Uganda"
replace country=14 if countryn=="Ghana"
replace country=36 if countryn=="Senegal"
replace country=27 if countryn=="Mali"
replace country=20 if countryn=="Ivory Coast"

save "Data\cvars.dta", replace

//Merge MID and Cvars data
use "Data\cvars.dta", clear

sort country

merge country using "Final Data\MIDcomplete.dta"
duplicates report country 

tab _merge
drop _merge

drop country
rename countryn country

//Replace missing values with zeroes for countries for which there were no disputes with the U.S.
replace decdisp=0 if decdisp==.
replace dechost=0 if dechost==.
replace decdeaths=0 if decdeaths==.
replace fivedisp=0 if fivedisp==.
replace fivehost=0 if fivehost==.
replace fivedeaths=0 if fivedeaths==.

tab country decdisp if decdisp~=.
tab country decdisp if decdisp>0
tab decdisp, missing

save "Data\cvars.dta", replace

//end of file
************************************************************************************************************************************
