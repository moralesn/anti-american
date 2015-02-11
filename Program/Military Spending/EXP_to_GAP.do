//Author: Norberto R. Morales
//Project: Anti-American Public Opinion
//Purpose:  Merge Military Aid, Militarized Disputes (MID), and GAP Data
//Created: 07/01/2013
//Last Updated: 09/22/2013
//Machine(Windows 7): S/No: 0039130478
************************************************************************************************************************************
clear
capture cd "C:\Users\Antigone\Dropbox\Papers\anti-american_paper_data" // set working directory
set more off
************************************************************************************************************************************
use "Data\EXPmain.dta", clear

//Generate country name variable
gen country=.
replace country=43 if ccode==640 
replace country=26 if ccode== 820
replace country=31 if ccode== 770
replace country=22 if ccode== 663 
replace country=29 if ccode== 600
replace country=10 if ccode== 651
replace country=17 if ccode== 850
replace country=25 if ccode== 660 
replace country=11 if ccode==530  
replace country=42 if ccode== 510
replace country=30 if ccode== 475
replace country=2 if ccode== 771
replace country=23 if ccode== 501
replace country=16 if ccode== 750
replace country=45 if ccode== 500
replace country=14 if ccode== 452
replace country=36 if ccode== 433
replace country=27 if ccode== 432
replace country=20 if ccode== 437

//Merge EXP and GAP/MID data

sort country

merge country using "Data\cvarsMID.dta"
duplicates report country 

tab _merge
drop _merge

rename country countryid

rename countryn country

order country ccode, before(year)

//Replace missing values with zeroes for countries for which there were no disputes with the U.S.
replace decexp=0 if decexp==.
replace fiveexp=0 if fiveexp==.

tab country decexp if decexp~=.
tab country decexp if decexp>0
tab decexp, missing

tab country fiveexp if fiveexp~=.
tab country fiveexp if fiveexp>0
tab fiveexp, missing

tab country decdisp if decdisp~=.
tab country decdisp if decdisp>0
tab decdisp, missing

tab country fivedisp if fivedisp~=.
tab country fivedisp if fivedisp>0
tab fivedisp, missing

rename country countryn
rename countryid country
sort country

save "Data\cvarsMIDEXP.dta", replace

//end of file
************************************************************************************************************************************
