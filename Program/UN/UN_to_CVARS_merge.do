//Author: Norberto R. Morales
//Project: Anti-American Public Opinion
//Purpose: Merge Dyadic Militarized Disputes (MID) and UN vote data
//Created: 07/01/2013
//Last Updated: 09/22/2013
*Machine(Windows 7): S/No: 0039130478
****************************************************************************************************************
clear
capture cd "C:\Users\Antigone\Dropbox\Papers\anti-american_paper_data" // set working directory
set more off
************************************************************************************************************************************
//Merge MID and UN data
use "Data\cvarsMIDEXP.dta", clear

sort country

merge country using "Data\dyad_votedist.dta"
duplicates report country 

tab _merge
drop _merge

drop country
rename countryn country

//Replace missing values with zeroes for countries for which there were no vote distance data with U.S.
replace decvote=0 if decvote==.
replace fivevote=0 if fivevote==.

sum decvote if decvote~=., d
sum decvote if decvote>0, d

save "Data\cvarsALL.dta", replace

//end of file
************************************************************************************************************************************

