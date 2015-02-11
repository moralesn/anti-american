//Author: Norberto R. Morales
//Project: Anti-American Public Opinion
//Purpose: Clean UN vote data (dyadic)
//Created: 07/01/2013
//Last Updated: 09/22/2013
//Machine(Windows 7): S/No: 0039130478
************************************************************************************************************************************
clear
capture cd "C:\Users\Antigone\Dropbox\Papers\anti-american_paper_data" // set working directory
set more off
************************************************************************************************************************************
use "Data-Raw\UN\dyad_votedist.dta", clear

//Keep variable for 1998-2007
keep if year>1997 & year<2008

keep if ccode1==2
drop if ccode2==2

//Generate decade and five year variable
gen dec=1 if year~=.
label var dec "Decade dummy"

gen five=1 if year>2002
replace five=0 if year<2003
label var five "Last Five Year Dummy"

//Generate country name variable

rename countryname2 countryn

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

//Collapse by country of interest (21 countries in BlaydesLinzer APSR 2012):

//Collapse data by ten years

sort ccode2 year
collapse (rawsum) idealpointdistance (max) dec five country, by (ccode2 year) 

rename idealpointdistance vote

save "Data\dyad_votedist.dta", replace

by ccode2: egen decvote=mean(vote) if dec==1

by ccode2: egen fivevote=mean(vote) if five==1

rename ccode2 ccode
keep if year==2007
keep if country~=.

sort country

save "Data\dyad_votedist.dta", replace

//end of file
************************************************************************************************************************************

