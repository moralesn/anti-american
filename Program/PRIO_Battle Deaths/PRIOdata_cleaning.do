//Author: Norberto R. Morales
//Project: Anti-American Public Opinion
//Purpose: Clean Militarized Disputes (MID) data (MID-4 level data at participant level)
//Created: 07/01/2013
//Last Updated: 09/22/2013
*Machine(Windows 7): S/No: 0039130478
****************************************************************************************************************
clear
capture cd "C:/Users/Antigone/Dropbox/Papers/anti-american_paper_data"   // set working directory
set more off
************************************************************************************************************************************
insheet using "Data-Raw\MID\MIDB_4.01.csv", comma clear  //read in data
save "Data\mid.dta", replace

/*
*mostly 2000-pakistan, iraq, iran, afganistan, syria, etc./2008-somalia, pakistan

/*keep if ccode==2 & styear<2007 & endyear>2007
sort dispnum3 */

*dispute 4575 starts before 2007 but ends after it

keep if styear==2007|endyear==2007|dispnum3==4575
sort dispnum3
*/

//Define temporal domain (1997-2007)
keep if styear>1996 & endyear<2008 
sort dispnum3


//Define countries included in data set
keep if ccode==2 | ccode==640 | ccode==770 | ccode==663 | ccode==600 | ccode==651 | ccode==850 | ccode==660 | ccode==530 | ccode==510 | ccode==690 | ccode==475 | ccode==771 | ccode==501 | ccode==750 | ccode==500| ccode==452 | ccode==433 | ccode==432 | ccode==437
sort dispnum3

duplicates report dispnum3
duplicates tag dispnum3 , gen(cases) //observations with duplicates are those where a country shares a dispute with the U.S. for a given dispute number
label var cases "Observations to be included in data"

tab cases, nomissing
drop if cases==0
tab cases, nomissing

gen narr=1 if cases>1
label var narr "Check MID narratives for U.S. aggresion"

list dispnum3 ccode styear if narr==1
drop if dispnum3==4186 //NATO coalition at Kosovo
drop if dispnum3==4273 //Iraq-Kuwait inccident
drop if dispnum3==4283 //Post-911 NATO
drop if dispnum3==4343 //NATO and Yugoslavia

drop if ccode==2 //drop US, such that only right hand side countries remain for merging


//Reshape dates accordingly

*Reshape long to stack Start Year with End Year for each country
sort ccode styear
gen cid=_n
label var cid "Observation Identifier"
order dispnum3 dispnum4 stabb ccode cid sidea, before (stday)

*Assigns prefix to all variables
renpfix "" v

reshape long v, i(vdispnum3-vsidea) j(year 1998-2007)

*Eliminates prefix to all variables
capture renpfix v ""

//Convert all -9s into missing values equalling (.).
mvdecode _all, mv(-9)

//Create conflict counter variables

replace cid=_n

*Code for conflict initiation and completion years
gen ini=1 if year==styear & styear~=. 
label var ini "Conflict initiation Year MID"

gen compl=1 if year==endyear & endyear~=.
label var compl "Conflict completion Year MID"

sort dispnum3

*Code for conflict duration for year 
by dispnum3: gen dur=1 if ini[_n-1]==1 & compl[_n-1]~=1 & compl~=1   
by dispnum3: replace dur=1 if dur[_n-1]==1 & compl~=1 & endyear~=. 
label var dur "Conflict years between start and end of conflict Year MID"

*Code for conflict for year
by dispnum3: gen disp=1 if ini==1 
by dispnum3: replace disp=1 if dur==1  
by dispnum3: replace disp=1 if compl==1 
label var disp "Conflict Year MID" 

keep if disp==1

//Generate decade and five year variable
gen dec=1 if year~=.
label var dec "Decade dummy"

gen five=1 if year>2002
replace five=0 if year<2003
label var five "Last Five Year Dummy"

save "Data\mid.dta", replace

//Collapse data by decade
sort ccode year
keep if dec==1
collapse (count) disp (max) hostlev fatality, by (ccode)

rename disp decdisp
rename hostlev dechost
rename fatality decdeaths

kountry ccode, from(cown) to (cowc)
rename _COWC_ country
replace country="ETH" if ccode==530
order country, before(ccode)

save "Data\decadeMID.dta", replace

//Collapse data by five years
use "Data\mid.dta", clear

sort ccode year
keep if five==1
collapse (count) disp (max) hostlev fatality, by (ccode) 

rename disp fivedisp
rename hostlev fivehost
rename fatality fivedeaths

kountry ccode, from(cown) to (cowc)
rename _COWC_ country
replace country="ETH" if ccode==530
order country, before(ccode)

save "Data\fiveMID.dta", replace

//end of file
************************************************************************************************************************************
