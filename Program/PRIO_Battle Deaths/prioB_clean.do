//Author: Norberto Morales
//Project: Anti-American Public Opinion
//Purpose: Import and clean PRIO Data to create stages and battledeaths variables of conflict (sidea)
//Last Modified: September 22, 2014
//Machine(Windows 7): S/No: 0039130478
****************************************************************************************************************
*Available online at: http://www.prio.no/Data/Armed-Conflict/UCDP-PRIO/Armed-Conflicts-Version-X-2009/
****************************************************************************************************************
clear
capture cd "C:\Users\Antigone\Dropbox\Papers\anti-american_paper_data" // set working directory
set more off

*Import PRIO excel data into .dta format
import excel "Data-Raw\PRIO\PRIO_bd3.0.xls", firstrow sheet("bdonly")

save "Data\prio.dta", replace

keep id year bdeadlow bdeadhig bdeadbes location sidea sideb terr cumint startdate startdate2 epend ependdate gwnoa gwnob type

drop if gwnob~=2 //drop if not U.S. dyad based on sidea first

/*Generate Start Month and End Month variables

gen StartMonth1=month(startdate2)
gen StartMonth2=month(startdate)
gen EndMonth=month(ependdate)
order StartMonth1 StartMonth2 EndMonth, after(ependdate)
*/

gen year_1a=year(startdate2)
label var year_1a "Year conflict begins with 25+ deaths PRIO"
order year_1a, after(gwnoa )

gen year_1b=year(ependdate)
label var year_1a "Year conflict ends PRIO"
order year_1b, after(year_1a)

gen year_2a=year(startdate)
label var year_1a "Year conflict begins with 1+ deaths PRIO"
order year_2a, after(year_1b)

*Reshape to stack split Side names and Side codes
gen cid=_n
label var cid "Observation identifier"

sort id year_1a
order year_1a year_1b year_2a cid, after(id)

reshape long sidea gwnoa sideb gwnob, i(id-cid) j(stub)

keep if stub==1

*Destring gwnoa  and  gwnob codes to have numbers.
destring gwnoa , replace ignore("," ", ")
destring  gwnob, replace ignore("," ", ")

*Convert all -99s into missing values equalling (.). See PRIO codebook.
mvdecode _all, mv(-99)

*Code for the size of conflict (large or small). Large if battle-related deaths => 999 and small if battle-related deaths => 25
gen large=1 if Int==2
label var large "Large sized conflict PRIO"

gen small=1 if Int==1
label var small "Minor sized conflict PRIO"

gen size= 1 if small==1
replace size=2 if large==1
label var size "Size of conflict PRIO"

*Reshape long to stack startdate2/startdate with EndDate for each country-year

replace cid=_n
order gwnoa sidea gwnob sideb, after (cid)

*Assigns prefix to all variables
renpfix "" v

reshape long v, i(vid-vgwnoa) j(year 1938-2011)

*Eliminates prefix to all variables
capture renpfix v ""

/*Reshape long to stack StartMonth1/StartMonth2 with EndMonth for each country-year

foreach v in `r(varlist)' {
    rename `v' v `v'
}

reshape long v, i(id-ependdate) j(month 1-12)
*/

*Code for conflict initiation for year 1 and completion 
gen ini1=1 if year==year_1a & year_1a~=. 
label var ini1 "Conflict initiation Year with 25+ deaths PRIO"

gen compl=1 if year==year_1b & year_1b~=.
label var compl "Conflict completion PRIO"

*Code for conflict initiation and completion for year 2
gen ini2=1 if year==year_2a & year_2a~=. 
label var ini2 "Conflict initiation Year with 1+ deaths PRIO"

sort cid

*Code for conflict preparation for years 1 and 2
by cid: gen prep1=1 if ini1[_n+1]==1 
label var prep1 "Conflict preparation Year with 25+ deaths PRIO"

by cid: gen prep2=1 if ini2[_n+1]==1 
label var prep2 "Conflict preparation Year with 1+ deaths PRIO"

*Code for conflict duration for year 1 & year 2
by cid: gen dur1=1 if ini1[_n-1]==1 & compl[_n-1]~=1 & compl~=1   
by cid: replace dur1=1 if dur1[_n-1]==1 & compl~=1 & year_1b~=. 
label var dur1 "Conflict years between start and end of conflict Year with 25+ deaths PRIO"

by cid: gen dur2=1 if ini2[_n-1]==1 & compl[_n-1]~=1 & compl~=1  
by cid: replace dur2=1 if dur2[_n-1]==1 & compl~=1 & year_1b~=. 
label var dur2 "Conflict years between start and end of conflict Year with 1+ deaths PRIO"

*Code for conflict for year 1 & year 2
by cid: gen war1=1 if ini1==1 
by cid: replace war1=1 if dur1==1  
by cid: replace war1=1 if compl==1 
label var war1 "Conflict Year with 25+ deaths PRIO" 

by cid: gen war2=1 if ini2==1 
by cid: replace war2=1 if dur2==1 
by cid: replace war2=1 if compl==1 
label var war2 "Conflict Year with 1+ deaths PRIO"

sort cid year

des
sum

save "Data\prio_dyad1.dta", replace

keep id location sideacid year year_1a year_1b year_2a Type intra extra large small size startdate startdate2 EpEnd ependdate gwnoa gwnob compl ini1 ini2 prep1 prep2 dur1 dur2 war1 war2   
keep if war1==1|war2==1|prep1==1|prep2==1

sort cid year

des
sum

save "Data\sidea_temp.dta", replace

//end of file
******************************************************************************************************************************************
