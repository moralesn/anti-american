//Author: Norberto R. Morales
//Project: Anti-American Public Opinion
//Purpose: Clean PRIO Data to create stages and size variables of conflict (JoinerA)
//Last Modified: May 9, 2013
//Machine(Windows 7): S/No: 0039130478
****************************************************************************************************************
*Available online at: http://www.prio.no/Data/Armed-Conflict/UCDP-PRIO/Armed-Conflicts-Version-X-2009/
****************************************************************************************************************
clear
capture cd "E:\PRIO"
set more off

*Import PRIO excel data into .dta format
import excel "Data-Raw\PRIO\Main Conflict Table.xls", firstrow sheet("Sheet1")

save "Data\prio.dta", replace

keep ID Location SideA2nd SideB2nd Terr Int CumInt Type StartDate StartDate2 EpEnd EpEndDate GWNOA2nd GWNOB2nd GWNOLoc Region

/*Generate Start Month and End Month variables

gen StartMonth1=month(StartDate2)
gen StartMonth2=month(StartDate)
gen EndMonth=month(EpEndDate)
order StartMonth1 StartMonth2 EndMonth, after(EpEndDate)
*/

gen year_1a=year(StartDate2)
label var year_1a "Year conflict begins with 25+ deaths PRIO"
order year_1a, after(GWNOA)

gen year_1b=year(EpEndDate)
label var year_1a "Year conflict ends PRIO"
order year_1b, after(year_1a)

gen year_2a=year(StartDate)
label var year_1a "Year conflict begins with 1+ deaths PRIO"
order year_2a, after(year_1b)

*Split GWNOA code and SideA name as strings
split GWNOA2nd, p(" , " ", ")
split SideA2nd, p(" , " ", ")
split GWNOB2nd, p(" , " ", ")
split SideB2nd, p(" , " ", ")

drop GWNOA2nd
drop SideA2nd
drop GWNOB2nd
drop SideB2nd

*Reshape to stack split Side names and GWNO codes
gen cid=_n
label var cid "Observation identifier"

sort ID year_1a
order year_1a year_1b year_2a cid, after(ID)

reshape long SideA2nd GWNOA2nd SideB2nd GWNOB2nd, i(ID-cid) j(stub)

keep if stub==1

*Destring GWNO codes to have numbers.
destring GWNOA2nd, replace ignore("," ", ")
destring GWNOB2nd, replace ignore("," ", ")

*Convert all -99s into missing values equalling (.). See PRIO codebook.
mvdecode _all, mv(-99)

*Code for the nature of conflict (intrastate or interstate)
gen jintra=1 if Type==3|Type==4
label var jintra "Intrastate conflict Joiners PRIO"

gen jextra=1 if Type==1|Type==2
label var jextra "Interstate conflict Joiners PRIO"

*Code for the size of conflict (large or small). Large if battle-related deaths => 999 and small if battle-related deaths => 25
gen jlarge=1 if Int==2
label var jlarge "Large sized conflict Joiners PRIO"

gen jsmall=1 if Int==1
label var jsmall "Minor sized conflict Joiners PRIO"

gen jsize= 1 if jsmall==1
replace jsize=2 if jlarge==1
label var jsize "Size of conflict Joiners PRIO"

*Reshape long to stack StartDate2/StartDate with EndDate for each country-year

replace cid=_n
order GWNOA2nd SideA2nd GWNOB2nd SideB2nd, after (cid)

*Assigns prefix to all variables
renpfix "" v

reshape long v, i(vID-vGWNOA2nd) j(year 1938-2011)

*Eliminates prefix to all variables
capture renpfix v ""

/*Reshape long to stack StartMonth1/StartMonth2 with EndMonth for each country-year

foreach v in `r(varlist)' {
    rename `v' v `v'
}

reshape long v, i(ID-GWNOA) j(month 1-12)
*/

*Code for conflict initiation for year 1 and completion 
gen jini1=1 if year==year_1a & year_1a~=. 
label var jini1 "Conflict initiation Year with 25+ deaths Joiners PRIO"

gen jcompl=1 if year==year_1b & year_1b~=.
label var jcompl "Conflict completion Joiners PRIO"

*Code for conflict initiation and completion for year 2
gen jini2=1 if year==year_2a & year_2a~=. 
label var jini2 "Conflict initiation Year with 1+ deaths Joiners PRIO"

sort cid

*Code for conflict preparation for years 1 and 2
by cid: gen jprep1=1 if jini1[_n+1]==1 
label var jprep1 "Conflict preparation Year with 25+ deaths Joiners PRIO"

by cid: gen jprep2=1 if jini2[_n+1]==1 
label var jprep2 "Conflict preparation Year with 1+ deaths Joiners PRIO"

*Code for conflict duration for year 1 & year 2
by cid: gen jdur1=1 if jini1[_n-1]==1 & jcompl[_n-1]~=1 & jcompl~=1   
by cid: replace jdur1=1 if jdur1[_n-1]==1 & jcompl~=1 & year_1b~=. 
label var jdur1 "Conflict years between start and end of conflict Year with 25+ deaths Joiners PRIO"

by cid: gen jdur2=1 if jini2[_n-1]==1 & jcompl[_n-1]~=1 & jcompl~=1  
by cid: replace jdur2=1 if jdur2[_n-1]==1 & jcompl~=1 & year_1b~=. 
label var jdur2 "Conflict years between start and end of conflict Year with 1+ deaths Joiners PRIO"

*Code for conflict for year 1 & year 2
by cid: gen jwar1=1 if jini1==1 
by cid: replace jwar1=1 if jdur1==1  
by cid: replace jwar1=1 if jcompl==1 
label var jwar1 "Conflict Year with 25+ deaths Joiners PRIO" 

by cid: gen jwar2=1 if jini2==1 
by cid: replace jwar2=1 if jdur2==1 
by cid: replace jwar2=1 if jcompl==1 
label var jwar2 "Conflict Year with 1+ deaths Joiners PRIO"

*Generate GWNOA codes for those countries that do not have one
sort SideA2nd GWNOA2nd
egen id= group(SideA2nd) if GWNOA2nd==.
replace GWNOA2nd=id if GWNOA2nd==.
replace GWNOA2nd= GWNOA2nd+2000 if id~=.
drop id

sort cid year

des
sum

save "Data\jprio_dyad1.dta", replace

keep ID Location SideA2nd cid year year_1a year_1b year_2a Terr Int CumInt Type jintra jextra jlarge jsmall jsize StartDate StartDate2 EpEnd EpEndDate GWNOA2nd GWNOLoc Region jcompl jini1 jini2 jprep1 jprep2 jdur1 jdur2 jwar1 jwar2   
keep if jwar1==1|jwar2==1|jprep1==1|jprep2==1

sort cid year

des
sum

save "Data\joinera_temp.dta", replace

******************************************************************************************************************************************
