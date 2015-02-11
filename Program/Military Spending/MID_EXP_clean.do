//Author: Norberto R. Morales
//Project: Anti-American Public Opinion
//Purpose: Clean Militarized Disputes (MID) data to merge to military aid data
//Created: 07/01/2013
//Last Updated: 09/22/2013
//Machine(Windows 7): S/No: 0039130478
************************************************************************************************************************************
clear
capture cd "C:\Users\Antigone\Dropbox\Papers\anti-american_paper_data" // set working directory
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
 
#delimit ;
keep if dispnum3==4188|dispnum3==4193| dispnum3==4208| dispnum3==4210| dispnum3==4215|dispnum3== 4221|dispnum3==4223|dispnum3==4233|
dispnum3==4248|dispnum3==4252|dispnum3==4255|dispnum3==4256|dispnum3==4258|dispnum3==4264|dispnum3==4265|dispnum3==4273|dispnum3==4274|
dispnum3==4275|dispnum3==4277|dispnum3==4289|dispnum3==4290|dispnum3==4291|dispnum3==4305|dispnum3==4310|dispnum3==4311|dispnum3==4313|
dispnum3==4320|dispnum3==4324|dispnum3==4325|dispnum3==4344|dispnum3==4345|dispnum3==4346|dispnum3==4349|dispnum3==4350|dispnum3==4352|
dispnum3==4353|dispnum3==4355|dispnum3==4356|dispnum3==4357|dispnum3==4362|dispnum3==4363|dispnum3==4369|dispnum3==4372|dispnum3==4373|
dispnum3==4374|dispnum3==4375|dispnum3==4376|dispnum3==4378|dispnum3==4380|dispnum3==4381|dispnum3==4383|dispnum3==4383|dispnum3==4385|
dispnum3==4387|dispnum3==4389|dispnum3==4391|dispnum3==4394|dispnum3==4410|dispnum3==4415|dispnum3==4431|dispnum3==4434|dispnum3==4435|
dispnum3==4452|dispnum3==4453|dispnum3==4458|dispnum3==4460|dispnum3==4471|dispnum3==4516|dispnum3==4520|dispnum3==4525|dispnum3==4528|
dispnum3==4531|dispnum3==4552|dispnum3==4553|dispnum3==4554|dispnum3==4555|dispnum3==4556|dispnum3==4557|dispnum3==4558|dispnum3==4559|
dispnum3==4560|dispnum3==4561|dispnum3==4562|dispnum3==4563|dispnum3==4564|dispnum3==4565|dispnum3==4566|dispnum3==4567|dispnum3==4568|
dispnum3==4570|dispnum3==4571|dispnum3==4576|dispnum3==4577|dispnum3==4580|dispnum3==4133|dispnum3==4138;

drop if ccode==2 

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

sort ccode year

//Merge MID to EXP
merge ccode year using "Data\EXP.dta"

duplicates report dispnum3 ccode year 
duplicates list dispnum3 ccode year

sort dispnum3 ccode year

rename _merge _merge1
keep if dispnum3~=.
sort ccode year

save "Data\EXPmain.dta", replace

//Merge EXP names to MID
merge ccode using "Data\namesexp.dta"

duplicates report dispnum3 ccode year 

sort dispnum3 ccode year
save "Data\EXPmain.dta", replace

//Collapse by country of interest (21 countries in BlaydesLinzer APSR 2012):

//Collapse data by ten years
by dispnum3: replace ccode2=ccode2[_n+_n] if ccode2==.
by dispnum3: replace ccode2=ccode2[_n-1] if ccode2==.  

drop if country==country2

use "Data\EXPmain.dta", clear

sort ccode2 year
collapse (rawsum) FY (max) dec five, by (ccode2 year) 

rename FY exp

by ccode2: egen decexp=sum(exp) if dec==1

by ccode2: egen fiveexp=sum(exp) if five==1

rename ccode2 ccode
keep if year==2007

save "Data\EXPmain.dta", replace

//end of file
************************************************************************************************************************************

