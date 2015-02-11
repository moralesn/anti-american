//Author: Norberto R. Morales
//Project: Anti-American Public Opinion
//Purpose: Collapse PRIO Data 
//Last Modified: May 9, 2013
//Machine(Windows 7): S/No: 0039130478
****************************************************************************************************************
*Available online at: http://www.prio.no/Data/Armed-Conflict/UCDP-PRIO/Armed-Conflicts-Version-X-2009/
****************************************************************************************************************
clear
capture cd "E:\PRIO"
set more off

****************************************************************************************************************************************************************
*Collapse PRIO data for main sides into country-year units.  Collapse using the rawsum of conflict episodes for all stages of conflict, size and intra/extra
****************************************************************************************************************************************************************
use "Data\prio1_temp.dta", clear

*Collapse conflict stages by SideA, ccode, and year

collapse (rawsum)intra extra large small ini1 compl ini2 prep1 prep2 dur1 dur2 war1 war2, by(SideA ccode year)

duplicates report ccode year

renpfix "" p
renpfix pc c
renpfix py y

sort ccode year

*Re-label all variables of interest
label var pintra "Intrastate conflict PRIO"
label var pextra "Interstate conflict PRIO"
label var psmall "Minor sized conflict PRIO"
label var plarge "Large sized conflict PRIO"
label var pini1 "Conflict initiation Year with 25+ deaths PRIO"
label var pini2 "Conflict initiation Year with 1+ deaths PRIO"
label var compl "Conflict completion PRIO"
label var pprep1 "Conflict preparation Year with 25+ deaths PRIO"
label var pprep2 "Conflict preparation Year with 1+ deaths PRIO"
label var pdur1 "Conflict years between start and end of conflict Year with 25+ deaths PRIO"
label var pdur2 "Conflict years between start and end of conflict Year with 1+ deaths PRIO"
label var pwar1 "Conflict Year with 25+ deaths PRIO" 
label var pwar2 "Conflict Year with 1+ deaths PRIO"

save "Final Data\priomain.dta", replace

/*
****************************************************************************************************************************************************************
*Collapse PRIO data for joiners into country-year units.  Collapse using the rawsum of conflict episodes for all stages of conflict, size and intra/extra
****************************************************************************************************************************************************************
use "Data\prio2_temp.dta", clear

*Collapse conflict stages by SideA2nd, ccode and year

collapse (rawsum)jintra jextra jlarge jsmall jini1 jcompl jini2 jprep1 jprep2 jdur1 jdur2 jwar1 jwar2, by(SideA2nd ccode year)

duplicates report ccode year

renpfix "" p
renpfix pc c
renpfix py y

sort ccode year

*Re-label all variables of interest
label var pjintra "Intrastate conflict Joiners PRIO"
label var pjextra "Interstate conflict Joiners PRIO"
label var pjsmall "Minor sized conflict Joiners PRIO"
label var pjlarge "Large sized conflict Joiners PRIO"
label var pjini1 "Conflict initiation Year with 25+ deaths Joiners PRIO"
label var pjini2 "Conflict initiation Year with 1+ deaths Joiners PRIO"
label var pjcompl "Conflict completion Joiners PRIO"
label var pjprep1 "Conflict preparation Year with 25+ deaths Joiners PRIO"
label var pjprep2 "Conflict preparation Year with 1+ deaths Joiners PRIO"
label var pjdur1 "Conflict years between start and end of conflict Year with 25+ deaths Joiners PRIO"
label var pjdur2 "Conflict years between start and end of conflict Year with 1+ deaths Joiners PRIO"
label var pjwar1 "Conflict Year with 25+ deaths Joiners PRIO" 
label var pjwar2 "Conflict Year with 1+ deaths Joiners PRIO"

save "Final Data\priojoiners.dta", replace

//end file
*******************************************************************************************************************************************
