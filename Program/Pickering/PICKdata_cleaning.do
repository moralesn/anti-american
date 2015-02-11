//Author: Norberto Morales
//Project: Anti-American Public Opinion
//Purpose: Explore Low-Level Battle Data (Pickering)
//Created: 07/01/2013
//Last Updated: 09/22/2013
*Machine(Windows 7): S/No: 0039130478
****************************************************************************************************************
clear
capture cd "C:\Users\Antigone\Dropbox\Papers\anti-american_paper_data" // set working directory
set more off
************************************************************************************************************************************
use "Data-Raw\Pickering\pickering.dta", clear  //read in data

keep if VAR_001==2
kountry  VAR_002, from(cown) to (cowc)
order _COWC_, before(VAR_002)

//end of file
************************************************************************************************************************************
