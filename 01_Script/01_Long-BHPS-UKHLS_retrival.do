/*****************************************************************************************
* MERGING INDIVIDUAL FILES FROM HARMONISED BHPS AND UKHLS IN LONG FORMAT                 *
* To match individual level files from the harmonised BHPS and Understanding Society     *
* in long format, you need to remove the wave prefixes in the two sets of files and      *
* generate a wave identifier that works across both sets of files. The pidp will         *
* work as the unique cross-wave identifier across both sets of files. This code only     *
* keeps individuals who took part in BHPS and drops those who joined as part of          *
* Understanding Society.                                                                 *
*****************************************************************************************/

// change current file location
cd "C:\work\Forschung\Brexit-Deprivation\09_Replication\02_Data"

// assign global macro to refer to Understanding Society data
global ukhls "C:\work\Forschung\Brexit-Deprivation\09_Replication\02_Data\UKDA-6931-stata\stata\stata13_se"

// assign global macro to refer to geo identifiers
global geo "C:\work\Forschung\Brexit-Deprivation\09_Replication\02_Data\UKDA-7248-stata\stata\stata13\ukhls"


// assign global macros for the lists of waves
global BHPSwaves "a b c d e f g h i j k l m n o p q r"
global UKHLSwaves_bh "b c d e f g h i j" // since BHPS respondents did not take 
									 // part in Wave 1, begin at Wave 2
									 // - update this to include 
									 // new waves as they are released
global UKHLSwaves "a b c d e f g h i j" // use all UKHLS waves
global UKHLSno 10	// number of waves of UKHLS data	



*-------------------------------------------------------------
*------------------- Individual level ------------------------			
*-------------------------------------------------------------				 


// loop through the relevant waves of Understanding Society
foreach w of global UKHLSwaves {

	// find the wave number
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz","`w'")
	
	// open the individual level file for that wave
	// use pidp pid `w'_age_dv `w'_paygu_dv using "$ukhls/ukhls_w`waveno'/`w'_indresp_protect", clear
	use "$ukhls/ukhls_w`waveno'/`w'_indresp_protect", clear
	
	// keep relevant variables
	
	* identifiers
	global vars_id pidp hidp pno memorig 
	
	* meta data 
	global var_met buno buno_dv sampst ivfio istrtdatd istrtdatm istrtdaty intdatd_dv intdatm_dv intdaty_dv indin01_lw indinus_xw indinub_xw indinui_xw
	
	* household
	global vars_hh nchild_dv husits huboss 
	
	* locality
	global vars_loc gor_dv mvever mvmnth mvyr origadd adcts addrmov_dv movdir lkmove xpmove xpmvmnth xpmvyr plnowm plnowy4 urban_dv
	
	* demographics
	// global vars_dem age_dv ukborn bornuk_dv plbornc_cc sex sex_dv mastat marstat jbstat race racel_dv nchild_dv ethn_dv
	global vars_dem age_dv sex sex_dv plbornc citzn1 yr2uk4 mastat marstat_dv race racel_dv depchl_dv fnspno mnspno mnspid fnspid
	
	* socec
	// global vars_socec hiqual_dv jbsoc90_cc jbsoc00_cc jbnssec_dv jbnssec8_dv jbnssec5_dv jbnssec3_dv fimnnet_dv fimnlabnet_dv a_fimnlabgrs_dv paygu_dv
	global vars_socec jbstat jbsemp jbhrs jbot jbttwt basnsa basrate basrest paynu_dv paygu_dv fimnlabgrs_dv fimngrs_dv fimnlabnet_dv fimnnet_dv jbnssec_dv jbnssec8_dv jbnssec5_dv jlnssec_dv jlnssec8_dv jlnssec5_dv finnow finfut save fiyrinvinc_dv fiyrdia debty
	
	* Employment
	global vars_empl jbstat jbes2000 nmpsp_dv nnmpsp_dv jbnssec3_dv jbnssec5_dv jbnssec_dv
	
	* educ
	global vars_educ edasp edtype feend hiqual_dv nhiqual_dv qfhigh_dv scend school
	
	* Neighbourhood
	global vars_nb simarea nbrcoh1 nbrcoh2 nbrcoh3 nbrcoh4 nbrcoh_dv nbrcohdk_dv nbrsnci_dv crdark llknbrd locserap locseras locserb locserc locserd locsere scopngbha scopngbhb scopngbhc scopngbhd scopngbhe scopngbhf scopngbhg scopngbhh
	
	* Distances
	global vars_dis distmov distmov_dv jsworkdis workdis mafar pafar chfar mlivedistf mlivedist
	
	* Attitudes
	global vars_att1 scopfama scopfamb scopfamd scopfamf scopfamh oprlg1 oprlg2 vote1 vote2 vote3 vote4 vote5 vote6 vote7 vote8 vote3_all poleff1 poleff2 poleff3 poleff4 
	global vars_att2 britid colbens1 colbens2 colbens3 colbens4 demorient futrd futre orga1 orgm1 talkmatter voteintent votenorm 
	
	* Health
	global vars_health sclfsat1 scsf1 sf1 sclfsato scghq1_dv scghq2_dv
	
	* Referendum 
	global vars_ref eumem euref voteeuref scotvot1 scotvot2 scotvot3 scotvot4
	
	// remove the wave prefix
	rename `w'_* *
	
	// keep relevant variables
	* keep variables (continue if not available)
	global varlist1 $vars_id
	global varlist $vars_dem $vars_loc $vars_educ $vars_socec $vars_empl $vars_hh $var_met $vars_nb $vars_dis $vars_att1 $vars_att2 $vars_health $vars_ref
	foreach v of global varlist {
		capture confirm var `v'							//Var exists?
		if !_rc {
			global varlist1 ${varlist1} `v'	//only existing vars in varlist1
		}
	}		
	keep $varlist1

	// generate a variable which records the wave number + 17 
	// - treating wave 2 ukhls as wave 19 of bhps --> TR: changed to 18!
	gen wave=`waveno'+18
	
	// save the file for future use
	save tmp_`w'_indresp, replace
}



// loop through the waves of ukhls from Wave 1
foreach w of global UKHLSwaves {

	if "`w'"=="a" {
	
		// reopen the first file created
		use tmp_`w'_indresp, clear
		
	// following times through the loop	
	} 
	else {
	
		// append each file in turn
		append using tmp_`w'_indresp
	
	}
	

}


// loop through the waves of ukhls 
// (using the global macro UKHLSno to define the last wave)
foreach n of numlist 1/$UKHLSno {
	
	// calculate which label value this label will apply to
	local waveref=`n'+18
	
	// add a label for each wave in turn
	lab def wave `waveref' "UKHLS Wave `n'", modify
}

// apply the label to the wave variable
lab val wave wave

// check how many observations are available from each wave
tab wave

// order
gen year =  intdaty_dv
order $vars_id $varlist
order pidp hidp pno wave year memorig

// Sort by id year
sort pidp wave

// Use xwavedat to update person-constant variables
global var_con birthm birthy ukborn bornuk_dv plbornc scend_dv feend_dv school_dv racel_dv ethn_dv generation yr2uk4 evermar_dv anychild_dv
merge m:1 pidp using "$ukhls\ukhls_wx\xwavedat_protect", keepusing(pidp $var_con)  update replace
drop if _merge<=2
drop _merge

// Code negatives as missing values
mvdecode _all, mv(-21/-10=.a\-9=.\-8/-7=.b\-2/-1=.)

// save the file containing all waves
save all_indresp, replace

// erase each temporary file using loops
foreach w of global UKHLSwaves {
	erase tmp_`w'_indresp.dta
}



*------------------------------------------------------------
*------------------- Household level ------------------------
*------------------------------------------------------------

// loop through the relevant waves of Understanding Society
foreach w of global UKHLSwaves {

	// find the wave number
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz","`w'")
	
	// open the hh level file for that wave
	use "$ukhls/ukhls_w`waveno'/`w'_hhresp_protect", clear
	
	// keep relevant variables
	
	* identifiers
	global vars_hid hidp
	
	* demographics 
	global var_hdem hhsize hhtype hhtype_dv agechy_dv nkids_dv nch02_dv nch34_dv nch511_dv nch1215_dv nemp_dv ncouple_dv fihhmngrs_dv fihhmnnet1_dv fihhmnlabnet_dv ieqmoecd_dv nemp_dv

	* accomodation
	global var_haccom tenure_dv hsownd hsval hscost mglife mgold mgnew hsroom rent rent_dv rentgrs_dv xphsdb
	global var_haccom2 mgynot mgextra houscost1_dv houscost2_dv 
	
	* neighbourhood
	global var_hnb crburg crcar crdrnk crdrnk crgraf crmugg crrace crrubsh crteen crvand ctband_dv
	
	* hh deprivation
	global var_hdep matdepa	matdepb matdepc	matdepd	matdepe	matdepf matdepg matdeph	matdepi	matdepj carown carval xphsdba xphsdb xphsdct hheat
	
	* pensioner deprivation
	global var_pdep pdepa1 pdepb1 pdepc1 pdepd1 pdepe1 pdepf1 pdepg1 pdeph1 pdepi1 pdepk1 pdepl1 pdepm1 pdepn1 pdepo1 pdepa2 pdepb2 pdepc2 pdepd2 pdepe2 pdepf2 pdepg2 pdeph2 pdepi2 pdepk2 pdepl2 pdepm2 pdepn2 pdepo2 npensioner npensioner_dv

	* child deprivation
	global var_cdep cdepdo1 cdepdo2 cdepdo3 cdepdo4 cdepdo5 cdepdo6 cdephave1 cdephave2 cdephave3 cdephave4 nkids015
	
	* reference person
	global var_head hrpid hrpno
	
	// remove the wave prefix
	rename `w'_* *
	capture confirm var origadd						//Var exists?
	if !_rc {
		rename origadd hhorigadd	//rename (same var in indresp
	}
	
	
	// keep relevant variables
	* keep variables (continue if not available)
	global varlist1 $vars_hid
	global varlist $var_hdem $var_haccom $var_haccom2 $var_henv $var_hdep $var_hnb $var_pdep $var_cdep $var_head 
	foreach v of global varlist {
		capture confirm var `v'							//Var exists?
		if !_rc {
			global varlist1 ${varlist1} `v'	//only existing vars in varlist1
		}
	}		
	keep $varlist1

	// generate a variable which records the wave number + 17 
	// - treating wave 2 ukhls as wave 19 of bhps --> TR: changed to 18!
	gen wave=`waveno'+18
	
	// save the file for future use
	save tmp_`w'_hhresp, replace
}

// loop through the waves of ukhls from Wave 1
foreach w of global UKHLSwaves {

	// first time through the loop
	if "`w'"=="a" {
	
		// reopen the first file created
		use tmp_`w'_hhresp, clear
		
	// following times through the loop	
	} 
	else {	
		
		// append each file in turn
		append using tmp_`w'_hhresp
	}
}

// loop through the waves of ukhls 
// (using the global macro UKHLSno to define the last wave)
foreach n of numlist 1/$UKHLSno {
	
	// calculate which label value this label will apply to
	local waveref=`n'+18
	
	// add a label for each wave in turn
	lab def wave `waveref' "UKHLS Wave `n'", modify
}

// apply the label to the wave variable
lab val wave wave

// check how many observations are available from each wave
tab wave

// order
order $vars_hid $varlist
order hidp wave

// Sort by id year
sort hidp wave


// Code negatives as missing values
mvdecode _all, mv(-21/-10=.a\-9=.\-8/-7=.b\-2/-1=.)


// save the file containing all waves
save all_hhresp, replace

// erase each temporary file using loops
foreach w of global UKHLSwaves {
	erase tmp_`w'_hhresp.dta
}


*--------------------------------------------------------------------
*------------------- Prepare geo identifiers ------------------------
*--------------------------------------------------------------------


// loop through the relevant waves of Understanding Society
foreach w of global UKHLSwaves {

	// find the wave number
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz","`w'")
	
	// open the individual level file for that wave
	use "$geo/`w'_lsoa11_protect", clear
	
	// remove the wave prefix
	rename `w'_* *
	
	// Drop hip
	keep hidp lsoa11

	// generate a variable which records the wave number + 17 
	// - treating wave 2 ukhls as wave 19 of bhps --> TR: changed to 18!
	gen wave=`waveno'+18
	
	// gen year
	gen year=1990+`waveno'+18
	
	// save the file for future use
	save tmp_`w'_lsoa, replace
}

// loop through the waves of ukhls from Wave 1
foreach w of global UKHLSwaves {
	
		// first time through the loop
	if "`w'"=="a" {
	
		// reopen the first file created
		use tmp_`w'_lsoa, clear
		
	// following times through the loop	
	} 
	else {	
		
		// append each file in turn
		append using tmp_`w'_lsoa
	}
}


// loop through the waves of ukhls 
// (using the global macro UKHLSno to define the last wave)
foreach n of numlist 1/$UKHLSno {
	
	// calculate which label value this label will apply to
	local waveref=`n'+18
	
	// add a label for each wave in turn
	lab def wave `waveref' "UKHLS Wave `n'", modify
}

// apply the label to the wave variable
lab val wave wave

// check how many observations are available from each wave
tab wave

// Sort by id year
sort hidp wave


// save the file containing all waves
save all_lsoa, replace

// erase each temporary file using loops
foreach w of global UKHLSwaves {
	erase tmp_`w'_lsoa.dta
}



*-------------------------------------------------------
*------------------- Merge data ------------------------
*-------------------------------------------------------

// Load ind
use all_indresp, clear

// merge hh data
merge m:1 hidp wave using all_hhresp
drop if _merge==2
drop _merge

// merge lsoa data
merge m:1 hidp wave using all_lsoa
drop if _merge==2
drop _merge


// Sort by id wave
sort hidp wave

// save the file containing all waves
save all_ukhls_stata, replace 

// save the file containing all waves
saveold all_ukhls, replace nolabel version(12) 

// Delete temp files
erase all_hhresp.dta
erase all_indresp.dta
erase all_lsoa.dta


