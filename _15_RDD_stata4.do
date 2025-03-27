
*cd "/home/depp/chercheurs/ltouitou/Data/"
clear
cd "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Data/Output"

global graph "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Graphs"


***
use cohort_2018_cleanP_imp.dta
keep ID_Eleve  Sexe Date_Naiss T1_Math_z T2_Math_z T3_Math_z
gen year=2018
gen dateT1=td(20sep2018)
gen dateT2=td(20jan2019)
gen dateT3=td(20sep2019)
save data_RDD,replace

forval y =2019/2021 {
use cohort_`y'_cleanP_imp.dta,clear
keep ID_Eleve  Sexe Date_Naiss T1_Math_z T2_Math_z T3_Math_z
gen year=`y'
global year = `y'
global yearp1 = `y'+1
gen dateT1=td(20sep$year)
gen dateT2=td(20jan$yearp1)
gen dateT3=td(20sep$yearp1)
append using data_RDD
save data_RDD, replace
}


use data_RDD, clear

gen date_birth=date(Date_Naiss,"YMD")
gen m_birth=month(date_birth)
gen d_birth=day(date_birth)
gen y_birth=year(date_birth)
tab y_birth
format date_birth %tdddmonYY

gen girl= (Sexe==2) if Sexe!=.

gen ageT1=dateT1-date_birth
gen ageT2=dateT2-date_birth
gen ageT3=dateT3-date_birth


* results on all cohorts:
drop ID_Eleve Sexe dateT1 dateT2 dateT3 Date_Naiss m_birth d_birth y_birth

matrix estimates = J(1,23,0)
matrix colnames estimates= year test girl  donought Nobs  estimate_left conventional_estimate biascorrected_estimate Nobs_bandwidth standard_p_val robust_p_val conventional_se robust_se poly_order days_left days_right Nobs_left Nobs_right diff p_val_dens_bc p_val_dens_cl t_dens_bc t_dens_cl 


forval don =0/1 {
	forval year = 2019/2021 {
			forval test=1/3 {

global y= `year'-6
global cut=td(1jan$y)
cap drop z
gen z=date_birth-$cut

if `don' ==0 {
global cond_don = " z>-360 & z<360"	
}

if `don' ==1 {
global cond_don = "(z>-360 & z<360) & !(z>=-2 & z<=3)"	
}

**** boys:
rdrobust T`test'_Math_z z if   girl==0  &  $cond_don , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)
local p_val_c=2*(1-normal(abs(e(tau_bc)/e(se_tau_rb))))
local p_val_r=2*(1-normal(abs(e(tau_cl)/e(se_tau_cl))))
local nobs= e(N_h_l)+e(N_h_r)
matrix pol=e(beta_p_l)
local est_l=pol[1,1]
local est_cl_B = e(tau_cl)
local est_bc_B = e(tau_bc)
local se_cl_B = e(se_tau_cl)
local se_bc_B = e(se_tau_rb)
matrix define res_B =  [$y, `test' , 0 , `don', e(N),  `est_l', e(tau_cl), e(tau_bc), `nobs', `p_val_c', `p_val_r', e(se_tau_cl), e(se_tau_rb), e(p), round(e(h_l)),round(e(h_r)), e(N_h_l), e(N_h_r) , 0 ]
*matrix define res_sd_B = [.,.,.,.,.,.,e(se_tau_cl), e(se_tau_rb),.,.,.,.,.,.,.,.,.,.,0]
rddensity  z if   girl==0  &  $cond_don ,  all 
matrix define res_B =  res_B ,  [e(pv_q) ,  e(pv_p) ,  e(T_q)  , e(T_p)  ]

**** girls:
rdrobust T`test'_Math_z z if   girl==1  &  $cond_don , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)
local p_val_c=2*(1-normal(abs(e(tau_bc)/e(se_tau_rb))))
local p_val_r=2*(1-normal(abs(e(tau_cl)/e(se_tau_cl))))
local nobs= e(N_h_l)+e(N_h_r)
matrix pol=e(beta_p_l)
local est_l=pol[1,1]
local est_cl_G = e(tau_cl)
local est_bc_G = e(tau_bc)
local se_cl_G = e(se_tau_cl)
local se_bc_G = e(se_tau_rb)
matrix define res_F =  [$y, `test' , 1 , `don', e(N),  `est_l', e(tau_cl), e(tau_bc), `nobs', `p_val_c', `p_val_r', e(se_tau_cl), e(se_tau_rb), e(p), round(e(h_l)),round(e(h_r)), e(N_h_l), e(N_h_r) ,0]
rddensity  z if   girl==1  &  $cond_don ,  all 
matrix define res_F =  res_F ,  [e(pv_q) ,  e(pv_p) ,  e(T_q)  , e(T_p)  ]

*** diff girls minus boys:
local diff_cl= `est_cl_G' - `est_cl_B'
local diff_bc= `est_bc_G' - `est_bc_B'
local se_diff_cl = sqrt( (`se_cl_G')^2 + (`se_cl_B')^2  )
local se_diff_bc = sqrt( (`se_bc_G')^2 + (`se_bc_B')^2  )
local p_val_diff_cl=2*(1-normal(abs( `diff_cl' /`se_diff_cl')))
local p_val_diff_bc=2*(1-normal(abs(`diff_bc' /`se_diff_bc')))
matrix define diff =  [.,.,.,.,.,.,`diff_cl', `diff_bc',.,`p_val_diff_cl', `p_val_diff_bc',`se_diff_cl',`se_diff_bc',.,.,.,.,.,1,.,.,.,.]
*matrix define diff_sd =  [.,.,.,.,.,.,`se_diff_cl', `se_diff_bc',.,.,.,.,.,.,.,.,.,.,2,.,.,.,.]

matrix estimates=[estimates \ res_B \  res_F \  diff  ]
matrix drop res_B    res_F   diff 
}
}
}


matrix list estimates
matrix estimates = estimates[2...,1...]
clear 
svmat estimates , names(col)

save estimates_RDD_raw, replace
*use estimates_RDD_raw,replace

use estimates_RDD_raw, clear
foreach var in  estimate_left conventional_estimate biascorrected_estimate conventional_se robust_se p_val_dens_bc p_val_dens_cl {
tostring(`var'), generate(`var'2) format(%9.3f) force	
drop `var'
rename 	`var'2 `var'
replace `var'="" if `var'=="."
}

*** parenthesis sd:
replace conventional_se= "(" + conventional_se + ")" 
replace robust_se= "(" + robust_se + ")" 

** stars:
replace conventional_estimate= conventional_estimate + " " + conventional_se
replace conventional_estimate= conventional_estimate + " ***" if standard_p_val<0.01
replace conventional_estimate= conventional_estimate + " *" if standard_p_val<=0.1 & standard_p_val>0.1 
replace conventional_estimate= conventional_estimate + " **" if standard_p_val<=0.05 & standard_p_val>0.01 

replace biascorrected_estimate= biascorrected_estimate + " " + robust_se
replace biascorrected_estimate= biascorrected_estimate + " ***" if robust_p_val<0.01
replace biascorrected_estimate= biascorrected_estimate + " *" if robust_p_val<=0.1 & robust_p_val>0.1 
replace biascorrected_estimate= biascorrected_estimate + " **" if robust_p_val<=0.05 & robust_p_val>0.01 

export excel using "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables/RDD_tests2", firstrow(variables) replace

rename test Test
gen temp= "T1" if Test==1
replace temp= "T2" if Test==2
replace temp= "T3" if Test==3

drop Test
rename temp Test

gen Sample= "Boys" if girl==0
replace Sample = "Girls" if girl==1
replace Sample = "Girls - Boys" if girl==.

rename year Year
gen Bandwidth_size = (days_left+days_right)/2

order Year Test Sample conventional_estimate biascorrected_estimate Nobs Nobs_bandwidth Bandwidth_size p_val_dens_bc p_val_dens_cl

replace Year= Year[_n-1] if Year==.
replace Test=Test[_n-1] if Test==""
export excel using "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables/RDD_tests_final2", firstrow(variables) replace



******************************************************
****** balancing tests for covariates
******************************************************

*** We retrieve covariates from intial data
use cohort_2018_cleanP_imp.dta,clear
keep ID_Eleve  Sexe Date_Naiss T1_Math_z T2_Math_z T3_Math_z   IPS_Etab_CP boy_proportion  Taille_Classe
gen year=2018
gen dateT1=td(20sep2018)
gen dateT2=td(20jan2019)
gen dateT3=td(20sep2019)
save data_RDD2,replace

forval y =2019/2021 {
use cohort_`y'_cleanP_imp.dta,clear
keep ID_Eleve  Sexe Date_Naiss T1_Math_z T2_Math_z T3_Math_z    IPS_Etab_CP boy_proportion  Taille_Classe
gen year=`y'
global year = `y'
global yearp1 = `y'+1
gen dateT1=td(20sep$year)
gen dateT2=td(20jan$yearp1)
gen dateT3=td(20sep$yearp1)
append using data_RDD2
save data_RDD2, replace
}


use data_RDD2, clear

gen date_birth=date(Date_Naiss,"YMD")
gen m_birth=month(date_birth)
gen d_birth=day(date_birth)
gen y_birth=year(date_birth)
tab y_birth
format date_birth %tdddmonYY

gen girl= (Sexe==2) if Sexe!=.

gen ageT1=dateT1-date_birth
gen ageT2=dateT2-date_birth
gen ageT3=dateT3-date_birth

* results on all cohorts:
drop ID_Eleve Sexe dateT1 dateT2 dateT3 Date_Naiss m_birth d_birth y_birth T1_Math_z T2_Math_z T3_Math_z

matrix estimates = J(1,19,0)
matrix colnames estimates= variable test girl  donought Nobs  estimate_left conventional_estimate biascorrected_estimate Nobs_bandwidth standard_p_val robust_p_val conventional_se robust_se poly_order days_left days_right Nobs_left Nobs_right diff

sum   IPS_Etab_CP boy_proportion  Taille_Classe
global varlist   IPS_Etab_CP boy_proportion  Taille_Classe
foreach var in $varlist {
	forval don =0/1 {
		forval year = 2019/2021 {	

global y= `year'-6
global cut=td(1jan$y)
cap drop z
gen z=date_birth-$cut


if `don' ==0 {
global cond_don = " z>-360 & z<360"	
}

if `don' ==1 {
global cond_don = "(z>-360 & z<360) & !(z>=-2 & z<=3)"	
}

display "``''"
rdrobust `var'  z  if   girl==0  &  $cond_don , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)
local p_val_c=2*(1-normal(abs(e(tau_bc)/e(se_tau_rb))))
local p_val_r=2*(1-normal(abs(e(tau_cl)/e(se_tau_cl))))
local nobs= e(N_h_l)+e(N_h_r)
matrix pol=e(beta_p_l)
local est_l=pol[1,1]

local est_cl_B = e(tau_cl)
local est_bc_B = e(tau_bc)

local se_cl_B = e(se_tau_cl)
local se_bc_B = e(se_tau_rb)

matrix define res_B =  [$y, 0 , 0 , `don', e(N),  `est_l', e(tau_cl), e(tau_bc), `nobs', `p_val_c', `p_val_r', e(se_tau_cl), e(se_tau_rb), e(p), round(e(h_l)),round(e(h_r)), e(N_h_l), e(N_h_r) , 0 ]

rdrobust `var'  z if   girl==1  &  $cond_don , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)
local p_val_c=2*(1-normal(abs(e(tau_bc)/e(se_tau_rb))))
local p_val_r=2*(1-normal(abs(e(tau_cl)/e(se_tau_cl))))
local nobs= e(N_h_l)+e(N_h_r)
matrix pol=e(beta_p_l)
local est_l=pol[1,1]

local est_cl_G = e(tau_cl)
local est_bc_G = e(tau_bc)

local se_cl_G = e(se_tau_cl)
local se_bc_G = e(se_tau_rb)

matrix define res_F =  [$y, 0 , 1 , `don', e(N),  `est_l', e(tau_cl), e(tau_bc), `nobs', `p_val_c', `p_val_r', e(se_tau_cl), e(se_tau_rb), e(p), round(e(h_l)),round(e(h_r)), e(N_h_l), e(N_h_r) ,0]

local diff_cl= `est_cl_G' - `est_cl_B'
local diff_bc= `est_bc_G' - `est_bc_B'

local se_diff_cl = sqrt( (`se_cl_G')^2 + (`se_cl_B')^2  )
local se_diff_bc = sqrt( (`se_bc_G')^2 + (`se_bc_B')^2  )

local p_val_diff_cl=2*(1-normal(abs( `diff_cl' /`se_diff_cl')))
local p_val_diff_bc=2*(1-normal(abs(`diff_bc' /`se_diff_bc')))


matrix define diff =  [.,.,.,.,.,.,`diff_cl', `diff_bc',.,`p_val_diff_cl', `p_val_diff_bc',`se_diff_cl',`se_diff_bc',.,.,.,.,.,1]
matrix define diff_sd =  [.,.,.,.,.,.,`se_diff_cl', `se_diff_bc',.,.,.,.,.,.,.,.,.,.,2]

matrix estimates=[estimates \ res_B \  res_F \  diff  ]
matrix drop res_B    res_F   diff diff_sd

}
}
}


matrix list estimates
matrix estimates = estimates[2...,1...]
clear 
svmat estimates , names(col)

gen covariate = ""
replace covariate = "SES school"  if _n<=18
replace covariate = "share boys in class"   if _n >=19 & _n<=36
replace covariate =  "Class size" if _n >=37 & _n<=54

save estimates_RDD_raw_covs, replace
use estimates_RDD_raw_covs,clear

foreach var in  estimate_left conventional_estimate biascorrected_estimate conventional_se robust_se {
tostring(`var'), generate(`var'2) format(%9.3f) force	
drop `var'
rename 	`var'2 `var'
replace `var'="" if `var'=="."
}

*** parenthesis sd:
replace conventional_se= "(" + conventional_se + ")" 
replace robust_se= "(" + robust_se + ")" 

** stars:
replace conventional_estimate= conventional_estimate + " " + conventional_se
replace conventional_estimate= conventional_estimate + " ***" if standard_p_val<0.01
replace conventional_estimate= conventional_estimate + " *" if standard_p_val<=0.1 & standard_p_val>0.1 
replace conventional_estimate= conventional_estimate + " **" if standard_p_val<=0.05 & standard_p_val>0.01 

replace biascorrected_estimate= biascorrected_estimate + " " + robust_se
replace biascorrected_estimate= biascorrected_estimate + " ***" if robust_p_val<0.01
replace biascorrected_estimate= biascorrected_estimate + " *" if robust_p_val<=0.1 & robust_p_val>0.1 
replace biascorrected_estimate= biascorrected_estimate + " **" if robust_p_val<=0.05 & robust_p_val>0.01 

export excel using "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables/RDD_tests_covs", firstrow(variables) replace

drop test

gen Sample= "Boys" if girl==0
replace Sample = "Girls" if girl==1
replace Sample = "Girls - Boys" if girl==.

rename variable Year
gen Bandwidth_size = (days_left+days_right)/2

order Year covariate Sample donought conventional_estimate biascorrected_estimate Nobs Nobs_bandwidth Bandwidth_size

replace Year= Year[_n-1] if Year==.
replace Test=Test[_n-1] if Test==""
export excel using "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables/RDD_tests_final_covs", firstrow(variables) replace


********************************************************************
*************** Using a fix small bandwidth
********************************************************************

use data_RDD, clear

gen date_birth=date(Date_Naiss,"YMD")
gen m_birth=month(date_birth)
gen d_birth=day(date_birth)
gen y_birth=year(date_birth)
tab y_birth
format date_birth %tdddmonYY

gen girl= (Sexe==2) if Sexe!=.

gen ageT1=dateT1-date_birth
gen ageT2=dateT2-date_birth
gen ageT3=dateT3-date_birth


* results on all cohorts:
drop ID_Eleve Sexe dateT1 dateT2 dateT3 Date_Naiss m_birth d_birth y_birth

matrix estimates = J(1,19,0)
matrix colnames estimates= year test girl  donought Nobs  estimate_left conventional_estimate biascorrected_estimate Nobs_bandwidth standard_p_val robust_p_val conventional_se robust_se poly_order days_left days_right Nobs_left Nobs_right diff

forval don =0/1 {
	forval year = 2019/2021 {
			forval test=1/3 {
				
global y= `year'-6
global cut=td(1jan$y)
cap drop z
gen z=date_birth-$cut

if `don' ==0 {
global cond_don = " z>-360 & z<360"	
}

if `don' ==1 {
global cond_don = "(z>-360 & z<360) & !(z>=-2 & z<=3)"	
}

rdrobust T`test'_Math_z z if   girl==0  &  $cond_don , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust) h(45)
local p_val_c=2*(1-normal(abs(e(tau_bc)/e(se_tau_rb))))
local p_val_r=2*(1-normal(abs(e(tau_cl)/e(se_tau_cl))))
local nobs= e(N_h_l)+e(N_h_r)
matrix pol=e(beta_p_l)
local est_l=pol[1,1]

local est_cl_B = e(tau_cl)
local est_bc_B = e(tau_bc)

local se_cl_B = e(se_tau_cl)
local se_bc_B = e(se_tau_rb)

matrix define res_B =  [$y, `test' , 0 , `don', e(N),  `est_l', e(tau_cl), e(tau_bc), `nobs', `p_val_c', `p_val_r', e(se_tau_cl), e(se_tau_rb), e(p), round(e(h_l)),round(e(h_r)), e(N_h_l), e(N_h_r) , 0 ]

rdrobust T`test'_Math_z z if   girl==1  &  $cond_don , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust) h(45)
local p_val_c=2*(1-normal(abs(e(tau_bc)/e(se_tau_rb))))
local p_val_r=2*(1-normal(abs(e(tau_cl)/e(se_tau_cl))))
local nobs= e(N_h_l)+e(N_h_r)
matrix pol=e(beta_p_l)
local est_l=pol[1,1]

local est_cl_G = e(tau_cl)
local est_bc_G = e(tau_bc)

local se_cl_G = e(se_tau_cl)
local se_bc_G = e(se_tau_rb)

matrix define res_F =  [$y, `test' , 1 , `don', e(N),  `est_l', e(tau_cl), e(tau_bc), `nobs', `p_val_c', `p_val_r', e(se_tau_cl), e(se_tau_rb), e(p), round(e(h_l)),round(e(h_r)), e(N_h_l), e(N_h_r) ,0]

local diff_cl= `est_cl_G' - `est_cl_B'
local diff_bc= `est_bc_G' - `est_bc_B'

local se_diff_cl = sqrt( (`se_cl_G')^2 + (`se_cl_B')^2  )
local se_diff_bc = sqrt( (`se_bc_G')^2 + (`se_bc_B')^2  )

local p_val_diff_cl=2*(1-normal(abs( `diff_cl' /`se_diff_cl')))
local p_val_diff_bc=2*(1-normal(abs(`diff_bc' /`se_diff_bc')))


matrix define diff =  [.,.,.,.,.,.,`diff_cl', `diff_bc',.,`p_val_diff_cl', `p_val_diff_bc',`se_diff_cl',`se_diff_bc',.,.,.,.,.,1]
matrix define diff_sd =  [.,.,.,.,.,.,`se_diff_cl', `se_diff_bc',.,.,.,.,.,.,.,.,.,.,2]

matrix estimates=[estimates \ res_B \  res_F \  diff  ]
matrix drop res_B    res_F   diff diff_sd

}
}
}


matrix list estimates
matrix estimates = estimates[2...,1...]
clear 
svmat estimates , names(col)

save estimates_RDD_raw_bd45, replace
use estimates_RDD_raw_bd45,clear

foreach var in  estimate_left conventional_estimate biascorrected_estimate conventional_se robust_se {
tostring(`var'), generate(`var'2) format(%9.3f) force	
drop `var'
rename 	`var'2 `var'
replace `var'="" if `var'=="."
}


*** parenthesis sd:
replace conventional_se= "(" + conventional_se + ")" 
replace robust_se= "(" + robust_se + ")" 

** stars:
replace conventional_estimate= conventional_estimate + " " + conventional_se
replace conventional_estimate= conventional_estimate + " ***" if standard_p_val<0.01
replace conventional_estimate= conventional_estimate + " *" if standard_p_val<=0.1 & standard_p_val>0.1 
replace conventional_estimate= conventional_estimate + " **" if standard_p_val<=0.05 & standard_p_val>0.01 

replace biascorrected_estimate= biascorrected_estimate + " " + robust_se
replace biascorrected_estimate= biascorrected_estimate + " ***" if robust_p_val<0.01
replace biascorrected_estimate= biascorrected_estimate + " *" if robust_p_val<=0.1 & robust_p_val>0.1 
replace biascorrected_estimate= biascorrected_estimate + " **" if robust_p_val<=0.05 & robust_p_val>0.01 

export excel using "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables/RDD_tests_bd45", firstrow(variables) replace

rename test Test
gen temp= "T1" if Test==1
replace temp= "T2" if Test==2
replace temp= "T3" if Test==3

drop Test
rename temp Test

gen Sample= "Boys" if girl==0
replace Sample = "Girls" if girl==1
replace Sample = "Girls - Boys" if girl==.

rename year Year
gen Bandwidth_size = (days_left+days_right)/2

order Year Test Sample conventional_estimate biascorrected_estimate Nobs Nobs_bandwidth Bandwidth_size

replace Year= Year[_n-1] if Year==.
replace Test=Test[_n-1] if Test==""
export excel using "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables/RDD_tests_final_bd45", firstrow(variables) replace

*************************************************
****** First stage:
*************************************************

use data_RDD, clear

gen date_birth=date(Date_Naiss,"YMD")
gen m_birth=month(date_birth)
gen d_birth=day(date_birth)
gen y_birth=year(date_birth)
tab y_birth
format date_birth %tdddmonYY

gen girl= (Sexe==2) if Sexe!=.

gen ageT1=dateT1-date_birth
gen ageT2=dateT2-date_birth
gen ageT3=dateT3-date_birth


* results on all cohorts:
drop ID_Eleve Sexe dateT1 dateT2 dateT3 Date_Naiss m_birth d_birth y_birth

matrix estimates = J(1,18,0)
matrix colnames estimates= year test girl  donought Nobs  estimate_left conventional_estimate biascorrected_estimate Nobs_bandwidth standard_p_val robust_p_val conventional_se robust_se poly_order days_left days_right Nobs_left Nobs_right


forval year = 2019 {
		forval test=1/3 {
			forval girl= 0/1 {
		
global y= `year'-6
global cut=td(1jan$y)
cap drop z
gen z=date_birth-$cut

global cond = " z>-360 & z<360"	

rdrobust ageT`test'  z if   girl==`girl'  &  $cond , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)

local p_val_c=2*(1-normal(abs(e(tau_bc)/e(se_tau_rb))))
local p_val_r=2*(1-normal(abs(e(tau_cl)/e(se_tau_cl))))

local nobs= e(N_h_l)+e(N_h_r)
matrix pol=e(beta_p_l)
local est_l=pol[1,1]
matrix define res =  [$y, `test' , `girl', 0, e(N),  `est_l', e(tau_cl), e(tau_bc), `nobs', `p_val_c', `p_val_r', e(se_tau_cl), e(se_tau_rb), e(p), round(e(h_l)),round(e(h_r)), e(N_h_l), e(N_h_r) ]
matrix define res_sd = [.,.,.,.,.,.,e(se_tau_cl), e(se_tau_rb),.,.,.,.,.,.,.,.,.,.]

matrix estimates=[estimates \ res\ res_sd]
matrix drop res res_sd
}
}
}

matrix list estimates
matrix estimates = estimates[2...,1...]
clear 
svmat estimates , names(col)
foreach var in  estimate_left conventional_estimate biascorrected_estimate {
tostring(`var'), generate(`var'2) format(%9.3f) force	
drop `var'
rename 	`var'2 `var'
replace `var'="" if `var'=="."
}

*** parenthesis sd:
replace conventional_estimate= "(" + conventional_estimate + ")" if Nobs==.
replace biascorrected_estimate= "(" + biascorrected_estimate + ")" if Nobs==.

** stars:

replace conventional_estimate= conventional_estimate + "***" if standard_p_val<0.01
replace conventional_estimate= conventional_estimate + "*" if standard_p_val<=0.1 & standard_p_val>0.1 
replace conventional_estimate= conventional_estimate + "**" if standard_p_val<=0.05 & standard_p_val>0.01 


replace biascorrected_estimate= biascorrected_estimate + "***" if standard_p_val<0.01
replace biascorrected_estimate= biascorrected_estimate + "*" if standard_p_val<=0.1 & standard_p_val>0.1 
replace biascorrected_estimate= biascorrected_estimate + "**" if standard_p_val<=0.05 & standard_p_val>0.01 

export excel using "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Tables/RDD_tests_FS", firstrow(variables) replace



*******************
******* Key graphs:
*******************

use data_RDD, clear

gen date_birth=date(Date_Naiss,"YMD")
gen m_birth=month(date_birth)
gen d_birth=day(date_birth)
gen y_birth=year(date_birth)
tab y_birth
format date_birth %tdddmonYY

gen girl= (Sexe==2) if Sexe!=.

gen ageT1=dateT1-date_birth
gen ageT2=dateT2-date_birth
gen ageT3=dateT3-date_birth


* results on all cohorts:
drop ID_Eleve Sexe dateT1 dateT2 dateT3 Date_Naiss m_birth d_birth y_birth


**** 1) histogram

forval year = 2019/2021 {
global y= `year'-6
global y0= `year'-7
global cut=td(1jan$y)
cap drop z
gen z=date_birth-$cut
hist date_birth if z<45 & z>=-45, bin(90) xlabel(#6, valuelabel) percent xtitle(Date of birth) xline($cut) title($y0-$y)
graph save "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Graphs/hist`year'.gph", replace
graph export "/home/depp/projets_inter/PSE-ecartsFG/PSE-ecartsFG-echanges/Export/Graphs/hist`year'.png", replace
}

graph combine "$graph/hist2019.gph"  "$graph/hist2020.gph" "$graph/hist2021.gph", rows(3)
graph save "$graph/hist_combined.gph",replace
graph export "$graph/hist_combined.png",replace
graph export "$graph/hist_combined.pdf",replace

**** 2) rdplot (2018-2019 only)

rdrobust T`test'_Math_z z if   girl==`girl'  , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)

cap drop z
global cut1819=td(1jan2013)
gen z=date_birth-$cut1819

rdplot T1_Math_z z if girl==0  & z>-360 & z<360 , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust) xtitle(Days around cutoff) title(Boys' z-score at T1)
graph save "$graph/rdplot_T1_1819boys.gph",replace
graph export "$graph/rdplot_T1_1819boys.png",replace

rdplot T1_Math_z z if girl==1  & z>-360 & z<360 , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)
graph save "$graph/rdplot_T1_1819girls.gph",replace
graph export "$graph/rdplot_T1_1819girls.png",replace

graph combine "$graph/rdplot_T1_1819boys_ok.gph" "$graph/rdplot_T1_1819girls_ok.gph"
graph save "$graph/rdplot_T1_1819girlsboys.gph"
graph export "$graph/rdplot_T1_1819girlsboys.png"
graph export "$graph/rdplot_T1_1819girlsboys.pdf"

replace ageT1= ageT1/365

rdplot ageT1 z if girl==0  & z>-360 & z<360 , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust) xtitle(Days around cutoff)
graph save "$graph/rdplot_ageT1_1819boys.gph",replace

rdplot ageT1 z if girl==1  & z>-360 & z<360 , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)
graph save "$graph/rdplot_ageT1_1819girls.gph",replace

graph combine "$graph/rdplot_ageT1_1819boys_ok.gph" "$graph/rdplot_ageT1_1819girls_ok.gph" "$graph/rdplot_T1_1819boys_ok.gph" "$graph/rdplot_T1_1819girls_ok.gph"

*****************************************************
graph save "$graph/rdplot_ageT1_T1_1819girlsboys_ok.gph"
graph export "$graph/rdplot_ageT1_T1_1819girlsboys_ok.png"
graph export "$graph/rdplot_ageT1_T1_1819girlsboys_ok.pdf"


***** Zooming
rdplot T1_Math_z z if girl==0  & z>-60 & z<60 , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust) xtitle(Days around cutoff)
graph save "$graph/rdplot_T1_1819boys_zoom.gph",replace
graph export "$graph/rdplot_T1_1819boys_zoom.png",replace

rdplot T1_Math_z z if girl==1  & z>-60 & z<60 , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)
graph save "$graph/rdplot_T1_1819girls_zoom.gph",replace
graph export "$graph/rdplot_T1_1819girls_zoom.png",replace

*** Manual edits then:
graph combine "$graph/rdplot_T1_1819boys_zoom_ok.gph" "$graph/rdplot_T1_1819girls_zoom_ok.gph"
graph save "$graph/rdplot_T1_1819girlsboys_zoom_ok.gph"

graph use "$graph/rdplot_T1_1819boys_zoom_ok.gph" 
graph export "$graph/rdplot_T1_1819boys_zoom_ok.png",replace
graph export "$graph/rdplot_T1_1819boys_zoom_ok.pdf",replace

graph use "$graph/rdplot_T1_1819girls_zoom_ok.gph" 
graph export "$graph/rdplot_T1_1819girls_zoom_ok.png",replace
graph export "$graph/rdplot_T1_1819girls_zoom_ok.pdf",replace


****************************************************

rdplot ageT1 z if girl==0  & z>-60 & z<60 , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust) xtitle(Days around cutoff)
graph save "$graph/rdplot_ageT1_1819boys_zoom.gph",replace

rdplot ageT1 z if girl==1  & z>-60 & z<60 , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)
graph save "$graph/rdplot_ageT1_1819girls_zoom.gph",replace

* Edits 
graph combine "$graph/rdplot_ageT1_1819boys_zoom_ok.gph" "$graph/rdplot_ageT1_1819girls_zoom_ok.gph" "$graph/rdplot_T1_1819boys_zoom_ok.gph" "$graph/rdplot_T1_1819girls_zoom_ok.gph"
graph save "$graph/rdplot_combined_zoom_ok.gph",replace
graph export "$graph/rdplot_combined_zoom_ok.png",replace
graph export "$graph/rdplot_combined_zoom_ok.pdf",replace

graph use "$graph/rdplot_ageT1_1819boys_zoom_ok.gph" 
graph export "$graph/rdplot_ageT1_1819boys_zoom_ok.png",replace
graph export "$graph/rdplot_ageT1_1819boys_zoom_ok.pdf",replace

graph use "$graph/rdplot_ageT1_1819girls_zoom_ok.gph" 
graph export "$graph/rdplot_ageT1_1819girls_zoom_ok.png",replace
graph export "$graph/rdplot_ageT1_1819girls_zoom_ok.pdf",replace



/*
************************************************************
******* Manipulation test (manual tests - to be removed)
************************************************************
use data_RDD, clear

gen date_birth=date(Date_Naiss,"YMD")
gen m_birth=month(date_birth)
gen d_birth=day(date_birth)
gen y_birth=year(date_birth)
tab y_birth
format date_birth %tdddmonYY

gen girl= (Sexe==2) if Sexe!=.

gen ageT1=dateT1-date_birth
gen ageT2=dateT2-date_birth
gen ageT3=dateT3-date_birth

* results on all cohorts:
drop ID_Eleve Sexe dateT1 dateT2 dateT3 Date_Naiss m_birth d_birth y_birth

cap drop z
global cut1819=td(1jan2015)
gen z=date_birth-$cut1819

*rddensity z if z>-360 & z<360, plot
rddensity z if z>-360 & z<360 & girl==0, plot


cap drop z
global cut1819=td(1jan2015)
gen z=date_birth-$cut1819

*rddensity z if z>-360 & z<360, plot
rddensity z if z>-360 & z<360 & girl==0, plot
rddensity z if z>-360 & z<360 & girl==1, plot

rddensity z if (z>-360 & z<360) & !(z>=-2 & z<=3)  & girl==0, plot
rddensity z if (z>-360 & z<360) & !(z>=-2 & z<=3) & girl==1, plot

cap drop z2
gen z2=z+2 if z<-2
replace z2= z-3 if z>3
rddensity z2 if (z>-360 & z<360) & !(z>=-2 & z<=3)  & girl==0, plot

local year = 2019
global y= `year'-6
global y0= `year'-7
global cut=td(1jan$y)
cap drop z
gen z=date_birth-$cut
hist date_birth if z<45 & z>=-45, bin(90) xlabel(#6, valuelabel) percent xtitle(Date of birth) xline($cut) title($y0-$y)
*/
 





/*



global cut1819=td(1jan2013)
gen z=date_birth-$cut1819
rdrobust T1_Math_z z if z<360 & z>-360 , p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)



hist z if z<120 & z>-120

rdrobust T1_Math_z z if z<30 & z>-30

tab z if z<30 & z>-30


keep if z<200 & z>-200

rdrobust T1_Math_z z if z<10 & z>=-10, p(1) kernel(triangular) bwselect(mserd) scaleregul(1) all masspoints(adjust)

local p_val_r=2*(1-normal(abs(e(tau_bc)/e(se_tau_rb))))
local p_val_c=2*(1-normal(abs(e(tau_cl)/e(se_tau_cl))))

local nobs= e(N_h_l)+e(N_h_r)
matrix pol=e(beta_p_l)
local est_l=pol[1,1]

matrix define res =  [e(N), `est_l', e(tau_cl), e(tau_bc), `n_obs'  ]  //, `p_val_c', `p_val_r' , e(se_tau_cl), e(se_tau_rb), e(p), round(e(h_l), round(e(h_r), e(N_h_l), e(N_h_r) ]


matrix define res_sd = [.,.,.,e(se_tau_cl), e(se_tau_rb),.,.,.,.,.,.,.,.,.,.]

matrix estimates=[estimates \ res\ res_sd]
matrix drop res res_sd

rdrobust T1_Math_z z if girl==1
rdrobust T1_Math_z z if girl==0

rdrobust T3_Math_z z if girl==1
rdrobust T3_Math_z z if girl==0



rdrobust ageT1 z if girl==1
rdrobust ageT1 z if girl==0
*/




* net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace








