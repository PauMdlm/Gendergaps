clear
cd "F:\Users\thomas\Dropbox\Reperes_CP_CE1\Nature"
*cd "D:\Users\t.breda\Dropbox\Reperes_CP_CE1\Nature"

use subtest_level_data.dta

gen gg_z=gender_gap_initial/sd_initial
gen gg_z2= (moyenne_garcon-moyenne_fille)/ sd

gen difficulty= 1-moyenne_initial/max_initial

gen math= (time_pressure!=.)

egen id_subtest=group(exercice)
order cohort exercice gg_z  difficulty math id_subtest

gen T2= (test==2)
gen T3= (test==3)
				
twoway (scatter gg_z  difficulty if test==1,  mlabsize(tiny))   ///
			(scatter gg_z  difficulty if  test==2, mlabsize(tiny))  	///
					(scatter gg_z  difficulty if  test==3, mlabsize(tiny))  	///
					,  ytitle("Gender gap (boys minus girls, z-score)") xtitle("Test difficulty")  ///
						legend(order(1 "T1" 2 "T2" 3 "T3") cols(3))
graph export gg_difficulty_ok.pdf,replace
graph export gg_difficulty_ok.png,replace
					
xi i.cohort i.subtest i.id_subtest
egen difficulty_std=std(difficulty)


regress gg_z difficulty_std _Icohort* if math==1
outreg2  difficulty_std using Table_difficulty.xls, replace 

regress gg_z T2 T3   _Icohort*  if math==1
outreg2  difficulty_std T2 T3 using Table_difficulty.xls, append 

regress gg_z T2 T3 difficulty_std  _Icohort*  if math==1
outreg2  difficulty_std T2 T3 using Table_difficulty.xls, append 

regress gg_z  T2 T3  _Icohort* _Isub* if math==1
outreg2  difficulty_std T2 T3 using Table_difficulty.xls, append 

regress gg_z difficulty_std T2 T3  _Icohort* _Isub* if math==1
outreg2  difficulty_std T2 T3 using Table_difficulty.xls, append 

regress gg_z difficulty_std   _Icohort* _Iid_sub* if math==1
outreg2  difficulty_std T2 T3 using Table_difficulty.xls, append 


/*

**** A series of graphs not included in the paper:

cap drop memory
gen memory = .
replace memory=0 if math==1
replace memory=1 if math==1 & subtest=="Add" | subtest=="Subtract"  /*  | subtest=="Mental_calc" */

gen memory2 = .
replace memory2=0 if math==1
replace memory2=1 if math==1 & subtest=="Add" | subtest=="Subtract"   | subtest=="Mental_calc" 


twoway (scatter gg_z  difficulty) if math==1, ytitle("Gender gap (boys minus girls, z-score)") xtitle("Test difficulty")
graph export gg_difficulty1.pdf,replace

twoway (scatter gg_z  difficulty, mlabel(exercice) mlabsize(tiny)) if math==1, ytitle("Gender gap (boys minus girls, z-score)") xtitle("Test difficulty")
graph export gg_difficulty2.pdf,replace

twoway (scatter gg_z  difficulty if subtest=="Add", mlabel(exercice) mlabsize(tiny))   ///
			(scatter gg_z  difficulty if  subtest=="Subtract", mlabel(exercice) mlabsize(tiny))  	///
						,  ytitle("Gender gap (boys minus girls, z-score)") xtitle("Test difficulty")  legend(order(1 "Adding" 2 "Subtracting"))
graph export gg_difficulty3.pdf,replace


twoway (scatter gg_z  difficulty if math==1 & memory==0, mlabel(subtest) mlabsize(tiny)   ) ///
					(scatter gg_z  difficulty  if math==1 & memory==1, mlabel(subtest) mlabsize(tiny)  ), ///
						ytitle("Gender gap (boys minus girls, z-score)") xtitle("Test difficulty") legend(order(1 "Limited memory retrieval" 2 "More memory retrieval"))
graph export gg_difficulty_mem1.pdf,replace

twoway (scatter gg_z  difficulty if math==1 & memory2==0, mlabel(subtest) mlabsize(tiny)   ) ///
					(scatter gg_z  difficulty  if math==1 & memory2==1, mlabel(subtest) mlabsize(tiny)  ), ///
						ytitle("Gender gap (boys minus girls, z-score)") xtitle("Test difficulty") legend(order(1 "Limited memory retrieval" 2 "More memory retrieval"))
graph export gg_difficulty_mem2.pdf,replace

twoway (scatter gg_z  difficulty if exercice=="T2_Soustract_P", mlabel(exercice) mlabsize(tiny))   ///
			(scatter gg_z  difficulty if exercice=="T1_Resoud_Pb_P", mlabel(exercice) mlabsize(tiny))  	///
			(scatter gg_z  difficulty  if exercice=="T1_Ligne_Num_P" , mlabel(exercice) mlabsize(tiny)) ///
			(scatter gg_z  difficulty if exercice=="T2_Ligne_Num_P"  , mlabel(exercice) mlabsize(tiny))  ///
			(scatter gg_z  difficulty if exercice=="T3_Ligne_Num_P", mlabel(exercice) mlabsize(tiny))   ///
						,  ytitle("Gender gap (boys minus girls, z-score)") xtitle("Test difficulty")
						
					
twoway (scatter gg_z  difficulty if subtest=="Count",  mlabsize(tiny))   ///
			(scatter gg_z  difficulty if  subtest=="Add", mlabsize(tiny))  	///
					(scatter gg_z  difficulty if  subtest=="Subtract", mlabsize(tiny))  	///
			(scatter gg_z  difficulty if  subtest=="Read_num",  mlabsize(tiny))  	///
			(scatter gg_z  difficulty if  subtest=="Write_num", mlabsize(tiny))  	///
			(scatter gg_z  difficulty if  subtest=="Line_pos",   mlabsize(tiny))  	///
			(scatter gg_z  difficulty if  subtest=="Solve_pb",  mlabsize(tiny))  	///
			(scatter gg_z  difficulty if  subtest=="Compare",   mlabsize(tiny))  	///
					,  ytitle("Gender gap (boys minus girls, z-score)") xtitle("Test difficulty")  ///
						legend(order(1 "Count" 2 "Add" 3 "Subtract"  4 "Read num" 5 "Write num" 6 "Line pos" 7 "Solve pb" 8 "Compare"))
graph export gg_difficulty4.pdf,replace
graph export gg_difficulty4.png,replace						

*/
		





