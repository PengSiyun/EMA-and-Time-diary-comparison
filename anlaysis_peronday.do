*Project: ATUS and EMA overlap
*Author: Siyun Peng
*Date: 2025/3/18


******************************************************************************
**# 1 EMA data clean
******************************************************************************


clear all
use "C:\Users\siyunpeng\OneDrive - University of South Florida\SECHURA\work\EMA and ATUS\EMA_cleaned.dta", clear
cd "C:\Users\siyunpeng\OneDrive - University of South Florida\SECHURA\work\EMA and ATUS\results"


/*clean WHO*/


*N of valid activity
recode who (.=.) (else=1),gen(who0) 
bysort surveyid date_only : egen t_who=total(who0),mi

*calculate propotion of WHO
recode who (0=1) (.=.) (else=0),gen(alone)
replace alone=1 if modeint==2 //code digital contact as alone
bysort surveyid date_only: egen t_alone=total(alone),mi
gen prop_alone=t_alone/t_who

recode who (0=0) (.=.) (else=1),gen(nalone)
replace nalone=0 if modeint==2 //code digital contact as alone
bysort surveyid date_only: egen t_nalone=total(nalone),mi
gen prop_nalone=t_nalone/t_who


/*clean WHERE*/


*N of valid activity
recode where (.=.) (else=1),gen(where0) 
bysort surveyid date_only: egen t_where=total(where0),mi

*calculate propotion of WHERE
recode where (1=1) (.=.) (else=0),gen(home)
bysort surveyid date_only: egen t_home=total(home),mi
gen prop_home=t_home/t_where

recode where (3=1) (.=.) (else=0),gen(work)
bysort surveyid date_only: egen t_work=total(work),mi
gen prop_work=t_work/t_where

recode where (1 3=0) (.=.) (else=1),gen(whe_other)
bysort surveyid date_only: egen t_whe_other=total(whe_other),mi
gen prop_whe_other=t_whe_other/t_where

/*clean WHAT*/


*N of valid activity
recode activity (.=.) (else=1),gen(what) 
bysort surveyid date_only: egen t_what=total(what),mi // # valid activity for each respondent

*calculate propotion of WHAT
recode activity (1=1) (.=.) (else=0),gen(eat)
bysort surveyid date_only: egen t_eat=total(eat),mi // # eat/drinking for each respondent
gen prop_eat=t_eat/t_what

recode activity (6=1) (.=.) (else=0),gen(chore)
bysort surveyid date_only: egen t_chore=total(chore),mi
gen prop_chore=t_chore/t_what

recode activity (1 6=0) (.=.) (else=1),gen(wha_other)
bysort surveyid date_only: egen t_wha_other=total(wha_other),mi
gen prop_wha_other=t_wha_other/t_what

gen ema_complete=t_what/4 
lab var ema_complete "Prop. completed EMA"



/*clean covariates*/


*Finished aggregating EMA data, reduce to aggregated respondent level
duplicates drop surveyid date_only,force 
rename su_id_p2p SU_ID

*get weights
merge m:1 SU_ID using "C:\Users\siyunpeng\OneDrive - University of South Florida\P2P\P2P data\Final Weights\P2P_panel_weights.dta",nogen keep(match)

*clean
lab var age "Age"

recode gender (1=0) (2=1),gen(women)
lab var women "Women"
lab de women 0 "Men" 1 "Women"
lab val women women

recode race (1=1) (2/6=0),gen(white)
lab var white "White"
lab de white 1 "White" 0 "Non-White"
lab val white white

recode emp_status (1 2=1) (4/10=0),gen(working)
lab var working "Employed"
lab define working 0 "Not employed" 1 "Employed"
lab values working working

drop edu
label drop edu
recode educ (1=1) (2=2) (3 4=3) (5=4),gen(edu)
lab def edu 1 "Less than HS" 2 "HS or GED" 3 "Some college" 4 "College"
lab val edu edu
lab var edu "Education"

recode marital_status (1=1) (4/6=0),gen(partner)
lab def partner 1 "Partnered" 0 "No partner" 
lab val partner partner
lab var partner "Partner status"





/*append with ATUS data*/


*keep necessary variables
keep surveyid weight_sechura psu strata age women white working edu partner prop_* date_only time_only ema_complete weekday holiday //date_only time_only are EMA time and date

append using "C:\Users\siyunpeng\OneDrive - University of South Florida\SECHURA\work\EMA and ATUS\ATUS_cleaned.dta",gen(source)
lab def source 0 "EMA" 1 "Time diary"
lab values source source
lab var source "Data source"





***************************************************************
**# 2 ATUS data clean (need to revise the code in gen age into replace age=teage if source==1)
***************************************************************

  


*convert minutes into proportion in total non-missing time during 10 hours (8-10 and 12-20)
replace prop_alone=time_alone/(600-time_alone_miss) if source==1
replace prop_nalone=time_nalone/(600-time_alone_miss) if source==1

replace prop_home=time_home/(600-time_whe_miss) if source==1
replace prop_work=time_work/(600-time_whe_miss) if source==1
replace prop_whe_other=time_whe_other/(600-time_whe_miss) if source==1

replace prop_eat=time_eat/(600-time_wha_miss) if source==1
replace prop_chore=time_chore/(600-time_wha_miss) if source==1
replace prop_wha_other=time_wha_other/(600-time_wha_miss) if source==1

label var prop_alone "Alone"
label var prop_nalone "Not alone"
label var prop_home "Home"
label var prop_work "Workplace"
label var prop_whe_other "Third place"
label var prop_eat "Eat/drink"
label var prop_chore "Household chores"
label var prop_wha_other "Other activity"

*covariates
replace age=teage if source==1

recode tesex (1=0) (2=1),gen(women1)
replace women=women1 if source==1

recode ptdtrace_y (1=1) (2/8=0),gen(white1)
replace white=white1 if source==1

recode trdpftpt (1 2=1) (-1=0),gen(working1)
replace working=working1 if source==1

recode peeduca_y (31/38=1) (39=2) (40/42=3) (43/46=4),gen(edu1)
replace edu=edu1 if source==1

recode pemaritl (1 2=1) (3/6=0),gen(partner1)
replace partner1=1 if cohabit==1
replace partner=partner1 if source==1

recode tudiaryday (2/6=1) (1 7=0)
replace weekday=tudiaryday if source==1

replace holiday=trholiday if source==1

rename gestfips state
label define fipsmidwest 17 "Illinois" 18 "Indiana" 19 "Iowa" 20 "Kansas"  ///
      26 "Michigan" 27 "Minnesota" 29 "Missouri" 31 "Nebraska"      ///
      38 "North Dakota" 39 "Ohio" 46 "South Dakota" 55 "Wisconsin"
label values state fipsmidwest

replace surveyid=tucaseid if source==1

*weights
gen weights=tufinlwgt if source==1
replace weights=weight_sechura if source==0

*keep necessary variables
keep source surveyid tucaseid weights weight_sechura psu strata tufinlwgt finlwgt* age women white working edu partner holiday weekday prop_* date_only time_only tuyear tumonth state ema_complete //date_only time_only are EMA time and date

*drop one EMA case whose age=53
drop if age<55
drop prop_nalone


	
		
***************************************************************
**# 3 indicator approach to compare
***************************************************************


*descriptive table
preserve
foreach y of varlist prop_* {
	bysort surveyid: egen `y'ema=mean(`y') if source==0
	replace `y'=`y'ema if source==0
	drop `y'ema
}
duplicates drop surveyid if source==0,force //aggregated to person level for EMA

svyset [pw=weights]
desctable women age white i.edu working partner prop_*, filename("descriptives") stats(svymean sd min max n) group(source)

*baseline comparison: basically compare means between two data
svyset [pw=weights]
dtable  i.women age i.white i.edu i.working i.partner prop_*,svy by(source,nototals test) export(diff_base.docx,replace)

*visualization
eststo clear
foreach y of varlist prop_* {
eststo `y': reg `y' ib1.source [pweight=weights] 
}
coefplot (prop_alone, aseq("Alone")) (prop_home, aseq("Home")) (prop_work, aseq("Workplace")) (prop_whe_other, aseq("Third place")) (prop_eat, aseq("Eat/drink")) (prop_chore, aseq("Household chores")) (prop_wha_other, aseq("Other activity")) , xline(0) drop(women age white *.edu working partner _cons) swapnames legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) title("EMA - Time diary")
graph export "diff_base.tif",replace

*week-person sensitivity analysis
eststo clear
foreach y of varlist prop_* {
	local controls "women age white i.edu working partner" 
	eststo `y': reg `y' ib1.source `controls' [pweight=weights]
}
esttab * using "reg.csv",label replace title("Person/week analysis") b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted nobaselevels 

coefplot (prop_alone, aseq("Alone")) (prop_home, aseq("Home")) (prop_work, aseq("Workplace")) (prop_whe_other, aseq("Third place")) (prop_eat, aseq("Eat/drink")) (prop_chore, aseq("Household chores")) (prop_wha_other, aseq("Other activity")) , xline(0) drop(women age white *.edu working partner _cons) swapnames legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) title("EMA - Time diary")
graph export "control_diff_week.tif",replace

restore


/*compare EMA vs. time diary*/


*Run regressions and store predictions
eststo clear
foreach y of varlist prop_* {
	local controls "women age white i.edu working partner holiday weekday" 
	*mixed `y' || surveyid: if source==0, pweight(weights) //no variation in prediction
    *predict `y'_0b if source==0
	eststo `y'0: mixed `y' `controls' || surveyid: if source==0, pweight(weights)
    predict `y'_0 if source==0 & !missing(`y')

    *reg `y' [pweight=weights] if source==1
    *predict `y'_1b if source==1
    eststo `y'1: reg `y' `controls' [pweight=weights] if source==1
    predict `y'_1 if source==1 & !missing(`y')
}
esttab *0 using "reg.csv",label append title("EMA") b(%5.3f) se(%5.3f) nogap r2 bic compress nonum noomitted nobaselevels 
esttab *1 using "reg.csv",label append title("Time diary") b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted nobaselevels 


*indicator method to compare predictions
eststo clear
foreach y of varlist prop_alone prop_home prop_work prop_whe_other prop_eat prop_chore prop_wha_other {
    *gen `y'_hat0 = .
    *replace `y'_hat0 = `y'_0b if source==0
    *replace `y'_hat0 = `y'_1b if source==1
    gen `y'_hat1 = .
    replace `y'_hat1 = `y'_0 if source==0
    replace `y'_hat1 = `y'_1 if source==1

    *reg `y'_hat0 ib1.source, vce(cluster surveyid) 
    *est store `y'b
	reg `y'_hat1 ib1.source, vce(cluster surveyid) 
    est store `y'
}
esttab * using "reg.csv",label append title("indicator approach") b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted nobaselevels 

coefplot (prop_alone, aseq("Alone")) (prop_home, aseq("Home")) (prop_work, aseq("Workplace")) (prop_whe_other, aseq("Third place")) (prop_eat, aseq("Eat/drink")) (prop_chore, aseq("Household chores")) (prop_wha_other, aseq("Other activity")) , xline(0) drop(women age white *.edu working partner _cons) swapnames legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) title("EMA - Time diary")
graph export "control_diff.tif",replace

*alternative figure
eststo clear
foreach y of varlist prop_alone prop_home prop_work prop_whe_other prop_eat prop_chore prop_wha_other {
	reg `y'_hat1 ib1.source, vce(cluster surveyid) 
	margins i.source
    marginsplot, recast(bar) tit("`: var label `y''",size(medlarge)) ytit("") xtit("") xlab(,angle(45) labsize(medlarge)) legend(off) plotopts(mlabel(_margin) mlabf(%12.2f) mlabp(10) mlabcolor(black) lcolor(red) fcolor(red%50*0.5)) ciopt(color(black)) saving(`y',replace) level(83.4)
}
graph combine "prop_alone" "prop_home" "prop_work" "prop_whe_other" "prop_eat" "prop_chore" "prop_wha_other" ,ycommon row(1)
graph export "control_diff2.tif",replace

*Cohen's d
eststo clear
foreach y of varlist prop_alone prop_home prop_work prop_whe_other prop_eat prop_chore prop_wha_other {
    * Combine the predictions into a single variable with source indicator
    gen `y'_cd = .
    replace `y'_cd = `y'_0 if source == 0
    replace `y'_cd = `y'_1 if source == 1
    
    * Calculate Cohen's d
    esize twosample `y'_cd, by(source) cohensd
    display "`y'"
}


/* plot Cohen's d*/
	
	
* Clear any previous estimates and create a temporary file to store results
eststo clear
tempfile cohen_results
tempname cohen_file
file open `cohen_file' using `cohen_results', write replace

* Write header to file
file write `cohen_file' "variable" _tab "cohens_d" _n

* Loop through each variable to calculate Cohen's d
foreach y of varlist prop_alone prop_home prop_work prop_whe_other prop_eat prop_chore prop_wha_other {
    * Combine the predictions into a single variable with source indicator
    cap drop `y'_cd
    gen `y'_cd = .
    replace `y'_cd = `y'_0 if source == 0
    replace `y'_cd = `y'_1 if source == 1
    
    * Calculate Cohen's d
    quietly esize twosample `y'_cd, by(source) cohensd
    local d = r(d)
    
    * Store result in file
    file write `cohen_file' "`y'" _tab (`d') _n
}

* Close the file
file close `cohen_file'

* Import the results into a dataset
preserve
import delimited using `cohen_results', clear

* Generate a variable for the order - keep original order of variables
gen order = _n

* Generate color variable based on effect size magnitude
gen color = ""
replace color = "navy" if cohens_d >= 0.8 | cohens_d <= -0.8      // Large effect
replace color = "blue" if (cohens_d >= 0.5 & cohens_d < 0.8) | (cohens_d <= -0.5 & cohens_d > -0.8)  // Medium effect
replace color = "midblue" if (cohens_d >= 0.2 & cohens_d < 0.5) | (cohens_d <= -0.2 & cohens_d > -0.5)  // Small effect
replace color = "gray" if cohens_d > -0.2 & cohens_d < 0.2        // Negligible effect

* Format Cohen's d values for display
gen label_text = string(cohens_d, "%5.2f")

* Create graph with bar values displayed - 50% transparency and larger labels
twoway ///
    (bar cohens_d order, barwidth(0.8) color(red) fintensity(50)) ///
    (scatter cohens_d order, msymbol(none) mlabel(label_text) mlabposition(0) mlabcolor(black) mlabsize(small)), ///
    xlabel(1 "Alone" 2 "Home" 3 "Workplace" ///
           4 "Third place" 5 "Eat/drink" 6 "Household chores" 7 "Other activity", ///
           angle(45) labsize(small)) ///
    xtitle("") ytitle("Cohen's d") title("Cohen's d Effect Sizes") ///
    yline(-0.8 0.8, lcolor(gs12) lpattern(dash)) ///
    ylabel(-2(0.5)2, grid) yscale(range(-1.1 1.1)) ///
    text(0.9 6.8 "Large (≥0.8)", place(e) color(navy) size(small)) ///
    text(-0.9 6.8 "Large (≤-0.8)", place(e) color(navy) size(small)) ///
    legend(off)

* Export graph to PNG file (optional)
graph export "cohens_d_plot.tif", replace width(1200) height(800)

* Display table of Cohen's d values
list variable cohens_d, clean

restore

* Clean up
cap drop *_cd

	
		
***************************************************************
**# 4 sensitivity analysis
***************************************************************



/* Indiana only */


eststo clear
foreach y of varlist prop_alone prop_home prop_work prop_whe_other prop_eat prop_chore prop_wha_other {
	reg `y'_hat1 ib1.source if source==0 | state==18, vce(cluster surveyid) 
    est store `y'
}
esttab * using "reg.csv",label append title("Indiana") b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted nobaselevels 

coefplot (prop_alone, aseq("Alone")) (prop_home, aseq("Home")) (prop_work, aseq("Workplace")) (prop_whe_other, aseq("Third place")) (prop_eat, aseq("Eat/drink")) (prop_chore, aseq("Household chores")) (prop_wha_other, aseq("Other activity")) , xline(0) drop(women age white *.edu working partner _cons) swapnames legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) title("EMA - Time diary")
graph export "control_diff_ind.tif",replace



/* Same months */


eststo clear
foreach y of varlist prop_alone prop_home prop_work prop_whe_other prop_eat prop_chore prop_wha_other {
	reg `y'_hat1 ib1.source if source==0 | tumonth==11 | tumonth==12 | tumonth==1 | tumonth==2 | tumonth==3, vce(cluster surveyid) 
    est store `y'
}
esttab * using "reg.csv",label append title("Same months") b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted nobaselevels 

coefplot (prop_alone, aseq("Alone")) (prop_home, aseq("Home")) (prop_work, aseq("Workplace")) (prop_whe_other, aseq("Third place")) (prop_eat, aseq("Eat/drink")) (prop_chore, aseq("Household chores")) (prop_wha_other, aseq("Other activity")) , xline(0) drop(women age white *.edu working partner _cons) swapnames legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) title("EMA - Time diary")
graph export "control_diff_Month.tif",replace

 
/* proportion complete in EMA (prop_complete corelates with source, multicollinearity)*/


eststo clear
foreach y of varlist prop_alone prop_home prop_work prop_whe_other prop_eat prop_chore prop_wha_other {
	local controls "women age white i.edu working partner holiday weekday" 
	mixed `y' `controls' ema_complete || surveyid: if source==0, pweight(weights)
	est store `y'
}
esttab * using "reg.csv",label append title("EMA complete rate") b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted nobaselevels 

coefplot (prop_alone, aseq("Alone")) (prop_home, aseq("Home")) (prop_work, aseq("Workplace")) (prop_whe_other, aseq("Third place")) (prop_eat, aseq("Eat/drink")) (prop_chore, aseq("Household chores")) (prop_wha_other, aseq("Other activity")) , xline(0) keep(ema_complete) swapnames legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) title("Effect of EMA completion rate")
graph export "EMA_miss.tif",replace


  
/* retired/unemployed only */


*Run regressions and store predictions [need to redo this to remove working as a control]
eststo clear
foreach y of varlist prop_alone prop_home prop_work prop_whe_other prop_eat prop_chore prop_wha_other {
	local controls "women age white i.edu partner holiday weekday" 
	*mixed `y' || surveyid: if source==0, pweight(weights) //no variation in prediction
    *predict `y'_0b if source==0
	eststo `y'0: mixed `y' `controls' || surveyid: if source==0 & working==0, pweight(weights)
    predict `y'_0w if source==0 & working==0 & !missing(`y')

    *reg `y' [pweight=weights] if source==1
    *predict `y'_1b if source==1
    eststo `y'1: reg `y' `controls' [pweight=weights] if source==1 & working==0
    predict `y'_1w if source==1 & working==0 & !missing(`y')
}

*indicator method to compare predictions
eststo clear
foreach y of varlist prop_alone prop_home prop_work prop_whe_other prop_eat prop_chore prop_wha_other {
    *gen `y'_hat0 = .
    *replace `y'_hat0 = `y'_0b if source==0
    *replace `y'_hat0 = `y'_1b if source==1
    gen `y'_hat1w = .
    replace `y'_hat1w = `y'_0w if source==0 & working==0
    replace `y'_hat1w = `y'_1w if source==1 & working==0

    *reg `y'_hat0 ib1.source, vce(cluster surveyid) 
    *est store `y'b
	reg `y'_hat1w ib1.source if working==0, vce(cluster surveyid) 
    est store `y'
}
esttab * using "reg.csv",label append title("Not working") b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted nobaselevels 
coefplot (prop_alone, aseq("Alone")) (prop_home, aseq("Home")) (prop_work, aseq("Workplace")) (prop_whe_other, aseq("Third place")) (prop_eat, aseq("Eat/drink")) (prop_chore, aseq("Household chores")) (prop_wha_other, aseq("Other activity")) , xline(0) drop(women age white *.edu working partner _cons) swapnames legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) title("EMA - Time diary")
graph export "control_diff_notworking.tif",replace
