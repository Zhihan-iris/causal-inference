*==============================================================================
*						    PKU-UChicago IPAL 2026
*  
*                           Programming in Stata   
*             ======================================================

*                   【Instructor】Dr. Yanran Chen, 
*                                Capital University of Economics and Business
**                  【Email】yanran.chen@pku.edu.cn

*     ================================================================================================
*   【Pre-requisites /Target audience】
*     This course is intended for the upper undergraduate students in Economics,Business or other social science majors. 
*   Graduate students are also welcome. 
*   Prior training in Applied Econometrics is required. 
*   However, this course is set for totally beginners or freshman in terms of econometric software, 
*   even that students have not been exposed to programming languages at all. 
*   The main focus of the course is to identify, demonstrate and interpret econometric models by Stata, 
*   and aims to help students get started with their own research as soon as possible. 
*   Thus, we will not spend much time on the theoretical part of econometric models.
*    ================================================================================================
*
*
*                 ====================================================
*                        Session 4：Casual Inference Strategies (II): 
*                                   Regression Discontinuity
*                 ====================================================
*

*   【Description of the Session】
* 		This part is mainly an extended or improved training, 
*		and will provide some advanced applied micro-econometric methods, 
* 		especially those involving causal inference and commonly used in empirical research. 
* 		They are all some practical methods, and will allow you to get started with their own research as soon as possible.
*   	In this session, we will provide an overview of RDD estimation in Stata, 
*   	involving the basic approach, crucial statistical tests and some necessary graphic skills to demonstrate the results. 




*** Housekeeping ***

	set more off
	clear all
	cd "/Users/yanran/Documents/Uchicago/2026/Data for stata lab"


***************Programming Tips in Stata

*【Marco】

	*【Local marco】

		*-Definition and manipulation

		local x 2
			dis `x' 
		
		*-Basic Functions
			
			*- Store scalars
	
			local x=2+2
			dis `x'
			
			local x 2+2		
			dis `x'
	
			*-Store strings
			local x "Learning Stata, writing paper"
			dis "`x'"
			
			*-Store variable names
			sysuse auto, clear
			local xx "price weight mpg foreign"
			sum `xx'
			//equal to *sum price weight mpg foreign*
			reg `xx'
			//equal to *reg price weight mpg foreign*
			
			*-Extract variable labels
			ssc install des2
			// Describe current dataset (clickable output)

			sysuse auto,clear 
			des2
			local lab: var label foreign  // The local name is *lab*, which holds the label of the *foreign* 
			// Use local to extract data attributes
			dis "`lab'" // show the label
			label var mpg  "`lab'"  // Assign the label of the *foreign* to the *mpg* 
			des2 foreign mpg
			
			*-Extract labels of variable's values
			sysuse auto,clear 
			local lab: value label foreign // The local name is *lab*, which holds the vaule label of the *foreign* 
			dis "`lab'" // show the vaule label
			label value  rep78 "`lab'"
			des2 foreign rep78


	*【Global marco】

		*-Definition and manipulation
		 global aa "This is my first global!"      
     	 dis "$aa"
		 //We need type *$* when indicating the global we defined 

    	 global x1 = 5
    	 global x2 = 2^$x1
    	 dis $x2

		*-Tips in regression
			sysuse auto, clear
   			global  robust ",robust"    //options in common
	  		global y "price"
			global x "weight"
   		 	global control "mpg rep78 headroom trunk length" 
	 
	 		sum $y $x $control
			//equal to *sum price weight mpg rep78 headroom trunk length*
	 
	 	*-column(1)	
	 		reg $y $x $robust  
	 		//Equal to: reg price weight , r
   		 est store m1
	 
	 	*-column(2)
   		 reg $y $x $control $robust   
	 	//Equal to: reg price weight mpg rep78 headroom trunk length, r
   		 est store m2
	
	 	*-column(3)
   		 reg $y $x $control i.foreign $robust    
	 	//Equal to: reg price weight mpg rep78 headroom trunk length i.foreign, r
   		 est store m3
	 
	 	local s "using mytable.rtf"
		//holds the path
	 	local mm "m1 m2 m3"
		//holds the models we stored
   		esttab `mm' `s', nogap s(N r2_a) addnote($robust  ) replace	 
	 
	*-Extension readings:
    	view browse "https://www.lianxh.cn/news/4d57e771feba7.html"
	


*【Loop statements and conditional statements】


	*【forvalues: Loops in values】

		/*	Syntax:

		forvalues i = range {
		 command
		}
		*/

		forvalues i = 1(2)14{
		   dis  `i'
		}
		//(2): indicate the interval is 2
		
		*-Example 1:
		sysuse nlsw88, clear
		global yx "wage hours collgrad ttl_exp"
		
		forvalues i = 1/4{        //The series of equal differences with a common difference of 1
		  dis "Occupation == " `i' 
		  reg $yx if occupation==`i'  //equal to *reg wage hours collgrad ttl_exp if occupation==1/2/3/4*
		  est store m`i'
		}
		esttab m1 m2 m3 m4, nogap s(r2_a N)
		tab occu

		*-Example 2:

		forvalues x = 1/4 {
         if mod(`x', 2)==1 {  ////Remainder of divide by 2
           display "`x' is odd"
		   display "Haha!"
         	}
         else {
           display "`x' is even"
         	}
      	}


	*【foreach: Loops in variables】

		/*	Syntax:
		foreach var of varlist {
		command
		}
		*/
		

		*-Example 1:
		sysuse auto, clear
		foreach var of varlist price-length {
			gen ln`var' = ln(`var')
			}
			
		*-Example 2:
		sysuse nlsw88, clear
		local vars "wage hours ttl_exp grade"
		foreach v of varlist `vars'{
			gen ln`v'=ln(`v')
 		     }      



*** Read in the raw data ***

	use lec4_grade, clear



*** question 1: OLS regression ***

//add additional control variables step by step 
	reg avgmath classize,r

	reg avgmath classize disadv ,r

	gen esquare=enrollment^2
	reg avgmath classize disadv enrollment esquare,r

//Limit the sample to schools with enrollment between 20 and 60 students
	drop if enrollment>60
	drop if enrollment<20
	reg avgmath classize disadv enrollment esquare,r



*** question 2: fuzzy RD-Manual estimation ***

	gen largeclass=.
	replace largeclass=1 if enrollment<=40
	replace largeclass=0 if enrollment>40

	//left side regression
	reg avgmath largeclass disadv enrollment esquare if enrollment<40&enrollment>=35
	matrix coef_left=e(b)
	local intercept_left=coef_left[1,5]

	//right side regression
	reg avgmath largeclass disadv enrollment esquare if enrollment<=45&enrollment>=40
	matrix coef_right=e(b)
	local intercept_right=coef_right[1,5]

	//get intercept difference
	local difference =`intercept_right'-`intercept_left'
	macro list



*** question 3: fuzzy RD-2SLS estimation ***

	gen func= enrollment/(int((enrollment-1)/40)+1)  //IV
	ivregress 2sls avgmath disadv enrollment esquare (largeclass=func), vce(robust) first



*** question 4: fuzzy RD-Automatic estimation ***

//ssc install rdrobust first
	rdrobust avgmath enrollment,c(40) p(1) q(2) covs(disadv  ) kernel(triangular) level(95) h(5) all

//graph
	rdplot avgmath enrollment,c(40) p(1) graph_options(title(Figure) xtitle(enrollment) ytitle(avgmath))

//Manipulate tests
	
	DCdensity classize, breakpoint(40) generate(Xj Yj r0 fhat se_fhat)
	
	// or
	ssc install rddensity
	ssc install lpdensity
	help rddensity



*** Housekeeping ***

	clear all





*							========================================

*								 - Assignments -

*				Replicate the fuzzy RD estimation in RD lab.
*				Please try to apply rdrobust and rdplot for RD-Automatic estimation by yourself 
*				(don't need to submit this part).
*				Show 4 graphs needed in RD as well as the regression results. 
*				(Hint: *rddensity* *rdplot* may help you)

*				【Required】
*					1. The "jumping" graph
*					2. Density of Running Variable (by *rddensity* or *DCdensity*)

*				【Optional】
*					1. Placebo tests on other variables (No "jumps").
*					2. Robustness checks: by changing the kernel function, the order of the polynomial etc

*				Submit your own codes and results(including the graphs)

*。                         =========================================
*


*							       - Extension Learning -

*				Source:
*				Cattaneo, M. D., Idrobo, N. and Titiunik, R. (2019).
*				A Practical Introduction to Regression Discontinuity Designs.
*
*				This extension is written as an annotated replication guide.
*				It uses the paper-replication dataset from the link below, while
*				leaving the earlier classroom example based on lec4_grade unchanged.
*
*				Local data file:
*				data/RDD/Cattaneo_Idrobo_Titiunik_2019_RD.dta
*
*				Repository data link:
*				https://raw.githubusercontent.com/Zhihan-iris/causal-inference/main/data/RDD/Cattaneo_Idrobo_Titiunik_2019_RD.dta
*
*				Case background:
*				The example studies close municipal elections. The treatment is
*				whether an Islamic party candidate barely wins the mayoral election.
*				The comparison is not between all Islamic-party winners and all
*				non-Islamic-party winners. Instead, the RD design compares elections
*				decided by very small margins around zero.
*
*				Substantive setting:
*				The running variable is the Islamic margin of victory. The cutoff is
*				zero. Municipalities just above the cutoff are compared with those
*				just below the cutoff. The outcome is the female high school share.
*				A positive RD estimate means that municipalities where the Islamic
*				candidate barely won have a higher female high school share than
*				municipalities where the Islamic candidate barely lost.
*
*				Important interpretation:
*				The estimated effect is local to close elections near the cutoff.
*				It should not be interpreted as the average effect of Islamic-party
*				mayors in all municipalities.
*
*				Empirical logic:
*				RD identifies a local treatment effect at the cutoff if potential
*				outcomes and predetermined covariates evolve smoothly through the
*				cutoff, and if units cannot precisely manipulate the running variable.



*** Extension 0: Data and variable setting ***

	* This block makes the code reproducible.
	* First, Stata checks whether the dataset is already in the current folder.
	* If the file is missing, Stata downloads it from the original online link.
	capture confirm file "data/RDD/Cattaneo_Idrobo_Titiunik_2019_RD.dta"
	if _rc {
		copy "https://raw.githubusercontent.com/Zhihan-iris/causal-inference/main/data/RDD/Cattaneo_Idrobo_Titiunik_2019_RD.dta" ///
			"data/RDD/Cattaneo_Idrobo_Titiunik_2019_RD.dta", replace
	}

	* The analysis below uses the local copy. This avoids depending on internet
	* access every time students run the do-file.
	use "data/RDD/Cattaneo_Idrobo_Titiunik_2019_RD.dta", clear

	* Define global macros for variables used repeatedly below.
	* $y is the outcome: female high school percentage.
	* $x is the running variable: Islamic margin of victory.
	* T is the treatment indicator: it switches at the cutoff.
	* $cutoff is zero because X is already centered at the threshold.
	* $covariates contains baseline controls used for covariate adjustment.
	* $balance contains predetermined variables used in balance/falsification tests.
	global y "Y"
	global x "X"
	global cutoff 0
	global covariates "vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"
	global balance "hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"

	* Always begin by checking variable names and summary statistics.
	* This helps confirm that the data were loaded correctly.
	describe $y $x T $covariates prov_num
	summarize $y $x T $covariates

	* A quick check of the assignment rule.
	* Observations with X just below zero are close-loss municipalities.
	* Observations with X just above zero are close-win municipalities.
	tab T
	summarize $x if T == 0
	summarize $x if T == 1



*** Extension 1: RD plots and bin choices ***

	* RD graphs are not formal proof, but they are essential diagnostics.
	* They show whether the outcome appears to jump at the cutoff and whether
	* the fitted curves are driven by observations far away from the cutoff.

	* Figure 3a: Raw comparison of means.
	* nbins(2500 500) sets separate numbers of bins on the left and right.
	* p(0) fits local constants, so this graph emphasizes binned means rather
	* than a flexible fitted curve.
	rdplot $y $x, nbins(2500 500) p(0) ///
		graph_options(graphregion(color(white)) ///
		xtitle("Islamic Margin of Victory") ///
		ytitle("Female High School Percentage") ylabel(0(10)70))

	graph export "cit_rd_01_raw_comparison.png", width(1200) replace

	* Figure 3b: Local comparison of means.
	* The condition abs($x) <= 50 focuses on observations closer to the cutoff.
	* p(4) allows a fourth-order polynomial on each side for a flexible visual fit.
	rdplot $y $x if abs($x) <= 50, nbins(2500 500) p(4) ///
		graph_options(graphregion(color(white)) ///
		xtitle("Islamic Margin of Victory") ///
		ytitle("Female High School Percentage") ylabel(0(10)70))

	graph export "cit_rd_02_local_comparison.png", width(1200) replace

	* IMSE RD plot with evenly-spaced bins.
	* binselect(es) chooses an IMSE-optimal number of evenly-spaced bins.
	* Evenly-spaced bins divide the running-variable support into equal intervals.
	rdplot $y $x, binselect(es) ///
		graph_options(graphregion(color(white)) xtitle(Score) ytitle(Outcome))

	graph export "cit_rd_03_binselect_es.png", width(1200) replace

	* IMSE RD plot with quantile-spaced bins.
	* binselect(qs) chooses an IMSE-optimal number of quantile-spaced bins.
	* Quantile-spaced bins put approximately similar numbers of observations
	* into each bin, which is useful when the density of X is uneven.
	rdplot $y $x, binselect(qs) ///
		graph_options(graphregion(color(white)) xtitle(Score) ytitle(Outcome))

	graph export "cit_rd_04_binselect_qs.png", width(1200) replace

	* Mimicking variance RD plot with evenly-spaced bins.
	* esmv uses a mimicking-variance rule rather than the IMSE rule.
	* Comparing es/qs with esmv/qsmv shows whether the visual conclusion depends
	* on the way bins are selected.
	rdplot $y $x, binselect(esmv) ///
		graph_options(graphregion(color(white)) xtitle(Score) ytitle(Outcome))

	graph export "cit_rd_05_binselect_esmv.png", width(1200) replace

	* Mimicking variance RD plot with quantile-spaced bins.
	* qsmv is the quantile-spaced version of the mimicking-variance rule.
	rdplot $y $x, binselect(qsmv) ///
		graph_options(graphregion(color(white)) xtitle(Score) ytitle(Outcome))

	graph export "cit_rd_06_binselect_qsmv.png", width(1200) replace



*** Extension 2: Local polynomial point estimation ***

	* The rdrobust coefficient is the estimated jump in E[Y|X] at X = 0.
	* In this case, it estimates the local effect of barely electing an Islamic
	* mayor on the female high school share.
	* The sign and magnitude should be read in outcome units: percentage points.

	* Manual bandwidth: h = 20, uniform kernel.
	* This reproduces a simple local linear RD estimator with a researcher-chosen
	* bandwidth. The uniform kernel gives equal weight to all observations inside
	* the window and zero weight to observations outside the window.
	rdrobust $y $x, kernel(uniform) p(1) h(20)

	* Automatic MSE-optimal bandwidth: p = 1, triangular kernel.
	* p(1) estimates separate local linear regressions on the two sides.
	* kernel(triangular) gives more weight to observations closer to the cutoff.
	* bwselect(mserd) asks rdrobust to select a common MSE-optimal bandwidth
	* for the RD treatment effect.
	rdrobust $y $x, kernel(triangular) p(1) bwselect(mserd)

	* rdrobust stores useful estimation results in e().
	* e(h_l) is the selected bandwidth on the left side of the cutoff.
	* With mserd, the left and right bandwidths are the same.
	local h_mse = e(h_l)

	* Use the selected bandwidth to draw the associated RD plot.
	* This graph matches the estimation window used by rdrobust above.
	rdplot $y $x if abs($x) <= `h_mse', p(1) h(`h_mse') kernel(triangular) ///
		graph_options(graphregion(color(white)) ///
		xtitle("Islamic Margin of Victory") ///
		ytitle("Female High School Percentage") ylabel(0(10)70))

	graph export "cit_rd_07_mserd_bandwidth_plot.png", width(1200) replace

	* Remove the regularization term in bandwidth selection.
	* scaleregul(0) is a sensitivity check. It shows how much the selected
	* bandwidth and estimate change when the regularization term is removed.
	rdrobust $y $x, kernel(triangular) p(1) bwselect(mserd) scaleregul(0)



*** Extension 3: Statistical inference and bandwidth reporting ***

	* Show conventional, bias-corrected and robust inference.
	* The option all reports:
	* 1. conventional inference,
	* 2. bias-corrected point estimates,
	* 3. robust bias-corrected confidence intervals.
	* In applied RD work, the robust row is usually the main inferential result.
	rdrobust $y $x, kernel(triangular) p(1) bwselect(mserd) all

	* CER-optimal bandwidth for robust confidence intervals.
	* MSE-optimal bandwidths target point estimation.
	* CER-optimal bandwidths target coverage-error performance of confidence
	* intervals. They are often smaller and can produce wider confidence intervals.
	rdrobust $y $x, kernel(triangular) p(1) bwselect(cerrd) all
	local h_cer = e(h_l)

	* List all main MSE and CER bandwidth selectors.
	* rdbwselect is useful when reporting robustness across bandwidth rules.
	* mserd imposes a common bandwidth; msetwo allows different left/right
	* bandwidths; cerrd and certwo are the analogous CER choices.
	rdbwselect $y $x, kernel(triangular) p(1) all



*** Extension 4: Additional commands: covariates and clusters ***

	* Covariate-adjusted RD.
	* Covariates are not the source of identification in a valid RD design.
	* They can improve precision and adjust for small finite-sample imbalance.
	rdrobust $y $x, covs($covariates) p(1) kernel(triangular) ///
		bwselect(mserd) scaleregul(1)

	* Cluster-robust nearest-neighbor variance.
	* If observations may be correlated within provinces, clustering by prov_num
	* makes the standard errors more conservative for within-province dependence.
	rdrobust $y $x, p(1) kernel(triangular) bwselect(mserd) ///
		scaleregul(1) vce(nncluster prov_num)

	* Covariates and clusters together.
	* This combines covariate adjustment with clustered variance estimation.
	rdrobust $y $x, covs($covariates) p(1) kernel(triangular) ///
		bwselect(mserd) scaleregul(1) vce(nncluster prov_num)



*** Extension 5: Validity checks and falsification tests ***

	* The key RD assumption is continuity at the cutoff.
	* Because potential outcomes are not directly observed, we examine indirect
	* evidence: predetermined covariates should not jump at the cutoff, and the
	* density of the running variable should not show suspicious manipulation.

	* 5.1 RD plots for predetermined covariates.
	* These graphs repeat the RD visual exercise using variables that should not
	* be affected by the treatment. Large visible jumps would weaken credibility.
	foreach z of varlist lpop1994 partycount vshr_islam1994 i89 merkezp merkezi {
		rdplot `z' $x, graph_options(graphregion(color(white)) xtitle("Score"))
		graph export "cit_rd_balance_plot_`z'.png", width(1200) replace
	}

	* 5.2 Formal continuity-based analysis for covariates.
	* Here the covariate is placed on the left-hand side of rdrobust.
	* The desired result is no statistically meaningful discontinuity at X = 0.
	foreach z of global balance {
		rdrobust `z' $x, all
	}

	* 5.3 Same checks using CER-optimal bandwidth.
	* This repeats the balance tests with bandwidths designed for robust
	* confidence interval coverage.
	foreach z of global balance {
		rdrobust `z' $x, all bwselect(cerrd)
	}

	* 5.4 A simple binomial test example from the notes.
	* bitesti n k p tests whether the observed count k out of n trials is
	* consistent with probability p. Here the null is a 50/50 split.
	bitesti 100 53 1/2

	* 5.5 Density/manipulation test around the cutoff.
	* rddensity tests whether the density of the running variable is continuous
	* at the cutoff. A sharp density jump may indicate sorting or manipulation.
	* Required packages if not installed:
	* ssc install rddensity, replace
	* net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace

	capture drop temp*
	rddensity $x, plot plot_range(-50 50) plot_n(100 100) genvars(temp)

	graph export "cit_rd_08_density_test.png", width(1200) replace



*** Extension 6: Placebo cutoffs ***

	* Placebo cutoffs check whether discontinuities appear at artificial cutoffs.
	* If the design is credible, the main discontinuity should be concentrated at
	* the true cutoff, not at nearby fake cutoffs.

	* R stores one row per placebo cutoff:
	* column 1: cutoff value
	* column 2: selected bandwidth
	* column 3: conventional estimate
	* column 4: bias-corrected estimate
	* column 5: robust standard error
	* column 6: robust p-value
	* columns 7-8: robust 95% confidence interval
	* column 9: effective number of observations
	matrix define R = J(7,9,.)
	local r = 1

	forvalues c = -3(1)3 {
		* For fake cutoffs to the right of the true cutoff, use only treated-side
		* observations. For fake cutoffs to the left, use only control-side
		* observations. This avoids mixing the true discontinuity into placebo tests.
		if `c' > 0 {
			local condition "if $x >= 0"
		}
		else if `c' < 0 {
			local condition "if $x < 0"
		}
		else {
			local condition ""
		}

		rdrobust $y $x `condition', c(`c')

		* Store key estimates from each rdrobust run.
		matrix R[`r', 1] = `c'
		matrix R[`r', 2] = e(h_l)
		matrix R[`r', 3] = e(tau_cl)
		matrix R[`r', 4] = e(tau_bc)
		matrix R[`r', 5] = e(se_tau_rb)
		matrix R[`r', 6] = 2 * normal(-abs(R[`r', 4] / R[`r', 5]))
		matrix R[`r', 7] = R[`r', 4] - invnormal(0.975) * R[`r', 5]
		matrix R[`r', 8] = R[`r', 4] + invnormal(0.975) * R[`r', 5]
		matrix R[`r', 9] = e(N_h_l) + e(N_h_r)

		local r = `r' + 1
	}

	preserve
	clear
	svmat R

	* Plot placebo estimates and confidence intervals.
	* The horizontal line at zero helps students see whether fake cutoffs produce
	* effects that are distinguishable from zero.
	twoway (rcap R7 R8 R1, lcolor(navy)) ///
		(scatter R3 R1, mcolor(cranberry)), ///
		yline(0, lcolor(black) lpattern(dash)) ///
		graphregion(color(white)) xlabel(-3 -2 -1 0 1 2 3) ///
		ytitle("Placebo Treatment Effect") ///
		xtitle("Cutoff, x = 0 is the true cutoff") legend(off)

	graph export "cit_rd_09_placebo_cutoffs.png", width(1200) replace
	restore



*** Extension 7: Sensitivity analysis ***

	* 7.1 Donut-hole approach: remove observations very close to the cutoff.
	* The idea is to check whether the estimate is driven by observations located
	* extremely close to the cutoff. k is the radius of the excluded center window.
	matrix define RR = J(6,9,.)
	local r = 1

	forvalues k = 0(0.1)0.5 {
		rdrobust $y $x if abs($x) >= `k'

		* Store the RD estimate after excluding observations within k of the cutoff.
		matrix RR[`r', 1] = `k'
		matrix RR[`r', 2] = e(h_l)
		matrix RR[`r', 3] = e(tau_cl)
		matrix RR[`r', 4] = e(tau_bc)
		matrix RR[`r', 5] = e(se_tau_rb)
		matrix RR[`r', 6] = 2 * normal(-abs(RR[`r', 4] / RR[`r', 5]))
		matrix RR[`r', 7] = RR[`r', 4] - invnormal(0.975) * RR[`r', 5]
		matrix RR[`r', 8] = RR[`r', 4] + invnormal(0.975) * RR[`r', 5]
		matrix RR[`r', 9] = e(N_h_l) + e(N_h_r)

		local r = `r' + 1
	}

	preserve
	clear
	svmat RR

	* Plot how the estimated treatment effect changes as the donut hole grows.
	twoway (rcap RR7 RR8 RR1, lcolor(navy)) ///
		(scatter RR3 RR1, mcolor(cranberry)), ///
		yline(0, lcolor(black) lpattern(dash)) ///
		graphregion(color(white)) xlabel(0 0.1 0.2 0.3 0.4 0.5) ///
		ytitle("RD Treatment Effect") ///
		xtitle("Donut Hole Radius") legend(off)

	graph export "cit_rd_10_donut_hole.png", width(1200) replace
	restore

	* 7.2 Sensitivity to bandwidth.
	* Here we compare four bandwidths: the MSE-optimal bandwidth, twice the
	* MSE-optimal bandwidth, the CER-optimal bandwidth, and twice the
	* CER-optimal bandwidth.
	local h_mse2 = 2 * `h_mse'
	local h_cer2 = 2 * `h_cer'
	local bandwidths "`h_mse' `h_mse2' `h_cer' `h_cer2'"

	matrix define T = J(4,9,.)
	local r = 1

	foreach h of local bandwidths {
		rdrobust $y $x, h(`h')

		* Store the point estimate, robust standard error and confidence interval.
		matrix T[`r', 1] = `h'
		matrix T[`r', 2] = e(h_l)
		matrix T[`r', 3] = e(tau_cl)
		matrix T[`r', 4] = e(tau_bc)
		matrix T[`r', 5] = e(se_tau_rb)
		matrix T[`r', 6] = 2 * normal(-abs(T[`r', 4] / T[`r', 5]))
		matrix T[`r', 7] = T[`r', 4] - invnormal(0.975) * T[`r', 5]
		matrix T[`r', 8] = T[`r', 4] + invnormal(0.975) * T[`r', 5]
		matrix T[`r', 9] = e(N_h_l) + e(N_h_r)

		local r = `r' + 1
	}

	preserve
	clear
	svmat T

	* Plot whether the conclusion is stable across bandwidth choices.
	twoway (rcap T7 T8 T1, lcolor(navy)) ///
		(scatter T3 T1, mcolor(cranberry)), ///
		yline(0, lcolor(black) lpattern(dash)) ///
		graphregion(color(white)) xlabel(, angle(45)) ///
		ytitle("RD Treatment Effect") ///
		xtitle("Bandwidth") legend(off)

	graph export "cit_rd_11_bandwidth_sensitivity.png", width(1200) replace
	restore

	* 7.3 Sensitivity to polynomial order and kernel function.
	* A credible RD result should not depend entirely on one arbitrary
	* polynomial order or one kernel choice. This loop reports the main estimate
	* under several common specifications.
	foreach poly in 1 2 {
		foreach ker in triangular uniform epanechnikov {
			display "Polynomial order = `poly'; kernel = `ker'"
			rdrobust $y $x, p(`poly') kernel(`ker') bwselect(mserd) all
		}
	}
