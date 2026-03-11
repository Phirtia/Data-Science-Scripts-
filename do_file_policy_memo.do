tab b2a

summarize b2a, detail

gen xstrg = "0"
replace xstrg = "100% Domestic" if b2a == 100
replace xstrg = "Foreign Investiment" if b2a < 100

tab xstrg

generate foreign = (xstrg == "Foreign Investiment")
label define foreign_lbl 0 "100% Domestic" 1 "Foreign Investiment"
label values foreign foreign_lbl

tab foreign

gen ge8d_yes = (ge8d == 1)

tab ge8d
tab ge8d_yes

/// Treating j30e variable

label values j30e 
tab j30e

recode j30e (-9 = .)
tab j30e
tab j30e, nolabel

reg ge8d_yes foreign j30e
reg ge8d_yes j30e
logit ge8d_yes i.j30e // same results

/////////////////// IV II ////////////////

reg foreign k21 b5 b3a d3c a17 b1 b7a  a4a a6a a2 a3a, robust
reg ge8d_yes k21 b5 b3a d3c a17 b1 b7a a4a a6a a2 a3a, robust

* First stage alone
reg ge8d_yes a2 b7a d3c a3a, robust
test a2 b7a d3c a3a

* IV 2SLS
ivregress 2sls ge8d_yes j30e n2b (foreign = a2 b7a d3c a3a), robust
estat firststage
scalar fs_f_2sls = r(mineigenval)
estat endogenous
scalar endog_p = r(p)

///////////////////// Paper format table //////////


// labels
label variable foreign            "Foreign Ownership"
label variable j30e               "Political Instability"
label variable ge8d_yes           "Green Policy Adoption"
cap label variable instab_foreign "Foreign × Instability"

// Clear stored estimates
eststo clear

// Step 1: OLS Baseline
eststo m1: reg ge8d_yes foreign j30e, robust

// Step 2: OLS Interaction
cap gen instab_foreign = foreign * j30e
eststo m2: reg ge8d_yes foreign j30e instab_foreign, robust

// Step 3: IV 2SLS
eststo m3: ivregress 2sls ge8d_yes j30e (foreign = a2 b7a d3c a3a), robust
estat firststage
scalar fs_f_2sls = r(mineigenval)
estat endogenous
scalar endog_p = r(p)

// Step 4: LIML Robustness 
eststo m4: ivregress liml ge8d_yes j30e (foreign = a2 b7a d3c a3a), robust
estat firststage
scalar fs_f_liml = r(mineigenval)

// Export 
esttab m1 m2 m3 m4 using "results.rtf", replace ///
    label                                         /// 
    title("Table 1: Green Policy Adoption")       ///
    mtitles("OLS" "OLS Interaction" "IV 2SLS" "LIML") ///
    keep(foreign j30e instab_foreign)             ///
    order(foreign j30e instab_foreign)            ///
    star(* 0.10 ** 0.05 *** 0.01)                 ///
    b(%9.3f) se(%9.3f)                            ///
    brackets                                      /// 
    nogaps                                        ///
    stats(N r2,                                   ///
          fmt(%9.0f %9.3f)                        ///
          labels("Observations" "R-squared"))     ///
    scalars("fs_f_2sls First Stage F"             ///
            "endog_p  Endogeneity p-value"        ///
            "fs_f_liml First Stage F (LIML)")     ///
    sfmt(%9.3f)                                 /// 
    note("Robust standard errors in brackets."   ///
         "Instruments: a2, b7a, d3c, a3a."       ///
         "* p<0.10, ** p<0.05, *** p<0.01")
		 
		 
///// chart ////////////////

graph bar (mean) n2b, over(foreign, ///
        relabel(1 "Domestic" 2 "Foreign")) ///
    bar(1, color(navy)) ///
    bar(2, color(maroon)) ///
    blabel(bar, format(%4.2f) position(outside)) ///
    ytitle("Mean Energy Spending (n2b)") ///
    plotregion(margin(t+5)) ///
    scheme(s2color)
	
