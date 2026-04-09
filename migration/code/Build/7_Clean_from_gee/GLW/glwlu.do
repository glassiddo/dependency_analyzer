***********************************************************************
* This program documents how land use for cattle rearing between 2010 *
*     and 2020  correlates with tree cover loss over the same period  *
***********************************************************************

*  Preamble

cd "${projdir}"

* Importing data on land use for cattle rearing and tree cover loss

import delimited "..\MIGRATION AFRICA\DATA\RAW\AFRICA\GLW\sampleUnitsCattleLandUseAggregate.csv", clear bindquote(strict)

la var ipums_id           "IPUMS ID"
la var country            "Country"
la var admin_name         "Admin unit"
la var geotype            "Admin unit shape"
la var areaha             "Area (ha)"
la var tc2000total        "Total tree cover in 2000 (ha, weighted by percent cover)"
la var tclosstotal        "Tree cover loss 2001-2020 (ha, weighted by percent cover)"
la var tclossinter        "Tree cover loss 2010-2020 (ha, weighted by percent cover)"
la var lucattletotal      "Total cattle area 2010-2020 (ha)"
la var lucattleinter      "Cattle area emerging 2015-2020 (ha)"
la var tclosscattletotal  "Tree cover loss with cattle 2010-2020 (ha, weighted by percent cover)"
la var tclosscattleinter  "Tree cover loss with cattle 2015-2020 (ha, weighted by percent cover)"

* Land use for cattle rearing and tree cover loss across the countries of interest
preserve
	collapse (first) geotype (sum) areaha tc2000total tclosstotal tclossinter lucattletotal lucattleinter tclosscattletotal tclosscattleinter
	gen country = "All"
	gen admin_name = "All Countries"
	gen ipums_id = 0
	tempfile ALLLUCTCLOSS
	save `"`ALLLUCTCLOSS'"', replace
restore

append using `"`ALLLUCTCLOSS'"'

* Land use for cattle rearing and tree cover loss by countries
preserve
	collapse (first) geotype (sum) areaha tc2000total tclosstotal tclossinter lucattletotal lucattleinter tclosscattletotal tclosscattleinter, by(country)	
	
	gen shtclosstotal = tclosstotal*100/tc2000total
	la var shtclosstotal      "Share of tree cover lost 2001-2020 (%)"
	
	gen shtclossinter = tclossinter*100/tc2000total
	la var shtclossinter      "Share of tree cover lost 2010-2020 (%)"
	
	* Tree cover loss between 2010 and 2020
	#delimit ;
		graph bar (mean) shtclosstotal shtclossinter, 
		over(
			country,
			label(labsize(*0.5) labcolor("black") tlcolor("black") angle(90))
		)
		bargap(15)
		graphregion(fcolor(white)) 
		plotregion(fcolor(white) lcolor(none) ilcolor(white))
		bar(1, fcolor(maroon*.75) lcolor(black) lwidth(none)) 
		bar(2, fcolor(maroon*.25) lcolor(black) lwidth(none)) 
		blabel(bar, position(outside) format(%4.1f) size(tiny) color(black))
		ytitle("Share of 2000 tree cover lost (%)", height(4) axis(1) size(*.8) color("black")) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		yscale(noline)
		legend(
			lab(1 `"Tree cover lost 2001-2020"')  
			lab(2 `"Tree cover lost 2010-2020"')  
			pos(6) 
			ring(1) 
			row(1) 
			col(2) 
			size(2.0) 
			symxsize(10) 
			region(lcolor(white)) 
			margin(zero) 
			bmargin(zero) 
			alignment(middle) 
			placement(6) 
		)  
		caption("{bf:Source}: Authors' calculations based on data from the Global Forest Change dataset.", height(4) size(2.0) color(black))
		name(SHTCLOSSINTER, replace);
	#delimit cr
	graph export "..\MIGRATION AFRICA\OUTPUT\FIGURES\CORRELATIONS\SHTCLOSSINTER.png", as(png) name("SHTCLOSSINTER") width(10000) replace

	* Share of land allocated to cattle rearing by 2020
	
	gen shlucattletotal = lucattletotal*100/areaha
	la var shlucattletotal    "Share of area used for cattle 2010-2020 (%)"
	
	gen shlucattleinter = lucattleinter*100/areaha
	la var shlucattleinter    "Share of area for cattle emerging 2015-2020 (%)"
	
	#delimit ;
		graph bar (mean) shlucattletotal shlucattleinter, 
		over(
			country,
			label(labsize(*0.5) labcolor("black") tlcolor("black") angle(90))
		)
		bargap(15)
		graphregion(fcolor(white)) 
		plotregion(fcolor(white) lcolor(none) ilcolor(white))
		bar(1, fcolor(maroon*.75) lcolor(black) lwidth(none)) 
		bar(2, fcolor(maroon*.25) lcolor(black) lwidth(none)) 
		blabel(bar, position(outside) format(%4.1f) size(tiny) color(black))
		ytitle("Share of land area with cattle rearing (%)", height(4) axis(1) size(*.8) color("black")) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		yscale(noline)
		legend(
			lab(1 `"by 2020"')  
			lab(2 `"in 2015 and 2020"')  
			pos(6) 
			ring(1) 
			row(1) 
			col(2) 
			size(2.0) 
			symxsize(10) 
			region(lcolor(white)) 
			margin(zero) 
			bmargin(zero) 
			alignment(middle) 
			placement(6) 
		)  
		caption("{bf:Source}: Authors' calculations based on data from the Gridded Livestock of the World dataset.", height(4) size(2.0) color(black))
		name(SHLUCATTLE, replace);
	#delimit cr
	graph export "..\MIGRATION AFRICA\OUTPUT\FIGURES\CORRELATIONS\SHLUCATTLE.png", as(png) name("SHLUCATTLE") width(10000) replace
	
	* Tree cover loss with cattle land expension
	gen shtclossintercat = tclosscattleinter*100/tclossinter
	la var shtclossintercat   "Share of tree cover loss with cattle 2015-2020 (%)"
	
	#delimit ;
		graph bar (mean) shtclossintercat, 
		over(
			country,
			label(labsize(*0.5) labcolor("black") tlcolor("black") angle(90))
		)
		bargap(15)
		graphregion(fcolor(white)) 
		plotregion(fcolor(white) lcolor(none) ilcolor(white))
		bar(1, fcolor(maroon) lcolor(black) lwidth(none)) intensity(25) 
		blabel(bar, position(outside) format(%4.1f) size(tiny) color(black))
		ytitle("Share of tree cover loss (2010–2020) with cattle land expansion (%)", height(4) axis(1) size(*.8) color("black")) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		yscale(noline)
		legend(
			off
			pos(6) ring(1) row(1) col(2) size(2.0) symxsize(10) 
			region(lcolor(white)) margin(zero) bmargin(zero) 
			alignment(middle) placement(6) 
			stack
		)  
		caption("{bf:Source}: Authors' calculations based on data from the Gridded Livestock of the World and Global Forest Change datasets.", height(4) size(2.0) color(black))
		name(SHTCLOSSINTERCAT, replace);
	#delimit cr
	graph export "..\MIGRATION AFRICA\OUTPUT\FIGURES\CORRELATIONS\SHTCLOSSINTERCAT.png", as(png) name("SHTCLOSSINTERCAT") width(10000) replace
name("SHTCLOSSINTERCAT") replace
restore
