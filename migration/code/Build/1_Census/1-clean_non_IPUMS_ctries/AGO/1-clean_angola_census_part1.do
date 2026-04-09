*------------------------------------------------------------------------------*
*   Filename:       Angola_Census.do
*   Description:    Formating Angola raw data
*   Author:         Robin Benabid Jegaden (r.benabidjegaden@gmail.com)
*   Created on:     30 Dec 2141
*   Stata version:  15.1
*   Modified by:    Iddo Glass (glass.iddo@gmail.com), 03 November 2025
*------------------------------------------------------------------------------*

clear

*==============================================================================*
*                                                                              *
*   Settings                                                       *
*                                                                              *                                                                               
*==============================================================================*

set more off, perm
set mem 1000
set matsize 11000
*log using logfiles\Migration_pre_cleaning.log, replace
set trace off 
pause on

gl stem "C:\Users\iddo2\Dropbox\Migration Africa"
global dir "${stem}"
global rawdtadir "${stem}\data\Raw\Countries\AGO"

*==============================================================================*
*Generating Census data
use "$rawdtadir\Census\archives\ind_14.dta", clear
*------------------------------------------------------------------------------*
    * 1 census in 2014 of 2M4+ obs.(10%),          
    ** Using municipality of residence 5 years ago (consistent boundaries), 
    *** Relative stability between 2009 and 2014: strong migration pattern,
    **** Civil war ended in 2002.
*------------------------------------------------------------------------------*
*   IPUMS harmonization                            * 
*==============================================================================*

//Generate IPUMS census similar variables.
    gen country=.
        label var country "country"
    gen year=.
        label var year "year"
    gen urban=. 
        label var urban "urban-rural status"
    gen geo1_ao=. 
        label var geo1_ao "angola, province 2014 [level 1; consistent boundaries, gis]"
    gen mig1_5_ao=.
        label var mig1_5_ao "province of residence 5 years ago, angola; consistent boundaries, gis"
    gen geo2_ao=. 
        label var geo2_ao "angola, municipality 2014 [level 2; consistent boundaries, gis]"
    gen mig2_5_ao=. 
        label var mig2_5_ao "municipality of residence 5 years ago, angola; consistent boundaries, gis"
    gen migctry5=.
        label var migctry5 "country of residence 5 years ago"
    gen age=.
        label var age "age"
    gen empstat=.
        label var empstat "activity status (employment status)"
    gen indgen=.
        label var indgen "industry, general recode"

    *Country
    replace country=141
    label def country 141 "angola"
    label val country country
    
    *Census year
    replace year=2014
    
    *Urban status
    replace urban=1 if URBAN_RURAL==2 
    replace urban=2 if URBAN_RURAL==1
    label def urban 1 "rural" 2 "urban"
    label val urban urban
    
    *Province boundaries
        *2014
        forvalues i = 1/18 {
            replace geo1_ao = 141000 + `i' if PROV == `i'
        }
        label def geo1_ao 141001 "Cabinda" 141002 "Zaire" 141003 "Uíge" 141004 "Luanda" 141005 "Cuanza Norte" ///
                            141006 "Cuanza Sul" 141007 "Malanje" 141008 "Lunda Norte" 141009 "Benguela" 141010 "Huambo" ///
                            141011 "Bie" 141012 "Moxico" 141013 "Cuando Cubango" 141014 "Namibe" 141015 "Huila" ///
                            141016 "Cunene" 141017 "Lunda Sul" 141018 "Bengo"
        label val geo1_ao geo1_ao
        *2009
        forvalues i = 1/18 {
            replace mig1_5_ao = 141000 + `i' if RES2009_PROV == `i'
        }
            replace mig1_5_ao= 141098 if RES2009_PROV==99 //unknown/missing
            replace mig1_5_ao= 141099 if RES2009_PROV==0 //niu
        label def mig1_5_ao 141001 "Cabinda" 141002 "Zaire" 141003 "Uíge" 141004 "Luanda" 141005 "Cuanza Norte" ///
                            141006 "Cuanza Sul" 141007 "Malanje" 141008 "Lunda Norte" 141009 "Benguela" 141010 "Huambo" ///
                            141011 "Bie" 141012 "Moxico" 141013 "Cuando Cubango" 141014 "Namibe" 141015 "Huila" ///
                            141016 "Cunene" 141017 "Lunda Sul" 141018 "Bengo" 141098 "unknown" 141099 "niu (not in universe)"
        label val mig1_5_ao mig1_5_ao
        
    *Age
    replace age=AGE
    label def age 0 "less than 1 year" 1 "1 year" 2 "2 years"
    label val age age
    
    *Employment
        *Empstat
        label drop WORK_LAST_WEEK 
        label def WORK_LAST_WEEK 1 "Worked in any paid activity" 2 "Absent from paid work" ///
        3 "Worked in any unpaid activity" 4 "Did not work" 5 "Student (only studied)" ///
        6 "Domestic (Household tasks only)" 7 "Retired" 8 "Permanently unable to work" ///
        9 "Other(Specify)"
        label val WORK_LAST_WEEK WORK_LAST_WEEK
        replace empstat = 1 if inlist(WORK_LAST_WEEK,1,2,3) 
        replace empstat = 2 if WORK_LAST_WEEK==4 & AVAILABLE==1 & inlist(LOOKING,1,2) // didn't work, but looking for a job (1 - looked for a job, 2 - looked for a job for the first time, 3 - didn't look), and available (1- yes). I'm including actively looking, as it matches ILO definition and seems to better match other countries. However, the review of the census by the Angolan government ignores those (counts also looking=3 as part of labour force, see p63 in https://www.ine.gov.ao/Arquivos/arquivosCarregados/Carregados/Publicacao_637981512172633350.pdf)
        replace empstat = 3 if inlist(WORK_LAST_WEEK,5,6,7,8)
        replace empstat = 3 if WORK_LAST_WEEK==4 & ((LOOKING==3 & AVAILABLE==1) | AVAILABLE==2) // didn't work but didn't look for a job, or was unavailable
        // is absent from paid work - essentially on leave/unpaid work so still part of labour force
        replace empstat = 3 if WORK_LAST_WEEK==4 & missing(AVAILABLE) // edge cases, 3 observations
        replace empstat = 0 if WORK_LAST_WEEK==9
        replace empstat = 0 if WORK_LAST_WEEK==. // could also be labelled as 9, but seems like ipums usually labels these kind as 0
        label def empstat 0 "Niu (not in universe)" 1 "Employed" 2 "Unemployed" 3 "Inactive" 9 "Unkown/missing" 
        label val empstat empstat       
        
        *Indgen     

        // old version
//          replace indgen = 10 if strpos(str_label, "cultur") > 0 | strpos(str_label, "Cultur") > 0 | ///
//          strpos(str_label, "Pesca") > 0 | strpos(str_label, "pesca") > 0 | strpos(str_label, "florest") > 0
//          replace indgen = 20 if strpos(str_label, "Extrac") > 0  
//          replace indgen = 30 if strpos(str_label, "Fabricação") > 0  
//          replace indgen = 40 if strpos(str_label, "Instalação") > 0 
//          replace indgen = 50 if strpos(str_label, "Construção") > 0  | strpos(str_label, "construção") > 0
//          replace indgen = 60 if strpos(str_label, "Comércio") > 0  | strpos(str_label, "comércio") > 0
//          replace indgen = 70 if strpos(str_label, "restau") > 0  | strpos(str_label, "Restau") > 0
//          replace indgen = 80 if strpos(str_label, "Transport") > 0  | strpos(str_label, "transport") > 0             
//          replace indgen = 100 if strpos(str_label, "Administração") > 0  | strpos(str_label, "defesa") > 0
        
        // Save current dataset temporarily
        tempfile main_data
        save `main_data'
        
        // Load the crosswalk CSV - was made by letting LLM determine the matches based on the previous labels and the harmonization table of INDGEN in IPUMSI
        preserve
        import delimited "$rawdtadir\Census\industry_code_angola.csv", clear varnames(1) encoding("UTF-8")
        rename code merge_industry
        keep merge_industry indgen_code industry_label
        duplicates drop merge_industry, force
        
        tempfile crosswalk
        save `crosswalk'
        restore
        
        // Merge with main data
        capture drop merge_industry  // Drop if it exists from previous run
        gen merge_industry = INDUSTRY
        merge m:1 merge_industry using `crosswalk', keep(master match) keepusing(indgen_code) nogen
        
        // Replace indgen with the matched codes
        replace indgen = indgen_code if !missing(indgen_code)
        drop indgen_code merge_industry
        
        generate str_label = ""
        levelsof INDUSTRY, local(levels)
        foreach level of local levels {
            local label : label (INDUSTRY) `level'
            replace str_label = "`label'" if INDUSTRY == `level'
            }

        * NIU
        replace indgen = 0 if indgen ==. & empstat==1
        replace indgen = 0 if missing(indgen) // niu

        * Unknown (999)
        replace indgen = 999 if strpos(lower(str_label), "unknown") > 0
        replace indgen = 999 if strpos(lower(str_label), "niu") > 0
        replace indgen = 999 if INDUSTRY==99999

        label def indgen 10 "Agriculture, fishing, and forestry" 20 "Mining and extraction" 30 "Manufacturing" 40 "Electricity, gas, water and waste management" 50 "Construction" 60 "Wholesale and retail trade" 70 "hotels and restaurants" 80 "Transportation, storage, and communication" 90 "Financial services and insurance" 100 "Public administration and defense" 110 "Services, not specified" 111 "Business services and real estate" 112 "Education" 113 "Health and social work" 114 "Other services" 120 "Private household services" 130 "Other industry, ne.c." 0 "NIU (not in universe)" 999 "Unknown"
        label val indgen indgen

    *Municipality boundaries
        *2014
        replace geo2_ao=MUN
        replace geo2_ao=9999 if geo2_ao==1217 //defined as unknown because not labelled: impossible to match with GIS boundary
        //// iddo's update - the previous row was written by robin, but apparently the moxico labels are all incorrect, the labels are fixed separately in the r file
        label define geo2_ao 101 "Cabinda" 103 "Cacongo(ex_Lândana)" 105 "Buco Zau" 107 "Belize" ///
            201 "Mbanza Congo" 203 "Soyo" 205 "Nzetu" 207 "Tomboco" 209 "Nóqui" 211 "Cuimba" ///
            301 "Uíge" 303 "Ambuíla" 305 "Songo" 307 "Bembe" 309 "Negage" 311 "Bungo" ///
            313 "Maquela do Zombo" 315 "Damba" 317 "Cangola" 319 "Sanza Pombo" 321 "Quitexe" ///
            323 "Quimbele" 325 "Mulinga(Ex.Santa Cruz)" 327 "Puri" 329 "Mucaba (ex. Quinzala)" ///
            331 "Buengas (ex Nova Esperança)" 409 "Cazenga" 415 "Cacuaco" 417 "Viana" 419 "Luanda" ///
            421 "Belas" 423 "Icolo Bengo" 425 "Quissama" 501 "Cazengo" 503 "Lucala" 505 "Golungo Alto" ///
            507 "Cambambe" 509 "Ambaca" 511 "Quiculungo" 513 "Bolongongo" 515 "Banga" ///
            517 "Samba Cajú" 519 "Ngonguembo" 601 "Sumbe (ex. Ngunza)" 603 "Amboim (ex. Gabela)" ///
            605 "Quilenda" 607 "Porto Amboim" 609 "Libolo (ex. Calulo)" 611 "Quibala" 613 "Mussende" ///
            615 "Seles (ex. Uku Seles)" 617 "Conda" 619 "Cassongue" 621 "Cela ( ex Waku-Kungo)" ///
            623 "Ebo" 701 "Malanje" 703 "Cacuso" 705 "Calandula" 707 "Cambundi-Catembo" 709 "Quela" ///
            711 "Cahombo" 713 "Massango" 715 "Luquembo" 717 "Marimba" 719 "Cunda-Dia-Baze" ///
            721 "Quirima" 723 "Mucari" 725 "Cangandala" 727 "Quiwaba-N'Zogi" 801 "Lucapa" ///
            803 "Cambulo" 805 "Chitato" 807 "Cuilo" 809 "Caungula" 811 "Cuango" 813 "Lubalo" ///
            815 "Capenda-Camulemba" 817 "Xá-Muteba" 901 "Benguela" 903 "Baía Farta" 905 "Lobito" ///
            907 "Cubal" 909 "Ganda" 911 "Balombo" 913 "Bocoio" 915 "Caimbambo" 917 "Chongoroi" ///
            919 "Catumbela" 1001 "Huambo" 1003 "Tchikala-Tcholohanga" 1005 "Catchiungo" ///
            1007 "Bailundo" 1009 "Caála" 1011 "Ecunha" 1013 "Ukuma" 1015 "Longonjo" ///
            1017 "Mungo" 1019 "Londuimbale" 1021 "Tchinjenje" 1101 "Cuito" 1103 "Cunhinga (Vouga)" ///
            1105 "Chinguar" 1107 "Andulo" 1109 "N'harea" 1111 "Camacupa" 1113 "Cuemba" ///
            1115 "Chitembo" 1117 "Catabola (ex. Nova Sintra)" 1201 "Luena" 1203 "Camanongue" ///
            1205 "Luacano" 1207 "Cameia" 1209 "Budas-Lumbala-Nguimbo" 1211 "Luchazes" ///
            1213 "Alto Zambeze" 1215 "Luau" 1301 "Menongue" 1303 "Cuito Cuanavale" 1305 "Cuangar" ///
            1307 "Rivungo" 1309 "Mavinga" 1311 "Cuchi" 1313 "Dirico" 1315 "Nancova" ///
            1317 "Calai" 1401 "Namibe" 1403 "Tômbwa (ex. Porto Alexandre)" 1405 "Virei" ///
            1407 "Bibala" 1409 "Camucuio" 1501 "Lubango" 1503 "Cacula" 1505 "Chibia" 1507 "Caconda" ///
            1509 "Caluquembe" 1511 "Quilengues" 1513 "Cuvango" 1515 "Quipungo" 1517 "Matala" ///
            1519 "Chicomba" 1521 "Jamba" 1523 "Chipindo" 1525 "Gambos ( ex-Chiange)" 1527 "Humpata" ///
            1601 "Cuanhama" 1603 "Ombadja (ex. Cuamato)" 1605 "Cuvelai" 1607 "Curoca (ex.Oncocua)" ///
            1609 "Namacunde" 1611 "Cahama" 1701 "Saurimo" 1703 "Muconda" 1705 "Dala" ///
            1707 "Cacolo" 1801 "Dande (Caxito)" 1807 "Ambriz" 1809 "Nambuangongo" 1811 "Bula-Atumba" ///
            1813 "Dembos-Quibaxe" 1815 "Pango-Aluquem" 9999 "unknown"
        label val geo2_ao geo2_ao

        *2009
        /* Tried to recode it but it doesn't work because they did a bit of cleaning on RES2009_MUNIC, so we can retrieve the raw var/data
        list MUN if RES2009==2 & MUN==. | MUN==0 | MUN==9999
        gen res2009=.
        replace res2009 = MUN if RES2009==2
        replace res2009 = RES2009_OTHER_MUNIC if res2009==.
        */
        list MUN if RES2009==2 & RES2009_MUNIC!= MUN //check var RES2009_MUNIC is reliable: ok
        list MUN if RES2009!=2 & RES2009_MUNIC== MUN //check var RES2009_MUNIC is reliable: ok
        *Build mig2_5_ao with RES2009_MUNIC
        replace mig2_5_ao=RES2009_MUNIC
        replace mig2_5_ao=RES2009_OTHER_MUNIC if RES2009_OTHER_MUNIC!=. & mig2_5_ao==. | RES2009_OTHER_MUNIC!=. & mig2_5_ao==0 ///
        | RES2009_OTHER_MUNIC!=. & mig2_5_ao==9999 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==1 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==3 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==5 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==7 ///
        | RES2009_OTHER_MUNIC!=. & mig2_5_ao==199 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==299 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==399 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==499 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==599 ///
        | RES2009_OTHER_MUNIC!=. & mig2_5_ao==699 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==799 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==899 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==999 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==1099 ///
        | RES2009_OTHER_MUNIC!=. &  mig2_5_ao==1199 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==1299 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==1399 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==1499 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==1599 ///
        | RES2009_OTHER_MUNIC!=. & mig2_5_ao==1699 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==1799 | RES2009_OTHER_MUNIC!=. & mig2_5_ao==1899 
        *Harmonize unknown obs.
        replace mig2_5_ao=9999 if mig2_5_ao==199 | mig2_5_ao==299 | mig2_5_ao==399 | mig2_5_ao==499 | mig2_5_ao==599 ///
        | mig2_5_ao==699 | mig2_5_ao==799 | mig2_5_ao==899 | mig2_5_ao==999 | mig2_5_ao==1099 ///
        | mig2_5_ao==1199 | mig2_5_ao==1299 | mig2_5_ao==1399 | mig2_5_ao==1499 | mig2_5_ao==1599 ///
        | mig2_5_ao==1699 | mig2_5_ao==1799 | mig2_5_ao==1899 //niu
        replace mig2_5_ao=9999 if mig2_5_ao==1 | mig2_5_ao==3 | mig2_5_ao==5 | mig2_5_ao==7 ///
        | mig2_5_ao==401 | mig2_5_ao==403 | mig2_5_ao==407 | mig2_5_ao==411 | mig2_5_ao==1803
        *Foreign country
        replace mig2_5_ao=9998 if RES2009_FOREIGN!=. & mig2_5_ao==. | RES2009_FOREIGN!=. & mig2_5_ao==9999
        label define mig2_5_ao 101 "Cabinda" 103 "Cacongo(ex_Lândana)" 105 "Buco Zau" 107 "Belize" ///
            201 "Mbanza Congo" 203 "Soyo" 205 "Nzetu" 207 "Tomboco" 209 "Nóqui" 211 "Cuimba" ///
            301 "Uíge" 303 "Ambuíla" 305 "Songo" 307 "Bembe" 309 "Negage" 311 "Bungo" ///
            313 "Maquela do Zombo" 315 "Damba" 317 "Cangola" 319 "Sanza Pombo" 321 "Quitexe" ///
            323 "Quimbele" 325 "Mulinga(Ex.Santa Cruz)" 327 "Puri" 329 "Mucaba (ex. Quinzala)" ///
            331 "Buengas (ex Nova Esperança)" 409 "Cazenga" 415 "Cacuaco" 417 "Viana" 419 "Luanda" ///
            421 "Belas" 423 "Icolo Bengo" 425 "Quissama" 501 "Cazengo" 503 "Lucala" 505 "Golungo Alto" ///
            507 "Cambambe" 509 "Ambaca" 511 "Quiculungo" 513 "Bolongongo" 515 "Banga" ///
            517 "Samba Cajú" 519 "Ngonguembo" 601 "Sumbe (ex. Ngunza)" 603 "Amboim (ex. Gabela)" ///
            605 "Quilenda" 607 "Porto Amboim" 609 "Libolo (ex. Calulo)" 611 "Quibala" 613 "Mussende" ///
            615 "Seles (ex. Uku Seles)" 617 "Conda" 619 "Cassongue" 621 "Cela ( ex Waku-Kungo)" ///
            623 "Ebo" 701 "Malanje" 703 "Cacuso" 705 "Calandula" 707 "Cambundi-Catembo" 709 "Quela" ///
            711 "Cahombo" 713 "Massango" 715 "Luquembo" 717 "Marimba" 719 "Cunda-Dia-Baze" ///
            721 "Quirima" 723 "Mucari" 725 "Cangandala" 727 "Quiwaba-N'Zogi" 801 "Lucapa" ///
            803 "Cambulo" 805 "Chitato" 807 "Cuilo" 809 "Caungula" 811 "Cuango" 813 "Lubalo" ///
            815 "Capenda-Camulemba" 817 "Xá-Muteba" 901 "Benguela" 903 "Baía Farta" 905 "Lobito" ///
            907 "Cubal" 909 "Ganda" 911 "Balombo" 913 "Bocoio" 915 "Caimbambo" 917 "Chongoroi" ///
            919 "Catumbela" 1001 "Huambo" 1003 "Tchikala-Tcholohanga" 1005 "Catchiungo" ///
            1007 "Bailundo" 1009 "Caála" 1011 "Ecunha" 1013 "Ukuma" 1015 "Longonjo" ///
            1017 "Mungo" 1019 "Londuimbale" 1021 "Tchinjenje" 1101 "Cuito" 1103 "Cunhinga (Vouga)" ///
            1105 "Chinguar" 1107 "Andulo" 1109 "N'harea" 1111 "Camacupa" 1113 "Cuemba" ///
            1115 "Chitembo" 1117 "Catabola (ex. Nova Sintra)" 1201 "Luena" 1203 "Camanongue" ///
            1205 "Luacano" 1207 "Cameia" 1209 "Budas-Lumbala-Nguimbo" 1211 "Luchazes" ///
            1213 "Alto Zambeze" 1215 "Luau" 1301 "Menongue" 1303 "Cuito Cuanavale" 1305 "Cuangar" ///
            1307 "Rivungo" 1309 "Mavinga" 1311 "Cuchi" 1313 "Dirico" 1315 "Nancova" ///
            1317 "Calai" 1401 "Namibe" 1403 "Tômbwa (ex. Porto Alexandre)" 1405 "Virei" ///
            1407 "Bibala" 1409 "Camucuio" 1501 "Lubango" 1503 "Cacula" 1505 "Chibia" 1507 "Caconda" ///
            1509 "Caluquembe" 1511 "Quilengues" 1513 "Cuvango" 1515 "Quipungo" 1517 "Matala" ///
            1519 "Chicomba" 1521 "Jamba" 1523 "Chipindo" 1525 "Gambos ( ex-Chiange)" 1527 "Humpata" ///
            1601 "Cuanhama" 1603 "Ombadja (ex. Cuamato)" 1605 "Cuvelai" 1607 "Curoca (ex.Oncocua)" ///
            1609 "Namacunde" 1611 "Cahama" 1701 "Saurimo" 1703 "Muconda" 1705 "Dala" ///
            1707 "Cacolo" 1801 "Dande (Caxito)" 1807 "Ambriz" 1809 "Nambuangongo" 1811 "Bula-Atumba" ///
            1813 "Dembos-Quibaxe" 1815 "Pango-Aluquem" 9998 "abroad" 9999 "unknown"
        label val mig2_5_ao mig2_5_ao
        
        replace migctry5 = RES2009_FOREIGN
		replace migctry5 = . if migctry5 == 24 // prev country == ANGOLA should be NA
		
        label define migctry5 ///
        4 "Afghanistan" 8 "Albania" 10 "Antarctica" 12 "Algeria" 16 "Western Samoa" ///
        20 "Andorra" 24 "Angola" 28 "Antigua and Barbuda" 31 "Azerbaijan" 32 "Argentina" ///
        36 "Australia" 40 "Austria" 44 "Bahamas" 48 "Bahrain" 50 "Bangladesh" ///
        51 "Armenia" 52 "Barbados" 56 "Belgium" 60 "Bermuda" 64 "Bhutan" ///
        68 "Bolivia" 70 "Bosnia and Herzegovina" 72 "Botswana" 74 "Bouvet Island (Territory of Norway)" ///
        76 "Brazil" 84 "Belize" 86 "British Indian Ocean Territory" 90 "Solomon Islands" ///
        92 "Virgin Islands (England)" 96 "Brunei" 100 "Bulgaria" 104 "Myanmar" 108 "Burundi" ///
        112 "Belarus" 116 "Cambodia" 120 "Cameroon" 124 "Canada" 132 "Cape Verde" ///
        136 "Cayman Islands" 140 "Central African Republic" 144 "Sri Lanka" 148 "Chad" ///
        152 "Chile" 156 "China" 158 "Taiwan" 162 "Christmas Island" 166 "Cocos Islands" ///
        170 "Colombia" 174 "Comoros Islands" 175 "Mayotte" 178 "Congo" 180 "Democratic Republic of the Congo" ///
        184 "Cook Islands" 188 "Costa Rica" 191 "Croatia" 192 "Cuba" 196 "Cyprus" ///
        203 "Czech Republic" 204 "Benin" 208 "Denmark" 212 "Dominica" 214 "Dominican Republic" ///
        218 "Ecuador" 222 "El Salvador" 226 "Equatorial Guinea" 231 "Ethiopia" 232 "Eritrea" ///
        233 "Estonia" 234 "Faroe Islands" 238 "Falkland Islands (Malvin)" 239 "South Georgia and the South Sandwich Islands" ///
        242 "Fiji" 246 "Finland" 248 "Åland Islands" 250 "France" 254 "French Guiana" ///
        258 "French Polynesia" 260 "Territory of Southern France" 262 "Djibouti" 266 "Gabon" ///
        268 "Georgia" 270 "Gambia" 275 "Occupied Palestinian Territories" 276 "Germany" ///
        288 "Ghana" 292 "Gibraltar" 296 "Kiribati" 300 "Greece" 304 "Greenland" ///
        308 "Grenada" 312 "Guadeloupe" 316 "Guam (United States Territory)" 320 "Guatemala" ///
        324 "Guinea" 328 "Guyana" 332 "Haiti" 334 "Heard and McDonald Islands (Territory of Australia)" ///
        336 "Vatican" 340 "Honduras" 344 "Hong Kong" 348 "Hungary" 356 "India" ///
        360 "Indonesia" 364 "Iran" 368 "Iraq" 372 "Ireland" 376 "Israel" ///
        380 "Italy" 384 "Ivory Coast" 388 "Jamaica" 392 "Japan" 398 "Kazakhstan" ///
        400 "Jordan" 404 "Kenya" 408 "North Korea" 410 "South Korea" 414 "Kuwait" ///
        417 "Kyrgyzstan" 418 "Laos" 422 "Lebanon" 426 "Lesotho" 428 "Haiti" ///
        434 "Libya" 438 "Liechtenstein" 440 "Lithuania" 442 "Luxembourg" 446 "Macau" ///
        450 "Madagascar" 454 "Malawi" 458 "Malaysia" 462 "Maldives" 466 "Mali" ///
        470 "Malta" 474 "Martinique" 478 "Mauritania" 480 "Mauritius" 484 "Mexico" ///
        492 "Monaco" 496 "Mongolia" 498 "Moldova" 499 "Montenegro" 500 "Montserrat" ///
        504 "Morocco" 508 "Mozambique" 512 "Oman" 516 "Namibia" 520 "Nauru" ///
        524 "Nepal" 528 "Netherlands" 530 "Netherlands Antilles" 533 "Aruba" ///
        540 "New Caledonia" 548 "Vanuatu" 554 "New Zealand" 558 "Nicaragua" ///
        562 "Niger" 566 "Nigeria" 570 "Niue" 574 "Norfolk Islands" 578 "Norway" ///
        580 "Northern Mariana Islands" 581 "United States Minor Islands" 583 "Micronesia" ///
        584 "Marshall Islands" 585 "Palau" 586 "Pakistan" 591 "Panama" 598 "Papua New Guinea" ///
        600 "Paraguay" 604 "Peru" 608 "Philippines" 612 "Pitcairn Island" 616 "Poland" ///
        620 "Portugal" 624 "Guinea-Bissau" 626 "Timor Leste" 630 "Puerto Rico" 634 "Qatar" ///
        638 "Reunion Island" 642 "Romania" 643 "Russian Federation" 646 "Rwanda" ///
        652 "Saint Bartholomew" 654 "Saint Helena" 659 "Saint Kitts and Nevis" 660 "Anguilla" ///
        662 "Saint Lucia" 663 "Saint Martin" 666 "St. Pierre and Miquelon" 670 "Saint Vincent and Grenadines" ///
        674 "San Marino" 678 "Sao Tome and Principe" 682 "Saudi Arabia" 686 "Senegal" ///
        688 "Serbia" 690 "Seychelles Islands" 694 "Sierra Leone" 702 "Singapore" ///
        703 "Slovakia" 704 "Vietnam" 705 "Slovenia" 706 "Somalia" 710 "South Africa" ///
        716 "Zimbabwe" 724 "Spain" 732 "Western Sahara" 736 "Sudan" 740 "Suriname" ///
        744 "Svalbard and Jan Mayen Islands" 748 "Swaziland" 752 "Sweden" 756 "Switzerland" ///
        760 "Syria" 762 "Tajikistan" 764 "Thailand" 768 "Togo" 772 "Tokelau Islands" ///
        776 "Tonga" 780 "Trinidad and Tobago" 784 "United Arab Emirates" 788 "Tunisia" ///
        792 "Turkey" 795 "Turkmenistan" 796 "Turks and Caicos Islands" 798 "Tuvalu" ///
        800 "Uganda" 804 "Ukraine" 807 "Macedonia (Yugoslav Republic)" 818 "Egypt" ///
        826 "Great Britain (United Kingdom, UK)" 832 "Guernsey" 833 "Isle of Man" ///
        834 "Tanzania" 840 "USA" 850 "Virgin Islands (United States)" 854 "Burkina Faso" ///
        858 "Uruguay" 860 "Uzbekistan" 862 "Venezuela" 876 "Wallis and Futuna Islands" ///
        882 "Western Samoa (WSM)" 887 "Yemen" 894 "Zambia"

    label values migctry5 migctry5

save "$rawdtadir\Census\perlim14.dta", replace
    
*==============================================================================*
*                                                                              *
*   END SECTION:  Closing the do-file                                          *
*                                                                              *
*==============================================================================*
