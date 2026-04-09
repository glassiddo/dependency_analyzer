use "${projdir}\data\Raw\Africa\comtrade\DataJobID-2838294_2838294_TRADEAGNONAG.dta", clear

rename *, lower

* not necessary but useful to understand the categories

la def lproduct  1 "Food and live animals", modify
la def lproduct  2 "Beverages and tobacco", modify
la def lproduct  3 "Crude materials, inedible, except fuels", modify
la def lproduct  4 "Mineral fuels, lubricants and related materials", modify
la def lproduct  5 "Animal and vegetable oils and fats", modify
la def lproduct  6 "Chemicals", modify
la def lproduct  7 "Manufact goods classified chiefly by material", modify
la def lproduct  8 "Machinery and transport equipment", modify
la def lproduct  9 "Miscellaneous manufactured articles", modify
la def lproduct 10 "Commod. & transacts. Not class. Accord. To kind", modify

la val productcode lproduct 

gen ag = (productcode == 1)

bys reporteriso3 year tradeflowcode ag: egen value = total(tradevaluein1000usd)
la var value "`: var la tradevaluein1000usd'"

bysort reporteriso3 reportername year tradeflowcode ag (productcode): keep if _n == 1

keep reporteriso3 reportername year tradeflowcode tradeflowname ag value

reshape wide value, i(reporteriso3 year tradeflowcode) j(ag)

ren (value0 value1) (nonag ag)

drop tradeflowname 

reshape wide nonag ag, i(reporteriso3 year) j(tradeflowcode)
ren (*5 *6) (*_import *_export)

decode reportername, generate(country_name)

keep if country_name==	"Benin" | /// 
country_name== "Botswana"| ///
country_name==	 "Burkina Faso"| ///
country_name==	 "Cameroon"| ///
country_name== "Ghana"| ///
country_name==	 "Guinea"| ///
country_name==	"Kenya"| ///
country_name== "Lesotho"| ///
country_name== "Malawi"| ///
country_name==	"Mali"| ///
country_name==	"Mauritius"| ///
country_name==	"Mozambique"| ///
country_name==	 "Rwanda"| ///
country_name==	 "Senegal"| ///
country_name==	 "Sierra Leone"| ///
country_name==	 "South Africa"| ///
country_name==	 "Sudan"| ///
country_name==	 "Togo"| ///
country_name==	"Tanzania"| ///
country_name==	 "Uganda"| ///
country_name==	 "Zambia" | ///
country_name==	 "Zimbabwe"

keep  country_name year ag_import ag_export nonag_import nonag_export
order country_name year ag_import ag_export nonag_import nonag_export

compress
save "${projdir}\data\Build\Africa\Comtrade\comtrade.dta", replace