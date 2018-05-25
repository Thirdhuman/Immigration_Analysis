#Immigrant Welfare 2

#### Personal ####
# supplemental security income, child received - ssikidyn
# child covered by medicare/medicaid - ch_mc
# child covered by state’s chip - pchip

#### Personal ####

# covered by medicare - care
# covered by medicaid - caid
# wic benefits received - wicyn
# supplemental security income received - ssi_yn
# public assistance - person - paw_yn
# educational assistance - ed_yn
# survivor's benefits other than social security or veterans benefits - sur_yn
# unemployment compensation - uc_yn

z2010 <- subset(z, peinusyr >= 22 & i_stat == 3)
z2012 <- subset(z, peinusyr >= 23 & i_stat == 3)

svytotal( ~one, design = z2010)
																
public_charge <- function(x){
	k <- subset(x, caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1
													| ed_yn == 1)
	svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
 
public_charge(z)
public_charge(z2012)
public_charge(z2010)

z <- i_stat == 3

#### Household ####

# food stamps recipients - hfoodsp
# public housing project - hpublic
# educational assistance benefits - hed_yn
# energy assistance benefits - hengast 
# supplemental security benefits - hssi_yn
# children receiving free or reduced price lunches - hflunch

household_public_charge <- function(x){
	k <- subset(x,  care == 1 | caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1 | ed_yn == 1 |
													 pchip == 1 | ch_mc == 1 | ssikidyn == 1	| 
															hfoodsp == 1 | hpublic == 1 | hflunch == 1 | hengast == 1)
	svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}

household_only_public_charge <- function(x){
	k <- subset(x,  hfoodsp == 1 | hpublic == 1 | hflunch == 1 | hengast == 1)
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}

household_public_charge_nolunch <- function(x){
	k <- subset(x,  care == 1 | caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1 | ed_yn == 1 |
													 pchip == 1 | ch_mc == 1 | ssikidyn == 1	| 
															hfoodsp == 1 | hpublic == 1 | hengast == 1)
	svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}

household_public_charge(z)
household_public_charge(z2010)
household_public_charge(z2012)

household_only_public_charge(z)
household_only_public_charge(z2010)
household_only_public_charge(z2012)

hbh <- household_public_charge(z2010)

hbh_nl <- household_public_charge_nolunch(z2010)
write.xlsx(hbh_nl, "No_Lunch.xlsx")


fipscodes <- read.xlsx("~/Desktop/Poverty:Welfare/Tableau/gestfips.xlsx", sheetName = "Sheet1")


# public assistance - household - hpaw_yn
# supplemental unemployment benefits received - subuc
# medicaid, anyone in hhld covered by hmcaid
# medicare, anyone in hhld covered by hmcare
# social security income - hssval
# wic program benefits, anyone - hrwicyn

											pCh_mc 	<- function(x){ k <- subset(x, 	ch_mc == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}


											pCare	<- function(x){ k <- subset(x, 	care == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											pCaid	<- function(x){ k <- subset(x, 	caid == 1 | ch_mc == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											pWicyn	<- function(x){ k <- subset(x, 	wicyn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											pSsi_yn	<- function(x){ k <- subset(x, 	ssi_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											pPaw_yn	<- function(x){ k <- subset(x, 	paw_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											pEd_yn	<- function(x){ k <- subset(x, 	ed_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}
											pSur_yn	<- function(x){ k <- subset(x, 	sur_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											pUc_yn	<- function(x){ k <- subset(x, 	uc_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}
											
											pPchip	<- function(x){ k <- subset(x, 	pchip == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											
											pSsikidyn	<- function(x){ k <- subset(x, 	ssikidyn == 1	) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											hHfoodsp	<- function(x){ k <- subset(x, 	hfoodsp == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											hHpaw_yn	<- function(x){ k <- subset(x, 	hpaw_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											
											hHpublic	<- function(x){ k <- subset(x, 	hpublic == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
											hHed_yn	<- function(x){ k <- subset(x, 	hed_yn == 1  ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}
											hHflunch	<- function(x){ k <- subset(x, 	hflunch == 1  ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}
											hHengast	<- function(x){ k <- subset(x, 	hengast == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}
											Huc_yn	<- function(x){ k <- subset(x, 	uc_yn == 1) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}

pCare(z)
pCaid(z)
pWicyn(z)
pSsi_yn(z)
pPaw_yn(z)
pEd_yn(z)
pPchip	(z)
pCh_mc (z)	
pSsikidyn(z)
hHfoodsp(z)
hHpublic(z)
hHflunch(z)
hHengast(z)

pcare_i <- pCare(z2010)
pcare_i$variable <- "Medicare"

pcaid_i <- pCaid(z2010)
pcaid_i$variable <- "Medicaid"

pwic_i <- pWicyn(z2010)
pwic_i$variable <- "WIC"

pssi_i <- pSsi_yn(z2010)
pssi_i$variable <- "SSI"

ppaw_i <- pPaw_yn(z2010)
ppaw_i$variable <- "PAW"

pEd_i <- pEd_yn(z2010)
pEd_i$variable <- "Ed_assistance"

pchip_i <- pPchip	(z2010)
pchip_i$variable <- "pchip"

pfoodsp_i <- hHfoodsp(z2010)
pfoodsp_i$variable <- "SNAP"

ppublicH_i <- hHpublic(z2010)
ppublicH_i$variable <- "Public_Housing"

phflunch_i <- hHflunch(z2010)
phflunch_i$variable <- "Lunch"

pengast_i <- hHengast(z2010)
pengast_i$variable <- "LIHEAP"

hbh$variable <- "Totals"

State_Im_welfare <- Reduce(function(x, y) merge(x, y, all=TRUE), list(pengast_i, phflunch_i, ppublicH_i, 
																								pfoodsp_i, pchip_i, pEd_i, ppaw_i, pssi_i, pwic_i, pcaid_i, pcare_i, hbh))

fipscodes <- read.xlsx("~/Desktop/Poverty:Welfare/Tableau/gestfips.xlsx", sheetName = "Sheet1")

Final <- merge(fipscodes, State_Im_welfare)

write.xlsx(Final, "Immigrant Assistance (States).xlsx")

Final <- merge(fipscodes, hbh)

write.xlsx(Final, "Immigrant Assistance Totals (States).xlsx")

z <- read.xlsx("Immigrant Assistance (States).xlsx", 1)
Final_numbs <- merge(z, Final)

write.xlsx(Final_numbs, "Immigrant Assistance Totals (All States).xlsx")


#pCh_mc (z2010)	
#pSsikidyn(z2010)

pCare(z2012)
pCaid(z2012)
pWicyn(z2012)
pSsi_yn(z2012)
pPaw_yn(z2012)
pEd_yn(z2012)
pPchip	(z2012)
pCh_mc (z2012)	
pSsikidyn(z2012)
hHfoodsp(z2012)
hHpublic(z2012)
hHflunch(z2012)
hHengast(z2012)


household_bad_demographics <- function(x){
	k <- subset(x, (care == 1 | caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1 | ed_yn == 1 |
													 pchip == 1 | ch_mc == 1 | ssikidyn == 1	| 
															hfoodsp == 1 | hpublic == 1 | hflunch == 1 | hengast == 1) & i_stat == 3)
	svyby( ~one, by = ~ penatvty, design =  k, 	FUN = svytotal)}

household_bad_demographics(k)

k <- subset(z2010, (care == 1 | caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1 | ed_yn == 1 |
													 pchip == 1 | ch_mc == 1 | ssikidyn == 1	| 
															hfoodsp == 1 | hpublic == 1 | hflunch == 1 | hengast == 1) & i_stat == 3)

k <- update(k, prdtrace = factor( prdtrace ))
k <- update(k, pehspnon = factor( pehspnon ))
k <- update(k, a_sex = factor( a_sex ))
k <- update(k, a_age = factor( a_age))

svymean(  ~ a_age, design =  k)
svymean(  ~ a_sex, design =  k)
svymean(  ~ pehspnon, design =  k)
svymean(  ~ prdtrace, design =  k)

k <- update(k, a_age = factor( a_age))
svyquantile( ~ a_age, design =  k, c(.5))
svyquantile( ~ a_age, design =  z, c(.5))


household_bad_female(z)

  z <- update(z, i2_stat = ifelse( prcitshp <= 4, 0,ifelse(prcitshp == 1, 1, 0)))



