#Public Charge 2

#Bernie Welfare Tax

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

#### Taxes ####
# Social Security retirement payroll - fica
# State income tax liability, after all credits - statetax_ac
# State income tax liability, before credits - statetax_bc
# Federal income tax liability, after all credits - fedtax_ac
# Federal income tax liability, before credits - fedtax_bc

#### Earnings & Income ####
# Person income, total - ptotval

setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

library(survey)				# load survey package (analyzes complex design surveys)
library(reshape2)
library(plyr)

options( survey.replicates.mse = TRUE )

cpsasec_17=readRDS( file.path( path.expand( "~" ) , "CPSASEC" , "2017 cps asec.rds" ) )
#cpsasec_18=readRDS( file.path( path.expand( "~" ) , "CPSASEC" , "2018 cps asec.rds" ) )
z2018=svrepdesign(weights=~marsupwt,repweights="pwwgt[1-9]",type="Fay",rho=(1-1/sqrt(4)),data=cpsasec_17,combined.weights=TRUE);rm(cpsasec_17)

z2018=update(z2018,
	marrital_status=factor(a_maritl,
	labels=c("married - civilian spouse present","married - AF spouse present","married - spouse absent","widowed","divorced","separated","never married")),
	bachelors = ifelse(a_hga%in%0:40, 'Less than Bachelors', ifelse( a_hga%in%42:48, 'Bachelors or greater','NA')),
	hschool = ifelse(a_hga%in%0:39, 'Less than Highschool', ifelse( a_hga%in%43:48, 'Highschool or greater','NA')),
	yrs_educ = ifelse(a_hga%in%0:31, 0, 
		ifelse( a_hga==32, 4,
		ifelse( a_hga==33, 6,
		ifelse( a_hga==34, 8,
		ifelse( a_hga==35, 9,
		ifelse( a_hga==36, 10,
		ifelse( a_hga==37, 11,
		ifelse( a_hga==38 | a_hga==39, 12,
		ifelse( a_hga==40 | a_hga==41 | a_hga==42, 14,
		ifelse( a_hga==43, 16,
		ifelse( a_hga==44, 18,
		ifelse( a_hga==45, 20,
		ifelse( a_hga==46, 22, 0))))))))))))),
	potential_exp = (a_age - yrs_educ - 6),
	potential_exp2 = potential_exp*potential_exp,
	yrs_educ2 = yrs_educ*yrs_educ,
	lfstat =(ifelse( pemlr%in%1:2, 1,
		ifelse( pemlr%in%3:4, 2,
		ifelse( pemlr%in%5:7, 3, 0)))),
	urate = ifelse( lfstat%in%1:2, (lfstat==2/(lfstat==1| lfstat==2)), NA),
	lfstat=factor(lfstat,levels=c(1L, 2L, 3L),labels=c("Employed", "Unemployed", "Not in Labor Force")),
	i_stat = ifelse( prcitshp%in%1:3, 1,ifelse(prcitshp == 4, 2,ifelse(prcitshp == 5, 3, 1))),
	i2_stat =  ifelse( prcitshp%in%1:4, 0,ifelse(prcitshp == 5, 1, 0)),
	uninsure = ifelse( ahiper == 0, 0, ifelse(ahiper >= 1, 1, 0   )),
	ssi = ifelse( ssi_yn == 1, 1, ifelse(ssi_yn == 2, 0, 0)),
	pub_help = ifelse( paw_yn == 1, 1, ifelse(paw_yn == 2, 0, 0)),
	lunch_stat = ifelse( hflunch == 1, 1, ifelse(hflunch == 2, 0, 0)),
	avgsnapben = hfdval/h_numper,
	i_stat3 = i_stat/i2_stat,
	p_htotval = ifelse(htotval > 0, htotval, 0),
	p_hearnval = ifelse(hearnval > 0, hearnval, 0),
	p_fedtax_ac = ifelse(fedtax_ac > 0, fedtax_ac, 0),
	p_statetax_ac = ifelse(statetax_ac > 0, statetax_ac, 0),
	fed_tax_share= p_fedtax_ac * (ptotval/p_htotval),
	state_tax_share= p_statetax_ac * (ptotval/p_htotval)
	)
# ern_val
# HEARNVAL
# D FILESTAT 1 733 (1:6)
#  Tax Filer status
# V 1 .Joint, both <65
# V 2 .Joint, one <65 & one 65+
# V 3 .Joint, both 65+
# V 4 .Head of household
# V 5 .Single
# V 6 .Nonfiler

i2018 <- subset(z2018, peinusyr >= 22 & i_stat == 3)

svytotal( ~one, design = z2018)

public_charge2_alt <- function(x){
	k <- subset(x, caid == 1 
		& htotval == 2 
		& h_numper == 3
		& hflunch == 1
		& hearnval == 20000
		& hfoodsp == 1)
 print(svymean( ~f_mv_fs, design =  k))
	print(svymean( ~f_mv_sl, design =  k))
	print(svymean( ~hfdval, design =  k))
	print(svytotal( ~one, design =  k))
}

public_charge2_alt <- function(x){
	k <- subset(x,h_numper == 3
		& hflunch == 1
		& hearnval == 20000
		& hfoodsp == 1)
 print(svymean( ~f_mv_fs, design =  k))
	print(svymean( ~f_mv_sl, design =  k))
	print(svymean( ~hfdval, design =  k))
	print(svytotal( ~one, design =  k))}
			
public_charge <- function(x){
	k <- subset(x, caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1 | hlorent == 1 | hpublic == 1)
	print(svyby( ~fed_tax_share, by = ~ i2_stat, design =  subset(k, htotval > 0 & fedtax_ac > 0 & statetax_ac > 0 & htotval > 0), FUN = svytotal))
	print(svyby( ~state_tax_share, by = ~ i2_stat, design =  subset(k, htotval > 0 & fedtax_ac > 0 & statetax_ac > 0 & htotval > 0), FUN = svytotal))
	print(svyby( ~ern_val, by = ~ i2_stat, design =  subset(k, htotval > 0 & fedtax_ac > 0 & statetax_ac > 0 & htotval > 0), FUN = svytotal))
	print(svyby( ~one, by = ~ i2_stat, design =  subset(k, htotval > 0 & fedtax_ac > 0 & statetax_ac > 0 & htotval > 0), FUN = svytotal))
	}
public_charge(z2018)


public_charge2_alt(z2018)
public_charge2(z2018)


#### Household ####

# food stamps recipients - hfoodsp
# public housing project - hpublic
# educational assistance benefits - hed_yn
# energy assistance benefits - hengast 
# supplemental security benefits - hssi_yn
# children receiving free or reduced price lunches - hflunch

household_public_charge <- function(x){
	k <- subset(x,  care == 1 | caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1 | 
		ed_yn == 1 | pchip == 1 | ch_mc == 1 | ssikidyn == 1| 
		hfoodsp == 1 | hpublic == 1 | hflunch == 1 | hengast == 1)
	svyby( ~one, by = ~ gestfips, design =  k, FUN = svytotal)}

household_only_public_charge <- function(x){
	k <- subset(x,  hfoodsp == 1 | hpublic == 1 | hflunch == 1 | hengast == 1)
svyby( ~one, by = ~ i_stat, design =  k, FUN = svytotal)}

household_public_charge_nolunch <- function(x){
	k <- subset(x,  care == 1 | caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1 | ed_yn == 1 |
	pchip == 1 | ch_mc == 1 | ssikidyn == 1	| 
	hfoodsp == 1 | hpublic == 1 | hengast == 1)
	svyby( ~one, by = ~ gestfips, design =  k, FUN = svytotal)}

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
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}										pCare	<- function(x){ k <- subset(x, 	care == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 
		pCaid	<- function(x){ k <- subset(x, 	caid == 1 | ch_mc == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 										pWicyn	<- function(x){ k <- subset(x, 	wicyn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 										pSsi_yn	<- function(x){ k <- subset(x, 	ssi_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 										pPaw_yn	<- function(x){ k <- subset(x, 	paw_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 										pEd_yn	<- function(x){ k <- subset(x, 	ed_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}										pSur_yn	<- function(x){ k <- subset(x, 	sur_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 										pUc_yn	<- function(x){ k <- subset(x, 	uc_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}										pPchip	<- function(x){ k <- subset(x, 	pchip == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 										pSsikidyn<- function(x){ k <- subset(x, 	ssikidyn == 1	) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 										hHfoodsp<- function(x){ k <- subset(x, 	hfoodsp == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 										hHpaw_yn<- function(x){ k <- subset(x, 	hpaw_yn == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 										hHpublic<- function(x){ k <- subset(x, 	hpublic == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)} 	
		hHed_yn	<- function(x){ k <- subset(x, 	hed_yn == 1  ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}
		hHflunch<- function(x){ k <- subset(x, 	hflunch == 1  ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}
		hHengast<- function(x){ k <- subset(x, 	hengast == 1 ) 	
svyby( ~one, by = ~ gestfips, design =  k, 	FUN = svytotal)}
		Huc_yn	<- function(x){ k <- subset(x, 	uc_yn == 1) 	
svyby( ~one, by = ~ gestfips, design =  k, FUN = svytotal)}

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



