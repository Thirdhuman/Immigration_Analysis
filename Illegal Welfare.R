#Immigrants & Welfare

setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

library(survey)				# load survey package (analyzes complex design surveys)
library(reshape2)
library(plyr)

options( survey.replicates.mse = TRUE )
x=readRDS( file.path( path.expand( "~" ),"CPSASEC",paste(2017,"cps asec.rds") ) )

z = svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		combined.weights = T ,
		data=x)

z$mse <- TRUE

z <- update(z, i_stat = ifelse( prcitshp <= 3, 1,ifelse(prcitshp == 4, 2,ifelse(prcitshp == 5, 3, 1))))
z <- update(z, i2_stat = ifelse( prcitshp <= 4, 0,ifelse(prcitshp == 1, 1, 0)))
z <- update(z, uninsure = ifelse( ahiper == 0, 0, ifelse(ahiper >= 1, 1, 0)))
z <- update(z, ssi = ifelse( ssi_yn == 1, 1, ifelse(ssi_yn == 2, 0, 0)))
z <- update(z, pub_help = ifelse( paw_yn == 1, 1, ifelse(paw_yn == 2, 0, 0)))
z <- update(z, lunch_stat = ifelse( hflunch == 1, 1, ifelse(hflunch == 2, 0, 0)))
z <- update(z, avgsnapben = hfdval/h_numper)

svyby( ~one, by = ~ prcitshp, design =  z, 	FUN = svytotal)

svyby( ~one, by = ~ prcitshp, design =  z, 	FUN = svyquantile())

svyby( ~ptotval, by = ~ i_stat, design =  adults, FUN = svyquantile,  c( .5), ci=TRUE )

svyby( ~ptotval, by = ~ i_stat, design =  adults, FUN = svymean )


svyquantile( ~ptotval, design =  z, c(.5))


#### Populations ####

pov200 <- subset(z, povll < 8)
pov200_a <- subset(z, povll < 8 & a_age > 18 )
pov200_c <- subset(z, povll < 8 & a_age <= 18 )
adults <- subset(z, a_age > 18 )
children <- subset(z, a_age <= 18 )
adults_Im <- subset(z, a_age > 17 & i_stat == 3)

#### Figure 1 - Health Insurance - Medicaid/Uninsured  ####

medicaid <- function(x){	k <- update(x, caid = factor( caid )) 
svyby( ~caid, by = ~ i_stat, design =  k, 	FUN = svymean)}

kids2 <- update(z, govcare = ifelse(( kids1$ch_mc == 1	 & kids1$a_age < 15)	| ( kids1$pchip == 1) |
																										(kids1$caid ==1 & kids1$a_age >= 15) | (kids1$mcaid ==1 & kids1$a_age >= 15), 1,
																									 0))

medicaid(pov200_a)
medicaid(pov200)

adults_welfare <-subset(z, povll < 8 & a_age > 18 & hpaw_yn == 1)
adults_welfare <- update(adults_welfare, avg_ssi = (hpawval/h_numper))

adults_ssi <- subset(z, povll < 8 & a_age > 18 & hssi_yn == 1)
adults_ssi <- update(adults_ssi, avg_welfare = (hssival/h_numper))

svyby( ~avg_ssi, by = ~ i_stat, design =  adults_ssi, FUN = svymean)
svyby( ~avg_welfare, by = ~ i_stat, design =  adults_welfare, FUN = svymean)

uninsure <- function(x){	k <- update(x, uninsure = factor( uninsure )) 
svyby( ~uninsure, by = ~ i_stat, design =  k, 	FUN = svymean)}

uninsure(pov200_a)

#### Figure 2 ####

#### Figure 4 ####

h_foodspval <- function(x){	svyby( ~hfdval, by = ~ i_stat, design =  x, 	FUN = svytotal)}
# Household

h_foodstamp <- function(x){		k <- update(x, hfoodsp = factor( hfoodsp )) 
svyby( ~hfoodsp, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

h_foodstamp(pov200_a)

#### Figure 5 ####


#### Figure 6 ####

#### Figure 7 ####

ssi_y <- function(x){	k <- update(x, ssi = factor( ssi ))
svyby( ~ssi, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Person

pa_y <- function(x){	k <- update(x, pub_help = factor( pub_help ))
svyby( ~pub_help, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Person

ssi_y(pov200_a)
pa_y(pov200_a)

h_foodstamp(pov200_a)
h_foodstamp(pov200_c)

#### Lunch ####

hf_lunch <- function(x){	k <- update(x, lunch_stat = factor( lunch_stat ))
svyby( ~lunch_stat, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

hf_lunch(adults)
hf_lunch(pov200_a)
hf_lunch(parents)
hf_lunch(pov200_p)

#### Other ####
	
reci_pfare <- function(x){ k <- subset(x, paw_yn == 1 ) 	
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

val_pfare <- function(x){ svyby( ~paw_val, by = ~ i_stat, design =  x, 	FUN = svytotal)}
# Person / Household

h_lunch <- function(x){ k	<- subset(x, hflunch == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Household

ed_assistance <- function(x){k	<- subset(x, ed_yn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person

medicaid_number <- function(x){k	<- subset(x, caid == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person

medicaid_number(z)

pchip <- function(x){	k	<- subset(x, pchip == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person 

hssi_y <- function(x){	k	<- subset(x, hssi_yn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
#Household

ssi_y <- function(x){	k	<- subset(x, ssi_yn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person

ssi_value  <- function(x){svyby( ~ssi_val, by = ~ i_stat, design =  x, 	FUN = svytotal)}
# Person / Household

fed_dis <- function(x){
	k	<- subset(x, dis_sc1 == 3 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

wic_ben <- function(x){	k	<- subset(x, wicyn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

tanf <- function(x){	k	<- subset(x, paw_typ == 1)
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

tanf(adults)
tanf(pov200_a)


tanf_val <- function(x){	k	<- subset(x, paw_typ == 1)
svyby( ~paw_val, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

vet_ben <- function(x){	k	<- subset(x, vet_yn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person 

medicare <- function(x){	k	<- subset(x, care == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person

#### Percentage ####

h_wic_ben <- function(x){k <- update(x, hrwicyn = factor( hrwicyn )) 
svyby( ~hrwicyn, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

hvet_ben <- function(x){		k <- update(x, hvet_yn = factor( hvet_yn )) 
svyby( ~hvet_yn, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

hed_assistance <- function(x){		k <- update(x, hed_yn = factor( hed_yn )) 
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

h_public_housing <- function(x){		k <- update(x, hpublic = factor( hpublic )) 
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

energy_help <- function(x){			k <- update(x, hengast = factor( hengast )) 
svyby( ~hengast, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

pov200(energy_help)

h_wic_ben(pov200_a)


hssi_y(adults_Im)
ssi_value (z)

#### Extras - Programmatic Functions ####


dis_sc1

reci_pubfare(z)
foodstamp(z)
h_public_housing(z)

medicare <- subset(z, care == 1)
lunch <- subset(z, hflunch == 1)


svyby( ~paw_val , by = ~ i_stat, design =  reci_pubfare_a, 	FUN = svymean)
svyby( ~paw_val , by = ~ i_stat, design =  pov200_c, 	FUN = svymean)

svyby( ~one, by = ~ i_stat, design =  medicare, 	FUN = svytotal)
svyby( ~one, by = ~ i_stat, design =  lunch, 	FUN = svytotal)
svyby( ~one, by = ~ i_stat, design =  z, 	FUN = svytotal)

#### Welfare Reciepts ####

# covered by medicare - care
# child covered by medicare/medicaid - ch_mc
# child covered by state’s chip - pchip
# medicaid, anyone in hhld covered by hmcaid
# medicare, anyone in hhld covered by hmcare

# children receiving free or reduced price lunches - hflunch

# food stamps recipients - hfoodsp
# food stamps value - hfdval

# public assistance - household - hpaw_yn
# public assistance income - household - hpawval

# public housing project - hpublic

# wic program benefits, anyone - hrwicyn
# wic benefits received - wicyn

# social security income - hssval
# social security payments - hss_yn
# supplemental security benefits - hssi_yn
# supplemental security income - hssival
# supplemental security income amount received - ssi_val
# supplemental security income received - ssi_yn
# supplemental security income, child received - ssikidyn
# supplemental security income, reason 1 - resnssi1
# supplemental security income, reason 2 - resnssi2

# public assistance - person - paw_yn
# public assistance income - person - pawval

# educational assistance - ed_yn
# education assistance income - hedval
# educational assistance benefits - hed_yn

# energy assistance benefits - hengast 
# energy assistance income - hengval

# source of income, disability income, source 1 (Federal Government Disability = 3) - dis_sc1 
# disability income amount, source 1 - dis_val1
# disability income amount, source 2 - dis_val2
# supplemental unemployment benefits received - subuc
# survivor's benefits other than social security or veterans benefits - sur_yn

# financial assistance - fin_yn 
# other source of income - oi_off (1 = SS, 3 = AFDC (TANF?), )


#### Immigration / Demographics ####

# ratio of family income to low-income level - povll

# citizenship - prcitshp
# mother's country of birth - pemntvty
# demographics type of mother - pemomtyp
# country of birth - penatvty
# year of entry to the u.s. - peinusyr
# father's country of birth - pefntvty


