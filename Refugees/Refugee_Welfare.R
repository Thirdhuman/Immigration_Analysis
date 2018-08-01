library(survey)
library(reshape2)
library(plyr)

x <- readRDS( file.path( path.expand( "~" ) , "CPSASEC" , paste(2017,"cps asec.rds") ) )
codes<-read.csv('/Users/rorr/Desktop/Welfare_Policy/Data/Data_Explorations/Immigration/CPS_Appendix-H(CountryCodes).csv')
keeps <- c("Code", "Country")
codes<- codes[ , keeps, drop = FALSE]
# Generational Immmigration Dynamics
library(survey)				# load survey package (analyzes complex design surveys)
library(reshape2)
library(plyr)
library(openxlsx)
library(Hmisc)

codes=read.csv('/Users/rorr/Desktop/Welfare_Policy/Data/Data_Explorations/Immigration/CPS_Appendix-H(CountryCodes).csv')
refugee_admin=read.xlsx('/Users/rorr/Desktop/Welfare_Policy/Data/Data_Explorations/Immigration/Refugees/LPRRefugees-Final.xlsx')
str(refugee_admin)

#test=cleanme(refugee_admin)
#pop_68 = peinusyr >= 4
x=readRDS( file.path( path.expand( "~" ),"CPSASEC",paste(2017,"cps asec.rds") ) )
immigrant_dads<- subset(x, a_sex == 1 & a_age > 18 & prcitshp >= 4 , select = c(h_seq , a_lineno))
immigrant_moms<- subset(x, a_sex == 2 & a_age > 18 & prcitshp >= 4 , select = c(h_seq , a_lineno))
noncitizen_dads<- subset(x, a_sex == 1 & a_age > 18 & prcitshp >= 5 , select = c(h_seq , a_lineno))
noncitizen_moms<- subset(x, a_sex == 2 & a_age > 18 & prcitshp >= 5 , select = c(h_seq , a_lineno))
kids<- subset(x, a_age < 19, select = c(h_seq , a_lineno , prcitshp, pelndad, pelnmom))

immigrant_moms$i_mom_match <- 1
immigrant_dads$i_dad_match <- 1
noncitizen_moms$mom_match <- 1
noncitizen_dads$dad_match <- 1

before_nrow <- nrow( kids )
kids <- merge( kids , noncitizen_moms , all.x = T, by.x =c( "h_seq","pelnmom"),by.y = c( "h_seq" , "a_lineno" ) )
kids <- merge( kids , noncitizen_dads , all.x = T, by.x =c( "h_seq","pelndad"),by.y = c( "h_seq" , "a_lineno" ) )
stopifnot( nrow( kids ) == before_nrow )

kids$either_parent_noncitizen <- as.numeric( kids$mom_match %in% 1 | kids$dad_match %in% 1 )
before_nrow <- nrow( x )
x <- merge( x , kids , all.x = TRUE )
x[ is.na( x$either_parent_noncitizen ) , 'either_parent_noncitizen' ] <- 0
stopifnot( nrow( x ) == before_nrow )

kids<- subset(x, a_age < 19, select = c(h_seq , a_lineno , prcitshp, pelndad, pelnmom))
before_nrow <- nrow( kids )
kids <- merge( kids , immigrant_moms , all.x = T, by.x =c( "h_seq","pelnmom"),by.y = c( "h_seq" , "a_lineno" ) )
kids <- merge( kids , immigrant_dads , all.x = T, by.x =c( "h_seq","pelndad"),by.y = c( "h_seq" , "a_lineno" ) )
stopifnot( nrow( kids ) == before_nrow )

kids$either_parent_immigrant <- as.numeric( kids$i_mom_match %in% 1 | kids$i_dad_match %in% 1 )

before_nrow <- nrow( x )
x <- merge( x , kids , all.x = TRUE )
x[ is.na( x$either_parent_immigrant ) , 'either_parent_immigrant' ] <- 0
stopifnot( nrow( x ) == before_nrow )
table( x$either_parent_immigrant )

asec_2017=x[1:16,1:16]
colnames(x)

ASEC_2017 <-svrepdesign(weights = ~marsupwt, 
			repweights = "pwwgt[1-9]", 
			type = "Fay", 
			rho = (1-1/sqrt(4)),
			data = x ,
			combined.weights = T)

k=dput(x)
options( survey.replicates.mse = TRUE )
z$mse=TRUE
rm(kids,immigrant_dads,immigrant_moms,before_nrow,x)
# Define Immigrants (Inclusve vs. Exclusive) 
z=update(z, i_stat =  ifelse(prcitshp >= 4, 1,ifelse(prcitshp >= 1, 2, 0))) # Is Immigrant?
z=update(z, cit_stat = ifelse(prcitshp <= 4, 0,ifelse(prcitshp == 1, 1, 0))) # Is Citizen?
z=update(z, i_cit_stat = ifelse(prcitshp <= 3, 1,ifelse(prcitshp == 4, 2,ifelse(prcitshp == 5, 3, 0)))) # Is Immigrant and 

# Citizen?
z=update(z, i_parent = ifelse( (pefntvty >= 100 | pemntvty >= 100), 1, 0)) # Parent Immigrant?
z=update(z, i_gen_stat = ifelse(prcitshp >= 4, 1,ifelse(i_parent== 1, 2,ifelse(i_parent == 0, 3, 0)))) # Generational Stat
z=update(z, i_parent_f = factor( i_parent )) # Define Factor
z=update(z, i_stat_f = factor( i_stat )) # Define Factor
z=update(z,i_gen_kids= ifelse(prcitshp!=5&either_parent_immigrant==0,1,ifelse(prcitshp!=5&either_parent_immigrant==1,2,ifelse(prcitshp==5,3,0))))

z=update(z, i_gen_stat = ifelse(prcitshp >= 4, 1,ifelse(i_parent== 1, 2,ifelse(i_parent == 0, 3, 0)))) # Generational Stat

z=update(z,kid_stat_old= ifelse(prcitshp!=5&either_parent_noncitizen==0,1,
																																ifelse(prcitshp!=5&either_parent_noncitizen==1,2,
																																							ifelse(prcitshp==5,3,0))))


# Uninsured vs. Insured
z=update(z, uninsure = ifelse( ahiper == 0, 0, ifelse(ahiper >= 1, 1, 0)))
# Supplemental Security Income
z=update(z, ssi = ifelse( ssi_yn == 1, 1, ifelse(ssi_yn == 2, 0, 0)))
# Recieve TANF
z=update(z, pub_help = ifelse( paw_yn == 1, 1, ifelse(paw_yn == 2, 0, 0)))
# Average SNAP Benefit per person
z=update(z, avgsnapben = hfdval/h_numper)
# Recieve FoodStamp
z=update(z, snap_count = ifelse( hfoodsp == 1, 1, ifelse(hfoodsp == 2, 0, 0)))
# Supplemental Security Income
z=update(z, ssi_count = ifelse( ssi_yn == 1, 1, ifelse(ssi_yn == 2, 0, 0)))
# Social Security Count
z=update(z, ss_count = ifelse( ss_yn == 1, 1, ifelse(ss_yn == 0, 0, 0)))
# Diverse Government Care Stat
z=update(z,govcare=ifelse((z$ch_mc==1&z$a_age < 15)|( z$pchip==1)|(z$caid==1&z$a_age >=15)|(z$mcaid ==1&z$a_age>=15),1,0))

z=update(z, afghan_c1 = ifelse(
# Afghanistan: Cohort 1: 1991-1992
penatvty == ( 200 )& # # Afghanistan Code
peinusyr == ( 12 | 13 )  # Years(1990-1991 + 1992-1993)
# Years overcaptured: 1990 & 1993
# Percent overcaptured: 50% total years
 ,1, 0)) 
 
z=update(z,afghan_c2 = ifelse(
# Afghanistan: Cohort 2: 2001-2003
penatvty == ( 200   )& # # Afghanistan Code
peinusyr == ( 17| 18 )  # Years(2000-2001 + 2002-2003)
# Years overcaptured: 2000
# Percent overcaptured: 25% total years
,1,0))
 
z=update(z,albanian_c1 = ifelse(
# Albania
penatvty == ( 100 )& # # Albania Code
peinusyr == ( 12 )  # Years(1991)
# Years overcaptured: 1990
# Percent overcaptured: 50% total years
 ,1,0))
 
z=update(z, azerbaijani_c1 = ifelse(
# Azerbaijan
penatvty == ( 159 )& ## Azerbaijan Code
peinusyr == ( 18 | 19 ) #(2003-2004)
# Years overcaptured: 2003& 2004
# Percent overcaptured: 50% total years
,1,0))
 
z=update(z, bhutani_c1 = ifelse(
# Bhutan
penatvty == ( 203 )& # # Bhutan Code
peinusyr == ( 21 | 22 | 23 | 24 ) # Years (2008-2014)
# Years overcaptured: 2015& 2016& 2017
# Percent overcaptured: 33% total years
,1,0))
 
z=update(z, bosnian_c1 = ifelse(
# Bosnia
penatvty == ( 150 )& ## Bosnia and Herzegovina Code
peinusyr == ( 13 | 14 | 15 | 16 | 17 | 18 ) # Years (1993-2002)
# Years overcaptured: 1992& 2003
# Percent overcaptured: 18.18%
,1,0))
 
z=update(z,burmese_c1 = ifelse(
# Burma
penatvty == ( 205 )& ## Burma Code
peinusyr == ( 20 | 21 | 22 | 23 | 24 ) # Years (2007-2014)
# Years overcaptured: 2006 & 2015
# Percent overcaptured: 22.22%
,1,0))
 
z=update(z,cambodian_c1 = ifelse(
# Cambodia
penatvty == ( 206 )& ## Cambodia Code
peinusyr == ( 12  ) ## Years (1990))
# Years overcaptured: 1991
# Percent overcaptured: 50%
,1,0))
 
z=update(z,croatian_c1 = ifelse(
# Croatia
penatvty == ( 151 )& ## Croatia Code
peinusyr == (17  ) ## Years (2000-2001)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,czeschoslovakian_c1 = ifelse(
# Czechoslovakia
penatvty == ( 105 )& ## Czechoslovakia Code
peinusyr == ( 12  ) ## Years (1990-1991)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,eritrean_c1 = ifelse(
# Eritrea: Cohort 1: 2007
penatvty == ( 417 )& ## Eritrea Code
peinusyr == ( 20  ) ## Years (2006-2007)
# Years overcaptured: 2006
# Percent overcaptured: 50%
 ,1,0))
 
z=update(z,eritrean_c2 = ifelse(
# Eritrea: Cohort 2: 2009-2010
penatvty == ( 417 )& ## Eritrea Code
peinusyr == ( 21 | 22  ) ## Years  (2008-2011)
# Years overcaptured: 2008& 2011
# Percent overcaptured: 50%
,1,0))
 
z=update(z,eritrean_c3 = ifelse(
# Eritrea: Cohort 3: 2013-2014
penatvty == ( 417 )& ## Eritrea Code
peinusyr == ( 23 | 24  ) ## Years (2013-2014)
# Years overcaptured: 2012& 2015& 2016& 2017
# Percent overcaptured: 66.66%
,1,0))
 
z=update(z,estonian_c1 = ifelse(
# Estonia
penatvty == ( 155 )& ## Estonia Code
peinusyr == ( 19  ) ## Years (2004-2005)
# Years overcaptured: 2005
# Percent overcaptured: 50%
,1,0))
 
z=update(z,ethiopian_c1 = ifelse(
# Ethiopia
penatvty == (416 )& ## Ethiopia Code
peinusyr == ( 12 | 13  ) ## Years (1990-1993)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,iraqi_c1 = ifelse(
# Iraq: Cohort 1: 1992-1995
penatvty == ( 213 )& #Iraq Code
peinusyr == ( 13 | 14  ) ## Years (1992-1995)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,iraqi_c2 = ifelse(
#Iraq: Cohort 2: 2001
penatvty == (213)& #Iraq Code
peinusyr == ( 17  ) ## Years (2000-2001)
# Years overcaptured: 2000
# Percent overcaptured: 50%
,1,0))
 
z=update(z,iraqi_c3 = ifelse(
# Iraq: Cohort 3: 2008-2011
penatvty == (213)& #Iraq Code
peinusyr == ( 21 | 22 )
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,iraqi_c4 = ifelse(
# Iraq: Cohort 4: 2013-2014
penatvty == (213)& #Iraq Code
peinusyr == ( 23 | 24 )
# Years overcaptured: 2012& 2015& 2016& 2017
# Percent overcaptured: 33%
,1,0))
 
z=update(z,laotian_c1 = ifelse(
# Laos: Cohort 1: 1990-1997
penatvty == ( 223 )& #Laos Code
peinusyr == ( 12 | 13 | 14 | 15 )
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,laotian_c2 = ifelse(
# Laos: Cohort 2: 2004-2005
penatvty == (223)& #Laos Code
peinusyr == (19)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,liberian_c1 = ifelse(
# Liberia: Cohort 1: 1993
penatvty == (429)& #Liberia Code
peinusyr == (13)
# Years overcaptured: 1992
# Percent overcaptured: 50%
,1,0))
 
z=update(z,liberian_c2 = ifelse(
# Liberia: Cohort 2: 1999
penatvty == (429)& #Liberia Code
peinusyr == (16)
# Years overcaptured: 1998
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,liberian_c3 = ifelse(
# Liberia: Cohort 3: 2001
penatvty == (429)& #Liberia Code
peinusyr == (17)
# Years overcaptured: 2000
# Percent overcaptured: 50%
,1,0))
 
z=update(z,liberian_c4 = ifelse(
# Liberia: Cohort 4: 2004-2006
penatvty == (429)& #Liberia Code
peinusyr == ( 19 | 20 )
# Years overcaptured: 2007
# Percent overcaptured: 25%
 ,1,0))
 
 
z=update(z,libyan_c1 = ifelse(
# Libya
penatvty == (430)& #Libya Code
peinusyr == (12)
# Years overcaptured: 1990
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,moldovan_c1 = ifelse(
# Moldova: Cohort 1: 2001
penatvty == (162)& #Moldova Code
peinusyr == (17)
# Years overcaptured: 2000
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,moldovan_c2 = ifelse(
# Moldova: Cohort 2: 2004
penatvty == (162)& #Moldova Code
peinusyr == (19)
# Years overcaptured: 2005
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,serbian_c1 = ifelse(
# Serbia: Cohort 1: 1999
penatvty == (154)& #Elsewhere Code
peinusyr == (16)
# Years overcaptured: 1998
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,serbian_c2 = ifelse(
# Serbia: Cohort 2: 2002-2003
penatvty == (154)& #Elsewhere Code
peinusyr == (18)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0)) 
 
z=update(z,sierraleonean_c1 = ifelse(
# Sierra Leone: Cohort 1: 2001
penatvty == (447)& #Sierra Leone Code
peinusyr == (17)
# Years overcaptured: 2000
# Percent overcaptured: 50%
 ,1,0))
 
z=update(z,sierraleonean_c2 = ifelse(
# Sierra Leone: Cohort 2: 2003-2004
penatvty == (447)& #Sierra Leone Code
peinusyr == ( 18 | 19 )
# Years overcaptured: 2002& 2005
# Percent overcaptured: 50%
,1,0))
 
z=update(z,somalian_c1 = ifelse(
# Somalia: Cohort 1: 1992-1998
penatvty == (448)& #Somalia Code
peinusyr == (13 | 14 | 15 | 16 )
# Years overcaptured: 1999
# Percent overcaptured: 14.28%
,1,0)) 
 
z=update(z,somalian_c2 = ifelse(
# Somalia: Cohort 2: 2000-2001
penatvty == (448)& #Somalia Code
peinusyr == (17)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,somalian_c3 = ifelse(
# Somalia: Cohort 3: 2003-2014
penatvty == (448)& #Somalia Code
peinusyr == ( 18 | 19 | 20 | 21 | 22 | 23 | 24 )
# Years overcaptured: 2002& 2015& 2016& 2017
# Percent overcaptured: 25%
,1,0))
 
z=update(z,sudanese_c1 = ifelse(
# Sudan: Cohort 1: 1994-1995
penatvty == (451)& #Sudan Code
peinusyr == (14)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,sudanese_c2 = ifelse(
# Sudan: Cohort 2: 1998-2006
penatvty == (451)& #Sudan Code
peinusyr == ( 16 | 17 | 18 | 19 | 20 )
# Years overcaptured: 2007
# Percent overcaptured: 10%
,1,0)) 
 
z=update(z,sudanese_c3 = ifelse(
# Sudan: Cohort 3: 2012-2014
penatvty == (451)& #Sudan Code
peinusyr == ( 23 | 24 )
# Years overcaptured: 2015& 2016& 2017
# Percent overcaptured: 50%
,1,0))
 
z=update(z,togoan_c1 = ifelse(
# Togo: Cohort 1: 1995
penatvty == (454)& #Togo Code
peinusyr == (14)
# Years overcaptured: 1994
# Percent overcaptured: 50%
,1,0))
 
z=update(z,togoan_c2 = ifelse(
# Togo: Cohort 2: 2000
penatvty == (454)& #Togo Code
peinusyr == (17)
# Years overcaptured: 2001
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,vietnamese_c1 = ifelse(
# Vietnam
penatvty == (247)& #Vietnam Code
peinusyr == (14)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,congolese_c1 = ifelse(
# Democratic Republic of Congo: Cohort 1: 1993-1994
penatvty == (412)& #Democratic Republic of Congo Code
peinusyr == ( 13 | 14 )
# Years overcaptured: 1992& 1995
# Percent overcaptured: 50%
,1,0))
 
z=update(z,congolese_c2 = ifelse(
# Democratic Republic of Congo: Cohort 2: 2000
penatvty == (412)& #Democratic Republic of Congo Code
peinusyr == (17)
# Years overcaptured: 2001
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,congolese_c3 = ifelse(
# Democratic Republic of Congo: Cohort 3: 2004-2014
penatvty == (412)& #Democratic Republic of Congo Code
peinusyr == ( 19 | 20 | 21 | 22 | 23 | 24 )
# Years overcaptured: 2015& 2016& 2017
# Percent overcaptured: 21%
,1,0))

#refugee_naive
z=update(z,refugee_naive = ifelse(vietnamese_c1==1|
																																			togoan_c2==1|togoan_c1==1|
																																			sudanese_c3==1|sudanese_c2==1|sudanese_c1==1|
																																			somalian_c3==1|somalian_c2==1|somalian_c1==1|
																																			sierraleonean_c2==1|sierraleonean_c1==1|
																																			serbian_c2==1|serbian_c1==1|
																																			moldovan_c2==1|moldovan_c1==1|
																																			libyan_c1==1|
																																			liberian_c4==1|liberian_c3==1|liberian_c2==1|liberian_c1==1|
																																			laotian_c2==1|laotian_c1==1|
																																			iraqi_c4==1|iraqi_c3==1|iraqi_c2==1|iraqi_c1==1|
																																			ethiopian_c1==1|
																																			estonian_c1==1|
																																			eritrean_c3==1|eritrean_c2==1|eritrean_c1==1|
																																			czeschoslovakian_c1==1|
																																			croatian_c1==1|
																																			cambodian_c1==1|
																																			burmese_c1==1|
																																			bosnian_c1==1|
																																			bhutani_c1==1|
																																			azerbaijani_c1==1|
																																			albanian_c1==1|
																																			afghan_c2==1|afghan_c1==1|
																																			congolese_c3==1|congolese_c2==1|congolese_c1==1,1,0))


# currently overrties (existing i_stat)
z=update(z, i_stat = ifelse(refugee_naive==1, 1, ifelse(prcitshp <= 3, 2,ifelse(prcitshp == 4, 3,ifelse(prcitshp == 5, 4, 0))))) # Is Immigrant Refugee 


#### Populations ####
# Non Poverty

#z=subset(z, refugee_naive==1)

adults=subset(z, a_age > 18 )
child=subset(z, a_age <= 18 )
elderly=subset(z, a_age >= 65)
adults_Im=subset(z, a_age > 17 & i_stat == 3)
# Poverty
pov200=subset(z, povll < 8)
pov200_a=subset(z, povll < 8&a_age > 18 )
pov200_c=subset(z, povll < 8&a_age <= 17 )
pov200_e=subset(z, povll < 8&a_age >= 65)

svyby( ~one, by = ~ i_cit_stat, design =adults, FUN=svytotal)
svyby( ~one, by = ~ kid_stat_old, design =child, FUN=svytotal)

## Population Counts ##
# svyby( ~one, by = ~ prcitshp, design =  z, 	FUN = svytotal)
# svyby( ~ptotval, by = ~ i_stat, design =  adults, FUN = svyquantile,  c( .5), ci=TRUE )
# svyby( ~i_parent_f, by = ~ a_age, design =  k, FUN = svymean )}

###### WELFARE PROGRAM CODES ######

#### Personal-Child ####
# supplemental security income, child received - ssikidyn
# child covered by medicare/medicaid - ch_mc
# child covered by state’s chip - pchip

#### Personal ####
# Social Security Recieve: ss_yn 
# Social Security Value: ss_val
# Medicare, covered by - care
# Medicaid, covered by - caid
# WIC benefits received - wicyn
# supplemental security income received (SSI) - ssi_yn
# supplemental security income value (SSI) - ssi_val
# public assistance - person - paw_yn 
# educational assistance - ed_yn

#### Household ####
# food stamps recipients - hfoodsp
# supplemental security benefits - hssi_yn
# public housing project - hpublic
# educational assistance benefits - hed_yn
# energy assistance benefits - hengast 
# children receiving free or reduced price lunches - hflunch


#### Social Security Function  ####
SS=function(x){	k=(x) 
#Generations
total_population=svyby( ~one, by = ~ i_gen_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="one"]="total_population"
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
age_eligible_population=svyby( ~one, by = ~ i_gen_stat, design =k, FUN=svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
count=svyby( ~ss_count, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
value=svyby( ~ss_val, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
generation	=	merge(count, value, by='i_gen_stat')
generation	=	merge(generation, age_eligible_population, by='i_gen_stat')
generation	=	merge(generation, total_population, by='i_gen_stat')
generation$i_gen_stat=ifelse(generation$i_gen_stat==1,"Gen_1",ifelse(generation$i_gen_stat==2,"Gen_2",ifelse(generation$i_gen_stat==3,"Gen_3+", "error")))
colnames(generation)[colnames(generation)=="i_gen_stat"]="group"
generation$calc_type=1
#Immigrant
total_population=svyby( ~one, by = ~ i_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="one"]="total_population"
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
age_eligible_population=svyby( ~one, by = ~ i_stat, design =k, FUN=svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
count=svyby( ~ss_count, by = ~ i_stat, design =  k, 	FUN = svytotal)
value=svyby( ~ss_val, by = ~ i_stat, design =  k, 	FUN = svytotal)
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
immigrant	=	merge(count, value, by='i_stat')
immigrant	=	merge(immigrant, age_eligible_population, by='i_stat')
immigrant	=	merge(immigrant, total_population, by='i_stat')
#immigrant$i_stat=ifelse(immigrant$i_stat == 1,"Immigrant",ifelse(immigrant$i_stat == 2, "Non-immigrant", "error"))
colnames(immigrant)[colnames(immigrant)=="i_stat"]="group"
immigrant$calc_type=2
#Citizenship or Immigrant
total_population=svyby( ~one, by = ~ i_cit_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
colnames(total_population)[colnames(total_population)=="one"]="total_population"
age_eligible_population=svyby( ~one, by = ~ i_cit_stat, design =k, FUN=svytotal)
count=svyby( ~ss_count, by = ~ i_cit_stat, design =  k, 	FUN = svytotal)
value=svyby( ~ss_val, by = ~ i_cit_stat, design =  k, 	FUN = svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
nativity	=	merge(count, value, by='i_cit_stat')
nativity	=	merge(nativity, age_eligible_population, by='i_cit_stat')
nativity	=	merge(nativity, total_population, by='i_cit_stat')
nativity$i_cit_stat=ifelse(nativity$i_cit_stat==1,"Natives", ifelse(nativity$i_cit_stat==2, "Naturalized", "Noncitizens"))
colnames(nativity)[colnames(nativity)=="i_cit_stat"]="group"
nativity$calc_type=3
# Merge and calculate
program_name	=	rbind(generation, nativity, immigrant)
program_name <- within(program_name, per_beneficiary <- ss_val/ss_count)
program_name <- within(program_name, per_age_eligible_population <- ss_val/age_eligible_population)
program_name <- within(program_name, per_total_population <- ss_val/total_population)
program_name$se_count=NULL
program_name$se_value=NULL
program_name$se_age_eligible_population=NULL
program_name$se_total_population=NULL
colnames(program_name)[colnames(program_name)=="ss_val"]="program_value"
colnames(program_name)[colnames(program_name)=="ss_count"]="program_count"
return(program_name)}
A_Social_Security=(SS(elderly))
A_Social_Security$program_name=('Social Security')
A_Social_Security$poverty_200=('no')
P200_A_Social_Security=(SS(pov200_e))
P200_A_Social_Security$program_name=('Social Security')
P200_A_Social_Security$poverty_200=('yes')


#### Public Assistance Welfare Function  ####
CashWelfare=function(x){	k=(x) 
#Generations
total_population=svyby( ~one, by = ~ i_gen_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="one"]="total_population"
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
age_eligible_population=svyby( ~one, by = ~ i_gen_stat, design =k, FUN=svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
count=svyby( ~pub_help, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
value=svyby( ~paw_val, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
generation	=	merge(count, value, by='i_gen_stat')
generation	=	merge(generation, age_eligible_population, by='i_gen_stat')
generation	=	merge(generation, total_population, by='i_gen_stat')
generation$i_gen_stat=ifelse(generation$i_gen_stat==1,"Gen_1",ifelse(generation$i_gen_stat==2,"Gen_2",ifelse(generation$i_gen_stat==3,"Gen_3+", "error")))
colnames(generation)[colnames(generation)=="i_gen_stat"]="group"
generation$calc_type=1
#Immigrant
total_population=svyby( ~one, by = ~ i_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="one"]="total_population"
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
age_eligible_population=svyby( ~one, by = ~ i_stat, design =k, FUN=svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
count=svyby( ~pub_help, by = ~ i_stat, design =  k, 	FUN = svytotal)
value=svyby( ~paw_val, by = ~ i_stat, design =  k, 	FUN = svytotal)
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
immigrant	=	merge(count, value, by='i_stat')
immigrant	=	merge(immigrant, age_eligible_population, by='i_stat')
immigrant	=	merge(immigrant, total_population, by='i_stat')
#immigrant$i_stat=ifelse(immigrant$i_stat == 1,"Immigrant",ifelse(immigrant$i_stat == 2, "Non-immigrant", "error"))
colnames(immigrant)[colnames(immigrant)=="i_stat"]="group"
immigrant$calc_type=2
#Citizenship or Immigrant
total_population=svyby( ~one, by = ~ i_cit_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
colnames(total_population)[colnames(total_population)=="one"]="total_population"
age_eligible_population=svyby( ~one, by = ~ i_cit_stat, design =k, FUN=svytotal)
count=svyby( ~pub_help, by = ~ i_cit_stat, design =  k, 	FUN = svytotal)
value=svyby( ~paw_val, by = ~ i_cit_stat, design =  k, 	FUN = svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
nativity	=	merge(count, value, by='i_cit_stat')
nativity	=	merge(nativity, age_eligible_population, by='i_cit_stat')
nativity	=	merge(nativity, total_population, by='i_cit_stat')
nativity$i_cit_stat=ifelse(nativity$i_cit_stat==1,"Natives", ifelse(nativity$i_cit_stat==2, "Naturalized", "Noncitizens"))
colnames(nativity)[colnames(nativity)=="i_cit_stat"]="group"
nativity$calc_type=3
# Merge and calculate
program_name	=	rbind(generation, nativity, immigrant)
program_name <- within(program_name, per_beneficiary <- paw_val/pub_help)
program_name <- within(program_name, per_age_eligible_population <- paw_val/age_eligible_population)
program_name <- within(program_name, per_total_population <- paw_val/total_population)
program_name$se_count=NULL
program_name$se_value=NULL
program_name$se_age_eligible_population=NULL
program_name$se_total_population=NULL
colnames(program_name)[colnames(program_name)=="paw_val"]="program_value"
colnames(program_name)[colnames(program_name)=="pub_help"]="program_count"
return(program_name)}
CashWelfare_A=(CashWelfare(adults))
CashWelfare_A$program_name=('TANF')
CashWelfare_A$poverty_200=('no')
P200_CashWelfare_A=(CashWelfare(pov200_a))
P200_CashWelfare_A$program_name=('TANF')
P200_CashWelfare_A$poverty_200=('yes')


#### Supplemental Security Income Function ####
SSI=function(x){	k=(x) 
#Generations
total_population=svyby( ~one, by = ~ i_gen_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="one"]="total_population"
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
age_eligible_population=svyby( ~one, by = ~ i_gen_stat, design =k, FUN=svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
count=svyby( ~ssi_count, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
value=svyby( ~ssi_val, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
generation	=	merge(count, value, by='i_gen_stat')
generation	=	merge(generation, age_eligible_population, by='i_gen_stat')
generation	=	merge(generation, total_population, by='i_gen_stat')
generation$i_gen_stat=ifelse(generation$i_gen_stat==1,"Gen_1",ifelse(generation$i_gen_stat==2,"Gen_2",ifelse(generation$i_gen_stat==3,"Gen_3+", "error")))
colnames(generation)[colnames(generation)=="i_gen_stat"]="group"
generation$calc_type=1
#Immigrant
total_population=svyby( ~one, by = ~ i_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="one"]="total_population"
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
age_eligible_population=svyby( ~one, by = ~ i_stat, design =k, FUN=svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
count=svyby( ~ssi_count, by = ~ i_stat, design =  k, 	FUN = svytotal)
value=svyby( ~ssi_val, by = ~ i_stat, design =  k, 	FUN = svytotal)
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
immigrant	=	merge(count, value, by='i_stat')
immigrant	=	merge(immigrant, age_eligible_population, by='i_stat')
immigrant	=	merge(immigrant, total_population, by='i_stat')
#immigrant$i_stat=ifelse(immigrant$i_stat == 1,"Immigrant",ifelse(immigrant$i_stat == 2, "Non-immigrant", "error"))
colnames(immigrant)[colnames(immigrant)=="i_stat"]="group"
immigrant$calc_type=2
#Citizenship or Immigrant
total_population=svyby( ~one, by = ~ i_cit_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
colnames(total_population)[colnames(total_population)=="one"]="total_population"
age_eligible_population=svyby( ~one, by = ~ i_cit_stat, design =k, FUN=svytotal)
count=svyby( ~ssi_count, by = ~ i_cit_stat, design =  k, 	FUN = svytotal)
value=svyby( ~ssi_val, by = ~ i_cit_stat, design =  k, 	FUN = svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
nativity	=	merge(count, value, by='i_cit_stat')
nativity	=	merge(nativity, age_eligible_population, by='i_cit_stat')
nativity	=	merge(nativity, total_population, by='i_cit_stat')
nativity$i_cit_stat=ifelse(nativity$i_cit_stat==1,"Natives", ifelse(nativity$i_cit_stat==2, "Naturalized", "Noncitizens"))
colnames(nativity)[colnames(nativity)=="i_cit_stat"]="group"
nativity$calc_type=3
# Merge and calculate
program_name	=	rbind(generation, nativity, immigrant)
program_name <- within(program_name, per_beneficiary <- ssi_val/ssi_count)
program_name <- within(program_name, per_age_eligible_population <- ssi_val/age_eligible_population)
program_name <- within(program_name, per_total_population <- ssi_val/total_population)
program_name$se_count=NULL
program_name$se_value=NULL
program_name$se_age_eligible_population=NULL
program_name$se_total_population=NULL
colnames(program_name)[colnames(program_name)=="ssi_val"]="program_value"
colnames(program_name)[colnames(program_name)=="ssi_count"]="program_count"
return(program_name)}
SSI_A=(SSI(adults))
SSI_A$program_name=('SSI')
SSI_A$poverty_200=('no')
P200_SSI_A=(SSI(pov200_a))
P200_SSI_A$program_name=('SSI')
P200_SSI_A$poverty_200=('yes')

#### Supplemental Security Income Function ####
SNAP_Adult=function(x){	k=(x) 
#Generations
total_population=svyby( ~one, by = ~ i_gen_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="one"]="total_population"
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
age_eligible_population=svyby( ~one, by = ~ i_gen_stat, design =k, FUN=svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
count=svyby( ~snap_count, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
value=svyby( ~avgsnapben, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
generation	=	merge(count, value, by='i_gen_stat')
generation	=	merge(generation, age_eligible_population, by='i_gen_stat')
generation	=	merge(generation, total_population, by='i_gen_stat')
generation$i_gen_stat=ifelse(generation$i_gen_stat==1,"Gen_1",ifelse(generation$i_gen_stat==2,"Gen_2",ifelse(generation$i_gen_stat==3,"Gen_3+", "error")))
colnames(generation)[colnames(generation)=="i_gen_stat"]="group"
generation$calc_type=1
#Immigrant
total_population=svyby( ~one, by = ~ i_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="one"]="total_population"
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
age_eligible_population=svyby( ~one, by = ~ i_stat, design =k, FUN=svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
count=svyby( ~snap_count, by = ~ i_stat, design =  k, 	FUN = svytotal)
value=svyby( ~avgsnapben, by = ~ i_stat, design =  k, 	FUN = svytotal)
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
immigrant	=	merge(count, value, by='i_stat')
immigrant	=	merge(immigrant, age_eligible_population, by='i_stat')
immigrant	=	merge(immigrant, total_population, by='i_stat')
#immigrant$i_stat=ifelse(immigrant$i_stat == 1,"Immigrant",ifelse(immigrant$i_stat == 2, "Non-immigrant", "error"))
colnames(immigrant)[colnames(immigrant)=="i_stat"]="group"
immigrant$calc_type=2
#Citizenship or Immigrant
total_population=svyby( ~one, by = ~ i_cit_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
colnames(total_population)[colnames(total_population)=="one"]="total_population"
age_eligible_population=svyby( ~one, by = ~ i_cit_stat, design =k, FUN=svytotal)
count=svyby( ~snap_count, by = ~ i_cit_stat, design =  k, 	FUN = svytotal)
value=svyby( ~avgsnapben, by = ~ i_cit_stat, design =  k, 	FUN = svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
nativity	=	merge(count, value, by='i_cit_stat')
nativity	=	merge(nativity, age_eligible_population, by='i_cit_stat')
nativity	=	merge(nativity, total_population, by='i_cit_stat')
nativity$i_cit_stat=ifelse(nativity$i_cit_stat==1,"Natives", ifelse(nativity$i_cit_stat==2, "Naturalized", "Noncitizens"))
colnames(nativity)[colnames(nativity)=="i_cit_stat"]="group"
nativity$calc_type=3
# Merge and calculate
program_name	=	rbind(generation, nativity, immigrant)
program_name <- within(program_name, per_beneficiary <- avgsnapben/snap_count)
program_name <- within(program_name, per_age_eligible_population <- avgsnapben/age_eligible_population)
program_name <- within(program_name, per_total_population <- avgsnapben/total_population)
program_name$se_count=NULL
program_name$se_value=NULL
program_name$se_age_eligible_population=NULL
program_name$se_total_population=NULL
colnames(program_name)[colnames(program_name)=="avgsnapben"]="program_value"
colnames(program_name)[colnames(program_name)=="snap_count"]="program_count"
return(program_name)}
A_SNAP=(SNAP_Adult(adults))
A_SNAP$program_name=('SNAP')
A_SNAP$poverty_200=('no')
P200_SNAP=(SNAP_Adult(pov200_a))
P200_SNAP$program_name=('SNAP')
P200_SNAP$poverty_200=('yes')


C_SSI=function(x){	k=(x) 
total_population=svyby( ~one, by = ~ i_gen_kids, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
colnames(total_population)[colnames(total_population)=="one"]="total_population"
age_eligible_population=svyby( ~one, by = ~ i_gen_kids, design =k, FUN=svytotal)
count=svyby( ~ssi_count, by = ~ i_gen_kids, design =  k, 	FUN = svytotal)
value=svyby( ~ssi_val, by = ~ i_gen_kids, design =  k, 	FUN = svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
c_generations	=	merge(count, value, by='i_gen_kids')
c_generations	=	merge(c_generations, age_eligible_population, by='i_gen_kids')
c_generations	=	merge(c_generations, total_population, by='i_gen_kids')
c_generations$i_gen_kids=ifelse(c_generations$i_gen_kids==1,"ChildGen_3+", ifelse(c_generations$i_gen_kids==2, "ChildGen_2", "ChildGen_1"))
colnames(c_generations)[colnames(c_generations)=="i_gen_kids"]="group"
c_generations$calc_type=4
program_name	=	c_generations
program_name <- within(program_name, per_beneficiary <- ssi_val/ssi_count)
program_name <- within(program_name, per_age_eligible_population <- ssi_val/age_eligible_population)
program_name <- within(program_name, per_total_population <- ssi_val/total_population)
program_name$se_count=NULL
program_name$se_value=NULL
program_name$se_age_eligible_population=NULL
program_name$se_total_population=NULL
colnames(program_name)[colnames(program_name)=="ssi_val"]="program_value"
colnames(program_name)[colnames(program_name)=="ssi_count"]="program_count"
return(program_name)}
SSI_C=(C_SSI(child))
SSI_C$program_name=('SSI')
SSI_C$poverty_200=('no')
P200_SSI_C=(C_SSI(pov200_c))
P200_SSI_C$program_name=('SSI')
P200_SSI_C$poverty_200=('yes')

Child_CashWelfare=function(x){	k=(x) 
total_population=svyby( ~one, by = ~ i_gen_kids, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
colnames(total_population)[colnames(total_population)=="one"]="total_population"
age_eligible_population=svyby( ~one, by = ~ i_gen_kids, design =k, FUN=svytotal)
count=svyby( ~pub_help, by = ~ i_gen_kids, design =  k, 	FUN = svytotal)
value=svyby( ~paw_val, by = ~ i_gen_kids, design =  k, 	FUN = svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
c_generations	=	merge(count, value, by='i_gen_kids')
c_generations	=	merge(c_generations, age_eligible_population, by='i_gen_kids')
c_generations	=	merge(c_generations, total_population, by='i_gen_kids')
c_generations$i_gen_kids=ifelse(c_generations$i_gen_kids==1,"ChildGen_3+", ifelse(c_generations$i_gen_kids==2, "ChildGen_2", "ChildGen_1"))
colnames(c_generations)[colnames(c_generations)=="i_gen_kids"]="group"
c_generations$calc_type=4
program_name	=	c_generations
program_name <- within(program_name, per_beneficiary <- paw_val/pub_help)
program_name <- within(program_name, per_age_eligible_population <- paw_val/age_eligible_population)
program_name <- within(program_name, per_total_population <- paw_val/total_population)
program_name$se_count=NULL
program_name$se_value=NULL
program_name$se_age_eligible_population=NULL
program_name$se_total_population=NULL
colnames(program_name)[colnames(program_name)=="paw_val"]="program_value"
colnames(program_name)[colnames(program_name)=="pub_help"]="program_count"
return(program_name)}
C_CashW=(Child_CashWelfare(child))
C_CashW$program_name=('TANF')
C_CashW$poverty_200=('no')
P200_C_CashW=(Child_CashWelfare(pov200_c))
P200_C_CashW$program_name=('TANF')
P200_C_CashW$poverty_200=('yes')


# 2016 

Child_SNAP=function(x){	k=(x) 
total_population=svyby( ~one, by = ~ i_gen_kids, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
colnames(total_population)[colnames(total_population)=="one"]="total_population"
age_eligible_population=svyby( ~one, by = ~ i_gen_kids, design =k, FUN=svytotal)
count=svyby( ~snap_count, by = ~ i_gen_kids, design =  k, 	FUN = svytotal)
value=svyby( ~avgsnapben, by = ~ i_gen_kids, design =  k, 	FUN = svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
c_generations	=	merge(count, value, by='i_gen_kids')
c_generations	=	merge(c_generations, age_eligible_population, by='i_gen_kids')
c_generations	=	merge(c_generations, total_population, by='i_gen_kids')
c_generations$i_gen_kids=ifelse(c_generations$i_gen_kids==1,"ChildGen_3+", ifelse(c_generations$i_gen_kids==2, "ChildGen_2", "ChildGen_1"))
colnames(c_generations)[colnames(c_generations)=="i_gen_kids"]="group"
c_generations$calc_type=4
program_name	=	c_generations
program_name <- within(program_name, per_beneficiary <- avgsnapben/snap_count)
program_name <- within(program_name, per_age_eligible_population <- avgsnapben/age_eligible_population)
program_name <- within(program_name, per_total_population <- avgsnapben/total_population)
program_name$se_count=NULL
program_name$se_value=NULL
program_name$se_age_eligible_population=NULL
program_name$se_total_population=NULL
colnames(program_name)[colnames(program_name)=="avgsnapben"]="program_value"
colnames(program_name)[colnames(program_name)=="snap_count"]="program_count"
return(program_name)}
C_SNAP=(Child_SNAP(child))
C_SNAP$program_name=('SNAP')
C_SNAP$poverty_200=('no')
P200_C_SNAP=(Child_SNAP(pov200_c))
P200_C_SNAP$program_name=('SNAP')
P200_C_SNAP$poverty_200=('yes')



unique(names(c(P200_SSI_A,SSI_A,P200_CashWelfare_A,CashWelfare_A, A_Social_Security, P200_A_Social_Security, P200_C_SNAP, C_SNAP, SSI_C, P200_SSI_C, A_SNAP, P200_SNAP,
																				C_CashW, P200_C_CashW)))
unique(names(c(P200_SSI_A,SSI_A,P200_CashWelfare_A,CashWelfare_A, A_Social_Security, P200_A_Social_Security)))


final_dataset=rbind(P200_SSI_A,SSI_A,P200_CashWelfare_A,CashWelfare_A, A_Social_Security, P200_A_Social_Security, P200_C_SNAP, C_SNAP, SSI_C, P200_SSI_C, A_SNAP, P200_SNAP,C_CashW, P200_C_CashW)

unique(final_dataset$group)
write.csv(final_dataset, 'final_dataset_test.csv')

# Graphing
my_theme <- function(){theme_light() +theme(text = element_text(family = "Open Sans"), legend.text.align = 0,
		plot.title = element_text(size = 12, color = "gray30"),   # Set up the title style
		plot.subtitle = element_text(size = 10, color = "black"), # Set up the subtitle style
		plot.margin = unit(c(.05,.05,.05,.05), "cm"),                 # Add white space at the top and left
		panel.grid = element_blank(),#panel.border = element_blank(),
		axis.title = element_blank(),axis.ticks = element_blank(),#axis.text.x = element_blank(),
		axis.text.y = element_text(size = 9, color = "gray10"))}

fig_df= subset(final_dataset, poverty_200=='no' & (calc_type==1 | calc_type==4))
colnames(fig_df)

fig2_df=fig_df

fig_df$Generation=ifelse(fig_df$group=='Gen_3+'|fig_df$group=='ChildGen_3+','3+',
											ifelse(fig_df$group=='Gen_2'|fig_df$group=='ChildGen_2','2',
											ifelse(fig_df$group=='Gen_1'|fig_df$group=='ChildGen_1','1','0')))

fig2_df$Generation=ifelse(fig2_df$group=='Gen_3+'|fig2_df$group=='ChildGen_3+','3+',
											ifelse(fig2_df$group=='Gen_2'|fig2_df$group=='ChildGen_2','1-2',
											ifelse(fig2_df$group=='Gen_1'|fig2_df$group=='ChildGen_1','1-2','0')))

fig1=aggregate(data=fig_df, age_eligible_population ~ Generation+ program_name, FUN = sum)
fig2=aggregate(data=fig_df,cbind(age_eligible_population, program_value) ~ Generation+program_name,FUN=sum)
fig2 <- within(fig2, per_total_population<-program_value/age_eligible_population)

alt_fig1=aggregate(data=fig2_df,age_eligible_population~Generation+program_name,FUN=sum)
alt_fig2=aggregate(data=fig2_df,cbind(age_eligible_population, program_value)~Generation+program_name,FUN=sum)
alt_fig2<-within(alt_fig2, per_total_population<-program_value/age_eligible_population)

write.csv(alt_fig2, 'alt_fig2_dataset.csv')
write.csv(fig2, 'fig2_dataset.csv')


library(ggplot2)
plot1=ggplot(alt_fig2, aes(program_name, per_total_population, fill=Generation)) +my_theme() + geom_bar(aes(fill = Generation), position = "dodge", stat="identity")+ geom_text(aes(label=round(per_total_population,2)),position=position_dodge(width=0.9), vjust=-0.25)

plot2=ggplot(fig2, aes(program_name, per_total_population, fill=Generation)) +my_theme()+geom_bar(aes(fill=Generation), position = "dodge", stat="identity")+ geom_text(aes(label=round(per_total_population,2)),position=position_dodge(width=0.9), vjust=-0.25)
																																																																										ggsave(filename="Figure.png", plot=plot1)
ggsave(filename="Figure_Alt.png", plot=plot2)																																																								

figure_2 + geom_bar(aes(fill = gen), position = "dodge", stat="identity")

