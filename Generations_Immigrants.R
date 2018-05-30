# Generational Immmigration Dynamics
library(survey)				# load survey package (analyzes complex design surveys)
library(reshape2)
library(plyr)
#pop_68 = peinusyr >= 4
x=readRDS( file.path( path.expand( "~" ),"CPSASEC",paste(2017,"cps asec.rds") ) )
immigrant_dads<- subset(x, a_sex == 1 & a_age > 18 & prcitshp >= 4 , select = c(h_seq , a_lineno))
immigrant_moms<- subset(x, a_sex == 2 & a_age > 18 & prcitshp >= 4 , select = c(h_seq , a_lineno))
kids<- subset(x, a_age < 19, select = c(h_seq , a_lineno , prcitshp, pelndad, pelnmom))

immigrant_moms$mom_match <- 1
immigrant_dads$dad_match <- 1

before_nrow <- nrow( kids )
kids <- merge( kids , immigrant_moms , all.x = T, by.x =c( "h_seq","pelnmom"),by.y = c( "h_seq" , "a_lineno" ) )
kids <- merge( kids , immigrant_dads , all.x = T, by.x =c( "h_seq","pelndad"),by.y = c( "h_seq" , "a_lineno" ) )
stopifnot( nrow( kids ) == before_nrow )

kids$either_parent_immigrant <- as.numeric( kids$mom_match %in% 1 | kids$dad_match %in% 1 )

before_nrow <- nrow( x )
x <- merge( x , kids , all.x = TRUE )
x[ is.na( x$either_parent_immigrant ) , 'either_parent_immigrant' ] <- 0
stopifnot( nrow( x ) == before_nrow )
table( x$either_parent_immigrant )

z <-svrepdesign(weights = ~marsupwt, 
			repweights = "pwwgt[1-9]", 
			type = "Fay", 
			rho = (1-1/sqrt(4)),
			data = x ,
			combined.weights = T)
options( survey.replicates.mse = TRUE )
z$mse=TRUE
rm(kids,immigrant_dads,immigrant_moms,before_nrow,x)
# Define Immigrants (Inclusve vs. Exclusive) 
z=update(z, i_stat =  ifelse(prcitshp >= 4, 1,ifelse(prcitshp >= 1, 2, 0))) # Is Immigrant?
z=update(z, cit_stat = ifelse(prcitshp <= 4, 0,ifelse(prcitshp == 1, 1, 0))) # Is Citizen?
z=update(z, i_cit_stat = ifelse(prcitshp <= 3, 1,ifelse(prcitshp == 4, 2,ifelse(prcitshp == 5, 3, 0)))) # Is Immigrant and Citizen?
z=update(z, i_parent = ifelse( (pefntvty < 100 | pemntvty < 100), 0, 1)) # Parent Immigrant?
z=update(z, i_gen_stat = ifelse(prcitshp >= 4, 1,ifelse(i_parent== 1, 2,ifelse(i_parent == 0, 3, 0)))) # Generational Stat
z=update(z, i_parent_f = factor( i_parent )) # Define Factor
z=update(z, i_stat_f = factor( i_stat )) # Define Factor
z=update(z,i_gen_kids= ifelse(prcitshp!=5&either_parent_immigrant==0,1,ifelse(prcitshp!=5&either_parent_immigrant==1,2,ifelse(prcitshp==5,3,0))))

# Uninsured vs. Insured
z=update(z, uninsure = ifelse( ahiper == 0, 0, ifelse(ahiper >= 1, 1, 0)))
# Supplemental Security Income
z=update(z, ssi = ifelse( ssi_yn == 1, 1, ifelse(ssi_yn == 2, 0, 0)))
# Recieve TANF
z=update(z, pub_help = ifelse( paw_yn == 1, 1, ifelse(paw_yn == 2, 0, 0)))
# Average SNAP Benefit per person
z=update(z, avgsnapben = hfdval/h_numper)
# Supplemental Security Income
z=update(z, ssi_count = ifelse( ssi_yn == 1, 1, ifelse(ssi_yn == 2, 0, 0)))
# Social Security Count
z=update(z, ss_count = ifelse( ss_yn == 1, 1, ifelse(ss_yn == 0, 0, 0)))
# Diverse Government Care Stat
z=update(z,govcare=ifelse((z$ch_mc==1&z$a_age < 15)|( z$pchip==1)|(z$caid==1&z$a_age >=15)|(z$mcaid ==1&z$a_age>=15),1,0))

#### Populations ####
# Non Poverty
adults=subset(z, a_age > 18 )
kids=subset(z, a_age <= 18 )
elderly=subset(z, a_age >= 65)
adults_Im=subset(z, a_age > 17 & i_stat == 3)
# Poverty
pov200=subset(z, povll < 8)
pov200_a=subset(z, povll < 8&a_age > 18 )
pov200_c=subset(z, povll < 8&a_age <= 17 )
pov200_e=subset(z, povll < 8&a_age >= 65)

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
immigrant$i_stat=ifelse(immigrant$i_stat == 1,"Immigrant",ifelse(immigrant$i_stat == 2, "Non-immigrant", "error"))
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
immigrant$i_stat=ifelse(immigrant$i_stat == 1,"Immigrant",ifelse(immigrant$i_stat == 2, "Non-immigrant", "error"))
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
CashWelfare_A$program_name=('Cash Welfare')
CashWelfare_A$poverty_200=('no')
P200_CashWelfare_A=(CashWelfare(pov200_a))
P200_CashWelfare_A$program_name=('Cash Welfare')
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
immigrant$i_stat=ifelse(immigrant$i_stat == 1,"Immigrant",ifelse(immigrant$i_stat == 2, "Non-immigrant", "error"))
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

SSI=function(x){	k=(x) 
total_population=svyby( ~one, by = ~ i_gen_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
colnames(total_population)[colnames(total_population)=="one"]="total_population"
age_eligible_population=svyby( ~one, by = ~ i_gen_stat, design =k, FUN=svytotal)
count=svyby( ~ssi_count, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
value=svyby( ~ssi_val, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
nativity	=	merge(count, value, by='i_gen_stat')
nativity	=	merge(nativity, age_eligible_population, by='i_gen_stat')
nativity	=	merge(nativity, total_population, by='i_gen_stat')
nativity$i_gen_stat=ifelse(nativity$i_gen_stat==1,"Natives", ifelse(nativity$i_gen_stat==2, "Naturalized", "Noncitizens"))
colnames(nativity)[colnames(nativity)=="i_gen_stat"]="group"
nativity$calc_type=4
# Old
total_population=svyby( ~one, by = ~ i_gen_stat, design =z, FUN=svytotal)
colnames(total_population)[colnames(total_population)=="se"]="se_total_population"
colnames(total_population)[colnames(total_population)=="one"]="total_population"
age_eligible_population=svyby( ~one, by = ~ i_gen_stat, design =k, FUN=svytotal)
count=svyby( ~ssi_count, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
value=svyby( ~ssi_val, by = ~ i_gen_stat, design =  k, 	FUN = svytotal)
colnames(age_eligible_population)[colnames(age_eligible_population)=="se"]="se_age_eligible_population"
colnames(age_eligible_population)[colnames(age_eligible_population)=="one"]="age_eligible_population"
colnames(count)[colnames(count)=="se"]="se_count"
colnames(value)[colnames(value)=="se"]="se_value"
nativity	=	merge(count, value, by='i_gen_stat')
nativity	=	merge(nativity, age_eligible_population, by='i_gen_stat')
nativity	=	merge(nativity, total_population, by='i_gen_stat')
nativity$i_gen_stat=ifelse(nativity$i_gen_stat==1,"Natives", ifelse(nativity$i_gen_stat==2, "Naturalized", "Noncitizens"))
colnames(nativity)[colnames(nativity)=="i_gen_stat"]="group"
nativity$calc_type=4
# Create 
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



unique(names(c(P200_SSI_A,SSI_A,P200_CashWelfare_A,CashWelfare_A, A_Social_Security, P200_A_Social_Security)))
final_dataset=rbind(P200_SSI_A,SSI_A,P200_CashWelfare_A,CashWelfare_A, A_Social_Security, P200_A_Social_Security)
write.csv(final_dataset, 'final_dataset.csv')
#### Medicaid/Uninsured  ####
medicaid=function(x){	k=update(x, caid = factor( caid )) 
svyby( ~caid, by = ~ i_stat, design =  k, 	FUN = svymean)}

medicaid(adults)
medicaid(pov200)

#### TANF/Public Assistance ####

adults_welfare <-subset(z, povll < 8 & a_age > 18 & hpaw_yn == 1)
adults_welfare=update(adults_welfare, avg_ssi = (hpawval/h_numper))
adults_ssi=subset(z, povll < 8 & a_age > 18 & hssi_yn == 1)
adults_ssi=update(adults_ssi, avg_welfare = (hssival/h_numper))

svyby( ~avg_ssi, by = ~ i_stat, design =  adults_ssi, FUN = svymean)
svyby( ~avg_welfare, by = ~ i_stat, design =  adults_welfare, FUN = svymean)

pa_y=function(x){	k=update(x, pub_help = factor( pub_help ))
svyby( ~pub_help, by = ~ i_stat, design =  k, 	FUN = svymean)}

#### TANF/Public Assistance ####

#### SNAP ####
h_foodspval=function(x){	svyby( ~hfdval, by = ~ i_stat, design =  x, 	FUN = svytotal)}
h_foodspval()

h_foodspval=function(x){	svyby( ~hfdval, by = ~ i_stat, design =  x, 	FUN = svytotal)}
h_foodspval()


# Household
h_foodstamp=function(x){		k=update(x, hfoodsp = factor( hfoodsp )) 
svyby( ~hfoodsp, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household
h_foodstamp(pov200_a)

#### Figure 5 ####

val_pawf=function(x){ svyby( ~paw_val, by = ~ i_stat, design =  x, 	FUN = svytotal)}

#### Figure 6 ####

#### Figure 7 ####

ssi_y=function(x){	k=update(x, ssi = factor( ssi ))
svyby( ~ssi, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Person

# Person
ssi_y(pov200_a)
pa_y(pov200_a)

h_foodstamp(pov200_a)
h_foodstamp(pov200_c)

#### Lunch ####

#### Other ####

reci_pfare=function(x){ k=subset(x, paw_yn == 1 ) 	
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

# Person / Household

h_lunch=function(x){ k	=subset(x, hflunch == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Household

ed_assistance=function(x){k	=subset(x, ed_yn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person

medicaid_number=function(x){k	=subset(x, caid == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person
medicaid_number(z)

hssi_y=function(x){	k	=subset(x, hssi_yn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
#Household

ssi_y=function(x){	k	=subset(x, ssi_yn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person

ssi_value =function(x){svyby( ~ssi_val, by = ~ i_stat, design =  x, 	FUN = svytotal)}
# Person / Household

fed_dis=function(x){
	k	=subset(x, dis_sc1 == 3 )
	svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

wic_ben=function(x){	k	=subset(x, wicyn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

tanf=function(x){	k	=subset(x, paw_typ == 1)
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

tanf(adults)
tanf(pov200_a)


tanf_val=function(x){	k	=subset(x, paw_typ == 1)
svyby( ~paw_val, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

vet_ben=function(x){	k	=subset(x, vet_yn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person 

medicare=function(x){	k	=subset(x, care == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person

#### Percentage ####

h_wic_ben=function(x){k=update(x, hrwicyn = factor( hrwicyn )) 
svyby( ~hrwicyn, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

hvet_ben=function(x){		k=update(x, hvet_yn = factor( hvet_yn )) 
svyby( ~hvet_yn, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

hed_assistance=function(x){		k=update(x, hed_yn = factor( hed_yn )) 
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

h_public_housing=function(x){		k=update(x, hpublic = factor( hpublic )) 
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

energy_help=function(x){			k=update(x, hengast = factor( hengast )) 
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

medicare=subset(z, care == 1)
lunch=subset(z, hflunch == 1)


svyby( ~paw_val,by = ~ i_stat, design =  reci_pubfare_a, 	FUN = svymean)
svyby( ~paw_val,by = ~ i_stat, design =  pov200_c, 	FUN = svymean)

svyby( ~one, by = ~ i_stat, design =  medicare, 	FUN = svytotal)
svyby( ~one, by = ~ i_stat, design =  lunch, 	FUN = svytotal)
svyby( ~one, by = ~ i_stat, design =  z, 	FUN = svytotal)


#ch_hi - Children HI
#pchip - Children's health insurance program
#ch_mc - Children Medicare/Medicaid

kids=update(kids, govcare = ifelse((ch_mc == 1)	| (pchip == 1) |(caid ==1) | (mcaid ==1 ), 1,0))
kids=update(kids, govcare = factor( govcare ))

kids=update(kids, uninsured = 
			 	ifelse((hi_yn == 2 & mcaid == 2 & caid != 1 & champ == 2 & mcare != 1) |	(ch_hi == 3 & a_age < 15) & ch_mc != 1 & (pchip != 1), 1, 0))
kids=update(kids, uninsured = factor( uninsured ))

kids=update(kids, hsnapben = 
			 	ifelse(hfoodsp == 1,1,0 ))
kids=update(kids, hsnapben = factor( hsnapben ))

kids=update(kids, avgsnapben = hfdval/h_numper)

kids=update(kids, ssi_rate = ifelse(hssi_yn == 1, 1, 0))
kids=update(kids, ssi_rate = factor( ssi_rate ))

kids=update(kids, welfare = ifelse(hpaw_yn == 1, 1, 0))
kids=update(kids, welfare = factor( welfare ))

adults2=update(adults2, ssi_rate = ifelse(ssi_yn == 1, 1, 0))
adults2=update(adults2, ssi_rate = factor( ssi_rate ))

adults2=update(adults2, welfare = ifelse(paw_yn == 1, 1, 0))
adults2=update(adults2, welfare = factor( welfare ))

kids=update(kids, welfare_value = hpawval/h_numper)
kids=update(kids, ssi_value = hssival/h_numper)

#### Subsets

kids_good=subset(kids, prcitshp != 5 & either_parent_noncitizen == 0)
anchor_babies=subset(kids, prcitshp != 5 & either_parent_noncitizen == 1)
kids_badhombres=subset(kids, prcitshp == 5)

### Final Calcs

svymean(	~govcare ,	design = kids_good)
svymean(	~govcare ,	design = anchor_babies)
svymean(	~govcare ,	design = kids_badhombres)

svymean(	~uninsured ,	design = kids_good)
svymean(	~uninsured ,	design = anchor_babies)
svymean(	~uninsured ,	design = kids_badhombres)

svymean(	~hsnapben ,	design = kids_good)
svymean(	~hsnapben ,	design = anchor_babies)
svymean(	~hsnapben ,	design = kids_badhombres)

svyby( ~avgsnapben, by = ~ hfoodsp, design =  kids_good, FUN = svymean)
svyby( ~avgsnapben, by = ~ hfoodsp, design =  anchor_babies, FUN = svymean)
svyby( ~avgsnapben, by = ~ hfoodsp, design =  kids_badhombres, FUN = svymean)

svyby( ~avgsnapben, by = ~ i_stat, design =  adults1, FUN = svymean)

svymean(	~ssi_rate ,	design = kids_good)
svymean(	~ssi_rate ,	design = anchor_babies)
svymean(	~ssi_rate ,	design = kids_badhombres)

svymean(	~welfare ,	design = kids_good)
svymean(	~welfare ,	design = anchor_babies)
svymean(	~welfare ,	design = kids_badhombres)

svyby( ~ssi_rate, by = ~ i_stat, design =  adults2, FUN = svymean)
svyby( ~welfare, by = ~ i_stat, design =  adults2, FUN = svymean)


svymean(	~ssi_rate ,	design = kids_good)
svymean(	~ssi_rate ,	design = anchor_babies)
svymean(	~ssi_rate ,	design = kids_badhombres)

svymean(	~welfare ,	design = kids_good)
svymean(	~welfare ,	design = anchor_babies)
svymean(	~welfare ,	design = kids_badhombres)

kids_good1=subset(kids_good, hpaw_yn == 1)
anchor_babies1=subset(anchor_babies, hpaw_yn == 1)
kids_badhombres1=subset(kids_badhombres, hpaw_yn == 1)

svymean(	~welfare_value ,by = ~ hpaw_yn ,	design = kids_good1)
svymean(	~welfare_value ,by = ~ hpaw_yn ,	design = anchor_babies1)
svymean(	~welfare_value ,by = ~ hpaw_yn ,	design = kids_badhombres1)

kids_good2=subset(kids_good, hssi_yn == 1)
anchor_babies2=subset(anchor_babies, hssi_yn == 1)
kids_badhombres2=subset(kids_badhombres, hssi_yn == 1)

svymean(	~ssi_value,by = ~ hssi_yn,	design = kids_good2)
svymean(	~ssi_value,by = ~ hssi_yn,	design = anchor_babies2)
svymean(	~ssi_value,by = ~ hssi_yn,	design = kids_badhombres2)



peinusyr
