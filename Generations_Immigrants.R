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

# Define Immigrants (Inclusve vs. Exclusive)
z=update(z, i_stat =  ifelse(prcitshp >= 4, 1,ifelse(prcitshp >= 1, 2, 0)))
z=update(z, i_stat = ifelse(prcitshp <= 3, 1,ifelse(prcitshp == 4, 2,ifelse(prcitshp == 5, 3, 1))))
# Define Factor
z=update(z, i_stat_f = factor( i_stat ))
# Define Immigrant Mother / Father 
z=update(z, i_parent = ifelse( (pefntvty < 100 | pemntvty < 100), 0, 1))
z=update(z, i_parent_f = factor( i_parent ))

z=update(z, i2_stat = ifelse( prcitshp <= 4, 0,ifelse(prcitshp == 1, 1, 0)))
z=update(z, uninsure = ifelse( ahiper == 0, 0, ifelse(ahiper >= 1, 1, 0)))
z=update(z, ssi = ifelse( ssi_yn == 1, 1, ifelse(ssi_yn == 2, 0, 0)))
# Recieve TANF
z=update(z, pub_help = ifelse( paw_yn == 1, 1, ifelse(paw_yn == 2, 0, 0)))
# Average SNAP Benefit per person
z=update(z, avgsnapben = hfdval/h_numper)

#### Populations ####
pov200=subset(z, povll < 8)
pov200_a=subset(z, povll < 8 & a_age > 18 )
pov200_c=subset(z, povll < 8 & a_age <= 17 )
adults=subset(z, a_age > 18 )
children=subset(z, a_age <= 18 )
adults_Im=subset(z, a_age > 17 & i_stat == 3)

svyby( ~one, by = ~ prcitshp, design =  z, 	FUN = svytotal)
svyby( ~one, by = ~ prcitshp, design =  z, 	FUN = svyquantile())
svyby( ~ptotval, by = ~ i_stat, design =  adults, FUN = svyquantile,  c( .5), ci=TRUE )

noncit=subset(z,i_stat== 3)
naturali=subset(z,i_stat== 2)

# Food Stamps
check=function(x){	k=subset(z, i_stat == 1) 
svyby( ~i_parent_f, by = ~ a_age, design =  k, FUN = svymean )}
check(z)

svyby( ~one, by = ~ pefntvty, design =  naturali, FUN = svytotal )
PEFNTVTY

svyquantile( ~ptotval, design =  z, c(.5))

#### Figure 1 - Health Insurance - Medicaid/Uninsured  ####

medicaid=function(x){	k=update(x, caid = factor( caid )) 
svyby( ~caid, by = ~ i_stat, design =  k, 	FUN = svymean)}

kids2=update(z, govcare = ifelse(( kids1$ch_mc == 1	 & kids1$a_age < 15)	| ( kids1$pchip == 1) |
							 	(kids1$caid ==1 & kids1$a_age >= 15) | (kids1$mcaid ==1 & kids1$a_age >= 15), 1,0))

medicaid(pov200_a)
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

kids1=update(kids1, govcare = ifelse((ch_mc == 1)	| (pchip == 1) |(caid ==1) | (mcaid ==1 ), 1,0))
kids1=update(kids1, govcare = factor( govcare ))

kids1=update(kids1, uninsured = 
			 	ifelse((hi_yn == 2 & mcaid == 2 & caid != 1 & champ == 2 & mcare != 1) |	(ch_hi == 3 & a_age < 15) & ch_mc != 1 & (pchip != 1), 1, 0))
kids1=update(kids1, uninsured = factor( uninsured ))

kids1=update(kids1, hsnapben = 
			 	ifelse(hfoodsp == 1,1,0 ))
kids1=update(kids1, hsnapben = factor( hsnapben ))

kids1=update(kids1, avgsnapben = hfdval/h_numper)

kids1=update(kids1, ssi_rate = ifelse(hssi_yn == 1, 1, 0))
kids1=update(kids1, ssi_rate = factor( ssi_rate ))

kids1=update(kids1, welfare = ifelse(hpaw_yn == 1, 1, 0))
kids1=update(kids1, welfare = factor( welfare ))

adults2=update(adults2, ssi_rate = ifelse(ssi_yn == 1, 1, 0))
adults2=update(adults2, ssi_rate = factor( ssi_rate ))

adults2=update(adults2, welfare = ifelse(paw_yn == 1, 1, 0))
adults2=update(adults2, welfare = factor( welfare ))

kids1=update(kids1, welfare_value = hpawval/h_numper)
kids1=update(kids1, ssi_value = hssival/h_numper)

#### Subsets

kids_good=subset(kids1, prcitshp != 5 & either_parent_noncitizen == 0)
anchor_babies=subset(kids1, prcitshp != 5 & either_parent_noncitizen == 1)
kids_badhombres=subset(kids1, prcitshp == 5)

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
