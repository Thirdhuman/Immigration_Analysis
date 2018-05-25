### Failure

# 1968 Immigrants 
setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

library(survey)				# load survey package (analyzes complex design surveys)
library(MonetDBLite)

x=readRDS( file.path( path.expand( "~" ),"CPSASEC",paste(2016,"cps asec.rds") ) )

# Household subset 1
noncitizen_moms_70=subset(x, a_sex == 2 & a_age > 18 & prcitshp <= 4 & peinusyr >= 5, select = c(h_seq,a_lineno))
noncitizen_dads_70=subset(x, a_sex == 1 & a_age > 18 & prcitshp <= 4 & peinusyr >= 5, select = c(h_seq,a_lineno))
noncitizen_moms_early=subset(x, a_sex == 2 & a_age > 18 & peinusyr < 5 & prcitshp == 5, select = c(h_seq,a_lineno))
noncitizen_dads_early=subset(x, a_sex == 1 & a_age > 18 & peinusyr < 5 & prcitshp == 5, select = c(h_seq,a_lineno))
kids=subset(x, a_age < 19 & prcitshp < 4, select = c(h_seq,a_lineno,prcitshp, pelndad, pelnmom))
gen1_adult=subset(x, a_age >= 19 & prcitshp < 4,select=c(h_seq ,a_lineno,prcitshp, pelndad, pelnmom))

noncitizen_moms_70$mom_match_70=1
noncitizen_dads_70$dad_match_70=1
noncitizen_moms_early$mom_match_early=1
noncitizen_dads_early$dad_match_early=1

before_nrow=nrow( kids )
before_nrow_adult=nrow( gen1_adult )
kids=merge( kids ,noncitizen_moms_70,all.x = T,by.x =c( "h_seq" ,"pelnmom" ),by.y =c("h_seq","a_lineno"))
kids=merge( kids ,noncitizen_dads_70,all.x = T,by.x =c( "h_seq" ,"pelndad" ),by.y =c("h_seq","a_lineno"))
kids=merge( kids ,noncitizen_moms_early,all.x = T,by.x =c( "h_seq","pelnmom"),by.y=c("h_seq","a_lineno"))
kids=merge( kids ,noncitizen_dads_early,all.x = T,by.x =c( "h_seq","pelndad"),by.y=c("h_seq","a_lineno"))

gen1_adult=merge( gen1_adult ,noncitizen_moms_70 ,all.x=T,by.x =c("h_seq","pelnmom" ),by.y =c("h_seq","a_lineno"))
gen1_adult=merge( gen1_adult ,noncitizen_dads_70 ,all.x=T,by.x =c("h_seq","pelndad" ),by.y =c("h_seq","a_lineno"))
gen1_adult=merge( gen1_adult,noncitizen_moms_early,all.x=T,by.x =c("h_seq","pelnmom"),by.y=c("h_seq","a_lineno"))
gen1_adult=merge( gen1_adult,noncitizen_dads_early,all.x=T,by.x =c("h_seq","pelndad"),by.y=c("h_seq","a_lineno"))
stopifnot( nrow( kids ) == before_nrow )
stopifnot( nrow( gen1_adult ) == before_nrow_adult )

# Kids Immigrant Parents
kids$kid_parent_noncitizen_70=as.numeric( kids$mom_match_70 %in% 1 |kids$dad_match_70%in% 1 )
kids$kid_parent_noncitizen_early=as.numeric( kids$mom_match_early %in% 1 |kids$dad_match_early%in% 1 )
# Adults Immigrant Parents
gen1_adult$adult_parent_noncitizen_70=as.numeric( gen1_adult$mom_match_70 %in% 1 |gen1_adult$dad_match_70%in% 1 )
gen1_adult$adult_parent_noncitizen_early=as.numeric( gen1_adult$mom_match_early %in% 1|gen1_adult$dad_match_early%in%1)

#final merge
before_nrow=nrow( x )
x=merge( x ,kids,all.x=TRUE)
x=merge( x ,gen1_adult,all.x=TRUE)

x[ is.na(x$kid_parent_noncitizen_70),'kid_parent_noncitizen_70']<-0
x[ is.na(x$kid_parent_noncitizen_early),'kid_parent_noncitizen_early']<-0
x[ is.na(x$adult_parent_noncitizen_70),'adult_parent_noncitizen_70']<-0
x[ is.na(x$adult_parent_noncitizen_early),'adult_parent_noncitizen_early']<-0

stopifnot( nrow( x ) == before_nrow )

# See what we got
table( x$kid_parent_noncitizen_70 )
table( x$either_parent_noncitizen_early )
table( x$adult_parent_noncitizen_70 )
table( x$adult_parent_noncitizen_early )


Q <-svrepdesign(weights = ~marsupwt, 
		    repweights = "pwwgt[1-9]", 
		    type = "Fay", 
		    rho = (1-1/sqrt(4)),
		    data = x ,
		    combined.weights = T)
options( survey.replicates.mse = TRUE )
Q$mse=TRUE

#pop_68 = peinusyr >= 4
Q=update(Q, i_stat =  ifelse(prcitshp <= 3, 1,
				     ifelse(prcitshp == 4, 2,
				     	 ifelse(prcitshp == 5, 3, 0))))

#pop_68 = peinusyr >= 4

Q=update(Q, i_stat_f = factor( i_stat ))

Q=update(Q, i_stat = ifelse( prcitshp <= 3, 1,ifelse(prcitshp == 4, 2,ifelse(prcitshp == 5, 3, 1))))
Q=update(Q, i2_stat = ifelse( prcitshp <= 4, 0,ifelse(prcitshp == 1, 1, 0)))
Q=update(Q, uninsure = ifelse( ahiper == 0, 0, ifelse(ahiper >= 1, 1, 0)))
Q=update(Q, ssi = ifelse( ssi_yn == 1, 1, ifelse(ssi_yn == 2, 0, 0)))
Q=update(Q, pub_help = ifelse( paw_yn == 1, 1, ifelse(paw_yn == 2, 0, 0)))
Q=update(Q, lunch_stat = ifelse( hflunch == 1, 1, ifelse(hflunch == 2, 0, 0)))
Q=update(Q, avgsnapben = hfdval/h_numper)

svyby( ~one, by = ~ prcitshp, design =  Q, 	FUN = svytotal)
svyby( ~one, by = ~ prcitshp, design =  Q, 	FUN = svyquantile())
svyby( ~ptotval, by = ~ i_stat, design =  adults, FUN = svyquantile,  c( .5), ci=TRUE )
svyby( ~ptotval, by = ~ i_stat, design =  adults, FUN = svymean )


svyquantile( ~ptotval, design =  Q, c(.5))


#### Populations ####

pov200=subset(Q, povll < 8)
pov200_a=subset(Q, povll < 8 & a_age > 18 )
pov200_c=subset(Q, povll < 8 & a_age <= 18 )
adults=subset(Q, a_age > 17 )

children=subset(Q, a_age <= 18 )
adults_Im=subset(Q, a_age > 17 & i_stat == 3)

#### Figure 1 - Health Insurance - Medicaid/Uninsured  ####

medicaid=function(x){	k=update(x, caid = factor( caid )) 
svyby( ~caid, by = ~ i_stat, design =  k, 	FUN = svymean)}

kids2=update(Q, govcare = ifelse(( kids1$ch_mc == 1	 & kids1$a_age < 15)	| ( kids1$pchip == 1) |
					   	(kids1$caid ==1 & kids1$a_age >= 15) | (kids1$mcaid ==1 & kids1$a_age >= 15), 1,
					   0))

medicaid(pov200_a)
medicaid(pov200)

adults_welfare <-subset(Q, povll < 8 & a_age > 18 & hpaw_yn == 1)
adults_welfare=update(adults_welfare, avg_ssi = (hpawval/h_numper))

adults_ssi=subset(Q, povll < 8 & a_age > 18 & hssi_yn == 1)
adults_ssi=update(adults_ssi, avg_welfare = (hssival/h_numper))

svyby( ~avg_ssi, by = ~ i_stat, design =  adults_ssi, FUN = svymean)
svyby( ~avg_welfare, by = ~ i_stat, design =  adults_welfare, FUN = svymean)

uninsure=function(x){	k=update(x, uninsure = factor( uninsure )) 
svyby( ~uninsure, by = ~ i_stat, design =  k, 	FUN = svymean)}

uninsure(pov200_a)

#### Figure 2 ####

#### Figure 4 ####

h_foodspval=function(x){svyby( ~hfdval, by = ~ i_stat, design =  x, 	FUN = svytotal)}
# Household

h_foodstamp=function(x){k=update(x, hfoodsp = factor( hfoodsp )) 
svyby( ~hfoodsp, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Household

h_foodstamp(pov200_a)

#### FICA ####
fica_stat=function(x){	k=update(x, ssi = factor( ssi ))
svyby( ~fica, by = ~ i_stat, design =  k, 	FUN = svytotal)}
fica_stat(Q)

#### Figure 6 ####

#### Figure 7 ####

ssi_y=function(x){k=update(x, ssi = factor( ssi ))
svyby( ~ssi, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Person

pa_y=function(x){	k=update(x, pub_help = factor( pub_help ))
svyby( ~pub_help, by = ~ i_stat, design =  k, 	FUN = svymean)}
# Person

ssi_y(pov200_a)
pa_y(pov200_a)

h_foodstamp(pov200_a)
h_foodstamp(pov200_c)

#### FICA ####
payroll=function(x){ svyby( ~fica, by = ~ i_stat, design =  x, 	FUN = svytotal)}
number=function(x){ svyby( ~one, by = ~ i_stat, design =  x, 	FUN = svytotal)}
ss=function(x){	k	=subset(x, ss_yn == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}


ss(Q)
payroll(Q)
number(Q)

#### Other ####

reci_pfare=function(x){ k=subset(x, paw_yn == 1 ) 	
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person / Household

val_pfare=function(x){ svyby( ~paw_val, by = ~ i_stat, design =  x, 	FUN = svytotal)}
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

medicaid_number(Q)

pchip=function(x){	k	=subset(x, pchip == 1 )
svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
# Person 

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
ssi_value (Q)

#### Extras - Programmatic Functions ####


dis_sc1

reci_pubfare(Q)
foodstamp(Q)
h_public_housing(Q)

medicare=subset(Q, care == 1)
lunch=subset(Q, hflunch == 1)


svyby( ~paw_val,by = ~ i_stat, design =  reci_pubfare_a, 	FUN = svymean)
svyby( ~paw_val,by = ~ i_stat, design =  pov200_c, 	FUN = svymean)

svyby( ~one, by = ~ i_stat, design =  medicare, 	FUN = svytotal)
svyby( ~one, by = ~ i_stat, design =  lunch, 	FUN = svytotal)
svyby( ~one, by = ~ i_stat, design =  Q, 	FUN = svytotal)


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
