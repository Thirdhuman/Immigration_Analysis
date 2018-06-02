library(survey)
library(reshape2)
library(plyr)

x <- readRDS( file.path( path.expand( "~" ) , "CPSASEC" , paste(2017,"cps asec.rds") ) )
codes<-read.csv('/Users/rorr/Desktop/Welfare_Policy/Data/Data_Explorations/Immigration/CPS_Appendix-H(CountryCodes).csv')
keeps <- c("Code", "Country")
codes<- codes[ , keeps, drop = FALSE]

noncitizen_dads<- subset(x, a_sex == 1 & a_age > 18 & prcitshp >= 5 , select = c(h_seq , a_lineno))
noncitizen_moms<- subset(x, a_sex == 2 & a_age > 18 & prcitshp >= 5 , select = c(h_seq , a_lineno))
kids<- subset(x, a_age < 19, select = c(h_seq , a_lineno , prcitshp, pelndad, pelnmom))

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

table( x$either_parent_noncitizen )

Q <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = x ,
		combined.weights = T
	)


peinusyr


options( survey.replicates.mse = TRUE )
Q$mse <- TRUE

Q <- update(Q, i_stat = ifelse( prcitshp <= 3, 1,ifelse(prcitshp == 4, 2,ifelse(prcitshp == 5, 3, 1))))
Q <- update(Q, i_stat_f = factor( i_stat ))

Q <- update(Q, Welfare = ifelse( (caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1	| ed_yn == 1), 1, 0))
Q <- update(Q, noWelfare = ifelse( (caid == 2 & wicyn == 2 & ssi_yn == 2 & paw_yn == 2	& ed_yn == 2), 1, 0))

kids1 <- subset(Q, a_age <= 18 & povll < 8)
adults1 <- subset(Q, povll < 8 & a_age > 18 & hfoodsp == 1)
adults1 <- update(adults1, avgsnapben = hfdval/h_numper)
svyby( ~avgsnapben, by = ~ i_stat, design =  adults1, FUN = svymean)

adult_welfare <-subset(Q, povll < 8 & a_age > 18 & hpaw_yn == 1)
adult_welfare <- update(adult_welfare, welfare = hpawval/h_numper)

adult_ssi <- subset(Q, povll < 8 & a_age > 18 & hssi_yn == 1)
adult_ssi <- update(adult_ssi, ssi = hssival/h_numper)

svyby( ~hssival, by = ~ i_stat, design =  adult_ssi, FUN = svymean)
svyby( ~hpawval, by = ~ i_stat, design =  adult_welfare, FUN = svymean)

svyby( ~hssival, by = ~ i_stat, design =  adult_ssi, FUN = svymean)
svyby( ~hpawval, by = ~ i_stat, design =  adult_welfare, FUN = svymean)

adultsSS_Pov <- subset(Q, povll < 8 & a_age > 18 & ss_yn == 1)
adultsSS <- subset(Q, a_age > 18 & ss_yn == 1)

adult_non_welfare_naturalized <- subset(Q, noWelfare == 1 & a_age > 17 & i_stat == 2 & peinusyr == 24)
adult_non_welfare_noncitizen <- subset(Q, noWelfare == 1 & a_age > 17 & i_stat == 3 & peinusyr == 24)
adult_welfare_naturalized <- subset(Q, Welfare == 1 & a_age > 17 & i_stat == 2 & peinusyr == 24)
adult_welfare_noncitizen <- subset(Q, Welfare == 1 & a_age > 17 & i_stat == 3 & peinusyr == 24)

#(caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1	| ed_yn == 1)
#(caid == 2 & wicyn == 2 & ssi_yn == 2 & paw_yn == 2	& ed_yn == 2)
#df1<-svyby( ~ss_val, by = ~ penatvty, design =  adultsSS_naturalized, FUN = svymean)
#df2<-svyby( ~ss_val, by = ~ penatvty, design =  adultsSS_noncitizen, FUN = svymean)

df1<-svyby( ~one, by = ~ penatvty, design =  adult_non_welfare_naturalized, FUN = svytotal)
df2<-svyby( ~one, by = ~ penatvty, design =  adult_non_welfare_noncitizen, FUN = svytotal)
df3<-svyby( ~one, by = ~ penatvty, design =  adult_welfare_naturalized, FUN = svytotal)
df4<-svyby( ~one, by = ~ penatvty, design =  adult_welfare_noncitizen, FUN = svytotal)

df1$penatvty=as.character(df1$penatvty)
df2$penatvty=as.character(df2$penatvty)
df3$penatvty=as.character(df3$penatvty)
df4$penatvty=as.character(df4$penatvty)
codes$Code=as.character(codes$Code)

keeps <- c("one", 'penatvty')
df4<- df4[ , keeps, drop = FALSE]
df3<- df3[ , keeps, drop = FALSE]
df2<- df2[ , keeps, drop = FALSE]
df1<- df1[ , keeps, drop = FALSE]

names(df1)[names(df1) == 'one'] <- 'adult_non_welfare_naturalized'
names(df2)[names(df2) == 'one'] <- 'adult_non_welfare_noncitizen'
names(df3)[names(df3) == 'one'] <- 'adult_welfare_naturalized'
names(df4)[names(df4) == 'one'] <- 'adult_welfare_noncitizen'

df5<-merge(df1, codes, by.x = ('penatvty'), by.y = ('Code'), all =T)
df5<-merge(df2, df5, by.x = ('penatvty'), by.y = ('penatvty'), all =T)
df5<-merge(df3, df5, by.x = ('penatvty'), by.y = ('penatvty'), all =T)
df5<-merge(df4, df5, by.x = ('penatvty'), by.y = ('penatvty'), all =T)

write.csv(df5, "CountryOfBirth_Welfare_2017.csv")


svyby( ~i_stat, by = ~ penatvty, design =  adultsSS, FUN = svytotal)


#ch_hi - Children HI
#pchip - Children's health insurance program
#ch_mc - Children Medicare/Medicaid

kids1 <- update(kids1, govcare = ifelse((ch_mc == 1)	| (pchip == 1) |(caid ==1) | (mcaid ==1 ), 1,0))
kids1 <- update(kids1, govcare = factor( govcare ))

kids1 <- update(kids1, uninsured = 
	ifelse((hi_yn == 2 & mcaid == 2 & caid != 1 & champ == 2 & mcare != 1) |	(ch_hi == 3 & a_age < 15) & ch_mc != 1 & (pchip != 1), 1, 0))
kids1 <- update(kids1, uninsured = factor( uninsured ))

kids1 <- update(kids1, hsnapben = 
	ifelse(hfoodsp == 1,1,0 ))
kids1 <- update(kids1, hsnapben = factor( hsnapben ))

kids1 <- update(kids1, avgsnapben = hfdval/h_numper)

kids1<- update(kids1, ssi_rate = ifelse(hssi_yn == 1, 1, 0))
kids1<- update(kids1, ssi_rate = factor( ssi_rate ))

kids1<- update(kids1, welfare = ifelse(hpaw_yn == 1, 1, 0))
kids1<- update(kids1, welfare = factor( welfare ))

adults2<- update(adults2, ssi_rate = ifelse(ssi_yn == 1, 1, 0))
adults2<- update(adults2, ssi_rate = factor( ssi_rate ))

adults2<- update(adults2, welfare = ifelse(paw_yn == 1, 1, 0))
adults2<- update(adults2, welfare = factor( welfare ))

kids1 <- update(kids1, welfare_value = hpawval/h_numper)
kids1 <- update(kids1, ssi_value = hssival/h_numper)

#### Subsets

kids_good <- subset(kids1, prcitshp != 5 & either_parent_noncitizen == 0)
anchor_babies <- subset(kids1, prcitshp != 5 & either_parent_noncitizen == 1)
kids_badhombres <- subset(kids1, prcitshp == 5)

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

kids_good1 <- subset(kids_good, hpaw_yn == 1)
anchor_babies1 <- subset(anchor_babies, hpaw_yn == 1)
kids_badhombres1 <- subset(kids_badhombres, hpaw_yn == 1)

svymean(	~welfare_value ,by = ~ hpaw_yn ,	design = kids_good1)
svymean(	~welfare_value ,by = ~ hpaw_yn ,	design = anchor_babies1)
svymean(	~welfare_value ,by = ~ hpaw_yn ,	design = kids_badhombres1)

kids_good2 <- subset(kids_good, hssi_yn == 1)
anchor_babies2 <- subset(anchor_babies, hssi_yn == 1)
kids_badhombres2 <- subset(kids_badhombres, hssi_yn == 1)

svymean(	~ssi_value , by = ~ hssi_yn,	design = kids_good2)
svymean(	~ssi_value , by = ~ hssi_yn,	design = anchor_babies2)
svymean(	~ssi_value , by = ~ hssi_yn,	design = kids_badhombres2)
