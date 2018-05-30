#This script downloads the 2016 CPS and creates variables for individuals based on nativity.

setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

library(devtools)
#install_github("ajdamico/lodown")
library(lodown)

lodown( "cpsasec" , output_dir = file.path( path.expand( "~" ) , "CPSASEC" ) )

library(DBI)
db <- dbConnect( MonetDBLite::MonetDBLite() , sub_cps_cat$dbfolder )
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )					# connect to the SQLite database (.db)

x <- dbReadTable( db , 'asec15' )

noncitizen_dads <- dbGetQuery( db , "SELECT ph_seq , a_lineno FROM asec15 WHERE prcitshp = 5 AND a_sex = 1" )
noncitizen_moms <- dbGetQuery( db , "SELECT ph_seq , a_lineno FROM asec15 WHERE prcitshp = 5 AND a_sex = 2" )

kids <- dbGetQuery( db , "SELECT ph_seq , a_lineno , prcitshp, pelndad, pelnmom FROM asec15 WHERE a_age < 19")

noncitizen_moms$mom_match <- 1
noncitizen_dads$dad_match <- 1

before_nrow <- nrow( kids )
kids <- merge( kids , noncitizen_moms , all.x = TRUE , by.x = c( "ph_seq" , "pelnmom" ) , by.y = c( "ph_seq" , "a_lineno" ) )
kids <- merge( kids , noncitizen_dads , all.x = TRUE , by.x = c( "ph_seq" , "pelndad" ) , by.y = c( "ph_seq" , "a_lineno" ) )
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
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

PEINUSYR
peinusyr

options( survey.replicates.mse = TRUE )
Q$mse <- TRUE

Q <- update(Q, i_stat = ifelse( prcitshp <= 3, 1,ifelse(prcitshp == 4, 2,ifelse(prcitshp == 5, 3, 1))))
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

#ch_hi - Children HI
#pchip - Children's health insurance program
#ch_mc - Children Medicare/Medicaid

kids1 <- update(kids1, govcare = ifelse((ch_mc == 1)	| (pchip == 1) |
																										(caid ==1) | (mcaid ==1 ), 1,
																									 0))
kids1 <- update(kids1, govcare = factor( govcare ))

kids1 <- update(kids1, uninsured = 
	ifelse((hi_yn == 2 & mcaid == 2 & caid != 1 & champ == 2 & mcare != 1) |	(ch_hi == 3 & a_age < 15) & ch_mc != 1 & (pchip != 1), 1, 
																					0))
kids1 <- update(kids1, uninsured = factor( uninsured ))

kids1 <- update(kids1, hsnapben = ifelse(hfoodsp == 1,1,0 ))
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


# hpawval 
# ssi_val
# hfdval

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



#####
kids1$uninsure <- 
	ifelse(kids1$a_age >= 15 & (kids1$hi_yn == 2 | kids1$mcaid == 2 | kids1$caid == 2) | 
	(kids1$a_age < 15 & kids1$ch_hi == 3) | (kids1$pchip == 2), 1, 
ifelse((kids1$hi_yn == 1 | kids1$mcaid == 1 & kids1$a_age >= 15) | 
								(kids1$a_age < 15 & (kids1$ch_hi == 1 | kids1$ch_hi == 2)) | kids1$pchip == 1, 0, 
																					0))

kids1$uninsure <- ifelse((kids1$ahiper == 1 | kids1$ahiper == 2) & 
															kids1$ch_hi == 3 & kids1$pchip == 2, 1, 
																					0)

kids1$ssi <- ifelse( kids1$ssi_yn == 1, 1, ifelse(kids1$ssi_yn == 2, 0, 0))
kids1$pub_help <- ifelse( kids1$paw_yn == 1, 1, ifelse(kids1$paw_yn == 2, 0, 0))


svymean( ~govcare, design =  kids_badhombres)
svyby( ~govcare, by = ~ i_stat, design =  anchor_babies, 	FUN = svymean)
svyby( ~govcare, by = ~ i_stat, design =  kids_good, 	FUN = svymean)

mean(kids_badhombres$govcare, na.rm = TRUE)
mean(anchor_babies$govcare, na.rm = TRUE)
mean(kids_good$govcare, na.rm = TRUE)

mean(kids_badhombres$uninsure, na.rm = TRUE)
mean(anchor_babies$uninsure, na.rm = TRUE)
mean(kids_good$uninsure, na.rm = TRUE)

#sskkidyn = child recieve SSI

mean(kids1$ssi, na.rm = TRUE)
mean(kids1$pub_help, na.rm = TRUE)

kids_good <- subset(kids1, povll < 8 & prcitshp != 5 & either_parent_noncitizen == 0 & a_age <= 18)
anchor_babies <- subset(kids1, povll < 8 & prcitshp != 5 & either_parent_noncitizen == 1 & a_age <= 18)
kids_badhombres <- subset(kids1, povll < 8 & prcitshp == 5 & a_age <= 18)


