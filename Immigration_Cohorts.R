# Immigrant Age Cohort Analysis

library(survey)
library(reshape2)
library(plyr)

for (year in 1998:2017){
	if(year == 2014){
		next 
	} else {
		x <- readRDS( file.path( path.expand( "~" ) , "CPSASEC" , paste(year,"cps asec.rds") ) )
		
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
		
		if(year < 2005){
			#No Replicate Weights (Pre-2005)
			z <- svydesign(
				weights = ~a_ernlwt,
				data = x,
				ids = ~1)
		} else {
			# Replicate Weights (Post-2005)
			z <- svrepdesign(
				weights = ~ a_ernlwt ,
				repweights = "pwwgt[1-9]" ,
				type = "Fay" ,
				rho = ( 1 - 1 / sqrt( 4 ) ) ,
				data = x ,
				combined.weights = TRUE)
		}
		z$mse=TRUE
		
#### Immigrant Status Assignments ###
		z=update(z, i_stat =  ifelse(prcitshp <= 3, 1,	ifelse(prcitshp == 4, 2,ifelse(prcitshp == 5, 3, 0))))
		z=update(z, i_stat_f = factor( i_stat ))
		# Immigrant or Native Citizen
		z=update(z, i2_stat = ifelse( prcitshp <= 4, 0,ifelse(prcitshp == 1, 1, 0)))
		# Isurance or no?
		z=update(z, uninsure = ifelse( ahiper == 0, 0, ifelse(ahiper >= 1, 1, 0)))
		# Recieve SSI
		z=update(z, ssi = ifelse( ssi_yn == 1, 1, ifelse(ssi_yn == 2, 0, 0)))
		# Recieve Public Assistance (Cash) Welfare
		z=update(z, pub_help = ifelse( paw_yn == 1, 1, ifelse(paw_yn == 2, 0, 0)))
		# Subsidized School Lunch
		z=update(z, lunch_stat = ifelse( hflunch == 1, 1, ifelse(hflunch == 2, 0, 0)))
		# Household SNAP Benefit per person
		z=update(z, avgsnapben = hfdval/h_numper)
		
		# Count the populations
		svyby( ~one, by = ~ prcitshp, design =  z, 	FUN = svytotal)
		svyby( ~one, by = ~ prcitshp, design =  z, 	FUN = svyquantile())
		svyby( ~ptotval, by = ~ i_stat, design =  adults, FUN = svyquantile,  c( .5), ci=TRUE )
		svyby( ~ptotval, by = ~ i_stat, design =  adults, FUN = svymean )
		
		#### Populations ####
		pov200=subset(z, povll < 8)
		pov200_a=subset(z, povll < 8 & a_age > 18 )
		pov200_c=subset(z, povll < 8 & a_age <= 18 )
		adults=subset(z, a_age > 18 )
		children=subset(z, a_age <= 18 )
		adults_Im=subset(z, a_age > 17 & i_stat == 3)
		
		z <- subset( 
			z , 
			a_age %in% 18:64 &
				workyn %in% 1 &
				ern_yn %in% 1 &
				wexp %in% 1 & 
				wewkrs %in% 1
		)
		
		if( year <= 2000){
			z <- subset(z, 
							 hg_st60 == 93)
		}  else {
			z <- subset(z, 
							 gestfips == 6)} 	
		
		male <- subset( 
			z , 
			a_sex %in% 1 )
		female <- subset( 
			z , 
			a_sex %in% 2 )
		
		print (female_mean <- as.data.frame(svytotal(~ern_val, design = female, na.rm.all= T) / svytotal(~one, design = female , na.rm.all= T)))
		print (male_mean <- as.data.frame(svytotal(~ern_val, design = male , na.rm.all= T) /svytotal(~one, design = male , na.rm.all= T)))
		
		print(output <- svyby(
			~ern_val,
			~a_sex,
			design = z,
			svyquantile,
			quantiles= 0.5,
			ci=TRUE,
			vartype="ci"
		))
		
		colnames(female_mean)[colnames(female_mean)=="total"] <- "female_mean_wage"
		colnames(male_mean)[colnames(male_mean)=="total"] <- "male_mean_wage"
		
		if(year < 2005){
			female_mean <- subset(female_mean, select = -c(ern_val) )
			male_mean <- subset(male_mean, select = -c(ern_val) )
		} else {
			# Replicate Weights (Post-2005)
			colnames(female_mean)[colnames(female_mean)=="SE"] <- "female_mean_SE"
			colnames(male_mean)[colnames(male_mean)=="SE"] <- "male_mean_SE"
			
			female_mean <- subset(female_mean, select = -c(female_mean_SE) )
			male_mean <- subset(male_mean, select = -c(male_mean_SE) )
		}
		
		#Reshape
		#####
		print(output.melted <- melt(output[, -1], id.vars = NULL))
		# Transpose the values into a single row
		print(output_t <- t(output.melted[, 2]))
		
		# Assign new variable names
		print(colnames(output_t) <- paste0("r", rownames(output), output.melted[, 1]))
		if(year < 2005){
			colnames(output_t)[colnames(output_t)=="r1ern_val"] <- "male_median_wage"
			colnames(output_t)[colnames(output_t)=="r2ern_val"] <- "female_median_wage"
			colnames(output_t)[colnames(output_t)=="r1ci_l"] <- "male_median_ci_l"
			colnames(output_t)[colnames(output_t)=="r2ci_l"] <- "female_median_ci_l"
			colnames(output_t)[colnames(output_t)=="r1ci_u"] <- "male_median_ci_u"
			colnames(output_t)[colnames(output_t)=="r2ci_u"] <- "female_median_ci_u"
		} else {
			colnames(output_t)[colnames(output_t)=="r1V1"] <- "male_median_wage"
			colnames(output_t)[colnames(output_t)=="r2V1"] <- "female_median_wage"
			colnames(output_t)[colnames(output_t)=="r1cv%"] <- "male_median_cv"
			colnames(output_t)[colnames(output_t)=="r2cv%"] <- "female_median_cv"
			output_t <- subset(output_t, select = -c(r1cv,r2cv) )
		}
		
		# calculations
		print(output_t <- as.data.frame(output_t))
		output_t <- within(output_t, gap_median_num <- male_median_wage - female_median_wage)
		output_t <- within(output_t, gap_median <- female_median_wage/male_median_wage)
		
		print(output_c<- cbind(output_t, male_mean, female_mean))
		output_c <- within(output_c, gap_mean_num <- male_mean_wage - female_mean_wage)
		output_c <- within(output_c, gap_mean <- female_mean_wage/male_mean_wage)
		
		# add year
		output_c[ , "asec"] <- NA
		output_c$asec <- year
		output_c[ , "survey_year"] <- NA
		print(output_c$survey_year <- as.numeric( output_c$asec - 1))
		output_c<-as.data.frame(output_c)
		
		# Loop back to beggining
		if( year == 1998){
			final_output <- as.data.frame(output_c)
		} else {
			final_output <-  rbind.fill(final_output, output_c)
		}
	}
	colnames(final_output)	
	#final_output <- subset(final_output, select = -c(male_median_ci_l, female_median_ci_l, 
	#																																																	male_median_ci_u, female_median_ci_u, 
	#																																																	male_median_cv, female_median_cv,
	#																																																	gap_median_num, gap_mean_num
	#																																																	) )
	
	refcols <- c("survey_year")
	#
	final_output <- final_output[, c(refcols, setdiff(names(final_output), refcols))]
	print(names(final_output))
}
# End of Function


print(gpg <- gpg[, c("survey_year","male_median_wage","female_median_wage", "gap_median", 
			   "gap_median_num", "male_median_ci_l", "female_median_ci_l", "male_median_ci_u",
			   "female_median_ci_u","male_median_cv%","female_median_cv%","male_mean_wage", 
			   "female_mean_wage", "gap_mean", "gap_mean_num", "asec")])


#write.csv(final_output, "output2.csv")
#write.csv(final_output, "outputUSA.csv")

