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
peinusyr == ( 12 )& ## Years (1990))
# Years overcaptured: 1991
# Percent overcaptured: 50%
,1,0))
 
z=update(z,croatian_c1 = ifelse(
# Croatia
penatvty == ( 151 )& ## Croatia Code
peinusyr == (17 )& ## Years (2000-2001)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,czeschoslovakian_c1 = ifelse(
# Czechoslovakia
penatvty == ( 105 )& ## Czechoslovakia Code
peinusyr == ( 12 )& ## Years (1990-1991)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,eritrean_c1 = ifelse(
# Eritrea: Cohort 1: 2007
penatvty == ( 417 )& ## Eritrea Code
peinusyr == ( 20 )& ## Years (2006-2007)
# Years overcaptured: 2006
# Percent overcaptured: 50%
 ,1,0))
 
z=update(z,eritrean_c2 = ifelse(
# Eritrea: Cohort 2: 2009-2010
penatvty == ( 417 )& ## Eritrea Code
peinusyr == ( 21 | 22 )& ## Years  (2008-2011)
# Years overcaptured: 2008& 2011
# Percent overcaptured: 50%
,1,0))
 
z=update(z,eritrean_c3 = ifelse(
# Eritrea: Cohort 3: 2013-2014
penatvty == ( 417 )& ## Eritrea Code
peinusyr == ( 23 | 24 )& ## Years (2013-2014)
# Years overcaptured: 2012& 2015& 2016& 2017
# Percent overcaptured: 66.66%
,1,0))
 
z=update(z,estonian_c1 = ifelse(
# Estonia
penatvty == ( 155 )& ## Estonia Code
peinusyr == ( 19 )& ## Years (2004-2005)
# Years overcaptured: 2005
# Percent overcaptured: 50%
,1,0))
 
z=update(z,ethiopian_c1 = ifelse(
# Ethiopia
penatvty == (416 )& ## Ethiopia Code
peinusyr == ( 12 | 13 )& ## Years (1990-1993)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,iraqi_c1 = ifelse(
# Iraq: Cohort 1: 1992-1995
penatvty == ( 213 )& #Iraq Code
peinusyr == ( 13 | 14 )& ## Years (1992-1995)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,iraqi_c2 = ifelse(
#Iraq: Cohort 2: 2001
penatvty == (213)& #Iraq Code
peinusyr == ( 17 )& ## Years (2000-2001)
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
 
z=update(z,liberian_c4 = ifelse)
# Liberia: Cohort 4: 2004-2006
penatvty == (429)& #Liberia Code
peinusyr == ( 19 | 20 )
# Years overcaptured: 2007
# Percent overcaptured: 25%
 ,1,0))
 
 
z=update(z,libyan_c1 = ifelse)
# Libya
penatvty == (430))& #Libya Code
peinusyr == (12)
# Years overcaptured: 1990
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,moldovan_c1 = ifelse)
# Moldova: Cohort 1: 2001
penatvty == (162)& #Moldova Code
peinusyr == (17)
# Years overcaptured: 2000
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,moldovan_c2 = ifelse)
# Moldova: Cohort 2: 2004
penatvty == (162)& #Moldova Code
peinusyr == (19)
# Years overcaptured: 2005
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,serbian_c1 = ifelse)
# Serbia: Cohort 1: 1999
penatvty == (154)& #Elsewhere Code
peinusyr == (16)
# Years overcaptured: 1998
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,serbian_c2 = ifelse)
# Serbia: Cohort 2: 2002-2003
penatvty == (154)& #Elsewhere Code
peinusyr == (18)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0)) 
 
z=update(z,sierraleonean_c1 = ifelse)
# Sierra Leone: Cohort 1: 2001
penatvty == (447)& #Sierra Leone Code
peinusyr == (17)
# Years overcaptured: 2000
# Percent overcaptured: 50%
 ,1,0))
 
z=update(z,sierraleonean_c2 = ifelse)
# Sierra Leone: Cohort 2: 2003-2004
penatvty == (447)& #Sierra Leone Code
peinusyr == ( 18 | 19 )
# Years overcaptured: 2002& 2005
# Percent overcaptured: 50%
,1,0))
 
z=update(z,somalian_c1 = ifelse)
# Somalia: Cohort 1: 1992-1998
penatvty == (448)& #Somalia Code
peinusyr == (13 | 14 | 15 | 16 )
# Years overcaptured: 1999
# Percent overcaptured: 14.28%
,1,0)) 
 
z=update(z,somalian_c2 = ifelse)
# Somalia: Cohort 2: 2000-2001
penatvty == (448)& #Somalia Code
peinusyr == (17)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,somalian_c3 = ifelse)
# Somalia: Cohort 3: 2003-2014
penatvty == (448)& #Somalia Code
peinusyr == ( 18 | 19 | 20 | 21 | 22 | 23 | 24 )
# Years overcaptured: 2002& 2015& 2016& 2017
# Percent overcaptured: 25%
,1,0))
 
z=update(z,sudanese_c1 = ifelse)
# Sudan: Cohort 1: 1994-1995
penatvty == (451)& #Sudan Code
peinusyr == (14)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,sudanese_c2 = ifelse)
# Sudan: Cohort 2: 1998-2006
penatvty == (451)& #Sudan Code
peinusyr == ( 16 | 17 | 18 | 19 | 20 )
# Years overcaptured: 2007
# Percent overcaptured: 10%
,1,0)) 
 
z=update(z,sudanese_c3 = ifelse)
# Sudan: Cohort 3: 2012-2014
penatvty == (451)& #Sudan Code
peinusyr == ( 23 | 24 )
# Years overcaptured: 2015& 2016& 2017
# Percent overcaptured: 50%
,1,0))
 
z=update(z,togoan_c1 = ifelse)
# Togo: Cohort 1: 1995
penatvty == (454)& #Togo Code
peinusyr == (14)
# Years overcaptured: 1994
# Percent overcaptured: 50%
,1,0))
 
z=update(z,togoan_c2 = ifelse)
# Togo: Cohort 2: 2000
penatvty == (454)& #Togo Code
peinusyr == (17)
# Years overcaptured: 2001
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,vietnamese_c1 = ifelse)
# Vietnam
penatvty == (247)& #Vietnam Code
peinusyr == (14)
# Years overcaptured:
# Percent overcaptured: 0%
,1,0))
 
z=update(z,Congolese_c1 = ifelse)
# Democratic Republic of Congo: Cohort 1: 1993-1994
penatvty == (412)& #Democratic Republic of Congo Code
peinusyr == ( 13 | 14 )
# Years overcaptured: 1992& 1995
# Percent overcaptured: 50%
,1,0))
 
z=update(z,congolese_c2 = ifelse)
# Democratic Republic of Congo: Cohort 2: 2000
penatvty == (412)& #Democratic Republic of Congo Code
peinusyr == (17)
# Years overcaptured: 2001
# Percent overcaptured: 50%
,1,0)) 
 
z=update(z,congolese_c3 = ifelse)
# Democratic Republic of Congo: Cohort 3: 2004-2014
penatvty == (412)& #Democratic Republic of Congo Code
peinusyr == ( 19 | 20 | 21 | 22 | 23 | 24 )
# Years overcaptured: 2015& 2016& 2017
# Percent overcaptured: 21%
,1,0))