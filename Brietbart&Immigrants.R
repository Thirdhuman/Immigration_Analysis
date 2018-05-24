library(tidyverse)
library(fuzzyjoin)
library(hrbrthemes)
library(raster)
library(rgdal)
library(ggpolot)

#trend data
gtrends<- read_csv("/Users/rorr/Desktop/Welfare_Policy/Data/Data_Explorations/Immigration/Brietbart and Immigrants/CitiesByBrietbart.csv", skip = 2)
gtrends$DMA <- gtrends$City
gtrends<- read_csv("/Users/rorr/Desktop/Welfare_Policy/Data/Data_Explorations/Immigration/Brietbart and Immigrants/MetrosByBrietbart.csv", skip = 2)

#cities
us_cities <- read_csv("/Users/rorr/Desktop/Welfare_Policy/Data/Data_Explorations/Immigration/Brietbart and Immigrants/uscitiesv1.4.csv")
us_cities$name<-paste(us_cities$city,us_cities$state_id)

immigrants <- read_csv("/Users/rorr/Desktop/Welfare_Policy/Data/Data_Explorations/Immigration/Brietbart and Immigrants/census_immig_city.csv")


#clean
gtrends$state<-substr(gtrends$DMA,nchar(gtrends$DMA)-1,nchar(gtrends$DMA))
gtrends<-gtrends%>%
  arrange(-row_number())%>% #so it all works together
  mutate(DMA=strsplit(as.character(DMA),'-'))%>%
  unnest(DMA)
names(gtrends)[1]<-'count'

#add state
gtrends$name<-ifelse(substr(gtrends$DMA,nchar(gtrends$DMA)-1,nchar(gtrends$DMA))==gtrends$state,gtrends$DMA,paste(gtrends$DMA,gtrends$state))

#first go
first<-stringdist_left_join(gtrends,us_cities,by=c('name'),distance_col='dist')
first<-subset(first,dist<=0 & state == state_id)

#second go
second<-stringdist_left_join(immigrants,first,by=c('name.x'),distance_col='dist2')
trends<-subset(second,dist<=0 & state == state_id)
trends$count_rate <- (trends$count * trends$ForeignBorn_Rate)

labels<-subset(trends,count >=40) #for the labels

plot(trends$count, trends$ForeignBorn_Rate, main="Scatterplot Example", 
  	xlab="count ", ylab="foreign Born ", pch=19)
abline(lm(trends$count~trends$ForeignBorn_Rate), col="red") # regression line (y~x) 




#plot
us<-map_data('county')
solar<-ggplot(trends,aes(lng,lat)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='#dedede',fill=NA,alpha=.5,cex=1,show.legend = F)+
  geom_point(aes(color=count_rate,size=count_rate),alpha=.1,show.legend = F) +
  scale_colour_gradient(low = "blue", high = "red")+
  theme_ipsum(grid=F,plot_title_family = 'Slabo 27px',plot_title_face = 'bold',subtitle_size = 10,base_family = 'Roboto Condensed')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlim(-135,-65)+ylim(20,50)

solar
+labs(title='Google Trends for \'Brietbart\'',
           subtitle='Data for time period between 7/25/17 & 8/1/2017',
           caption='Search data from Google Trends\ Census estimates of foreign born') +
  geom_text(data=labels,aes(label=name.x,size=14) ,show.legend = F,check_overlap = T)
