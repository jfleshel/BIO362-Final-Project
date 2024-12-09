#DSEE Final Project

install.packages("maps")
library(readr)
library(dplyr)
library(ggplot2)
library(maps)

#J
EJSCREEN <- read.csv("/Users/jamiefleshel/Desktop/F&M/Fall_2024/DSEE/Intro_to_R/Final_Project/EJSCREEN.csv")

#S
EJSCREEN <- read_csv("G:/My Drive/BIO - Final Proyect/EJSCREEN.csv")

# Clean data
PennsylvaniaData <- filter(EJSCREEN, ST_ABBREV == "PA")
PennStudyData <- select(PennsylvaniaData, "ST_ABBREV", "CNTY_NAME", "ID", "LOWINCPCT", "LIFEEXPPCT", "PTSDF")

#Plots
Counties <- map_data("county")
PA <- filter(Counties, region=="pennsylvania")

#Pennsylvania
ggplot() + 
  geom_polygon( data=PA, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue" )

#Income by county
ggplot() + 
  geom_polygon( data=PA, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue" )


#Join county names
pacounty <- PennStudyData$CNTY_NAME

for(i in 1:length(unique(PA$subregion))) {
  
pacounty[grep(unique(PA$subregion)[i],pacounty, ignore.case=T)] <- unique(PA$subregion)[i]

}

#Join data for maps
PennStudyData$pacounty <- pacounty

PA <- rename(PA, pacounty = subregion)

PennStudyData <- full_join(PennStudyData,PA, by="pacounty")


#Average Low Income per County

mean_lowinc <- PennStudyData %>%
  group_by(pacounty) %>%
  summarize(mean_lowinc = mean(LOWINCPCT,na.rm=T))

PennStudyData <- left_join(PennStudyData, mean_lowinc, by="pacounty")

#MAP OF MEAN LOW INCOME BY COUNTY
low_inc_map <- ggplot()
(low_inc_map <- low_inc_map + geom_polygon( data=PennStudyData, 
                       aes(x=long, y=lat, group=group, fill=mean_lowinc)) +
                       scale_fill_gradient(low="white",high="purple")+
                       labs(fill="Mean Percent Low Income")+
                       theme_dark()+
                       xlab("\nLongitude")+
                       ylab("Latitude\n"))



#Average Life Expectancy per County

mean_life <- PennStudyData %>%
  group_by(pacounty) %>%
  summarize(mean_life = mean(LIFEEXPPCT,na.rm=T))
PennStudyData <- left_join(PennStudyData, mean_life, by="pacounty")

#MAP OF MEAN LIFE EXPECTANCY BY COUNTY
life_map <- ggplot()
(life_map <- life_map + geom_polygon( data=PennStudyData, 
                                            aes(x=long, y=lat, group=group, fill=mean_life)) +
    scale_fill_gradient(low="white",high="aquamarine4")+
    labs(fill="Mean Percent Low Life Expectancy")+
    theme_dark()+
    xlab("\nLongitude")+
    ylab("Latitude\n"))


#Average Prox per County

mean_prox <- PennStudyData %>%
  group_by(pacounty) %>%
  summarize(mean_prox = mean(PTSDF,na.rm=T))

PennStudyData <- left_join(PennStudyData, mean_prox, by="pacounty")

#MAP OF PROX TO HAZARDOUS WASTE BY COUNTY
prox_map <- ggplot()
(prox_map <- prox_map + geom_polygon( data=PennStudyData, 
                                            aes(x=long, y=lat, group=group, fill=mean_prox)) +
    scale_fill_gradient(low="white",high="deeppink3")+
    labs(fill="Mean Score on Proximity Index")+
    theme_dark()+
    xlab("\nLongitude")+
    ylab("Latitude\n"))


#Prox Hazardous Waste vs. Income

mean_county <- PennStudyData %>%
  group_by(pacounty) %>%
  summarize(mean_prox = mean(PTSDF,na.rm=T),
            mean_inc = mean(LOWINCPCT,na.rm=T),
            mean_life = mean(LIFEEXPPCT,na.rm=T))

(Prox_inc <-ggplot(mean_county, aes(x=mean_inc, y=mean_prox))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("\nMean Percent Low Income")+
  ylab("Mean Proximity to Hazardous Waste Index\n")+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(color = "purple"),
          axis.title.y = element_text(color="deeppink3")))

proxinc.m<- lm(mean_inc ~ mean_prox, data = mean_county)
summary(proxinc.m)

# Income vs mean life
(inc_life <-ggplot(mean_county, aes(x=mean_inc, y=mean_life))+
    geom_point()+
    geom_smooth(method = "lm")+
    theme_bw()+
    xlab("\nMean Percent Low Income")+
    ylab("Mean Percent Low Life Expectancy\n")+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(color = "purple"),
          axis.title.y = element_text(color="aquamarine4")))

inclife.m<- lm(mean_inc ~ mean_life, data = mean_county)
summary(inclife.m)

# Proximity vs. life
(prox_life <-ggplot(mean_county, aes(x=mean_prox, y=mean_life))+
    geom_point()+
    geom_smooth(method = "lm")+
    theme_bw()+
    xlab("\nMean Proximity to Hazardous Waste Index")+
    ylab("Mean Percent Low Life Expectancy\n")+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(color = "deeppink3"),
          axis.title.y = element_text(color="aquamarine4")))

proxlife.m<- lm(mean_prox ~ mean_life, data = mean_county)
summary(proxlife.m)

#ANOVA for Life expectancy by county
life.m <- lm(LIFEEXPPCT ~ CNTY_NAME, data=PennStudyData)
summary(life.m)
anova(life.m)

require(emmeans)
emmeans(life.m,pairwise ~ CNTY_NAME) #Comparison of all pairs of t-test

#ANOVA for prox by county
prox.m <- lm(PTSDF ~ CNTY_NAME, data=PennStudyData)
anova(prox.m)

#ANOVA for income by county
inc.m <- lm(LOWINCPCT ~CNTY_NAME, data=PennStudyData)
anova(inc.m)
