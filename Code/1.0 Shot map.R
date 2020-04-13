#Basic shot map

library(tidyverse)

#Load utility functions
source("./Code/UtilityFunctions.r")

events=read.csv("./Input/Sample_Game_1_RawEventsData.csv")

#Lets first find all shots and rename the coloums to a perfered convention

allShots=events %>% 
  filter(Type=="SHOT") %>% 
  select(team=Team, period=Period, event=Type, subType=Subtype, passer=From, 
         receiver=To, startFrame=Start.Frame, endFrame=End.Frame,
         startX=Start.X,endX=End.X, startY=Start.Y, endY=End.Y) %>% #Rename to convention
  mutate(team=tolower(team), #Lowercase team to fit with tracking data format
         startX=startX*105,startY=startY*68,endX=endX*105,endY=endY*68) #Convert to meter scale

#Lets now plot a dead simple shot map - using the "createOutline()" function to get a pitch
#And adding the shots as points with geom_point

simpleTeamShotMap=createOutline()+
  geom_point(data=allShots,aes(x=startX,y=startY))

print(simpleTeamShotMap)

#Lets add some colour based on the team

simpleTeamShotMap=createOutline()+
  geom_point(data=allShots,aes(x=startX,y=startY),
             col=ifelse(allShots$team=="home","steelblue","red"))

print(simpleTeamShotMap)


#Lets make sure the home team a trying to score to the right and the away team are trying to score to the left
#For this data set, teams changes half during halftime, this is not the stadard in other data sets, so be sure to check!

shotsHA=allShots %>% 
  mutate(startX=ifelse(period==2,105-startX,startX),
         startY=ifelse(period==2,68-startY,startY),
         endX=ifelse(period==2,105-endX,endX),
         endY=ifelse(period==2,68-endY,endY))

#We can also use another shape to get a small halo

simpleTeamShotMap=createOutline()+
  geom_point(data=shotsHA,aes(x=startX,y=startY), pch=21,
             fill=ifelse(shotsHA$team=="home","steelblue","red")) #Notice the change from col to fill, as col is the color for the halo

print(simpleTeamShotMap)

#Alternatively we can use the layering for ggplot and create our own halos

simpleTeamShotMap=createOutline()+
  geom_point(data=shotsHA,aes(x=startX,y=startY),
             cex=3)+
  geom_point(data=shotsHA,aes(x=startX,y=startY),
             col=ifelse(shotsHA$team=="home","steelblue","red"))

print(simpleTeamShotMap)

#Lets now add some context - normally we could add team names etc. here we just add "Home" and "Away" to the bottom of the plot

simpleTeamShotMap=createOutline()+
  geom_point(data=shotsHA,aes(x=startX,y=startY), pch=21,
             fill=ifelse(shotsHA$team=="home","steelblue","red"),
             cex=3)+
  geom_text(aes(x=c(103,2), y=c(-2,-2),label=c("Home","Away")),
            col=c("steelblue","red"), cex=5)+
  geom_text(aes(x=52.5, y=70,label=paste0("Home vs Away - Game 1")),
            cex=5)

print(simpleTeamShotMap)

#As a last detail lets highlight the goals using the "manual" halos

simpleTeamShotMap=createOutline()+
  geom_point(data=shotsHA %>% filter(str_detect(subType,"GOAL")),
             aes(x=startX,y=startY),
             cex=5)+
  geom_point(data=shotsHA,aes(x=startX,y=startY), pch=21,
             fill=ifelse(shotsHA$team=="home","steelblue","red"),
             cex=3)+
  geom_text(aes(x=c(103,2), y=c(-2,-2),label=c("Home","Away")),
            col=c("steelblue","red"), cex=5)+
  geom_text(aes(x=52.5, y=70,label=paste0("Home vs Away - Game 1")),
            cex=5)

print(simpleTeamShotMap)


#Exercise: Try to plot a single player - add context, examples: name, # of shots, # of goals