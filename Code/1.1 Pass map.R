#Basic pass map

library(tidyverse)

#Load utility functions
source("./Code/UtilityFunctions.r")

events=read.csv("./Input/Sample_Game_1_RawEventsData.csv")

#Lets first find all shots and rename the coloums to a perfered convention
passes=events %>% 
  filter(Type=="PASS"| (Type=="BALL LOST" & Subtype=="INTERCEPTION")) %>% 
  select(team=Team, period=Period, event=Type, passer=From, 
         receiver=To, startFrame=Start.Frame, endFrame=End.Frame,
         startX=Start.X,endX=End.X, startY=Start.Y, endY=End.Y) %>% #Rename to convention
  mutate(team=tolower(team), passId=1, passId=cumsum(passId), #Lowercase team to fit with tracking data format and add passId
         startX=startX*105,startY=startY*68,endX=endX*105,endY=endY*68)  #Convert to meter scale

#Lets pick a random player and fix the direction:
set.seed(1)

playerPasses=passes %>% filter(passer==sample(unique(passes$passer),1)) %>% 
  mutate(startX=ifelse(period==2,105-startX,startX),
         startY=ifelse(period==2,68-startY,startY),
         endX=ifelse(period==2,105-endX,endX),
         endY=ifelse(period==2,68-endY,endY))


#Lets now plot a simple pass map - using the "createOutline()" function to get a pitch
#And adding the start x,y as points with geom_point

simplePassMap=createOutline()+
  geom_point(data=playerPasses,aes(x=startX,y=startY))

print(simplePassMap)

#Lets and a indicator of direction by adding team name at their own goal
#We use str_to_title to bring back capitalization on team name
simplePassMap=createOutline()+
  geom_point(data=playerPasses,aes(x=startX,y=startY))+
  geom_text(aes(x=ifelse(unique(playerPasses$team)=="home",2,103),y=-2, label=str_to_title(unique(playerPasses$team))))

print(simplePassMap)

#Lets add some color - orange for success and blue for missed and lets use pch=21
#Also lets upscale the points and text a bit
simplePassMap=createOutline()+
  geom_point(data=playerPasses,aes(x=startX,y=startY),pch=21,cex=2,
             fill=ifelse(playerPasses$event=="PASS","steelblue","orange"))+
  geom_text(aes(x=ifelse(unique(playerPasses$team)=="home",2,103),y=-2, label=str_to_title(unique(playerPasses$team))),
            col="steelblue",cex=5)

print(simplePassMap)

#Now lets add the end destination of the passes but by stringing the start and end together by geom_segment
simplePassMap=createOutline()+
  geom_segment(data=playerPasses,aes(x=startX,y=startY,xend=endX,yend=endY),
               col=ifelse(playerPasses$event=="PASS","steelblue","orange"))+
  geom_point(data=playerPasses,aes(x=startX,y=startY),pch=21,cex=2,
             fill=ifelse(playerPasses$event=="PASS","steelblue","orange"))+
  geom_text(aes(x=ifelse(unique(playerPasses$team)=="home",2,103),y=-2, label=str_to_title(unique(playerPasses$team))),
            col="steelblue",cex=5)

print(simplePassMap) #One pass has no end coordinates - hence no segement is drawn


#Lets go back to the pass data frame and add a varible accouting for keypasses (pass leading directly to shot)

passes=events %>% group_by(Period) %>%
  arrange(Start.Frame) %>%
  mutate(keypass=ifelse(Type=="PASS" & str_detect(lead(Type),"SHOT"),1,0),
         shotX=ifelse(keypass==1,lead(Start.X),NA),
         shotY=ifelse(keypass==1,lead(Start.Y),NA)) %>% 
  filter(Type=="PASS"| (Type=="BALL LOST" & Subtype=="INTERCEPTION")) %>% 
  select(team=Team, period=Period, event=Type, passer=From, 
         receiver=To, startFrame=Start.Frame, endFrame=End.Frame,
         startX=Start.X,endX=End.X, startY=Start.Y, endY=End.Y, keypass, shotX,shotY) %>% #Rename to convention
  mutate(team=tolower(team), passId=1, passId=cumsum(passId), #Lowercase team to fit with tracking data format and add passId
         startX=startX*105,startY=startY*68,endX=endX*105,endY=endY*68,
         shotX=shotX*105, shotY=shotY*68)  #Convert to meter scale

#Lets pick the same player and fix the direction:
set.seed(1)

playerPasses=passes %>% filter(passer==sample(unique(passes$passer),1)) %>% 
  mutate(startX=ifelse(period==2,105-startX,startX),
         startY=ifelse(period==2,68-startY,startY),
         endX=ifelse(period==2,105-endX,endX),
         endY=ifelse(period==2,68-endY,endY),
         shotX=ifelse(period==2,105-shotX,shotX),
         shotY=ifelse(period==2,68-shotY,shotY))

#Now lets add a halo for all keypasses and a circle marking the position of resulting shots
simplePassMap=createOutline()+
  geom_segment(data=playerPasses,aes(x=startX,y=startY,xend=endX,yend=endY),
               col=ifelse(playerPasses$event=="PASS","steelblue","orange"))+
  geom_point(data=playerPasses %>% filter(keypass==1),aes(x=startX,y=startY),cex=3)+
  geom_point(data=playerPasses %>% filter(keypass==1),aes(x=shotX,y=shotY),cex=3, pch=1)+
  geom_point(data=playerPasses,aes(x=startX,y=startY),pch=21,cex=2,
             fill=ifelse(playerPasses$event=="PASS","steelblue","orange"))+
  geom_text(aes(x=ifelse(unique(playerPasses$team)=="home",2,103),y=-2, label=str_to_title(unique(playerPasses$team))),
            col="steelblue",cex=5)

print(simplePassMap) 
#One pass has no end coordinates - hence no segement is drawn
#Another curious thing is that 2 shots are taking place at the same coordinate hence only 4 shot markers from 5 keypasses
