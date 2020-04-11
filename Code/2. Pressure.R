#Loading libraries
library(tidyverse)

#Load utility functions, currently only pitch outline
source("./Code/UtilityFunctions.r")

#Parameter selection - arbitrary selected - pDim for pressure dimension
pDim=5  #Meters
plotting=TRUE #If no visual check is needed change to FALSE
testPass=10 #For visual check

#Load in data
events=read.csv("./Input/Sample_Game_1_RawEventsData.csv")
trackingData=readRDS("./Input/metrica_tracking_tidy.rds") %>% 
  filter(game_id==1 & !is.nan(x)) #Removing game 2 and all players not on the pitch

#Caculate the playing direction of each team
direction=trackingData %>% filter(team!="Ball") %>% 
  group_by(period) %>%
  arrange(period, frame) %>% 
  filter(frame==first(frame)) %>% 
  mutate(x=x-0.5) %>% 
  group_by(period,team) %>%
  summarise(x=mean(x)) %>% 
  mutate(direction=ifelse(x>0,0,1)) %>% 
  select(period, team, direction)


#Find passes in events
passes=events %>% 
  filter(Type=="PASS"| (Type=="BALL LOST" & Subtype=="INTERCEPTION")) %>% 
  select(team=Team, period=Period, event=Type, passer=From, 
         receiver=To, startFrame=Start.Frame, endFrame=End.Frame,
         startX=Start.X,endX=End.X, startY=Start.Y, endY=End.Y) %>% 
  mutate(team=tolower(team), passId=1, passId=cumsum(passId))


#Calculate for each pass the number of defenders within pDim meters of passer
pressureDataPasser=trackingData %>% 
  filter(team!="Ball") %>% 
  filter(frame %in% c(passes$startFrame)) %>%
  merge(.,passes %>% select(frame=startFrame, passer, passingTeam=team, passId), by="frame", all.x=T) %>% 
  mutate(passer=str_remove_all(passer,"Player")) %>% 
  group_by(passId) %>% 
  mutate(distVIP=sqrt((x*105-mean(ifelse(team==passingTeam & player==passer,x*105,NA),na.rm=T))^2+(y*68-mean(ifelse(team==passingTeam & player==passer,y*68,NA),na.rm=T))^2)) %>% 
  filter(team!=passingTeam) %>% 
  mutate(applyingPressure=ifelse(distVIP<=pDim,1,0)) %>% 
  {filter(.,passId==testPass) ->> pressurePlotDataPass } %>% 
  summarise(applyingPressureToPass=sum(applyingPressure))

#Same for each reception - unsuccessful passes will be "NA"
pressureDataReciever=trackingData %>% 
  filter(team!="Ball") %>% 
  filter(frame %in% c(passes$endFrame)) %>%
  merge(.,passes %>% select(frame=endFrame, receiver, passingTeam=team, passId), by="frame", all.x=T) %>% 
  mutate(receiver=str_remove_all(receiver,"Player")) %>% 
  group_by(passId) %>% 
  mutate(distVIP=sqrt((x*105-mean(ifelse(team==passingTeam & player==receiver,x*105,NA),na.rm=T))^2+(y*68-mean(ifelse(team==passingTeam & player==receiver,y*68,NA),na.rm=T))^2)) %>% 
  filter(team!=passingTeam) %>% 
  mutate(applyingPressure=ifelse(distVIP<=pDim,1,0)) %>% 
  {filter(.,passId==testPass) ->> pressurePlotDataReception } %>%
  summarise(applyingPressureToReception=sum(applyingPressure))

#Merge the results together and back unto the passes data frame
passesWithPressure=merge(passes,merge(pressureDataPasser,pressureDataReciever, by="passId"), by="passId")

#Visual check - Defenders applying pressure highlighed with black circle:

passingFrame=trackingData %>% 
  filter(period==passes$period[testPass] & frame==passes$startFrame[testPass]) 

p=createOutline()+
  geom_point(data=pressurePlotDataPass %>% filter(distVIP<=pDim), aes(x,y), cex=5)+
  geom_point(data=passingFrame, aes(x,y), 
             col=ifelse(passingFrame$team=="Ball", "orange",ifelse(passingFrame$team=="home","steelblue","red")),
             cex=ifelse(passingFrame$team=="Ball",1,3))
  


receivingFrame=trackingData %>% 
  filter(period==passes$period[testPass] & frame==passes$endFrame[testPass]) 

r=createOutline()+
  geom_point(data=pressurePlotDataReception %>% filter(distVIP<=pDim), aes(x,y), cex=5)+
  geom_point(data=receivingFrame,aes(x,y), 
             col=ifelse(receivingFrame$team=="Ball", "orange",ifelse(receivingFrame$team=="home","steelblue","red")),
             cex=ifelse(receivingFrame$team=="Ball",1,3))

library(grid)
library(gridExtra)
if(plotting==T){
  grid.arrange(p,r,nrow=2)}


#Question: Fix unsuccessful passes - either: 
  #A. Guess pass target and calculate press on that player 
  #AND/OR 
  #B. Caculate pressure on defender intercepting the pass

#Question: Is pressure dimension the same in all directions?

