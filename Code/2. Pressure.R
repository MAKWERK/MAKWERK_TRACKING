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
  #Done by looking at the values of x for each team when normalized around the halfway line
direction=trackingData %>% filter(team!="Ball") %>% 
  group_by(period) %>%
  arrange(period, frame) %>% 
  filter(frame==first(frame)) %>% 
  mutate(x=x-0.5) %>% #Normalizing around halfway line
  group_by(period,team) %>%
  summarise(x=mean(x)) %>% #Team centoids
  mutate(direction=ifelse(x>0,0,1)) %>% #If negative then positive direction and vice-versa
  select(period, team, direction)


#Find passes in events
passes=events %>% 
  filter(Type=="PASS"| (Type=="BALL LOST" & Subtype=="INTERCEPTION")) %>% 
  select(team=Team, period=Period, event=Type, passer=From, 
         receiver=To, startFrame=Start.Frame, endFrame=End.Frame,
         startX=Start.X,endX=End.X, startY=Start.Y, endY=End.Y) %>% #Rename to perfeered standard
  mutate(team=tolower(team), passId=1, passId=cumsum(passId)) #Lowercase team to fit with tracking data format and add passId


#Calculate for each pass the number of defenders within pDim meters of passer
  # For both we: Keep only the players and the frames which has passes starting/ending in them (1.)
  # Then we merge on pass info such as passer/reciever and passing team, and fix the player format (2.)
  # Then for each pass we calculate the distance from all players to the passer (3.)
  # Keeping only the defending team we then create a binary variable for players inside the pressure dimension (4.)
  # This we pipe out for plotting, can be delete if plotting is not needed (5.)
  # Lastly we sum up the number of defenders applying presssure (6.)

pressureDataPasser=trackingData %>% #1.
  filter(team!="Ball") %>% #1.
  filter(frame %in% c(passes$startFrame)) %>% #1.
  merge(.,passes %>% select(frame=startFrame, passer, passingTeam=team, passId), by="frame", all.x=T) %>% #2.
  mutate(passer=str_remove_all(passer,"Player")) %>%  #2.
  group_by(passId) %>% #3.
  mutate(distVIP=sqrt((x*105-mean(ifelse(team==passingTeam & player==passer,x*105,NA),na.rm=T))^2
                      +(y*68-mean(ifelse(team==passingTeam & player==passer,y*68,NA),na.rm=T))^2)) %>% #3.
  filter(team!=passingTeam) %>% #4.
  mutate(applyingPressure=ifelse(distVIP<=pDim,1,0)) %>% #4.
  {filter(.,passId==testPass) ->> pressurePlotDataPass } %>% #5.
  summarise(applyingPressureToPass=sum(applyingPressure)) #6.

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

#Create plotting datasets
passingFrame=trackingData %>% 
  filter(period==passes$period[testPass] & frame==passes$startFrame[testPass]) 
receivingFrame=trackingData %>% 
  filter(period==passes$period[testPass] & frame==passes$endFrame[testPass]) 


#Plot p(assingframe) and r(ecptionframe) - col is color based on team and cex scales the points, here ball is smaller than players
  # Added first are the larger black circles for pressing players, this is the first line due to the layering of ggplot

p=createOutline()+
  geom_point(data=pressurePlotDataPass %>% filter(distVIP<=pDim), aes(x,y), cex=5)+
  geom_point(data=passingFrame, aes(x,y), 
             col=ifelse(passingFrame$team=="Ball", "orange",ifelse(passingFrame$team=="home","steelblue","red")),
             cex=ifelse(passingFrame$team=="Ball",1,3))
  
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

