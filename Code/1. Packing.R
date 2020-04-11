#Loading libraries
library(tidyverse)

#Load utility functions, currently only pitch outline
source("./Code/UtilityFunctions.r")

#Should the script plot a visual test? If not change plotting to FALSE
plotting=TRUE

#Load in data
events=read.csv("./Input/Sample_Game_1_RawEventsData.csv")
trackingData=readRDS("./Input/metrica_tracking_tidy.rds") %>% 
  filter(game_id==1 & !is.nan(x)) #We remove game 2 and all players not on the pitch

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

#Visual Test
testPass=105

passingFrame=trackingData %>% 
  filter(period==passes$period[testPass] & frame==passes$startFrame[testPass]) 

p=createOutline()+
  geom_point(data=passingFrame, aes(x,y), 
             col=ifelse(passingFrame$team=="Ball", "orange",ifelse(passingFrame$team=="home","steelblue","red")),
             cex=ifelse(passingFrame$team=="Ball",1,3))
  

receivingFrame=trackingData %>% 
  filter(period==passes$period[testPass] & frame==passes$endFrame[testPass]) 

r=createOutline()+
  geom_point(data=receivingFrame,aes(x,y), 
             col=ifelse(receivingFrame$team=="Ball", "orange",ifelse(receivingFrame$team=="home","steelblue","red")),
             cex=ifelse(receivingFrame$team=="Ball",1,3))

library(grid)
library(gridExtra)
if(plotting==T){
grid.arrange(p,r,nrow=2)}

#Packing calculation - the 105, 68 and 34 comes from the dimension of a standard football pitch in meters

#First, for each pass calculate the distance to the goal
packingPasses=passes %>%
  merge(.,direction, by=c("period","team")) %>% 
  mutate(oppGoalLine=ifelse(direction==1,1,0),
         distToOppGoalStart=sqrt((startX*105-oppGoalLine*105)^2+(startY*68-34)^2),
         distToOppGoalEnd=sqrt((endX*105-oppGoalLine*105)^2+(endY*68-34)^2))

#Then for each starting frame calculate each players distance to own goal
packingDataBefore=trackingData %>%
  filter(frame %in% c(passes$startFrame)) %>% 
  merge(.,direction, by=c("period","team")) %>% 
  mutate(ownGoalLine=ifelse(direction==1,0,1),
         distToOwnGoal=sqrt((x*105-ownGoalLine*105)^2+(y*68-34)^2)) %>% 
  merge(.,packingPasses %>% select(frame=startFrame, passId, distToOppGoalStart, passingTeam=team), by="frame", all=T) %>% 
  filter(passingTeam!=team) %>% 
  group_by(passId) %>% 
  summarise(behindBallStart=sum(ifelse(distToOwnGoal<distToOppGoalStart,1,0)))

#Same for each end frame
packingDataAfter=trackingData %>%
  filter(frame %in% c(passes$endFrame)) %>% 
  merge(.,direction, by=c("period","team")) %>% 
  mutate(ownGoalLine=ifelse(direction==1,0,1),
         distToOwnGoal=sqrt((x*105-ownGoalLine*105)^2+(y*68-34)^2)) %>% 
  merge(.,packingPasses %>% select(frame=endFrame, passId, distToOppGoalEnd, passingTeam=team), by="frame", all=T) %>% 
  filter(passingTeam!=team) %>% 
  group_by(passId) %>% 
  summarise(behindBallEnd=sum(ifelse(distToOwnGoal<distToOppGoalEnd,1,0)))


#Merge the two results with the dataframe containing passes and calculate Packing as the number of defenders played past and
# the ratio of defenders removed by pass.
passesWithPacking=merge(passes,merge(packingDataBefore,packingDataAfter, by="passId"), by="passId") %>% 
  mutate(packing=behindBallStart-behindBallEnd,
         ratioOfDefendersRemoved=1-ifelse(behindBallStart==0,0,(behindBallEnd/behindBallStart)))

#Question to be solved: How do we score unsuccessful passes?
#Analysis question: Is ratio of defenders removed better than packing? Does the context added matter?