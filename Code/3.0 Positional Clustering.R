#Cluster analysis 

library(tidyverse)

#Load utility functions
source("./Code/UtilityFunctions.r")

events=read.csv("./Input/Sample_Game_1_RawEventsData.csv")

#We will attempt to assign players to clusters based on their avg pass position
#This will proxy as position and context in future scripts

passes=events %>% 
  filter(Type=="PASS"| (Type=="BALL LOST" & Subtype=="INTERCEPTION")) %>% 
  select(team=Team, period=Period, event=Type, passer=From, 
         receiver=To, startFrame=Start.Frame, endFrame=End.Frame,
         startX=Start.X,endX=End.X, startY=Start.Y, endY=End.Y) %>% #Rename to convention
  mutate(team=tolower(team), passId=1, passId=cumsum(passId), #Lowercase team to fit with tracking data format and add passId
         startX=startX*105,startY=startY*68,endX=endX*105,endY=endY*68) %>%  #Convert to meter scale
  mutate(startX=ifelse(period==2,105-startX,startX),
         startY=ifelse(period==2,68-startY,startY),
         endX=ifelse(period==2,105-endX,endX),
         endY=ifelse(period==2,68-endY,endY)) %>% 
  mutate(startX=ifelse(team=="away",105-startX,startX),
         startY=ifelse(team=="away",68-startY,startY),
         endX=ifelse(team=="away",105-endX,endX),
         endY=ifelse(team=="away",68-endY,endY)) %>% 
  group_by(passer, team) %>% 
  summarise(avgX=mean(startX), avgY=mean(startY))

clusterSolutionLength=kmeans(passes[,3],4, nstart = 100)
clusterSolutionWidth=kmeans(passes[,4],3, nstart = 100)

passes$clustL=clusterSolutionLength$cluster
passes$clustW=clusterSolutionWidth$cluster

passes$clust=paste(passes$clustL,"-",passes$clustW)

simpleTeamShotMap=createOutline()+
  geom_point(data=passes,aes(x=avgX,y=avgY, fill=as.factor(clust)), cex=3, pch=21)
print(simpleTeamShotMap)

