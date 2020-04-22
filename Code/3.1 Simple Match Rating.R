#Simple algorithm for player rating

#Fist lets source the 3.0 Positional Clustering script
source("./Code/3.0 Positional Clustering.R")
rm(clusterSolutionLength,clusterSolutionWidth, avgPassPos)

#Lets then calculate the number of:
  # Shots
  # Passes
  # Interceptions
#For all "CM's"
cm=positions %>% filter(position=="CM")

mid=events %>% mutate(Team=tolower(Team)) %>%  
  merge(.,cm %>% select(Team=team, From=passer), by=c("Team", "From")) %>% 
  group_by(Team, From) %>% 
  summarise(shots=sum(ifelse(Type=="SHOT",1,0)),
            passes=sum(ifelse(Type=="PASS"| (Type=="BALL LOST" & Subtype=="INTERCEPTION"),1,0)),
            interceptions=sum(ifelse(Type=="RECOVERY" & Subtype=="INTERCEPTION",1,0))) %>% 
  select(team=Team, everything())

#From the tracking file we can quickly calculate mins, this might be possiable from events such as subsitutions and red cards

mins=readRDS("./Input/metrica_tracking_tidy.rds") %>% 
  filter(game_id==1 & !is.nan(x)) %>% group_by(team, player) %>% 
  summarise(count=n()) %>% 
  mutate(From=paste0("Player",player),mins=count/25/60) %>% 
  select(team, From, mins)

#Lest adjust the CM events by mins played and normalize them
midAdj=merge(mid, mins, by=c("team", "From")) %>% 
  mutate_at(c("shots","passes","interceptions"), ~ (.)/mins) %>% 
  mutate_at(c("shots","passes","interceptions"), ~ scale(.))


#A simple (and bad) algorithm for player score can now be calculated:
  # First we define some weights
wS=1
wP=1
wI=1

  #Then we calculate a weighted score for each player
score=midAdj %>% mutate(matchScore=as.numeric((wS*shots+wP*passes+wI*interceptions)/(wS+wP+wI)))

#Lest look at the "best" players:
score %>% arrange(-matchScore) %>% select(team, From, matchScore)

#Now lets split the score in 2 parts, offensive and defensive and plot them on a simple scatter plot
scores=midAdj %>% 
  mutate(offScore=as.numeric((wS*shots+wP*passes)/(wS+wP)),
         defScore=(wI*interceptions)/(wI))

s=ggplot()+
  geom_point(data=scores, aes(x=defScore, y=offScore, col=as.factor(team), label=From), size=3)+
  geom_segment(data=scores, aes(x=0,xend=0,y=-2.5,yend=2.5), linetype=2)+
  geom_segment(data=scores, aes(x=2.5,xend=-2.5,y=0,yend=0), linetype=2)+
  scale_colour_manual(values = c("steelblue", "red"), guide = FALSE)+
  geom_text(aes(x=2.5,y=-2.0, label="Away Team"), col="steelblue")+
  geom_text(aes(x=2.5,y=-2.2, label="Home Team"), col="red")+
  theme_minimal()+
  xlim(-2.5,2.5)+
  ylim(-2.5,2.5)
s

plotly::ggplotly(s)
