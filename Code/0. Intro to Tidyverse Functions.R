#Intro to tidyverse functions

library(tidyverse)

events=read.csv("./Input/Sample_Game_1_RawEventsData.csv")

#Lets count the total number of successful passes!

passes=events %>% 
  filter(Type=="PASS") %>% 
  summarise(count=n())

#Code can be read as:
  # Created data frame named "passes"
  # Do this by "summarizing" then number of rows (n())
  # For all rows in "events" where "Type" is "PASS"
#OR simply read backwards
  # Count the rows where Type is PASS in data frame events and save it in a data frame named passes

rm(passes)


#Lets count the successful passes for each player!

passesPerPlayer=events %>% 
  filter(Type=="PASS") %>% 
  group_by(Team, From) %>% 
  summarise(count=n())

#Breaking down the code again:
  # Count the rows for each value of "Team" and "From" where Type is PASS in data frame events and save it in a data frame named passesPerPlayer

rm(passesPerPlayer)


#Lets calculated the accuracy for each player!

passingAccuracy=events %>% 
  filter(Type=="PASS" | (Type=="BALL LOST" & Subtype=="INTERCEPTION")) %>% 
  group_by(Team,From) %>% 
  summarise(passes=n(),successful=sum(ifelse(Type=="PASS",1,0))) %>% 
  mutate(accuracy=successful/passes)

#Lets just break down the new things:
  # Mutate lets you create new variables, here we calculate accuracy by dividing successful passes with the total number of passes made
  # ifelse lets you state the outcome based on a logical test - here we set the value to 1 if Type is PASS else we set it to 0
  # sum lets you, as the name suggest sum values - again here we use group_by so for each value of Team and From we sum up the values created by the ifelse function