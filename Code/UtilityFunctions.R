library(tidyverse)
library(ggforce)
#library(signal)

createOutline <- function(){
  o=ggplot()+
    xlim(-10,115)+
    ylim(-10,78)+
    #Touch and goal lines
    geom_segment(aes(x=0,xend=105,y=0,yend=0))+
    geom_segment(aes(x=0,xend=0,y=0,yend=68))+
    geom_segment(aes(x=0,xend=105,y=68,yend=68))+
    geom_segment(aes(x=105,xend=105,y=0,yend=68))+
    #Halfway line and kickoff spot
    geom_segment(aes(x=52.5,xend=52.5,y=0,yend=68))+
    geom_point(aes(52.5,34))+
    #Boxes
    geom_segment(aes(x=0,xend=16.5,y=34-(40.32/2),yend=34-(40.32/2)))+
    geom_segment(aes(x=0,xend=16.5,y=34+(40.32/2),yend=34+(40.32/2)))+
    geom_segment(aes(x=16.5,xend=16.5,y=34-(40.32/2),yend=34+(40.32/2)))+
    geom_segment(aes(x=105,xend=105-16.5,y=34-(40.32/2),yend=34-(40.32/2)))+
    geom_segment(aes(x=105,xend=105-16.5,y=34+(40.32/2),yend=34+(40.32/2)))+
    geom_segment(aes(x=105-16.5,xend=105-16.5,y=34-(40.32/2),yend=34+(40.32/2)))+
    #Small boxes
    geom_segment(aes(x=0,xend=5.5,y=34-(18.32/2),yend=34-(18.32/2)))+
    geom_segment(aes(x=0,xend=5.5,y=34+(18.32/2),yend=34+(18.32/2)))+
    geom_segment(aes(x=5.5,xend=5.5,y=34-(18.32/2),yend=34+(18.32/2)))+
    geom_segment(aes(x=105,xend=105-5.5,y=34-(18.32/2),yend=34-(18.32/2)))+
    geom_segment(aes(x=105,xend=105-5.5,y=34+(18.32/2),yend=34+(18.32/2)))+
    geom_segment(aes(x=105-5.5,xend=105-5.5,y=34-(18.32/2),yend=34+(18.32/2)))+
    #Goals
    geom_segment(aes(x=-(2.44),xend=-(2.44),y=34-(7.32/2)), yend=34+(7.32/2))+
    geom_segment(aes(x=0,xend=-(2.44),y=34+(7.32/2)), yend=34+(7.32/2))+
    geom_segment(aes(x=0,xend=-(2.44),y=34-(7.32/2)), yend=34-(7.32/2))+
    geom_segment(aes(x=105+(2.44),xend=105+(2.44),y=34-(7.32/2)), yend=34+(7.32/2))+
    geom_segment(aes(x=105,xend=105+(2.44),y=34+(7.32/2)), yend=34+(7.32/2))+
    geom_segment(aes(x=105,xend=105+(2.44),y=34-(7.32/2)), yend=34-(7.32/2))+
    #Penalty spots
    geom_point(aes(11,34), col="grey20")+
    geom_point(aes(105-11,34), col="grey20")+
    #Circles
    geom_circle(aes(x0=52.5,y0=34, r=9.15))+
    annotate("path",
             x = (0 + 11) + 9.15 * cos(seq(-0.295*pi, 0.295*pi, length.out = 300)),
             y = 34+11 * sin(seq(-0.295*pi, 0.295*pi, length.out = 300))) +
    annotate("path",
             x = (105-11) - 9.15 * cos(seq(-0.295*pi, 0.295*pi, length.out = 300)),
             y = 34-11 * sin(seq(-0.295*pi, 0.295*pi, length.out = 300))) +
    #Void the theme & coord_fixed
    coord_fixed()+
    theme_void()
  return(o)
}

addVelocitiesRaw <- function(data,dim1,dim2,timeDiff=0.4){
  vel=data %>% group_by(player, period) %>% mutate(vx=x-lag(x)/timeDiff, vy=y-lag(y)/timeDiff, speed=sqrt(vx^2+vy^2)) %>% ungroup() %>% select(vx,vy, speed)
  data$vx=vel$vx
  data$vy=vel$vy
  data$speed=vel$speed
  return(data)
  #Need to implement player and period coloum arguments, aswell as dynamic time difference
}

addVelocitiesSGF <- function(data,dim1,dim2, window=7,polyorder=1,timeDiff=0.4){
  #paramters chosen to match Laurie from FOT
  vel=data %>% group_by(player, period) %>% 
    mutate(x=signal::sgolayfilt(x,p=polyorder,n=window),y=sgolayfilt(y,p=polyorder,n=window)) %>% 
    mutate(vx=x-lag(x)/timeDiff, vy=y-lag(y)/timeDiff, speed=sqrt(vx^2+vy^2)) %>% ungroup() %>% select(vx,vy, speed)
  data$vx=vel$vx
  data$vy=vel$vy
  data$speed=vel$speed
  return(data)
  #Need to implement player and period coloum arguments, aswell as dynamic time difference
}

