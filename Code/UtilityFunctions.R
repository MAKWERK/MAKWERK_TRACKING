library(tidyverse)
library(ggforce)

createOutline <- function(){
  o=ggplot()+
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

