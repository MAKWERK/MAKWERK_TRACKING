createOutline <- function(theme="classic"){
  o=ggplot()+
    #Touch and goal lines
    geom_segment(aes(x=0,xend=1,y=0,yend=0))+
    geom_segment(aes(x=0,xend=0,y=0,yend=1))+
    geom_segment(aes(x=0,xend=1,y=1,yend=1))+
    geom_segment(aes(x=1,xend=1,y=0,yend=1))+
    #Halfway line and kickoff spot
    geom_segment(aes(x=0.5,xend=0.5,y=0,yend=1))+
    geom_point(aes(0.5,0.5))+
    #Boxes
    geom_segment(aes(x=0,xend=16.5/105,y=0.5-(40.32/2)/68,yend=0.5-(40.32/2)/68))+
    geom_segment(aes(x=0,xend=16.5/105,y=0.5+(40.32/2)/68,yend=0.5+(40.32/2)/68))+
    geom_segment(aes(x=16.5/105,xend=16.5/105,y=0.5-(40.32/2)/68,yend=0.5+(40.32/2)/68))+
    geom_segment(aes(x=1,xend=1-16.5/105,y=0.5-(40.32/2)/68,yend=0.5-(40.32/2)/68))+
    geom_segment(aes(x=1,xend=1-16.5/105,y=0.5+(40.32/2)/68,yend=0.5+(40.32/2)/68))+
    geom_segment(aes(x=1-16.5/105,xend=1-16.5/105,y=0.5-(40.32/2)/68,yend=0.5+(40.32/2)/68))+
    #Small boxes
    geom_segment(aes(x=0,xend=5.5/105,y=0.5-(18.32/2)/68,yend=0.5-(18.32/2)/68))+
    geom_segment(aes(x=0,xend=5.5/105,y=0.5+(18.32/2)/68,yend=0.5+(18.32/2)/68))+
    geom_segment(aes(x=5.5/105,xend=5.5/105,y=0.5-(18.32/2)/68,yend=0.5+(18.32/2)/68))+
    geom_segment(aes(x=1,xend=1-5.5/105,y=0.5-(18.32/2)/68,yend=0.5-(18.32/2)/68))+
    geom_segment(aes(x=1,xend=1-5.5/105,y=0.5+(18.32/2)/68,yend=0.5+(18.32/2)/68))+
    geom_segment(aes(x=1-5.5/105,xend=1-5.5/105,y=0.5-(18.32/2)/68,yend=0.5+(18.32/2)/68))+
    #Goals
    geom_segment(aes(x=-(2.44/105),xend=-(2.44/105),y=0.5-(7.32/2)/68), yend=0.5+(7.32/2)/68)+
    geom_segment(aes(x=0,xend=-(2.44/105),y=0.5+(7.32/2)/68), yend=0.5+(7.32/2)/68)+
    geom_segment(aes(x=0,xend=-(2.44/105),y=0.5-(7.32/2)/68), yend=0.5-(7.32/2)/68)+
    geom_segment(aes(x=1+(2.44/105),xend=1+(2.44/105),y=0.5-(7.32/2)/68), yend=0.5+(7.32/2)/68)+
    geom_segment(aes(x=1,xend=1+(2.44/105),y=0.5+(7.32/2)/68), yend=0.5+(7.32/2)/68)+
    geom_segment(aes(x=1,xend=1+(2.44/105),y=0.5-(7.32/2)/68), yend=0.5-(7.32/2)/68)+
    #Void the theme
    theme_void()
  return(o)
  }
