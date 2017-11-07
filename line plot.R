ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  geom_point(data=means,aes(x=x,y=y),size=7,color="red")

ggplot()+
  geom_line(data=line,aes(x=x,y=y))+
  geom_point(data=means,aes(x=x,y=y),size=7,color="red")+
  geom_point(data=dat,aes(x=x,y=y))

ggplot()+
  geom_point(data=means,aes(x=x,y=y),color="red")+
  geom_line(data=line,aes(x=x,y=y))+
  scale_x_continuous(limits=c(-10,30))+
  scale_y_continuous(limits=c(-20,80))+
  geom_point(data=dat,aes(x=x,y=y))

ggplot()+
  geom_point(data=father.son,aes(x=fheight,y=sheight))+
  geom_line(data=line,aes(x=x,y=y))
