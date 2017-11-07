ggplot()+
  geom_line(data=dykstra,aes(x=Rdate,y=AVG,color=player))+
  geom_line(data=murray,aes(x=Rdate,y=AVG,color=player))+
  geom_line(data=brett,aes(x=Rdate,y=AVG,color=player))+
  xlab("date")+
  ylab("batting average")+
  ggtitle("Batting Average Leaders, 1990")+
  geom_line(data=four_hundred,aes(x=date,y=AVG), size=2)


  