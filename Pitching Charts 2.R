nh<-nh%>%
  arrange(tfs)

temp<-nh%>%
  group_by(num)%>%
  summarize(num_of_pitches=n())

lapply(temp$num_of_pitches,seq)
 
nh$pitch_enum<-unlist(lapply(temp$num_of_pitches,seq))

batter="Pedro Alvarez"
inning = 5
ab<-nh%>%filter(batter_name == batter,inning.x == inning)

ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=ab,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red","blue","green","purple","black"))+
  geom_text(data=ab,aes(label = stand,x=stand_xcoord), y=2.5,size=10)+
  xlim(-2,2)+
  ylim(0,4.5)+
  ggtitle(paste("Inning ",inning,": ",batter,sep=""))+
  geom_text(data=ab,aes(label=des2,x=px,y=pz),vjust=2)+
  geom_text(data=ab,aes(label=pitch_enum,x=px,y=pz),vjust=3)



      batter="Jose Tabata"
      inning = 9
      
      ab<-nh%>%filter(batter_name == batter,inning.x == inning)
      
      plot<-ggplot()+
        geom_path(data=sz,aes(x=x,y=z))+
        coord_equal()+
        xlab("feet from home plate")+
        ylab("feet above ground")+
        geom_point(data=ab,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
        scale_size(range=c(2,5))+
        scale_color_manual(values=c("red","blue","green","purple","black"))+
        geom_text(data=ab,aes(label = stand,x=stand_xcoord), y=2.5,size=10)+
        xlim(-2,2)+
        ylim(0,4.5)+
        ggtitle(paste("Inning ",inning,": ",batter,sep=""))+
        geom_text(data=ab,aes(label=des2,x=px,y=pz),vjust=2)+
        geom_text(data=ab,aes(label=pitch_enum,x=px,y=pz),vjust=3)
      
      ggsave("plot.png",plot)

unique(nh$num)

colors<-c("red","blue","green","purple","black")
names(colors)<-c("fastball","changeup","slider","curveball","cutter")

for(i in unique(nh$num)){
  
      ab<-nh%>%filter(num==i)
      
      batter=ab$batter_name[1]
      inning=ab$inning.x[1]
      pitches=unique(ab$pitch_description)
      
      zmax<-(max(ab$start_speed)-75.4)/22
      zmin<-(max(ab$start_speed)-75.4)/22
      
      plot<-ggplot()+
        geom_path(data=sz,aes(x=x,y=z))+
        coord_equal()+
        xlab("feet from home plate")+
        ylab("feet above ground")+
        geom_point(data=ab,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
        scale_size(range=c(3*zmin+2,3*zmax+2))+
        scale_color_manual(values=colors[pitches])+
        geom_text(data=ab,aes(label = stand,x=stand_xcoord), y=2.5,size=10)+
        xlim(-2,2)+
        ylim(0,5)+
        ggtitle(paste("Inning ",inning,": ",batter,sep=""))+
        geom_text(data=ab,aes(label=des2,x=px,y=pz),vjust=2)+
        geom_text(data=ab,aes(label=pitch_enum,x=px,y=pz),vjust=3)
      
      ggsave(paste("atbat", i, ".png",sep=""),plot)
      
      
}


