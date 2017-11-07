nh<-inner_join(atbat,pitch,by="num")%>%
  filter(inning_side.x=="top")%>%
  select(num,start_tfs,stand,event,inning.x,batter_name,des,tfs,start_speed,px,pz,pitch_type)

#X-coordinates for an average strikezone
x<-c(-0.95,0.95,0.95,-0.95,-0.95)
#y-coordinates for an average strikezone
z<-c(1.6,1.6,3.5,3.5,1.6)

ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=nh,aes(x=px,y=pz,size=start_speed,color=pitch_type))+
  scale_size(range=c(1,7))

#remove column pitch type and store it in a temp variable
temp<-nh$pitch_type

#which func compares number which matches a certain value at specified vector
which(temp=="FF") #read out all numbers
temp[which(temp=="FF")] #read out all of the value in quotes (in this case "FF")
#replace "FF" with "fastball"
temp[which(temp=="FF")]<-"fastball"
#replace names of other pitches
temp[which(temp=="CU")]<-"curveball"
temp[which(temp=="CH")]<-"changeup"
temp[which(temp=="SL")]<-"slider"
temp[which(temp=="FC")]<-"cutter"

ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=nh,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
  scale_size(range=c(1,7))

#scaling color
#scale_color_hue(h=c(start,end)) start and end are between 0 and 360 sets color range and picks point evenly spaced between start and end-> 
#red = 0-30,orange = 30-60,yellow = 60-90,greens = 90-180, blues = 180-270, purple = 270-300, pinks =300-360
#ex: scale_color_hue(h=c(0,90)) for 3 points this would pick a reddish an orangish and a yellowish color
#scale_color_hue(c=x) x is between 0 and 100 -- 0 is 0% saturation = gray, 100 is 100% saturation = full color
#ex: scale_color_hue(c=50) this would make the pionts 50% dull in color
#scale_color_hue(l=x) x is between 0 and 100 -- 0 = black, 100 = a very white-ish shade of the color ex:red at 100 = light pink

#scale_color_brewer(palette="Dark2") sets colors based on a predefined R-palette

#scale_color_manual(values=("red","blue","green","yellow", "black"))

ggplot()+
       geom_path(data=sz,aes(x=x,y=z))+
       coord_equal()+
       xlab("feet from home plate")+
       ylab("feet above ground")+
       geom_point(data=nh,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
       scale_size(range=c(2,5))+
       scale_color_manual(values=c("red","blue","green","purple","black"))

#factoring 
#str(nh$pitch_description) shows structure of nh column pitch_description 
#structure is essentially data type, name, and length(i.e. 1:105)

#nh$pitch_description<-factor(nh$pitch_description)
#turns the original character strings of the column pitch_description into factors
#and assigns levels i.e. 5 items get numbers from 1-5
#nh$pitch_description<-factor(nh$pitch_description,levels=c("fastball", "changeup","slider","curveball", "cutter"))

ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=nh,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red","blue","green","purple","black"))+
  facet_wrap(~stand)+
  geom_text(data=nh,aes(label = stand), x=1.5,y=2.5,size=20)

stand_xcoord[which(stand_xcoord=="L")]<-1.5
stand_xcoord[which(stand_xcoord=="R")]<--1.5
stand_xcoord<-as.numeric(stand_xcoord)
nh$stand_xcoord<-stand_xcoord

ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=nh,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red","blue","green","purple","black"))+
  facet_wrap(~stand)+
  geom_text(data=nh,aes(label = stand,x=stand_xcoord), y=2.5,size=20)

nh$stand<-factor(nh$stand,levels=c("R","L"))

#pitch plot by atbat
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=nh,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red","blue","green","purple","black"))+
  facet_wrap(~num)+
  geom_text(data=nh,aes(label = stand,x=stand_xcoord), y=2.5,size=9)

#add batter name label
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=nh,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red","blue","green","purple","black"))+
  facet_wrap(~num)+
  geom_text(data=nh,aes(label = stand,x=stand_xcoord), y=2.5,size=9)+
  geom_text(data=nh,aes(label=batter_name),x=0,y=0.5, size = 3)

#add inning label
ggplot()+
  geom_path(data=sz,aes(x=x,y=z))+
  coord_equal()+
  xlab("feet from home plate")+
  ylab("feet above ground")+
  geom_point(data=nh,aes(x=px,y=pz,size=start_speed,color=pitch_description))+
  scale_size(range=c(2,5))+
  scale_color_manual(values=c("red","blue","green","purple","black"))+
  facet_wrap(~num)+
  geom_text(data=nh,aes(label = stand,x=stand_xcoord), y=2.5,size=7.5)+
  geom_text(data=nh,aes(label=batter_name),x=0,y=0.5, size = 3)+
  geom_text(data=nh,aes(label=inning.x),x=0, y=4.5,size=3)


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
  geom_text(data=ab,aes(label=des,x=px,y=pz),vjust=2)

des<-nh$des
event<-nh$event
indices<-which(des=="In play, out(s)")
des[indices]<-event[indices]

nh$des2<-des

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
  geom_text(data=ab,aes(label=des2,x=px,y=pz),vjust=2)




