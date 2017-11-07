bdat<-inner_join(events,days,by=c("gameid"))%>%
  mutate(HR = as.numeric(result==23))%>%
  mutate(year = substr(date,1,4))%>%
  mutate(month = substr(date,5,6))%>%
  mutate(day = substr(date,7,8))%>%
  mutate(Rdate = as.Date(paste(year,month,day,sep = "-")))

luzinski<-bdat%>%
  filter(batterid=="luzig001")%>%
  select(HR,Rdate)%>%
  arrange(Rdate)%>%
  mutate(tot_HR=cumsum(HR))%>%
  mutate(name="Greg Luzinski")

schmidt<-bdat%>%
  filter(batterid=="schmm001")%>%
  select(HR,Rdate)%>%
  arrange(Rdate)%>%
  mutate(tot_HR=cumsum(HR))%>%
  mutate(name = "Mike Schmidt")

ggplot()+
  geom_line(data=luzinski,aes(x=Rdate,y=tot_HR, color = name))+
  geom_line(data=schmidt, aes(x=Rdate,y=tot_HR, color = name))+
  xlab("date")+
  ylab("home runs")+
  ggtitle("Schmidt/Luzinski 1975 home run race")

# generating player info for all gamesin 1990
# bdat has columns of gameid, batterid,result,AB,date,year,month,dat,rdate,H


bdat<-inner_join(events,days, by=c("gameid"))%>%
  mutate(year=substr(date,1,4))%>%
  mutate(month=substr(date,5,6))%>%
  mutate(day=substr(date,7,8))%>%
  mutate(Rdate =as.Date(paste(year,month,day,sep="-")))%>%
  mutate(H=as.numeric(result>=20 & result<=23))%>%
  mutate(AB=as.numeric(AB))
  
# use this to generate a specific batter by batterid using filter
#this is used for lenny dykstra
dykstra<-bdat%>%
  filter(batterid =="dyksl001")%>%
  select(H, AB, Rdate)%>%
  arrange(Rdate)%>%
  mutate(tot_H=cumsum(H),tot_AB=cumsum(AB))%>%
  mutate(AVG=round(tot_H/tot_AB,3))%>%
  mutate(player="Lenny Dykstra")
