murray<-bdat%>%
  filter(batterid =="murre001")%>%
  select(H, AB, Rdate)%>%
  arrange(Rdate)%>%
  mutate(tot_H=cumsum(H),tot_AB=cumsum(AB))%>%
  mutate(AVG=round(tot_H/tot_AB,3))%>%
  mutate(player="Eddie Murray")