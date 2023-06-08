library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
final<-data.frame()

for(i in c("COOK","BRONX","SAN DIEGO","MIAMI-DADE")){
cdc_data<-read.csv(paste(i,".csv",sep=""))

cdc_data<-cdc_data %>%
  mutate(year=year(ymd(paste(case_month,"-01"))),
         month=month(ymd(paste(case_month,"-01"))))%>%
  filter(ethnicity=="Hispanic/Latino",
         age_group %in% c("50 to 64 years","18 to 49 years","65+ years")) %>%
  select(c("year","month"))

cdc_month<- cdc_data %>%
  group_by(year,month)%>%
  summarise(case=n()) %>%
  mutate(county=i)

final<-rbind(final,cdc_month)
}
final<-final%>%
  mutate(date=as.Date(paste(year,month,1,sep="-"),format="%Y-%m-%d"))

write.csv(final,file="month_wise_cases_per_county_exclusive.csv")

final%>%
  ggplot()+
  geom_col(aes(x=date,y=case))+
  facet_wrap(~county,scale="free_y")


  