library(readxl)
stayathome<-read.csv('Stayathome.csv')
cases<-read.csv("us-states.csv")
stayathome$starts<-as.Date(stayathome$Start,format = "%d/%m/%y")
stayathome$ends<-as.Date(stayathome$End,format = "%d/%m/%y")  
#install.packages("dplyr")
library(dplyr)
df1<-stayathome
df1<-df1[complete.cases(df1), ]

df2<-df1 %>%
  rowwise() %>% 
  do(data.frame(.[1], date = seq(as.Date(.$starts), as.Date(.$ends), by = "1 day")))

df2$isstayhome=1



cases$date<-as.Date(cases$date)
colnames(df2)=c("state","date","isstayhome")





df3<-merge(x = cases, y = df2, by = c("state","date"), all.x = TRUE)
df3$isstayhome[is.na(df3$isstayhome)] <- 0

df4<-df3[df3$date>as.Date("03-20-2020",format = "%m-%d-%y"),]

library(plm)



df3_updated = read.csv("df4 (1).csv", stringsAsFactors = FALSE)

df3_updated
#date subset >20 March  2020
df4 = subset(df3_updated, as.Date(df3_updated$date, format = "%m/%d/%y") > as.Date("03/20/2020", format = "%m/%d/%y"))
df4 = subset(df4, as.Date(df4$date, format = "%m/%d/%y") < as.Date("06/5/2020", format = "%m/%d/%y"))
head(df4)
#df4$counts = df4$counts + 0.001 
library(plm)
df4 = subset(df4, df4$counts != 0)
df4$state = as.factor(df4$state)
df4$date = as.Date(df4$date, format = "%m/%d/%y")
df4$date = as.factor(df4$date)
df4
reg_1 =  lm(percent.change.offset ~ isstayhome + state, data = df4)
summary(reg_1)



 
