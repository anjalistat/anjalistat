rm(list=ls())
install.packages("here")
install.packages("tidyr")
install.packages("dplyr")
install.packages("readxl")
library(here)
library(tidyr)
library(dplyr)
library(readxl)
setwd("E:/")
##############Companywise_data#####################

firm_data<-read_xlsx("CLP.xlsx",sheet=1)

#firm_data=firm_data[0:20,]          #5-10 year
#firm_data=firm_data[20:40,]          #5-10 year

#firm_data=firm_data[40:58,]          #10-15 year  
#head(firm_data)

firm_data <- firm_data %>% 
  gather( "comp", "price", -Date)
firm_data <- firm_data %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))
firm_data1 <- firm_data %>%
  group_by(comp) %>% 
 
  mutate(i = seq(0,57*3,by=3))%>%
  mutate(ret = (price/first(price))^(12/i)-1)%>%
  mutate(lowvar=sd(ret[which(ret<=last(ret))],))%>%
  mutate(upvar=sd(ret[which(ret>last(ret))],))

#write.csv(firm_data1,"EPSReturns 0-5.csv")
#write.csv(firm_data1,"EPSReturns 5-10.csv")
write.csv(firm_data1,"CLPReturns.csv")








############RETURN CALCULATION FOR 0-5 YEARS
firm_data<-read_xlsx("CLP.xlsx",sheet=1)
firm_data5=firm_data[1:20,]             #5 year
firm_data5 <- firm_data5 %>% 
  gather( "comp", "price", -Date)
firm_data5 <- firm_data5 %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))
firm_data51 <- firm_data5 %>%
  group_by(comp) %>% 
  
  mutate(i = seq(0,19*3,by=3))%>%
  mutate(ret = (price/first(price))^(12/i)-1)%>%
  mutate(lowvar=var(ret[which(ret<=last(ret))],))%>%
  mutate(upvar=var(ret[which(ret>last(ret))],))

############RETURN CALCULATION FOR 5-10 YEARS
firm_data<-read_xlsx("CLP.xlsx",sheet=1)
firm_data10=firm_data[20:40,]             #5-10 year
firm_data10 <- firm_data10 %>% 
  gather( "comp", "price", -Date)
firm_data10 <- firm_data10 %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))
firm_data101 <- firm_data10 %>%
  group_by(comp) %>% 
  
  mutate(i = seq(0,20*3,by=3))%>%
  mutate(ret = (price/first(price))^(12/i)-1)%>%
  mutate(lowvar=var(ret[which(ret<=last(ret))],))%>%
  mutate(upvar=var(ret[which(ret>last(ret))],))


############RETURN CALCULATION FOR 10-15 YEARS
firm_data<-read_xlsx("CLP.xlsx",sheet=1)
firm_data15=firm_data[40:58,]             #10-15 year
firm_data15 <- firm_data15 %>% 
  gather( "comp", "price", -Date)
firm_data15 <- firm_data15 %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))
firm_data151 <- firm_data15 %>%
  group_by(comp) %>% 
  
  mutate(i = seq(0,18*3,by=3))%>%
  mutate(ret = (price/first(price))^(12/i)-1)%>%
  mutate(lowvar=var(ret[which(ret<=last(ret))],))%>%
  mutate(upvar=var(ret[which(ret>last(ret))],))


firm_data1[,8]= "0-15y"
firm_data51[,8]= "0-5y"
firm_data101[,8]= "5-10y"
firm_data151[,8]= "10-15y"



return<-rbind(firm_data1,firm_data51,firm_data101,firm_data151)
head(return)
names(return)<-c("Date", "Company","Price", "i", "HPR", "lower variance", "Upper variance", "HP")
############DENSITY PLOT###########

#install.packages("ggplot2")

# Loading packages
library(ggplot2)
library(dplyr)
p <- ggplot(data = return,
            mapping = aes(x = HPR, color = HP))
p + geom_density(alpha = 0.5)+
scale_x_continuous(name = "Holding Period return per holding period",
                   breaks = seq(-1, 2, 0.2),
                   limits=c(-1, 2)) +
  scale_y_continuous(name = "Density") +
  ggtitle("                              Density plot of Return")

MRHP<-aggregate(HPR ~ HP, data= return, function(x) c(mean = mean(x), sd = sd(x)))  # mean and sd of HPR by HP

#write.csv(firm_data1,"EPSRetur  10-15.csv")
###########################Lower and Upper Variance####################
Stat.Ret<-aggregate(firm_data1,by=list(firm_data1$comp),FUN=last)
Stat.Ret=Stat.Ret[,c(-2:-5)]
names(Stat.Ret)=c("Company", "Last Return", "Lower Variance", "Upper Variance")
write.csv(Stat.Ret,"Return Statistics.csv")

##################market Return############

mkt_data<-read_xlsx("mkt.xlsx",sheet=1)
mkt_data <- mkt_data %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))
#mkt_data=mkt_data[1:20,]




mkt_data1 <- mkt_data %>%
  mutate(i = seq(0,57*3,by=3))%>%
  mutate(ret = (Price/first(Price))^(12/i)-1)
mkt_data11=mkt_data1[,c(1,4)]
names(mkt_data11)=c("Date","mkt_Return")





#####################Reshpae data as ss######################

re_data=firm_data1[,c(1,2,5)]
attach(re_data)
names(re_data)=c("Date","Company","Ri_Return")
head(re_data)

long33 <- data.frame(Date=re_data$Date ,Company = re_data$Company ,
                     Ri_Return= re_data$Ri_Return)
which(long33$Ri_Return=="NA")
head(long33)

Ri=reshape(long33,timevar = "Company",idvar ="Date" ,direction = "wide") 
# used to calculate beta's

