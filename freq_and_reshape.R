#########_Frequency Distribution_##############
breaks1=c(-1,-0.2,-0.1,0, 0.25, 0.40, 0.55, 0.70, 0.85, 1.00, 1.15, 166) #seq(-0.2,0.1,0.1)    # half-integer sequence -0.25,1.15, by=0.15
breaks1
#breaks1=c(-1,breaks1,max(firm_data1$ret))
duration.cut1 = data.frame(cut(firm_data1$ret, breaks1))
table(duration.cut1)
#which(duration.cut1==0)
names(duration.cut1)=c("classes")#,"Return"
duration.freq1 = table(duration.cut1)
duration.freq1

#which(duration.freq1=="NA")
#freq=cbind(duration.freq1)

firm_data1=cbind(firm_data1,duration.cut1)
head(firm_data1)
firm_data2=firm_data1[,c(1,2,6)]
names(firm_data2)=c("Dates","Company","Return_classes")
head(firm_data2)

#firm_data2=read.csv("Return_classes.csv")
#reshape#expand.grid(Name = c("Dora", "John", "Rob")

############_Reshape data_####################
############_company_######################

long3 <- data.frame(Date=firm_data2$Dates,Company = firm_data2$Company,
                    Return_classes = firm_data2$Return_classes)
which(long3$Return_classes=="NA")
head(long3)
long3$Return_classes=recode(long3$Return_classes, '(-1,-0.2]' ="A",'(-0.2,-0.1]' = "B", '(-0.1,0]'= "C",'(0,0.25]'= "D",'(0.25,0.4]'="E",'(0.4,0.55]'="F",'(0.55,0.7]'="G",'(0.7,0.85]'="H",'(0.85,1]'="I",'(1,1.15]'="J",'(1.15,166]'="K") ###,'(0.8,166]'="L")#,'(0.9,1]'="9",'(1,166]'="10")

reshape_data=reshape(long3,timevar = "Company",idvar ="Date" ,direction = "wide")





