# 03_MutateData.Rmd

library(fishWiDNR)   # for setDBClasses(), changeDBNames()
library(dplyr)       # for filter(), select(), mutate(), rename()
library(lubridate)   # for month()
library(FSA)         # for capFirst(), expandCounts()

setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData/Washburn_Test")
d <- read.csv("WASHBURN_fish_raw_data.csv",stringsAsFactors=FALSE,na.strings=c("-","NA",""))
d <- setDBClasses(d,type="RDNR")
d <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
d1 <- filter(d,Species=="LAKE STURGEON",Waterbody.Name=="TREGO LAKE",!is.na(Weight.Pounds))
d1 <- select(d1,Species,Survey.Year,Survey.Begin.Date,Len,Weight.Pounds)
head(d1,n=4)

tmp <- mutate(d1,loglen=log(Len),logwt=log(Weight.Pounds))
head(tmp,n=4)

tmp <- mutate(d1,mon1=month(Survey.Begin.Date),
                  mon2=month(Survey.Begin.Date,label=TRUE))
head(tmp,n=4)

tmp <- mutate(d1,Species1=capFirst(Species),
                  Species2=capFirst(Species,which="first"))
head(tmp,n=4)

tmp <- mutate(d1,lcat2=lencat(Len,w=2),
                  lcat2a=lencat(Len,w=2,as.fact=TRUE),
                  lcatA=lencat(Len,breaks=c(0,46,54,56,58,70)),
                  lcatB=lencat(Len,breaks=c(small=0,medium=50,large=60,very_large=70),use.names=TRUE) )
head(tmp)
xtabs(~lcat2,data=tmp)
xtabs(~lcat2a,data=tmp)
xtabs(~lcatA,data=tmp)
xtabs(~lcatB,data=tmp)

tmp <- rename(d1,year=Survey.Year,wt=Weight.Pounds)
head(tmp)

tmp <- changeDBNames(d1)
names(tmp)

tmp <- changeDBNames(tmp,from="R",to="RDNR")
names(tmp)

tmp <- changeDBNames(tmp,from="RDNR",to="DNR")
write.csv(tmp,"LKS_Trego.csv",row.names=FALSE)

