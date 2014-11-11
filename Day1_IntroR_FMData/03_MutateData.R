# 03_MutateData.Rmd

library(fishWiDNR)   # for setDBClasses(), changeDBNames(), expandCounts()
library(dplyr)       # for filter(), select(), mutate(), rename()
library(lubridate)   # for month()
library(FSA)         # for capFirst()

setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("FMDB_Sawyer.csv",stringsAsFactors=FALSE)
d <- setDBClasses(d,type="RDNR")
d <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
d1 <- filter(d,Species=="LAKE STURGEON",Waterbody.Name=="BARKER LAKE",!is.na(Weight.Pounds))
d2 <- select(d1,Species,Survey.Year,Survey.Begin.Date,Len,Weight.Pounds)
head(d2)

tmp <- mutate(d2,loglen=log(Len),logwt=log(Weight.Pounds))
head(tmp)

tmp <- mutate(d2,mon1=month(Survey.Begin.Date),
                  mon2=month(Survey.Begin.Date,label=TRUE))
head(tmp)

tmp <- mutate(d2,Species1=capFirst(Species),
                  Species2=capFirst(Species,which="first"))
head(tmp)

tmp <- mutate(d2,lcat2=lencat(Len,w=2),
                  lcat2a=lencat(Len,w=2,as.fact=TRUE),
                  lcatA=lencat(Len,breaks=c(46,54,56,58,70)),
                  lcatB=lencat(Len,breaks=c(small=0,medium=50,large=60,very_large=70),use.names=TRUE)
             )
head(tmp)
xtabs(~lcat2,data=tmp)
xtabs(~lcat2a,data=tmp)
xtabs(~lcatA,data=tmp)
xtabs(~lcatB,data=tmp)

tmp <- rename(d2,year=Survey.Year,wt=Weight.Pounds)
head(tmp)

tmp <- changeDBNames(d1)
names(tmp)

tmp <- changeDBNames(tmp,from="R",to="RDNR")
names(tmp)

tmp <- changeDBNames(tmp,from="RDNR",to="DNR")
write.csv(tmp,"LKS_Barker14.csv",row.names=FALSE)

