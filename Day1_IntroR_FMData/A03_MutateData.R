# 03_MutateData.Rmd

# load needed packages
library(fishWiDNR)   # for setDBClasses(), changeDBNames()
library(dplyr)       # for filter(), select(), mutate(), rename()
library(lubridate)   # for month()
library(FSA)         # for expandCounts(), capFirst(), filterD()

# load FM data and expand lengths ... mostly copied code from first and second handouts
setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("SAWYER_fish_raw_data_012915.csv",stringsAsFactors=FALSE,na.strings=c("-","NA",""))
d <- setDBClasses(d,type="RDNR")
d <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")

d1 <- filterD(d,Species=="LAKE STURGEON",Waterbody.Name=="BARKER LAKE",!is.na(Weight.Pounds))
d1 <- select(d1,Species,Survey.Year,Survey.Begin.Date,Len,Weight.Pounds)
headtail(d1,n=2)

tmp <- mutate(d1,loglen=log(Len),logwt=log(Weight.Pounds))
headtail(tmp,n=2)

tmp <- mutate(d1,mon1=month(Survey.Begin.Date),
                  mon2=month(Survey.Begin.Date,label=TRUE))
headtail(tmp,n=2)

tmp <- mutate(d1,Species1=capFirst(Species),
                  Species2=capFirst(Species,which="first"))
headtail(tmp,n=2)

tmp <- mutate(d1,lcat2=lencat(Len,w=2),
                  lcat2a=lencat(Len,w=2,as.fact=TRUE),
                  lcatA=lencat(Len,breaks=c(46,54,56,58,70)),
                  lcatB=lencat(Len,breaks=c(small=0,medium=50,large=60,very_large=70),use.names=TRUE) )
headtail(tmp)

xtabs(~lcat2,data=tmp)
xtabs(~lcat2a,data=tmp)
xtabs(~lcatA,data=tmp)
xtabs(~lcatB,data=tmp)

tmp <- rename(d1,year=Survey.Year,wt=Weight.Pounds)
headtail(tmp)

tmp <- changeDBNames(d1)
names(tmp)

tmp <- changeDBNames(tmp,from="R",to="RDNR")
names(tmp)

tmp <- changeDBNames(tmp,from="RDNR",to="DNR")
names(tmp)
tmp$"Weight Pounds"

write.csv(tmp,"LKS_Barker.csv",row.names=FALSE)
