# 04_SizeStructure_I.Rmd

library(fishWiDNR)   # for setDBClasses()
library(dplyr)       # for filter(), select(), mutate(), group_by(), summarize()
library(FSA)         # for Summarize(), hist(), expandCounts()
library(lubridate)   # for month()

setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("FMDB_Sawyer_MultiYr_APEX.csv",stringsAsFactors=FALSE,na.strings=c("-","NA",""))
d <- setDBClasses(d,type="RDNR")
d <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
d <- mutate(d,Mon=month(Survey.Begin.Date,label=TRUE))
d <- select(d,Species,Waterbody.Name,Survey.Year,Gear,Survey.Begin.Date,Mon,Len)

Spr <- filter(d,Survey.Year==2013,Mon %in% c("Apr","May","Jun"))
Spr <- droplevels(Spr)
BGSpr <- filter(Spr,Species=="BLUEGILL")
BGSpr <- droplevels(BGSpr)
BGSprLC <- filter(BGSpr,Waterbody.Name=="LAKE CHETAC",Gear=="BOOM SHOCKER")
BGSprLC <- droplevels(BGSprLC)

head(BGSprLC)

Summarize(~Len,data=BGSprLC,digits=2)

hist(~Len,data=BGSprLC)

hist(~Len,data=BGSprLC,xlab="Total Length (In.)",ylab="Number of Bluegill",
     xlim=c(3,9),ylim=c(0,80),col="salmon")

hist(~Len,data=BGSprLC,xlab="Total Length (In.)",ylab="Number of Bluegill",
     xlim=c(3,9),ylim=c(0,40),breaks=seq(3,9,0.2))

BGSpr <- group_by(BGSpr,Waterbody.Name)
summarize(BGSpr,n=n(),meanLen=mean(Len))                    # see use of na.rm=TRUE below

summarize(BGSpr,n=n(),valid_n=sum(!is.na(Len)),
           meanLen=mean(Len,na.rm=TRUE),sdLen=sd(Len,na.rm=TRUE),
           minLen=min(Len,na.rm=TRUE),maxLen=max(Len,na.rm=TRUE)  )

BGSpr <- filter(BGSpr,Len>=3)
summarize(BGSpr,n=n(),valid_n=sum(!is.na(Len)),
           meanLen=round(mean(Len,na.rm=TRUE),2),sdLen=round(sd(Len,na.rm=TRUE),2),
           minLen=min(Len,na.rm=TRUE),maxLen=max(Len,na.rm=TRUE),
           PSDQ=perc(Len,6,digits=0),PSD7=perc(Len,7,digits=0),PSDP=perc(Len,8,digits=0)  )

Spr <- group_by(Spr,Waterbody.Name,Species)
summarize(Spr,n=n(),valid_n=sum(!is.na(Len)),
           meanLen=round(mean(Len,na.rm=TRUE),2),sdLen=round(sd(Len,na.rm=TRUE),2)  )

