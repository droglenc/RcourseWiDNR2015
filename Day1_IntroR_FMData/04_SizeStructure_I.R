# 04_SizeStructure_I.Rmd

library(fishWiDNR)   # for setDBClasses(), expandCounts()
library(dplyr)       # for filter(), select(), mutate(), group_by(), summarize()
library(FSA)         # for Summarize(), hist()
library(lubridate)   # for month()

setwd("C:/aaaWork/Web/fishR/Courses/WIStatewide2015/CourseMaterial")
d <- read.csv("FMDB_Sawyer.csv",stringsAsFactors=FALSE)
d <- setDBClasses(d,type="RDNR")
d <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
d <- mutate(d,Mon=month(Survey.Begin.Date,label=TRUE))
d <- select(d,Species,Waterbody.Name,Survey.Year,Gear,Survey.Begin.Date,Mon,Len)

Spr13 <- filter(d,Survey.Year==2013,Mon %in% c("Apr","May","Jun"))
BGSpr13 <- filter(Spr13,Species=="BLUEGILL")
BGSpr13LC <- filter(BGSpr13,Waterbody.Name=="LAKE CHETAC",Gear=="BOOM SHOCKER")

head(BGSpr13LC)

Summarize(~Len,data=BGSpr13LC,digits=2)

hist(~Len,data=BGSpr13LC)

hist(~Len,data=BGSpr13LC,xlab="Total Length (In.)",ylab="Number of Bluegill",
     xlim=c(3,9),ylim=c(0,80),col="salmon")

hist(~Len,data=BGSpr13LC,xlab="Total Length (In.)",ylab="Number of Bluegill",
     xlim=c(3,9),ylim=c(0,40),breaks=seq(3,9,0.2))

BGSpr13 <- droplevels(BGSpr13)
BGSpr13 <- group_by(BGSpr13,Waterbody.Name)

summarize(BGSpr13,n=n(),meanLen=mean(Len))

summarize(BGSpr13,n=n(),valid_n=length(Len[!is.na(Len)]),
           meanLen=mean(Len,na.rm=TRUE),sdLen=sd(Len,na.rm=TRUE),
           minLen=min(Len,na.rm=TRUE),maxLen=max(Len,na.rm=TRUE)
          )

BGSpr13 <- filter(BGSpr13,Len>=3)
summarize(BGSpr13,n=n(),valid_n=length(Len[!is.na(Len)]),
           meanLen=round(mean(Len,na.rm=TRUE),2),sdLen=round(sd(Len,na.rm=TRUE),2),
           minLen=min(Len,na.rm=TRUE),maxLen=max(Len,na.rm=TRUE),
           PSDQ=perc(Len,6,digits=0),PSD7=perc(Len,7,digits=0),PSDP=perc(Len,8,digits=0)
          )

Spr13 <- group_by(Spr13,Waterbody.Name,Species)
summarize(Spr13,n=n(),valid_n=length(Len[!is.na(Len)]),
           meanLen=round(mean(Len,na.rm=TRUE),2),sdLen=round(sd(Len,na.rm=TRUE),2)
          )

