# 02_FilterData.Rmd

library(fishWiDNR)   # for setDBClasses(), expandCounts()
library(dplyr)       # for select(), filter()
library(FSA)         # for Summarize()

setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("FMDB_Sawyer.csv",stringsAsFactors=FALSE)
d <- setDBClasses(d,type="RDNR")
d <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
names(d)

d1 <- select(d,Waterbody.Name,Gear,Survey.Year,Species,Len,Weight.Pounds,Gender,Mark.Given)
head(d1)
tail(d1)

tmp <- select(d,County:Swims.Station.Id)
head(tmp)

tmp <- select(d,-(Station.Name:Status.Code))
head(tmp)

tmp <- select(d,starts_with("Length"))   # there is also an ends_with
names(tmp)

tmp <- select(d,Srvy.Seq.No,Species,Len,contains("Mark"))
head(tmp)

levels(d1$Gear)
xtabs(~Gear,data=d1)

xtabs(~Waterbody.Name+Gear,data=d1)      # only partial results shown

tmp <- filter(d1,Waterbody.Name=="BARBER LAKE")
xtabs(~Waterbody.Name,tmp)               # only partial results shown

tmp <- droplevels(tmp)
xtabs(~Waterbody.Name,tmp)

tmp <- filter(d1,Waterbody.Name %in% c("BARBER LAKE","LAKE CHETAC"))
tmp <- droplevels(tmp)
xtabs(~Waterbody.Name,tmp)

LCblg <- filter(d1,Waterbody.Name=="LAKE CHETAC",Species=="BLUEGILL")
xtabs(~Gear,LCblg)

LCblg <- filter(LCblg,Gear=="BOOM SHOCKER")
Summarize(~Len,data=LCblg)

LCblgPREF <- filter(LCblg,Len>=7)
Summarize(~Len,data=LCblgPREF)

sturgWts <- filter(d1,Species=="LAKE STURGEON",!is.na(Weight.Pounds))
head(sturgWts)

