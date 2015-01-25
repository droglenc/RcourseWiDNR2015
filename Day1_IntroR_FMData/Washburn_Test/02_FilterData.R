# 02_FilterData.Rmd

library(fishWiDNR)   # for setDBClasses()
library(dplyr)       # for select(), filter()
library(FSA)         # for Summarize(), expandCounts()

setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData/Washburn_Test")
d <- read.csv("WASHBURN_fish_raw_data.csv",stringsAsFactors=FALSE,na.strings=c("-","NA",""))
d <- setDBClasses(d,type="RDNR")
d <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
names(d)

d1 <- select(d,Waterbody.Name,Gear,Survey.Year,Species,Len,Weight.Pounds,Gender,Mark.Given)
headtail(d1)

tmp <- select(d,County:Swims.Station.Id)
headtail(tmp)

tmp <- select(d,-(Station.Name:Status.Code))
headtail(tmp)

tmp <- select(d,starts_with("Length"))                        # there is also an ends_with
names(tmp)

tmp <- select(d,Survey.Seq.No,Species,Len,contains("Mark"))
headtail(tmp)

levels(d1$Gear)
xtabs(~Gear,data=d1)

xtabs(~Waterbody.Name+Gear,data=d1)                           # only partial results shown

tmp <- filter(d1,Waterbody.Name=="TOTAGATIC RIVER")
xtabs(~Waterbody.Name,data=tmp)                               # only partial results shown

tmp <- droplevels(tmp)
xtabs(~Waterbody.Name,data=tmp)

tmp <- filter(d1,Waterbody.Name %in% c("TOTAGATIC RIVER","SPRING LAKE"))
tmp <- droplevels(tmp)
xtabs(~Waterbody.Name,data=tmp)

LCblg <- filter(d1,Waterbody.Name=="SPRING LAKE",Species=="BLUEGILL")
LCblg <- droplevels(LCblg)
xtabs(~Gear,data=LCblg)

LCblg <- filter(LCblg,Gear=="BOOM SHOCKER")
Summarize(~Len,data=LCblg,digits=2)

LCblgPREF <- filter(LCblg,Len>=3)
Summarize(~Len,data=LCblgPREF,digits=2)

sturgWts <- filter(d1,Species=="LAKE STURGEON",!is.na(Weight.Pounds))
headtail(sturgWts)

