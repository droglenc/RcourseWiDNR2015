# 02_FilterData.Rmd

# load needed packages
library(fishWiDNR)   # for setDBClasses()
library(dplyr)       # for select(), filter()
library(FSA)         # for expandCounts(), Summarize(), filterD()

# load FM data and expand lengths ... copied code from first handout
setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("SAWYER_fish_raw_data_012915.csv",stringsAsFactors=FALSE,na.strings=c("-","NA",""))
d <- setDBClasses(d,type="RDNR")
d <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
names(d)

d1 <- select(d,Waterbody.Name,Gear,Survey.Year,Species,Len,Weight.Pounds,Gender)
headtail(d1)

tmp <- select(d,County:Station.Name)
headtail(tmp)

tmp <- select(d,-(Station.Name:Status.Code))
headtail(tmp)

tmp <- select(d,starts_with("Length"))                        # there is also an ends_with
names(tmp)

tmp <- select(d,Survey.Seq.No,Species,Len,contains("Mark"))
headtail(tmp)

levels(d1$Waterbody.Name)
xtabs(~Waterbody.Name,data=d1)                                # only partial results shown
xtabs(~Waterbody.Name+Gear,data=d1)                           # only partial results shown

tmp <- filter(d1,Waterbody.Name=="BARBER LAKE")
xtabs(~Waterbody.Name,data=tmp)                               # only partial results shown

tmp <- droplevels(tmp)
xtabs(~Waterbody.Name,data=tmp)

tmp <- filterD(d1,Waterbody.Name=="BARBER LAKE")
xtabs(~Waterbody.Name,data=tmp)

tmp <- filterD(d1,Waterbody.Name %in% c("BARBER LAKE","LAKE CHETAC"))
xtabs(~Waterbody.Name,data=tmp)

LCblg <- filterD(d1,Waterbody.Name=="LAKE CHETAC",Species=="BLUEGILL",Gear=="BOOM SHOCKER")
xtabs(~Gear+Species,data=LCblg)

weird <- filterD(d1,Species=="Iowa Darter" | Weight.Pounds>100)
weird
( weird <- filterD(d1,Species=="IOWA DARTER" | Weight.Pounds>100) )

LCblgPREF <- filterD(LCblg,Len>=7)
Summarize(~Len,data=LCblgPREF,digits=2)

sturgWts <- filterD(d1,Species=="LAKE STURGEON",!is.na(Weight.Pounds))
headtail(sturgWts)
