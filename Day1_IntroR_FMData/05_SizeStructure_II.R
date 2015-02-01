# 05_SizeStructure_II.Rmd
library(fishWiDNR)   # for setDBClasses()
library(FSA)         # for Summarize(), hist(), expandCounts()
library(magrittr)    # for %<>%
library(dplyr)       # for %>%, filter(), select(), mutate(), group_by(), summarize()
library(lubridate)   # for month()
setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("SAWYER_fish_raw_data_012915.csv",stringsAsFactors=FALSE,na.strings=c("-","NA","")) %>%
      setDBClasses(type="RDNR") %>%
      expandCounts(~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len") %>%
      mutate(Mon=month(Survey.Begin.Date,label=TRUE),Species1=capFirst(Species)) %>%
      select(Species,Species1,Waterbody.Name,Survey.Year,Mon,Gear,Len)
Spr <- filterD(d,Survey.Year==2013,Mon %in% c("Apr","May","Jun"))
BGSpr <- filterD(Spr,Species=="BLUEGILL")
BGSprLC <- filterD(BGSpr,Waterbody.Name=="LAKE CHETAC",Gear=="BOOM SHOCKER")
SprLC <- filterD(Spr,Waterbody.Name=="LAKE CHETAC")
head(BGSprLC)
brks <- psdVal("Bluegill",units="in",addLens=7)
BGSprLC %<>% mutate(lcat=lencat(Len,breaks=brks),
                     lcat1=lencat(Len,breaks=brks,use.names=TRUE),
                     lcat2=lencat(Len,breaks=brks,use.names=TRUE,drop.levels=TRUE))
head(BGSprLC)
xtabs(~lcat,data=BGSprLC)
xtabs(~lcat1,data=BGSprLC)
( freq <- xtabs(~lcat2,data=BGSprLC) )
( rcum <- rcumsum(freq) )
rcum["stock"]                                                 # demo number of stock fish
rcum/rcum["stock"]*100
BGSpr %<>% mutate(lcat2=lencat(Len,breaks=brks,use.names=TRUE,drop.levels=TRUE))
( freq <- xtabs(~Waterbody.Name+lcat2,data=BGSpr) )
apply(freq,MARGIN=1,FUN=rcumsum)                              # apply result has wrong orientation
( rcum <- t(apply(freq,MARGIN=1,FUN=rcumsum)) )
rcum <- rcum[,-1]                                             # remove "substock" column
rcum/rcum[,"stock"]*100
SprLC %<>% mutate(lcat2=psdAdd(Len,Species1,units="in"))
head(SprLC)

( freq <- xtabs(~Species+lcat2,data=SprLC) )
( rcum <- t(apply(freq,MARGIN=1,FUN=rcumsum)) )
rcum <- rcum[,-1] 
rcum/rcum[,"stock"]*100
