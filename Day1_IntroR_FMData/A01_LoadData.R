# 01_LoadData.Rmd

library(fishWiDNR)   # for setDBClasses()
library(FSA)         # for headtail(), expandCounts()

setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("SAWYER_fish_raw_data_012915.csv",stringsAsFactors=FALSE,na.strings=c("-","NA",""))
d <- setDBClasses(d,type="RDNR")
str(d)
headtail(d)   # also can use tail(d) or head(d)
nrow(d)

# without random digits
d1 <- expandCounts(d,~Number.of.Fish)

# with random digits
d1 <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
nrow(d1)

# sum of Number.of.Fish variable (note from above the number of rows that had zero fish)
sum(d$Number.of.Fish,na.rm=TRUE)

d1$Length.or.Lower.Length.IN
d1$Species
