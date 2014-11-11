# 01_LoadData.Rmd
# Some of the output was suppressed when making the final document.
# Thus, running this script will give somewhat different results.

library(fishWiDNR)   # for setDBClasses()

setwd("C:/aaaWork/Web/fishR/Courses/WIStatewide2015/CourseMaterial")
d <- read.csv("FMDB_Sawyer.csv",stringsAsFactors=FALSE)
d <- setDBClasses(d,type="RDNR")
str(d)

head(d)   # also can use tail(d)
nrow(d)

d1 <- expandCounts(d,~Number.of.Fish)

d1 <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
nrow(d1)

sum(d$Number.of.Fish)

d1$Length.or.Lower.Length.IN

d1$Species

