# 03_SummarizeAgeData.Rmd
source("02_AgeLengthKey.R")
ls()
hist(~Age..observed.annuli.,data=waeM.fnl,xlab="Age (yrs)",ylab="Male Walleye Captured")

histStack(Length.or.Lower.Length.IN~Age..observed.annuli.,data=waeM.fnl,xlab="Total Length (in.)",
           col="gray.colors",right=FALSE,legend.pos="topright")

waeM.sumlen <- waeM.fnl %>%
  group_by(Age..observed.annuli.) %>%
  summarize(n=n(),mean=mean(Length.or.Lower.Length.IN),sd=sd(Length.or.Lower.Length.IN),
            min=min(Length.or.Lower.Length.IN),max=max(Length.or.Lower.Length.IN))
waeM.sumlen

plotH(n~Age..observed.annuli.,data=waeM.sumlen,xlab="Age (yrs)",ylab="Male Walleye Captured",
       xlim=c(1.5,9.5),width=0.9)
plot(Length.or.Lower.Length.IN~Age..observed.annuli.,data=waeM.fnl,pch=16,col=rgb(0,0,0,1/10),
      xlab="Age",ylab="Total Length (mm)",ylim=c(10,21))
lines(mean~Age..observed.annuli.,data=waeM.sumlen,lwd=2,lty=2)
