# 03_SummarizeAgeData.Rmd

setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day2_AgeGrowth")
source("02_AgeLengthKey.R")

# to demonstrate what is in the workspace after the sourceing
ls()

hist(~Age..observed.annuli.,data=waeM.fnl,xlab="Age (yrs)",ylab="Male Walleye Captured")

histStack(Length.or.Lower.Length.IN~Age..observed.annuli.,data=waeM.fnl,xlab="Total Length (in.)",
           right=FALSE,legend.pos="topright")

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

plot(Length.or.Lower.Length.IN~Age..observed.annuli.,data=waeM.fnl,pch=16,col=rgb(0,0,0,1/10),
      xlab="Age",ylab="Total Length (mm)",ylim=c(10,21))
lines(mean~Age..observed.annuli.,data=waeM.sumlen,lwd=2,lty=2)

wae.JM <- data.frame(age=1:17,
                     state=c(6.5,9.8,12.0,14.1,16.1,17.8,19.3,20.7,21.8,
                             22.9,23.8,24.5,25.1,25.9,25.5,25.8,25.2),
                     NOR=c(6.4,9.5,11.7,13.8,15.8,17.5,19.1,20.5,21.6,
                           22.7,23.7,24.4,25.2,25.8,25.6,25.6,NA))
lines(state~age,data=wae.JM,lwd=2,lty=2,col="red")
lines(NOR~age,data=wae.JM,lwd=2,lty=2,col="blue")


# ############################################################
# ############################################################
# # Create a formatted table as a picture that allows results
# # to be cirectly input into presentations or reports
# #    largely from Gretchen Hansen
# ############################################################

# # Load some extra packages
library(ggplot2)
library(gridExtra)
library(magrittr)

# # Change the name of "age" variable in waeM.sumlen
# # and round the mean and sd
waeM.sumlen %<>% rename(Age=Age..observed.annuli.) %>%
  mutate(mean=round(mean,1),sd=round(sd,2))

# # Create a title for the table and the outputted file
ttl <- "Mean Length-at-Age for Nelson Lake Walleye, Spring 2013"

# # This creates the table (note the use of show.rownames=TRUE)
p <- ggplot(waeM.sumlen,aes(x=1,y=1)) +
  annotation_custom(tableGrob(waeM.sumlen,show.rownames=FALSE)) +
  ggtitle(ttl) +
  theme_bw() +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title=element_text(size=rel(1.2),face="bold",vjust=2),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank())
p

ggsave(paste0(ttl,".png"),p,width=6,height=3.5)
# ############################################################
# ############################################################
