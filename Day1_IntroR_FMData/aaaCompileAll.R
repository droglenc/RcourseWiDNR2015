
library(rmarkdown)

fn <- "01_LoadData.Rmd"
render(fn,"all")
swvCode(fn,moreItems=c("knitr","row.names","tmp","1:","fn"),topnotes=c(fn,"Some of the output was suppressed when making the final document.","Thus, running this script will give somewhat different results."))

fn <- "02_FilterData.Rmd"
render(fn,"all")
swvCode(fn,moreItems=c("knitr","fn","1:"),topnotes=c(fn))

fn <- "03_MutateData.Rmd"
render(fn,"all")
swvCode(fn,moreItems=c("knitr","fn"),topnotes=c(fn))

fn <- "04_SizeStructure_I.Rmd"
render(fn,"all")
swvCode(fn,moreItems=c("knitr","fn"),topnotes=c(fn))

fn <- "05_SizeStructure_II.Rmd"
render(fn,"all")
swvCode(fn,moreItems=c("knitr","fn"),topnotes=c(fn))
