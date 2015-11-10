library(dplyr)
library(magrittr)
library(lubridate)
library(bit64)
library(stringr)

lns <- readLines("~/RData/.Rhistory") %>% str_split(pattern=":",n=2)
hist_db <- data_frame(epoch=as.integer64(sapply(lns,"[[",1)),history=sapply(lns,"[[",2))

hist_db %<>% mutate(nice_date = as.POSIXct(epoch/1000,origin = "1970-01-01",tz = "EET"))
hist_db %<>% mutate(day = ceiling_date(nice_date,unit = "day")-days(1))

hist_db %<>% select(-epoch)

dd <- hist_db$day %>% unique %>% sort

ff <- "~/RData/hist_nice.txt"
cat("R history","\n",rep("-",80),"\n",file=ff,sep="")

for(i in 1:length(dd)) {
  cat("\n\n",format(dd[i]),"\n",rep("-",80),"\n",file=ff,sep="",append=TRUE)
  hist_db %>% filter(day==dd[i]) %>% select(nice_date,history) %>% arrange(nice_date) %>%
    write.table(ff,sep="\t", quote=F, row.names=FALSE, col.names=FALSE, append=TRUE)
}
