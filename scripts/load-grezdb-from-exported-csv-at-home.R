#load data from prod-mon.csv at home
monthly <- read.csv("prod-mon.csv",sep=";",dec=".")
monthly[[2]] <- as.POSIXct(monthly[[2]], "%Y-%m-%d")
monthly[["BHP"]] <- as.numeric(monthly[["BHP"]])
