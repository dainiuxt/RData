#load data
library("RODBC")
db<-file.path("Z:/TVS/DB/BackEnd/BackEnd.accdb") #connect database
channel<-odbcConnectAccess2007(db) #internal RODBC function
monthly<-sqlFetch(channel,"T_PROD_MON") #read production data from database table
close(channel)
monthly_bak <- monthly
write.table(monthly,file="monthly_raw.csv", sep=";",row.names=F)
writeLines("Monthly production table loaded from database.\nBackup copy of loaded data created.\n\"monthly_raw.csv\" created.")
