#' Function for dataloading
#'
#' @param filepath Location of source file
#' @param dbtable Name of database table you would like to import.
#' Default is "T_PROD_MON"
#' @param backfilename Name of csv file where the copy of imported
#' data will be stored. Default value "monthly.csv"
#' @keywords wells data import
#' @export
#' @examples
#' monthly <- wloaddata("Z:/TVS/DB/GREZINIAI.accdb")

wloaddata <- function(filepath,
                      dbtable = "T_PROD_MON",
                      backfilename = "monprod.csv", ...)
{
    require("RODBC", quietly = TRUE)
    #connect database
    db <- file.path(filepath)
    #internal RODBC function
    channel <- odbcConnectAccess2007(db)
    #read production data from database table
    df <- sqlFetch(channel, dbtable)
    close(channel)
    #save a copy of imported data to csv file
    write.table(df, file = backfilename, sep = ";", row.names = F)
    return(df)
}
