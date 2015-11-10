#' Script which creates production data subsets for each oilfield
#'
#' Actually this is script not the function. It created for data splitting
#' automation.
#' When you ron this script, SQL sentence runs via sqldf() library and creates
#' separate dataframes for particular oilfieds.
#' All subsets are hardcoded. If you would like to add new field or remove
#' the existing one, you should edit this file.
#' Input dataframe also hardcoded as "monthly".
#'
#' @param No input parameters for this script.
#'
#' @return output Created separate dataframes for particular fields. Dataframes
#' named as FIELDNAME.sub Data structure in dataframes is identical to monthly
#' dataframe structure.
#'
#' @keywords no keywords
#'
#' @export
#'
#' @examples
#' wsubbyfields()

wsubbyfields <- function() {
    require("sqldf", quietly = TRUE)
    return(ABL.sub <- sqldf("select * from monthly where Well_N like 'ABL%'"))
    AUKS.sub <- sqldf("select * from monthly where Well_N like 'AUKS%'")
    ANTK.sub <- sqldf("select * from monthly where Well_N like 'ANTK%'")
    LIZ.sub <- sqldf("select * from monthly where Well_N like 'LIZ%'")
    VEZ.sub <- sqldf("select * from monthly where Well_N like 'VEZ%'")
    ZAD.sub <- sqldf("select * from monthly where Well_N like 'ZAD%'")
    GRK.sub <- sqldf("select * from monthly where Well_N like 'GRK%'")
    KRT.sub <- sqldf("select * from monthly where Well_N like 'KRT%'")
    NSD.sub <- sqldf("select * from monthly where Well_N like 'NSD%'")
    GEN.sub <- sqldf("select * from monthly where Well_N like 'GEN%'")
}
