#' Function for creating additional variables (summation, grouping
#' and cumulatives of imported monprod)
#'
#' @param monprod Name of monprodframe in R
#' @keywords wells monprod cumulatives variables
#' @export
#' @examples
#' wnewvars(monthly)

wnewvars <- function() {
    require("dplyr", quietly = TRUE)
    #Creating calculated variables from read monprod
    monprod$QFM <- mapply(sum, monprod$QOM, monprod$QWM)

    #Liquid production, m3/month
#     monprod$WCT <- mapply((monprod$QWM / monprod$QFM) * 100) #WaterCut, % (WCT)
    #create variables for cummulatives
    monprod$QOM1 <- monprod$QOM
    monprod$QOM1[is.na(monprod$QOM1)] <- 0
    monprod$QWM1 <- monprod$QWM
    monprod$QWM1[is.na(monprod$QWM1)] <- 0
    monprod$QFM1 <- monprod$QOM1 + monprod$QWM1
    monprod$QIW1 <- monprod$QIW
    monprod$QIW1[is.na(monprod$QIW1)] <- 0
    monprod <- monprod[order(as.Date(monprod$Date_m, format="%Y-%m-%d")),]
    monprod = monprod %>% group_by(Well_N) %>% mutate(CumOil=cumsum(QOM1))
    monprod = monprod %>% group_by(Well_N) %>% mutate(CumWat=cumsum(QWM1))
    monprod = monprod %>% group_by(Well_N) %>% mutate(CumF=cumsum(QFM1))
    monprod = monprod %>% group_by(Well_N) %>% mutate(CumInj=cumsum(QIW1))
    writeLines("New variables \"QFM\", \"WCT\" created \"as is\".
\n\"QOM1\", \"QWM1\" and \"QFM1\" created with \"NA\" values replaced by zero.
\nmonprod ordered by Date_m, then running totals \"CumOil\", \"CumWat\" \"CumF\" and \"CumInj\" calculated.")
}
