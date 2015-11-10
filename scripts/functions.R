dtudataload <- function() {
  source("fun/s_load.R")
}

dtudataproces <- function() {
  source("fun/s_new_variables.R")
}

dtudatasubset <- function() {
  source("fun/s_subset_by_fields.R")
}

dtutotals <- function() {
  source("fun/s_summarize_totals.R")
}

source("fun/s_wprodow.R")
source("fun/s_winjow.R")
source("fun/s_wprod.R")
source("fun/s_winj.R")
source("fun/s_fprodow.R")
source("fun/s_fprod.R")
source("fun/s_save_to_xlsx.R")

dtumonthly <- function() {
  source("fun/s_monthly_update.R")
}
