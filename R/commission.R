library(tidyverse)

#' Calculates product comission based on sales.
#' 0 - 300 %5
#' 301 to 750 10%
#' 751 and aobove 15%
#'
#' @param product_sales A number. Product sales
#' @return The comission on the product sales.
#' @examples
#' product_calc(100.00)
#' @export
product_calc <- function(product_sales) {
  if (product_sales >= 751) {
    comission <- (product_sales -750) * 0.15
    comission <- comission + ((750 -300) * 0.1)
    comission <- comission + ((300 -0) * 0.05)
  } else if (product_sales >= 301 && product_sales < 751) {
    comission <- (product_sales - 300) * 0.1
    comission <- comission + ((300 - 0) * 0.05)
  } else {
    comission <- product_sales * 0.05
  }

  return(round(comission,2))
}

#' Calculates product and sales comission.
#'
#' @param perf_path The path to staff performance report
#' @param targets_path The path to targets files
#' @return Dataframe with comission calculated.
#' @examples
#' comission_calc('perf.csv','targets.csv')
#' @export
comission_calc <- function(perf_path, targets_path) {

  # Load the data
  csv_staff_cols <- c('Employee','ClientVisitsNum','ClientVisitsNew','ClientVisitsRQs','ClientVisitsRat','ServicesExVat','ServicesIncVat','CoursesExVat','CoursesIncVat','ProductsExVat','ProductsIncVat','TotalExVat','TotalIncVat','AvgPerClientExVat','AvgPerClientIncVat')
  csv_staff_perf <- read_csv(perf_path, skip=6, col_names = csv_staff_cols)
  csv_staff_targets <- read_csv(targets_path)

  # Join the data
  merged <- merge(x = csv_staff_targets, y = csv_staff_perf, by.x = "Employee", by.y = "Employee")

  # Add treament totals
  merged$TreatmentTotalExVat <- merged$ServicesExVat + merged$CoursesExVat
  merged$TreatmentTotalIncVat <- merged$ServicesIncVat + merged$CoursesIncVat

  # Treatment commission
  merged$TreatmentCommission <- ifelse(merged$TreatmentTotalInc > merged$TreatmentTarget, merged$TreatmentTarget * 0.03, 0)

  # Product comission
  merged$ProductCommission = sapply(merged$ProductsIncVat, product_calc)

  # Round the values
  merged$TreatmentCommission <- round(merged$TreatmentCommission, 2)
  merged$ProductCommission <- round(merged$ProductCommission, 2)

  return(merged)
}



