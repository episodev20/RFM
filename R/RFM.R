#' get_max_PurchAmount
#'
#  Description
#' Function that return weighted rfm scores for customers
#'
#  Detail arguments like a data description
#' @details
#' \code{table} contains the transaction data. The data set must must be a data.table and contain a
#'             column labeled "Customer" that allows unique customer identification
#'             and a column labeled "TransDate", indicating the purchase date.
#'             The column "PurchAmount" specifies the total spending per purchase.
#'
#  Arguments that are passed as input to the function
#' @param table a data object with columns TransDate, Customer, PurchAmount, Cost
#' @param date a specification for the time of interest. Must be of format "dd.mm.yyy"
#'
#  Returned values with a description of what the function returns
#' @return table with RFM scores and corresponding group for each Customer
#'
#  Examples with a set of example R code on how to use the function
#' @examples
#' rfm_function(table, date="01.10.2020", weight_frequency = 0.2,
#' weight_recency = 0.2, weight_monetary_value = 0.6)
#'
#  Import packages that are required for using your package
#' @import data.table, Hmisc
#  Careful: some packages have functions with overlapping names. If this is the case,
#           only import specific functions from a package. Here, lubridate and data.table
#           share the function quarter(). To avoid conflicts, only load functions you need
#           from lubridate with @importFrom package function1 function2
#' @importFrom lubridate dmy, as_datetime
#'
#  Include export to make sure roxygen2 knows to create the NAMESPACE file, to make
#  the package accessible to other users
#' @export


rfm_function <- function(table, time_format = "%d.%m.%Y", weight_frequency = 1/3, weight_recency = 1/3, weight_monetary_value = 1/3) {

  library(data.table)
  library(lubridate)
  transactions <- data.table::as.data.table(transactions)
  # solution with as_datetime
  # format is specified as in as.POSIXct documentation
  # important to replace "-" with "." (relative to examples), cause we have time delimiter "."
  transactions[, TransDate:= as_datetime(TransDate, format=time_format)]

  ##################################################
  ### Scoring model ################################
  ##################################################

  # 1.
  library(Hmisc)

  #2.
  transactions[,Recency:= (as.Date(transactions$TransDate) - as.Date(today())) * -1]
  transactions[,Customer:=as.numeric(Customer)]
  transactions[,Recency:=as.numeric(Recency)]
  rfm <- transactions[, list(Monetary_value=mean(PurchAmount), Recency=max(Recency), Frequency=as.numeric(.N)), by=Customer]


  #3.
  # use cut2() inside of table() to se distribution
  # c() inside of cut2() are cutting points to adjust distribution

  Monetary_value <- cut2(rfm$Monetary_value, g=3, levels.mean=T)
  levels(Monetary_value) <- c(1,2,3)

  Recency <- cut2(rfm$Recency, c(2000,3200,4000), g=3)
  levels(Recency) <- c(1,2,3)

  Frequency <- cut2(rfm$Frequency, g=3)
  levels(Frequency) <- c(1,2,3)

  rfm2 <- rfm
  rfm2$Monetary_value <- as.numeric(Monetary_value)
  rfm2$Recency <- as.numeric(Recency)
  rfm2$Frequency <- as.numeric(Frequency)

  #4.
  # for some reason mean doesnt wrok inside
  rfm2[,RFM_score:=(Monetary_value+Recency+Frequency)/3]

  rfm2[,RFM_score:=(Recency*weight_recency+Frequency*weight_frequency+Monetary_value*weight_monetary_value)/3]
  rfm2$RFM_group <- round(rfm2$RFM_score)
  return(rfm2)
}
