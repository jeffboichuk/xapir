library(testthat)
library(dplyr)
library(xapir)

if(identical(tolower(Sys.getenv("NOT_CRAN")), "true")){
  test_check("xapir")
}
