if(!require(pacman)) install.packages("pacman")
pacman::p_load(shiny, shinythemes, tidyverse, forecast, patchwork, tseries, urca, DT)

sum_ma <- function(ma_){
  sum_MA <- 2
  while(sum(sum_MA) > 1){
    sum_MA <- runif(ma_, -1, 1)
  }
  sum_MA
}
