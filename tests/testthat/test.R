library(testthat)
library(priorityMatrix)

hcPareto <- createParetoChart()
expect_that(hcPareto,is_a("highchart"))
