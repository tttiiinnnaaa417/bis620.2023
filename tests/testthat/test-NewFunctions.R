library(testthat)
library(ggplot2)
library(randomForest)
test_that("binary.barplot returns plots", {
    data(adsl)
    result <- binary.barplot(adsl, c("DTH"), "PFSCR")
    expect_true(inherits(result, "ggplot"))
})
