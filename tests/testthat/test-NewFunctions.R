library(testthat)
library(ggplot2)
library(randomForest)
test_that("binary.barplot returns plots", {
    data(adsl)
    result <- binary.barplot(adsl, c("DTH"), "PFSCR")
    expect_true(inherits(result, "ggplot"))
})

test_that("rf.imp.barplot returns a barplot and prints importance", {
  sample_model <- randomForest(PFSCR ~ ., data = dl)
  expect_output_print({
    rf.imp.barplot(sample_model, "blue")
  }) 
})
