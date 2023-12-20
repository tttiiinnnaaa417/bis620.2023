library(testthat)
library(ggplot2)
library(randomForest)
test_that("binary.barplot returns plots", {
  expect_silent({
    data(adsl)
    binary.barplot(adsl, c("DTH"), "PFSCR")
  })
})

# Test for rf.imp.barplot
test_that("rf.imp.barplot returns a barplot and prints importance", {
  data(adsl)
  sample_model <- randomForest(factor(PFSCR) ~ ., data = adsl[1:100,], importance = TRUE)
  expect_output_print({
    rf.imp.barplot(sample_model, "blue")
  }, regexp = "Mean Decrease in Accuracy")
})
