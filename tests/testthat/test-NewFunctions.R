test_that('binary.barplot returns plots', {
  mydata <- data.frame(
    Var1 = sample(c("A", "B"), 100, replace = TRUE),
    Var2 = sample(c("C", "D"), 100, replace = TRUE),
    GroupVar = sample(c(0, 1), 100, replace = TRUE)
  )
  plots <- binary.barplot(mydata, c("Var1", "Var2"), "GroupVar")
  expect_true(all(sapply(plots, inherits, "gg")))
})

# Test binary.barplot handles error scenarios
test_that('binary.barplot handles errors', {
  mydata <- data.frame(
    Var1 = sample(c("A", "B"), 100, replace = TRUE),
    Var2 = sample(c("C", "D"), 100, replace = TRUE),
    GroupVar = sample(c(0, 1), 100, replace = TRUE)
  )
  expect_error(binary.barplot(mydata, c("NonExistentVar"), "GroupVar"))
})

# Test rf.imp.barplot prints variable importance and returns a barplot
test_that('rf.imp.barplot returns a barplot', {
  mydata <- data.frame(
    Var1 = sample(c("A", "B"), 100, replace = TRUE),
    Var2 = sample(c("C", "D"), 100, replace = TRUE),
    GroupVar = sample(c(0, 1), 100, replace = TRUE)
  )
  sample_model <- randomForest(GroupVar ~ ., data = mydata)
  plot <- rf.imp.barplot(sample_model, "blue")
  expect_true(inherits(plot, "gg"))
})