## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(haven)
library(purrr)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(tidyr)
library(caret)
library(randomForest)

# prepare data
load(file = "dl.rda")
load(file = "adlb.rda") # dim: 125123 7
load(file = "adsl.rda") # dim: 935 19

## -----------------------------------------------------------------------------
# unique subjects
length(unique(adlb$SUBJID)) # 935
length(unique(adsl$SUBJID)) # 935

# data cleaning
nrow(adsl[!(is.na(adsl$ATRT)), ]) # 935, meaning no NA value in ATRT (actual treatment)
nrow(adlb[adlb$LBBASE == "Y", ]) # 7399 rows of baseline lab test results

adlb.base <- adlb |> 
  select(SUBJID, LBTEST, LBBASE, LBSTRESN) |>
  filter(LBBASE == "Y") |>
  na.omit() # 7399

# use each lab test as a covariate (column)
adlb.base <- adlb.base |>
  pivot_wider(names_from = LBTEST, values_from = LBSTRESN) |>
  select(-LBBASE) |>
  na.omit() # 864 9 (8 lab tests)

# rename lab test covariates
adlb.base <- rename(adlb.base,
                    AlkalinePhosphatase = `Alkaline Phosphatase`,
                    LactateDehydrogenase = `Lactate Dehydrogenase`,
                    WhiteBloodCells = `White Blood Cells`,
                    CarcinoembryonicAntigen = `Carcinoembryonic Antigen`)

## -----------------------------------------------------------------------------
# select variables of interest from adsl
adsl.covar <- adsl |>
  select(SUBJID, ATRT, PRSURG, PFSCR, LIVERMET, DIAGMONS, AGE, SEX, B_WEIGHT, B_HEIGHT, DIAGTYPE) |>
  na.omit() # 923 11 (9 covariates; PFSCR is the response)

# recode binary covariates
# and add a new covariate BMI calculated from B_WEIGHT and B_HEIGHT
adsl.covar <- adsl.covar |>
  mutate(ATRT = case_when(ATRT == "FOLFOX alone" ~ "Single",
                          ATRT == "Panitumumab + FOLFOX" ~ "Combo"),
         BMI = B_WEIGHT/(B_HEIGHT/100)^2) |>
  select(-c(B_WEIGHT, B_HEIGHT))

# merge adlb.base (baseline lab results) and adsl.covar (subject covariates)
data.merge <- merge(adlb.base, adsl.covar, by = "SUBJID") |>
  select(-SUBJID) # 853 17

## -----------------------------------------------------------------------------
# create new functions

# (1) binary.barplot(mydata, x.vars, group.var)
# --------------------
# This function produces a sequence of multiple bar plots for independent variables of interest in a data frame, grouped and filled by the binary dependent variable. Each independent variable in the sequence will produce one bar plot.
# --------------------
# Inputs:
# mydata: name of the data frame, written in the form of mydata = name
# x.vars: a combined sequence of independent variables to be plotted, written in the form of x.vars = c("Var1", "Var2", "Var3", "...")
# group.var: the binary dependent variable by which the bar plot will be grouped and filled, written in the form of group.var = "Var"
# --------------------
binary.barplot <- function(mydata, x.vars, group.var) {
  for (x.var in x.vars) {
    p <- ggplot(mydata, aes(x = .data[[x.var]], 
                            group = .data[[group.var]], 
                            fill = .data[[group.var]])) +
      geom_bar() +
      labs(title = paste("Bar plot for", x.var, "grouped by", group.var),
           x = x.var,
           y = "Count",
           fill = group.var) +
      theme_minimal()
    
    print(p)
  }
}

# example usage:
binary.barplot(mydata = data.merge, 
               x.vars = c("AGE", "SEX", "LIVERMET", "DIAGTYPE"), 
               group.var = "PFSCR")

## -----------------------------------------------------------------------------
# proportions summary table
data.merge |>
  select(PFSCR, SEX, ATRT, PRSURG, LIVERMET, DIAGTYPE) |>
  tbl_summary(by = "PFSCR")

## -----------------------------------------------------------------------------
# full model
logifit1 <- glm(PFSCR ~ ., data = data.merge, family = "binomial")
summary(logifit1)

# backward feature selection using step AIC
step(logifit1, direction = "both")

# selected model
logifit2 <- glm(PFSCR ~ AlkalinePhosphatase + LactateDehydrogenase + Hemoglobin + AGE,
                data = data.merge, family = "binomial")
summary(logifit2)

## -----------------------------------------------------------------------------
set.seed(123)

# split data into training (70%) and test (30%)
train.index <- createDataPartition(data.merge$PFSCR, p = 0.7, list = FALSE)
train.data <- data.merge[train.index, ]
test.data <- data.merge[-train.index, ]

# fit RF model on training set and make predictions on test set
rf.model <- randomForest(factor(PFSCR) ~ ., data = train.data, importance = TRUE)
test.pred <- predict(rf.model, newdata = test.data, type = "response")

# training error
train.pred <- predict(rf.model, newdata = train.data, type = "response")
train.error <- mean(train.pred != train.data$PFSCR)
cat("Training Error:", train.error, "\n")

# test error
test.error <- mean(test.pred != test.data$PFSCR)
cat("Test Error:", test.error, "\n")

## -----------------------------------------------------------------------------
# extract and plot variable importance
varImpPlot(rf.model, type = 1, sort = TRUE)

# create a new function

# (2) rf.imp.barplot(model, bar.color)
# --------------------
# This function prints out the variable importance scores in a Random Forest model based on mean decrease in accuracy, and produces a horizontal bar plot with desired bar color to visualize the variable importance.
# --------------------
# Inputs:
# model: name of the Random Forest model, written in the form of model = name
# bar.color: the desired color of the bars, written in the form of bar.color = color
# --------------------
rf.imp.barplot <- function(model, bar.color) {
  imp.feat <- importance(model, type = 1, sort = TRUE)
  print(imp.feat)
  
  imp.feat <- as.array(imp.feat)
  p <- barplot(imp.feat[,1],
               names.arg = rownames(imp.feat),
               main = "Variable Importance",
               xlab = "Mean Decrease in Accuracy",
               col = bar.color,
               cex.names = 0.7,
               horiz = TRUE,
               las = 2)
  
  print(p)
}

# example usage
rf.imp.barplot(model = rf.model, bar.color = "pink")
