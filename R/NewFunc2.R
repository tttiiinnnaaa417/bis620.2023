# (2) rf.imp.barplot(model, bar.color)
# --------------------
# This function prints out the variable importance scores in a Random Forest model based on 
# mean decrease in accuracy, and produces a horizontal bar plot with desired bar color to 
# visualize the variable importance.
# --------------------
#' @param model Name of the Random Forest model, written in the form of model = name
#' @param bar.color The desired color of the bars, written in the form of bar.color = color
#' @importFrom randomForest importance
#' @importFrom graphics barplot
#' @export

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


