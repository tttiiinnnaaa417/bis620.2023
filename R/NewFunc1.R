# (1) binary.barplot(mydata, x.vars, group.var)
# --------------------
# This function produces a sequence of multiple bar plots for independent variables of interest 
# in a data frame, grouped and filled by the binary dependent variable. Each independent variable 
# in the sequence will produce one bar plot.
# --------------------
#' @param mydata Name of the data frame, written in the form of mydata = name
#' @param x.vars A combined sequence of independent variables to be plotted, 
#' written in the form of x.vars = c("Var1", "Var2", "Var3", "...")
#' @param group.var The binary dependent variable by which the bar plot will be grouped and filled, 
#' written in the form of group.var = "Var"
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @export
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
