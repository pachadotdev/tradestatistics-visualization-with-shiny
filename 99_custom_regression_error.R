custom_regression_error <- function() {
  feols(ERROR ~ UNFEASIBLE + ESTIMATION, 
        data = data.frame(
          ERROR = c(1,0,0), 
          UNFEASIBLE = c(0,1,0), 
          ESTIMATION = c(0,0,1)
        )
  )
}
