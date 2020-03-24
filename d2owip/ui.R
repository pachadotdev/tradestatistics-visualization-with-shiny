shinyUI(
  function(request) {
    dashboardPage(
      dashboardHeader(title = "Loans Demo"),
      
      dashboardSidebar(
        disable = F,
        sidebarMenu(
          menuItem("Model", icon = icon("th"), tabName = "model")
        )
      ),
      
      dashboardBody(
        fluidRow(
          tabItems(
            tabItem(
              tabName = "model",
              box(
                width = 12,
                title = "Controls",
                collapsible = TRUE,
                column(
                  width = 12,
                  column(
                    width = 4,
                    radioButtons("selected_model", "Select a model:",
                                 choices = c(
                                   "Generalized Linear Model (with CV)" = "glm",
                                   "Random Forest (with Early Stopping)" = "rf",
                                   "Gradient Boost Machine (with Early Stopping)" = "gbm",
                                   "Deep Learning (with Early Stopping)" = "dl"
                                 ),
                                 selected = "glm"
                    ),
                    checkboxGroupInput("selected_variables", "Select independent variables:",
                                       choices = names(loans)[names(loans) != "bad_loan"],
                                       selected = names(loans)[names(loans) != "bad_loan"])
                  ),
                  column(
                    width = 8,
                    sliderInput("selected_nfolds", "Number of folds", 5, min = 0, max = 10, step = 1),
                    sliderInput("selected_ntrees", "Number of trees", 500, min = 0, max = 1000, step = 5),
                    sliderInput("selected_pct_training", "Validation share:", 0.7, min = 0, max = 1, step = 0.05)
                  )
                )
              ),
              
              box(
                width = 12,
                title = "Model metrics",
                collapsible = TRUE,
                column(
                  width = 12,
                  column(
                    width = 4,
                    h2("Model summary"),
                    tableOutput("model_summary")
                  ),
                  column(
                    width = 4,
                    h2("Model performance"),
                    tableOutput("performance")
                  ),
                  column(
                    width = 4,
                    h2("Model classification"),
                    tableOutput("classification")
                  )
                )
              ),
              
              box(
                width = 12,
                title = "Model results",
                collapsible = TRUE,
                column(
                  width = 12,
                  column(
                    width = 6,
                    h2("Variable importance"),
                    plotOutput("variable_importance")
                  ),
                  column(
                    width = 6,
                    h2("Absolute standardized coefficients (top 10)"),
                    tableOutput("variable_importance_2")
                  )
                ),
                conditionalPanel(
                  condition = "input.selected_model == 'glm'",
                  column(
                    width = 12,
                    column(
                      width = 6,
                      h2("GLM coefficients (abs. value > 0.01)"),
                      tableOutput("coefficients")
                    ),
                    column(
                      width = 6,
                      h2("GLM normalized coefficients (abs. value > 0.01)"),
                      tableOutput("normalized_coefficients")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }
)