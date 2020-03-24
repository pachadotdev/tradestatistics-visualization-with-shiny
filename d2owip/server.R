shinyServer(
  function(input, output,session) {
    h2o.init()
    h2o.no_progress()
    
    loans_h2o <- reactive({
      d <- as.h2o(dplyr::select(loans, c(input$selected_variables, "bad_loan")))
      d
    })
    
    y <- "bad_loan"
    x <- reactive({
      setdiff(names(loans_h2o()), y)
    })
    
    splits <- reactive({
      share <- as.numeric(input$selected_pct_training[[1]])
      
      h2o.splitFrame(
        data = loans_h2o(),
        ratios = c(share, (1 - share) / 2),
        destination_frames = c("train", "valid", "test"),
        seed = 200100
      )
    })
    
    train <- reactive({
      splits()[[1]]
    })
    valid <- reactive({
      splits()[[2]]
    })
    test <- reactive({
      splits()[[3]]
    })
    
    model <- reactive({
      if (input$selected_model == "glm") {
        h2o.glm(
          x = x(),
          y = y,
          training_frame = train(),
          family = "binomial",
          nfolds = input$selected_nfolds,
          seed = 200100
        )
      } else {
        if (input$selected_model == "rf") {
          h2o.randomForest(
            x = x(),
            y = y,
            training_frame = train(),
            validation_frame = valid(),
            ntrees = input$selected_ntrees,
            stopping_rounds = 5,
            stopping_tolerance = 0.001,
            stopping_metric = "AUC",
            score_tree_interval = 20,
            seed = 200100
          )
        } else {
          if (input$selected_model == "gbm") {
            h2o.gbm(x = x(),
                    y = y,
                    training_frame = train(),
                    validation_frame = valid(),
                    ntrees = input$selected_ntrees,
                    stopping_rounds = 5,        
                    stopping_tolerance = 0.001, 
                    stopping_metric = "AUC",    
                    score_tree_interval = 20,
                    seed = 200100)
          } else {
            if (input$selected_model == "dl") {
              h2o.deeplearning(x = x(),
                               y = y,
                               training_frame = train(),
                               validation_frame = valid(),  
                               epochs = 20,
                               score_each_iteration = TRUE,
                               stopping_rounds = 5,        
                               stopping_metric = "AUC",    
                               stopping_tolerance = 0.001, 
                               seed = 200100)
            }
          }
        }
      }
    })
    
    output$model_summary <- renderTable({
      ms <- model()@model$model_summary
      ms <- as.data.frame(ms)
      ms <- tidyr::gather(ms, item, value)
      ms
    })
    
    output$variable_importance <- renderPlot({
      h2o.varimp_plot(model())
    })
    
    output$variable_importance_2 <- renderTable({
      head(h2o.varimp(model()), 10)
    })
    
    output$performance <- renderTable({
      if (input$selected_model == "glm") {
        pf <- c(h2o.auc(model(), xval = T), h2o.rmse(model(), xval = T))
      } else {
        pf <- c(h2o.auc(model(), xval = F), h2o.rmse(model(), xval = F))
      }
      names(pf) <- c("AUC", "RMSE")
      tibble::enframe(pf, name = "metric")
    })
    
    output$coefficients <- renderTable({
      co <- h2o.coef(model())[abs(h2o.coef(model())) > 0.01]
      tibble::enframe(co, name = "variable")
    })
    
    output$normalized_coefficients <- renderTable({
      co <- h2o.coef_norm(model())[abs(h2o.coef_norm(model())) > 0.01]
      tibble::enframe(co, name = "variable")
    })
    
    output$classification <- renderTable({
      cm <- h2o.confusionMatrix(model())
      cm$`0` <- as.integer(cm$`0`)
      cm$`1` <- as.integer(cm$`1`)
      cm$`Error` <- paste0(round(100 * cm$`Error`, 2), "%")
      colnames(cm) <- c("bad loans", "good loans", "error", "rate")
      as.data.frame(cm)
    })
  }
)