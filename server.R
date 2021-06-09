shinyServer(function(input, output) {
  forecast_func <-
    function(instrument, crossvalidation) {
      # get the data
      reticulate::source_python("dse.py")
      
      df <-
        dse_hist(startdate,
                 enddate,
                 instrument)
      
      # keep only the required columns
      df_mod <- df %>% select(DATE, CLOSEP)
      
      # rename the columns as per requirements
      names(df_mod) <- c("ds", "y")
      
      # fit the model
      model_fit <- prophet(df_mod, seasonality.mode = "additive")
      future_get <- make_future_dataframe(model_fit, periods = 68)
      future_get <-
        future_get %>% mutate(wd = weekdays(ds)) %>% filter(!wd %in% c("Friday", "Saturday")) %>% select(-wd)
      forecast_get <-
        predict(model_fit, future_get) %>% select(ds, yhat, yhat_lower, yhat_upper)
      
      # get the performance metrics
      if (crossvalidation == "Yes") {
        df.cv <-
          cross_validation(
            model_fit,
            initial = 100,
            horizon = 30,
            units = 'days'
          )
        model_eval <- performance_metrics(df.cv)
        model_eval$horizon <- as.integer(model_eval$horizon)
      }
      
      else {
        model_eval <-
          data.frame(
            horizon = c(""),
            mse = c(""),
            rmse = c(""),
            mae = c(""),
            mape = c(""),
            mdape = c(""),
            smape = c(""),
            coverage = c("")
          )
      }
      
      # plot the forecasts
      # pplot <-
      #   dyplot.prophet(model_fit, forecast_get, main = instrument)
      df_dt <- df %>% select(DATE, CLOSEP, VALUE)
      names(df_dt)[1] <- "ds"
      df_dt <-
        df_dt %>% mutate(ds = as.Date(ds, format = "%Y-%m-%d"))
      df_plot <- df_dt %>% full_join(forecast_get, by = "ds")
      df_plot <-
        df_plot %>% mutate_at(c(4:6), plyr::round_any, .10)
      
      pplot <- plot_ly(df_plot,
                       x = ~ ds,
                       #width = 1000,
                       #height = 450
                       ) %>%
                       
                       add_trace(
                         y = ~ CLOSEP,
                         size = ~ VALUE,
                         fill = ~ '',
                         type = "scatter",
                         mode = "lines+markers",
                         name = "Actual Price",
                         marker = list(color = "blue", opacity = 0.5),
                         text = ~ paste(
                           "</br> Date:",
                           ds,
                           "</br> Closing Price:",
                           CLOSEP,
                           "</br> Total Value:",
                           VALUE,
                           "M"
                         ),
                         hoverinfo = "text"
                       ) %>% add_trace(
                         y = ~ yhat,
                         name = "Forecasted Price",
                         type = "scatter",
                         mode = "lines"
                       ) %>% add_trace(
                         y = ~ yhat_upper,
                         name = "Upper Band",
                         type = "scatter",
                         mode = "lines",
                         line = list(dash = "dot")
                       ) %>% add_trace(
                         y = ~ yhat_lower,
                         name = "Lower Band",
                         type = "scatter",
                         mode = "lines",
                         line = list(dash = "dot")
                       ) %>% layout(
                         title = paste0("<br>", instrument),
                         xaxis = list(title = "Date"),
                         yaxis = list(title = "Closing Price"),
                         margin = list(t = 120),
                         legend = list(
                           orientation = "h",
                           x = 0,
                           y = 1.4
                         )
                       )
                       
                       # return the outcomes
                       return(list(output_forecast = pplot, output_eval = model_eval))
    }
  
  # generate the output
  output_get <-
    reactive({
      forecast_func(input$instrument, input$crossvalidation)
    })
  
  #renderDygraph if we want the default Dygraph
  output$output_forecast <- renderPlotly({
    output_get()$output_forecast
  })
  
  output$output_eval <- renderTable({
    output_get()$output_eval
  })
  
  # output$output_forecast <- renderPlotly({
  #   forecast_func(input$instrument, input$crossvalidation)$output_forecast
  # })
  #
  # output$output_eval <- renderTable({
  #   forecast_func(input$instrument, input$crossvalidation)$output_eval
  # })
  
  lapply(c("output_forecast", "output_eval"), function(x)
    outputOptions(output, x, suspendWhenHidden = F))
  
})
      