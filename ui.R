library(shiny)
shinyUI(fluidPage (
  theme = shinytheme("readable"),
  headerPanel("DSE Forecast"),
  sidebarPanel(
    width = 2,
    selectInput(
      "instrument",
      label = "Select Instrument",
      choices =
        inst_name,
      selected = inst_name[t_default]
    ),
    selectInput(
      "crossvalidation",
      label = "Perform Cross Validationn",
      choices =
        c("No", "Yes"),
      selected = "No"
    ),
    submitButton(text = "Forecast")
  ),
  mainPanel (h1(""),
             tabsetPanel(
               tabPanel("Forecasted Plot", plotlyOutput("output_forecast")), #dygraphOutput if we want default Dygraph
               tabPanel("Cross Validation", tableOutput("output_eval"))
             ))
))
