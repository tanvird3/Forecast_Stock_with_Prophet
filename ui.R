library(shiny)
shinyUI(fluidPage (
  theme = shinytheme("readable"),
  headerPanel("DSE Forecast"),
  sidebarLayout(
    position = "right",
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
    mainPanel (
      h1(""),
      align = "center",
      width = 10, # width of the mainpanel, total width of the window is 12, 10 is mainpanel, 2 is sidebar
      tabsetPanel(
        tabPanel("Forecasted Plot", plotlyOutput("output_forecast"), width = "100%"), # 100% means the plot would take the entire window
        #dygraphOutput if we want default Dygraph
        tabPanel("Cross Validation", tableOutput("output_eval"))
      )
    )
  )
))
