library(plotly)
#library(dygraphs)
library(prophet)
library(shinythemes)
library(shinyjs)
library(dplyr)

options(digits = 5)

# load the virtual env
reticulate::virtualenv_create("python35_env", python = "python3")
reticulate::virtualenv_install("python35_env",
                               packages = c("bdshare==0.2.0"),
                               ignore_installed = T)
reticulate::use_virtualenv("python35_env", required = TRUE)

# read instrument names
inst_url <- "https://raw.githubusercontent.com/tanvird3/instrument_list/master/Inst.csv"
inst_name <- readr::read_csv(url(inst_url))
#inst_name <- readr::read_csv("Inst.csv")
inst_name <- inst_name$TRADING.CODE
t_default <- which(inst_name == "ACI")

# set the dates
enddate <- Sys.Date()
startdate <- enddate - 365 * 2