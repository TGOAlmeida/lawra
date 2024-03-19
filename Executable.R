# Executable

library(shiny)
library(shinyWidgets)
library(tibble)
library(dplyr)
library(shinyFiles)


if(env2Run == "DEV")
{
  dirPrj <- "/Users/tiago/Documents/GitHub/lawra"
}
if(env2Run == "PRD")
{
  dirPrj <- "C:/Users/darcis/Desktop/Contratos_teste"
}

source(paste0(sourceCodePath, "server_lawra.R"))
source(paste0(sourceCodePath, "ui_lawra.R"))
