# Executable

library(shiny)
library(shinyWidgets)
library(tibble)
library(dplyr)
library(shinyFiles)
library(shinythemes)
library(pdftools)
library(stringr)
library(DT)


executeUI <- 0

#source(paste0(sourceCodePath, "server_lawra.R"))
source(paste0(sourceCodePath, "00.Functions.R"))
source(paste0(sourceCodePath, "ui_lawra.R"))
