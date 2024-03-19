################################################################################
# Removing all objects
rm(list=ls())


################################################################################
# Set project folder
dirPrj <- "/Users/tiago/Documents/Projetos/Darci/Input"
dirPrj <- "C:/Users/darcis/Desktop/Contratos_teste"
setwd(dirPrj)

################################################################################
# Loading libraries and codes
list.of.packages <- c("tidyverse", "pdftools", "stringr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

for (package in list.of.packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    if (!require(package, character.only = TRUE)) {
      print(paste("Package", package, "could not be loaded."))
    }
  }
}

source("00.Functions.R")


################################################################################
# Set input folder
dirArq2read <- "X:/Juridico/_ Contratos digitalizados/Desenvolvimento de Negócios"
dirArq2read <- "C:/Users/darcis/Desktop/Contratos_teste"
dirArq2read <- paste(dirPrj, "/Input", sep="")


################################################################################
# Set output folder
dirArq2save <- "C:/Users/darcis/Desktop/Contratos_rede"
dirArq2save <- paste(dirPrj, "/Output", sep="")


################################################################################
## Read every file
total_file_list <- list.files(dirArq2read, recursive = TRUE, full.names = TRUE)
total_file_data <- create_df_list_of_files(total_file_list)


################################################################################
## Select observations that are readable pdf

selected_file_data <- total_file_data %>% filter(is_pdf == TRUE & readable_pdf == TRUE)

df_analytic <- data.frame()

for(i in 1:dim(selected_file_data)[1])
{
  df_loop <- selected_file_data[i,]
  df <- get_info(df_loop)
  df_analytic <- rbind(df_analytic, df)
}

