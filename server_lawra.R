
server_lawra <- function(input, output, session) {
  dir_path <- reactiveVal()
  
  observeEvent(input$dirInput, {
    if (!is.null(input$dirInput$datapath)) {
      dir_path(input$dirInput$datapath)
    }
  })
  
  observeEvent(input$loadBtn, {
    req(dir_path())
    output$envVarOutput <- renderText({
      paste("Directory path selected:", dir_path())
    })
    
    # Get file details
    file_details <- get_file_details(dir_path())
    
    # Directory summary
    output$summaryOutput <- renderPrint({
      folders <- sum(file.info(file_details$File)$isdir)
      files <- nrow(file_details)
      pdf_files <- sum(tolower(file_details$File) %like% "\\.pdf$")
      paste("Folders:", folders, "\nFiles:", files, "\nPDF Files:", pdf_files)
    })
    
    # Display file details table
    output$fileTable <- renderTable({
      file_details
    })
  })
}


