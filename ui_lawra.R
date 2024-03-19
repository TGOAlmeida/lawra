

# Define UI
ui_lawra <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Directory File List"),
  sidebarLayout(
    sidebarPanel(
      textInput("dirInput", "Enter Directory Path:", placeholder = "/path/to/directory"),
      actionButton("loadBtn", "Load Directory")
    ),
    mainPanel(
      h3("File List"),
      #tableOutput("fileList")
      DT::dataTableOutput("fileList")
    )
  )
)

# Define server logic
server_lawra <- function(input, output, session) {
  # Create a reactiveValues object to store the file list
  fileList <- reactiveVal(NULL)
  
  observeEvent(input$loadBtn, {
    dir_path <- input$dirInput
    
    # Check if the directory path is not empty
    if (!is.null(dir_path) && dir_path != "") {
      # Get the list of files in the directory
      total_file_list <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
      total_file_data <- create_df_list_of_files(total_file_list)
      
      # Update the file list reactive value
      fileList(total_file_data)
    } else {
      # If directory path is empty, set file list to NULL
      fileList(NULL)
    }
  })
  
  # Display the file list as a dataframe
  output$fileList <- renderTable({
    fileList()
  })
}