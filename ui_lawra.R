ui_lawra <- fluidPage(
  titlePanel("LawRa"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dirInput", "Select Directory", buttonLabel = "Browse..."),
      actionButton("loadBtn", "Load Directory"),
      textOutput("envVarOutput")
    ),
    mainPanel(
      h3("Directory Summary"),
      verbatimTextOutput("summaryOutput"),
      h3("File Details"),
      tableOutput("fileTable")
    )
  )
)