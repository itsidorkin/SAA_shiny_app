library(plotly)
library(DT)
library(shinycssloaders)
fluidPage(
  titlePanel("Interface"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      navbarPage(
        title = 'Select',
        tabPanel(
          'file', fileInput(
            "file1", "Select file", F,
            c("text/csv",
              "text/comma-separated-values,text/plain",
              ".csv", ".dat")
          )
        ),
        tabPanel(
          'url', textInput(
            "file2", "URL", placeholder = "https://..."
          )
        )
      ),
      
      selectInput(
        "sep", "separator", 
        c("," = ",", ";" = ";", "Tab" = "\t", "space" = " "), 
        ";"
      ),
      selectInput(
        "dec", "decimal", 
        c("." = ".", "," = ","), "."
      ),
      textInput(
        "del", "delete column",placeholder = "1 2 3..."
      ),
      checkboxInput("header", "Header", T), 
      numericInput("k", "count clasters (for kmeans)", 
                   value = 3, min = 2),
      numericInput("eps", "eps (for dbscan)", 
                   value = 0.5, min = 0, step = 0.1),
      numericInput("minpts", "minpts (for dbscan)", 
                   value = 4, min = 1),
      numericInput("shape", "1", 
                   value = 1, min = 1),
      downloadButton("downloadTable1", 
                     "Save result kmeans (CSV)"),
      downloadButton("downloadTable2", 
                     "Save result dbscan (CSV)")
    ),
    mainPanel(
      navbarPage(
        title = "Machin learning",
        tabPanel("Table", dataTableOutput("table"),
                 textOutput("textt3")),
        tabPanel("kmeans 2d", plotOutput("graphics1") %>% withSpinner()),
        tabPanel("kmeans 3d+", plotlyOutput(
          "graphics3", width = "800px", height = "600px") %>% withSpinner(), 
          tableOutput("textK3")),
        tabPanel("dbscan 2d", plotOutput("graphics2")%>% withSpinner()),
        tabPanel("dbscan 3d+", plotlyOutput(
          "graphics4", width = "800px", height = "600px") %>% withSpinner(),
          tableOutput("textd3"))
      )
    )  
  )
)
