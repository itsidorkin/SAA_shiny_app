library(plotly)
library(DT)
library(shinycssloaders)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "k-means & DBSCAN"),
  dashboardSidebar(
    navbarPage(
      title = 'Select',
      tabPanel(
        'file', fileInput(
          "file1", "Select file", F,
          c("text/csv", ".csv", ".dat",
            "text/comma-separated-values,text/plain"
          )
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
      "del", "delete column", placeholder = "1 2 3..."
    ),
    checkboxInput("header", "Header", T),
    
    numericInput("k", "clusters (for k-means)", value = 3, min = 2),
    numericInput("eps", "eps (for dbscan)", value = 0.5, min = 0, step = 0.1),
    numericInput("minpts", "minpts (for dbscan)", value = 4, min = 1)
    
    #downloadButton("downloadTable1", "Save result k-means (CSV)"),
    #downloadButton("downloadTable2", "Save result dbscan (CSV)")
  ),
  dashboardBody(
    navbarPage(
      title = "Machin learning",
      tabPanel("Table", dataTableOutput("table")),
      tabPanel("kmeans 2d", plotOutput(
        "graphics_2d_k", width = "800px", height = "600px") %>% withSpinner(),
        tableOutput("textk2")
      ),
      tabPanel("kmeans 3d+", plotlyOutput(
        "graphics_3d_k", width = "800px", height = "600px") %>% withSpinner(),
        tableOutput("textk3")
      ),
      tabPanel("dbscan 2d",
               navbarPage(title = '',
                          tabPanel("v2 fast ggplot2", plotOutput("graphics_2d_D_v2_f", width = "800px", height = "600px") %>% withSpinner()),
                          tabPanel("v2 slow ggplot2", plotOutput("graphics_2d_D_v2_s", width = "800px", height = "600px") %>% withSpinner()),
                          tabPanel("v2 plot_ly", plotlyOutput("graphics_2d_D_v2_plotly", width = "800px", height = "600px") %>% withSpinner())
                          # tabPanel("v1 slow ggplot2", plotOutput("graphics_2d_D_v1", width = "800px", height = "600px") %>% withSpinner())
                          ),
               tableOutput("textd2")
      ),
      tabPanel("dbscan 3d+",
               navbarPage(title = '',
                          tabPanel("v2", plotlyOutput("graphics_Nd_D_v2", width = "800px", height = "600px") %>% withSpinner())
                          # tabPanel("v1", plotlyOutput("graphics_Nd_D_v1", width = "800px", height = "600px") %>% withSpinner())
               ),
               tableOutput("textd3")
      )
    )
  )
)