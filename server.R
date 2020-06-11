library(ggplot2)
library(ggalt)
library(ggforce)
library(SAA)
library(DT)
library(plotly)
function(input, output) {
  
  dataInput <- reactive({
    if (input$file2 != "") {
      a <- input$file2
    } else {
      a <- input$file1$datapath
    }
    
    if (input$del != "")
      del <- -as.numeric(t(data.frame(
        strsplit(x = input$del, split = " "))))
    else
      del <- T
    
    tab <- read.csv( a,
                     header = input$header,
                     sep = input$sep,
                     dec = input$dec)[del]
  })
  
  output$table <- renderDataTable(
    if (is.null(input$file1) & 
        (input$file2 == "")) {
      return(NULL)
    } else {
      datatable(dataInput(), list(searching = F))
    }
  )
  
  output$textt3 <- reactive({
    if (is.null(input$file1) & 
        (input$file2 == "")) {
      return(NULL)
    } else {
      length(unique(dataInput()[,2]))
    }
  })
  
  main_function1 <- reactive({
    skmeans(dataInput(),input$k)$result
  })
  
  plot3d <- function(clstr) {
    plot_ly(dataInput(),
            x = dataInput()[, 1], 
            y = dataInput()[, 2], 
            z = dataInput()[, 3],
            color = clstr,
            stroke = I('black')
    ) %>%
      add_markers()
  }
  
  plotNd <- function(clstr) {
    dimensions <- list()
    for (i in seq(ncol(dataInput()))) {
      dimensions[[i]] <- list(
        values = dataInput()[, i], 
        label = colnames(dataInput())[i]
      )
    }
    plot_ly(type = 'splom',
            dimensions = dimensions,
            color = clstr,
            stroke = I('black'),
            marker = list(size = 12)
    ) %>% style(
      diagonal = list(visible = F), 
      showupperhalf = F
    ) %>% layout(
      plot_bgcolor='rgb(245, 245, 245)'
    )
  }
  
  output$graphics1 <- renderPlot({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(dataInput()) == 2) {
        bin_kmean <- main_function1()
        ggplot() + geom_point(
          data = bin_kmean,
          aes(
            bin_kmean[, 1], 
            bin_kmean[, 2], 
            colour = clstr
          ),
          size = 4,
          show.legend = FALSE
        ) + geom_encircle(
          data = bin_kmean,
          aes(
            bin_kmean[, 1], 
            bin_kmean[, 2], 
            fill = clstr
          ),
          alpha = 0.2,
          expand = 0,
          s_shape = 0.8,
        ) + labs(title = "k-means")
      }
    }
  }, height = 600, width = 600)
  
  output$graphics3 <- renderPlotly({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(dataInput()) == 3) {
        plot3d(main_function1()$clstr)
      } else {
        if (ncol(dataInput()) > 3) {
          plotNd(main_function1()$clstr)
        }
      }
    }
  })
  
  output$textK3 <-renderTable({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(dataInput()) >= 3) {
        table <- matrix()
        clstr <- main_function1()$clstr
        total <- nrow(dataInput())
        for (i in c(1:length(unique(clstr)))) {
          table[i] <- paste0(
            i, " Кластер: ", 
            round(length(clstr[clstr == i]) / 
                    total * 100, 2),
            " % (", length(clstr[clstr == i]), ")")
        }
        table
      }
    }
  })
  
  output$downloadTable1 <- downloadHandler(
    filename = "kmeans.csv",
    content = function(file) {
      write.table(
        skmeans(dataInput(), input$k)$result,
        file,
        sep = ";",
        row.names = F,
        col.names = F
      )
    }
  )
  
  #---------------------------------------------
  
  main_function2 <- reactive({
    sdbscan(dataInput(), input$eps, input$minpts)
  })
  
  output$graphics2 <- renderPlot({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(dataInput()) == 2) {
        bin_dbscan <- main_function2()$graphics
        ggplot(
        ) + geom_point(
          aes(dataInput()[, 1], dataInput()[, 2]),
          size = 1
        ) + geom_circle(
          data = bin_dbscan$circle,
          iaes(
            x0 = bin_dbscan$circle[, 1],
            y0 = bin_dbscan$circle[, 2],
            r = input$eps,
            fill = clstr,
            color = clstr
          ),
          alpha = 0.25,
          show.legend = T
        ) + geom_encircle(
          data = bin_dbscan$encircle,
          aes(
            x = bin_dbscan$encircle[, 1],
            y = bin_dbscan$encircle[, 2],
            group = clstr
          ),
          alpha = 1,
          expand = 0,
          s_shape = 0.75
        ) + labs(title = "DBSCAN"
        ) + geom_circle(
          aes(
            x0 = bin_dbscan$noise[, 1],
            y0 = bin_dbscan$noise[, 2],
            r = input$eps),
          alpha = 1,
          show.legend = FALSE
        )
      }
    }
  }, height = 600, width = 600)
  
  output$graphics4 <- renderPlotly({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(dataInput()) == 3) {
        plot3d(main_function2()$result$clstr)
      } else {
        if (ncol(dataInput()) > 3) {
          plotNd(main_function2()$result$clstr)
        }
      }
    }
  })
  
  output$textd3 <-renderTable({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(dataInput()) >= 3) {
        table <- matrix()
        clstr <- main_function2()$result$clstr
        total <- nrow(dataInput())
        for (i in c(1:length(unique(clstr)))) {
          table[i] <- paste0(
            i," кластер: ",
            if (i != length(unique(clstr))) {
              round(length(clstr[clstr == i]) / 
                      total * 100, 2)
            } else {
              round(length(clstr[clstr == "noise"]) / 
                      total * 100, 2)
            }, " % (", 
            if (i != length(unique(clstr))) {
              round(length(clstr[clstr == i]))
            } else {
              round(length(clstr[clstr == "noise"]))
            },")")
        }
        table
      }
    }
  })
  
  output$downloadTable2 <- downloadHandler(
    filename = "dbscan.csv",
    content = function(file) {
      write.table(
        sdbscan(dataInput(), input$eps, input$minpts)$result,
        file,
        sep = ";",
        row.names = F,
        col.names = F
      )
    }
  )
}