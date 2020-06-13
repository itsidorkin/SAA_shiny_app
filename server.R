library(ggplot2)
library(ggalt)
library(ggforce)
library(SAA)
library(DT)
library(plotly)
function(input, output, session) {
  
  data <- reactive({
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
      datatable(data(), list(searching = F))
    }
  )
  
  output$textt3 <- reactive({
    if (is.null(input$file1) & 
        (input$file2 == "")) {
      return(NULL)
    } else {
      length(unique(data()[,2]))
    }
  })
  
  kmeans <- reactive({
    skmeans(data(),input$k)$result
  })
  
  plot3d <- function(clstr) {
    plot_ly(data(),
            x = data()[, 1], 
            y = data()[, 2], 
            z = data()[, 3],
            color = clstr,
            stroke = I('black')
    ) %>%
      add_markers()
  }
  
  plotNd <- function(clstr) {
    dimensions <- list()
    for (i in 1:ncol(data())) {
      dimensions[[i]] <- list(
        values = data()[, i], 
        label = colnames(data())[i]
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
      if (ncol(data()) == 2) {
        ggplot() + geom_point(
          data = kmeans(),
          aes(
            kmeans()[, 1], 
            kmeans()[, 2], 
            colour = clstr
          ),
          size = 4,
          show.legend = FALSE
        ) + geom_encircle(
          data = kmeans(),
          aes(
            kmeans()[, 1], 
            kmeans()[, 2], 
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
      if (ncol(data()) == 3) {
        plot3d(kmeans()$clstr)
      } else {
        if (ncol(data()) > 3) {
          plotNd(kmeans()$clstr)
        }
      }
    }
  })
  
  output$textK3 <-renderTable({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) >= 3) {
        table <- matrix()
        clstr <- kmeans()$clstr
        total <- nrow(data())
        for (i in 1:length(unique(clstr))) {
          table[i] <- paste0(
            i, " Кластер: ", 
            round(length(clstr[clstr == i]) / 
                    total * 100, 2),
            " % (", length(clstr[clstr == i]), 
            ")")
        }
        table
      }
    }
  })
  
  output$downloadTable1 <- downloadHandler(
    filename = "kmeans.csv",
    content = function(file) {
      write.table(
        skmeans(data(), input$k)$result,
        file,
        sep = ";",
        row.names = F,
        col.names = F
      )
    }
  )
  
  #---------------------------------------------
  
  dbscan <- reactive({
    sdbscan(data(), input$eps, input$minpts)
  })
  
  output$graphics2 <- renderPlot({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 2) {
        ggplot(
        ) + geom_circle(
          data = dbscan()$graphics$circle,
          aes(
            x0 = dbscan()$graphics$circle[, 1],
            y0 = dbscan()$graphics$circle[, 2],
            r = input$eps,
            fill = clstr,
            color = clstr
          ),
          alpha = 0.2,
          show.legend = T
        ) + geom_encircle(
          data = dbscan()$graphics$encircle,
          aes(
            x = dbscan()$graphics$encircle[, 1],
            y = dbscan()$graphics$encircle[, 2],
            group = clstr
          ),
          alpha = 1,
          expand = 0,
          s_shape = 0.8
        ) + labs(title = "DBSCAN"
        ) + geom_circle(
          aes(
            x0 = dbscan()$graphics$noise[, 1],
            y0 = dbscan()$graphics$noise[, 2],
            r = input$eps),
          alpha = 1
        ) + geom_point(
          aes(data()[, 1], data()[, 2])
        )
      }
    }
  }, height = 600, width = 600)
  
  output$graphics4 <- renderPlotly({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 3) {
        plot3d(dbscan()$result$clstr)
      } else {
        if (ncol(data()) > 3) {
          plotNd(dbscan()$result$clstr)
        }
      }
    }
  })
  
  output$textd3 <-renderTable({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) >= 3) {
        table <- matrix()
        clstr <- dbscan()$result$clstr
        total <- nrow(data())
        for (i in 1:length(unique(clstr))) {
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
        sdbscan(data(), input$eps, input$minpts)$result,
        file,
        sep = ";",
        row.names = F,
        col.names = F
      )
    }
  )

  session$onSessionEnded(stopApp)
}