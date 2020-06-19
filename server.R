library(ggplot2)
library(ggalt)
library(ggforce)
library(SAA)
library(DT)
library(plotly)

function(input, output, session) {
  
  #-------------------------------
  
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
    
    read.csv(a,
             header = input$header,
             sep = input$sep,
             dec = input$dec)[del]
  })
  
  plot2d_k <- function(dt) {
    ggplot() + geom_encircle(
      data = dt,
      aes(
        dt[, 1], 
        dt[, 2], 
        fill = clstr
      ),
      alpha = 2/3,
      expand = 0,
      s_shape = 1,
    ) + labs(title = "k-means"
    ) + geom_point(
      data = dt,
      aes(
        dt[, 1], 
        dt[, 2]
      )
    ) 
  }
  
  plot2d_d <- function(dt) {
    ggplot(
    ) + geom_circle(
      data = dt$graphics$circle,
      aes(
        x0 = dt$graphics$circle[, 1],
        y0 = dt$graphics$circle[, 2],
        r = input$eps,
        fill = clstr,
        color = clstr
      ),
      alpha = 0.5,
      show.legend = T
    ) + geom_encircle(
      data = dt$graphics$encircle,
      aes(
        dt$graphics$encircle[, 1],
        dt$graphics$encircle[, 2],
        group = clstr
      ),
      expand = 0,
      s_shape = 1
    ) + labs(title = "DBSCAN"
    ) + geom_circle(
      aes(
        x0 = dt$graphics$noise[, 1],
        y0 = dt$graphics$noise[, 2],
        r = input$eps),
      alpha = 1
    ) + geom_point(
      aes(data()[, 1], data()[, 2])
    )
  }
  
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
  
  anlz_clstr_k <- function(x) {
    table <- matrix()
    clstr <- x$clstr
    total <- nrow(data())
    for (i in 1:length(unique(clstr))) {
      table[i] <- paste0(
        i, " Кластер: ", 
        round(length(clstr[clstr == i]) / 
                total * 100, 2),
        " % (", length(clstr[clstr == i]), 
        ")")
    }
    return(table)
  }
  
  output$table <- renderDataTable(
    if (is.null(input$file1) & 
        (input$file2 == "")) {
      return(NULL)
    } else {
      datatable(data(), list(searching = F))
    }
  )
  
  anlz_clstr_d <- function(x) {
    table <- matrix()
    clstr <- x$result$clstr
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
    return(table)
  }
  
  #-------------------------------
  
  kmeans <- reactive({
    skmeans(data(),input$k)
  })
  
  output$graphics1 <- renderPlot({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 2) {
        plot2d_k(kmeans())
      }
    }
  })
  
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
  
  output$textk3 <-renderTable({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) >= 3) {
        anlz_clstr_k(kmeans())
      }
    }
  })
  
  output$textk2 <-renderTable({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 2) {
        anlz_clstr_k(kmeans())
      }
    }
  })
  
  output$downloadTable1 <- downloadHandler(
    filename = "kmeans.csv",
    content = function(file) {
      write.table(
        kmeans(),
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
        plot2d_d(dbscan())
      }
    }
  })
  
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
        anlz_clstr_d(dbscan())
      }
    }
  })
  
  output$textd2 <-renderTable({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 2) {
        anlz_clstr_d(dbscan())
      }
    }
  })
  
  output$downloadTable2 <- downloadHandler(
    filename = "dbscan.csv",
    content = function(file) {
      write.table(
        dbscan()$result,
        file,
        sep = ";",
        row.names = F,
        col.names = F
      )
    }
  )

  session$onSessionEnded(stopApp)
}