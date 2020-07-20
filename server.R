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
    p <- ggplot()
    if (nrow(dt$graphics$noise) != 0) {
      p <- p + geom_circle(
        aes(
          x0 = dt$graphics$noise[, 1],
          y0 = dt$graphics$noise[, 2],
          r = input$eps),
        alpha = 1
      )
    }
    if (nrow(dt$graphics$circle) != 0) {
      p <- p + geom_circle(
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
      )
    }
    p + labs(title = "dbscan_v1"
    ) + geom_point(
      aes(data()[, 1], data()[, 2])
    )
  }
  
  plot2d_d_v2_f <- function(df) {
    df_wo_noise <- df[df$clstr != 0,]
    df_only_noise <- df[df$clstr == 0,]
      ggplot() +
        geom_encircle(
          aes(df_wo_noise[,1],
              df_wo_noise[,2],
              fill = df_wo_noise$clstr),
          alpha = 2 / 3,
          expand = 0,
          s_shape = 1
        ) + geom_point(
          aes(
            df_wo_noise[,1], 
            df_wo_noise[,2]
          )
        ) + geom_point(
          aes(
            df_only_noise[,1], 
            df_only_noise[,2]
          ), 
          size = 4
        ) + labs(title = "dbscan_v2_f")
  }
  
  plot2d_d_v2_s <- function(df) {
    df_wo_noise <- df[df$clstr != 0,]
    df_only_noise <- df[df$clstr == 0,]
    p <- ggplot()
    if (nrow(df_wo_noise) != 0) {
      p <- p + geom_circle(
        aes(
          x0 = df_wo_noise[,1],
          y0 = df_wo_noise[,2],
          r = input$eps,
          fill = df_wo_noise$clstr,
          color = df_wo_noise$clstr
        ),
        alpha = 0.5,
        show.legend = T
      ) + geom_point(
        aes(
          df_wo_noise[,1], 
          df_wo_noise[,2]
        )
      )
    }
    if (nrow(df_only_noise) != 0) {
      p <- p + geom_point(
        aes(
          df_only_noise[,1], 
          df_only_noise[,2]
        ), 
        size = 4
      ) 
    }
    p + labs(title = "dbscan_v2_s"
    ) + geom_point(
      aes(data()[, 1], data()[, 2])
    )
  }
  
  plot2d_D_v2_plotly <- function(df) {
    plot_ly(x = df[, 1], 
            y = df[, 2], 
            color = df$clstr,
            stroke = I('black'),
            alpha_stroke = 1/3
    ) %>%
      add_markers()
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
  
  anlz_clstr_d <- function(x) {
    table <- matrix()
    clstr <- x$clstr
    total <- nrow(data())
    
    if (length(clstr[clstr == 0]) != 0) {
      len <- length(unique(clstr)) - 1
    } else {
      len <- length(unique(clstr))
    }
    
    for (i in 1:len) {
      table[i] <- paste0(
        i, " Кластер: ", 
        round(length(clstr[clstr == i]) / 
                total * 100, 2),
        " % (", length(clstr[clstr == i]), 
        ")")
    }
    table[len + 1] <- paste0(
      0, " Кластер: ", 
      round(length(clstr[clstr == 0]) / 
              total * 100, 2),
      " % (", length(clstr[clstr == 0]), 
      ")")
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
  
  #-------------------------------
  
  kmeans <- reactive({
    skmeans(data(),input$k)
  })
  
  output$graphics_2d_k <- renderPlot({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 2) {
        plot2d_k(kmeans())
      }
    }
  })
  
  output$graphics_3d_k <- renderPlotly({
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
  
  # dbscan_v1 <- reactive({
  #   sdbscan(data(), input$eps, input$minpts)
  # })
  
  dbscan_v2 <- reactive({
    sdbscan_v2(data(), input$eps, input$minpts)
  })
  
  # output$graphics_2d_D_v1 <- renderPlot({
  #   if ((is.null(input$file1) & 
  #        (input$file2 == ""))) {
  #     return(NULL)
  #   } else {
  #     if (ncol(data()) == 2) {
  #       plot2d_d(dbscan_v1())
  #     }
  #   }
  # })
  
  output$graphics_2d_D_v2_f <- renderPlot({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 2) {
        plot2d_d_v2_f(dbscan_v2())
      }
    }
  })
  
  output$graphics_2d_D_v2_s <- renderPlot({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 2) {
        plot2d_d_v2_s(dbscan_v2())
      }
    }
  })
  
  output$graphics_2d_D_v2_plotly <- renderPlotly({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 2) {
        plot2d_D_v2_plotly(dbscan_v2())
      }
    }
  })
  
  # output$graphics_Nd_D_v1 <- renderPlotly({
  #   if ((is.null(input$file1) & 
  #        (input$file2 == ""))) {
  #     return(NULL)
  #   } else {
  #     if (ncol(data()) == 3) {
  #       plot3d(dbscan_v1()$result$clstr)
  #     } else {
  #       if (ncol(data()) > 3) {
  #         plotNd(dbscan_v1()$result$clstr)
  #       }
  #     }
  #   }
  # })
  
  output$graphics_Nd_D_v2 <- renderPlotly({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 3) {
        plot3d(dbscan_v2()$clstr)
      } else {
        if (ncol(data()) > 3) {
          return(NULL)
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
        anlz_clstr_d(dbscan_v2())
      }
    }
  })
  
  output$textd2 <-renderTable({
    if ((is.null(input$file1) & 
         (input$file2 == ""))) {
      return(NULL)
    } else {
      if (ncol(data()) == 2) {
        anlz_clstr_d(dbscan_v2())
      }
    }
  })
  
  output$downloadTable2 <- downloadHandler(
    filename = "dbscan_v2.csv",
    content = function(file) {
      write.table(
        dbscan_v2()$result,
        file,
        sep = ";",
        row.names = F,
        col.names = F
      )
    }
  )

  session$onSessionEnded(stopApp)
}