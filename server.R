#Condicional

library(shiny)
library(plotly)

#setwd("C:/Users/isai_/Documents/DataScience/Modulo 2 R/Proyecto/www/Github-Proyecto/")

shinyServer(function(input, output) {
  library(ggplot2)
  library(dplyr)
  library(DescTools)
  violencia_mexico <-  read.csv("./csv/violencia_mexico.csv", header = T)
  
  
  #Gráfico de Histograma
  output$plot_histograma <- renderPlotly({
    estado <- filter(violencia_mexico, Estado == input$estado & (Tipo_delito == input$tipo_delito)) # Busca un estado y un delito
    
    x <- estado$Casos
    bin <- seq(min(x), max(x), length.out = input$bins + 1)

    ggplotly(
      estado %>%
        ggplot() + 
        aes(Casos) +
        geom_histogram( breaks = bin, col="black", fill = "blue") +
        labs(x = "Casos (por mes de Ene 2015 hasta Ene 2022)", y = "Frecuencia",
             title = paste("Casos de", estado$Tipo_delito[1], "en", estado$Estado[1])) +
        theme_light()
    )
    
    
  })
  # Gráficas de lineas
  output$plot_graph_line <- renderPlotly({ 
    vio_mexico <- filter(violencia_mexico, (Tipo_delito == input$tipo_delito_gl)) # Busca un estado y un delito
    vio_mexico <- mutate(vio_mexico, Fecha = as.Date(Fecha, "%Y-%m-%d"),Cve_estado = as.numeric(Cve_estado), Casos = as.numeric(Casos))
    
  
    ggplotly(
      vio_mexico %>%
        ggplot( aes(x=Fecha, y=Casos, group=factor(Estado), color=factor(Estado))) +
        geom_line() +
        geom_point(size = 1) +
        theme_bw() +
        theme(
          panel.border = element_blank(),
          panel.grid.major = element_line(color = "grey45"),
          panel.grid.minor = element_line(color = "grey25"),
          plot.caption =element_text(size=18,  face="italic", color="grey"),
          legend.position = "bottom")
    )
    
    

    
  })   
  
  # Gráficas de barras 1
  output$plot_graph_bar <- renderImage({ 
    library(tidyverse)
    library(gganimate)
    
    vio_mex_del <- read_csv("./csv/AnioEstadoDelito.csv")
    vio_mex_del <- mutate(vio_mex_del, Anio = as.numeric(stringr::str_sub(Anio,1,4)))
    vio_mex_delito <- filter(vio_mex_del, Tipo_delito == input$tipo_delito_an) # Busca un estado y un delito
    
    gdp_formatted_example <- vio_mex_delito %>%
      group_by(Anio) %>%
      # The * 1 makes it possible to have non-integer ranks while sliding
      mutate(rank = rank(-Casos),
             Value_rel = Casos/Casos[rank==1],
             Value_lbl = paste0(" ",round(Casos))) %>%
      group_by(Estado,Tipo_delito) %>% 
      filter(rank <=10) %>%
      ungroup()
    
    # Animation
    
    
    anim <- ggplot(gdp_formatted_example, aes(rank, group = Estado, 
                                              fill = as.factor(Estado), color = as.factor(Estado))) +
      geom_tile(aes(y = Casos/2,
                    height = Casos,
                    width = 0.9), alpha = 0.8, color = NA) +
      geom_text(aes(y = 0, label = paste(Estado, " ")), vjust = 0.2, hjust = 1) +
      geom_text(aes(y=Casos,label = Value_lbl, hjust=0)) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_reverse() +
      guides(color = FALSE, fill = FALSE) +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major.x = element_line( size=.1, color="grey" ),
            panel.grid.minor.x = element_line( size=.1, color="grey" ),
            plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
            plot.subtitle=element_text(size=15, hjust=0.5, face="italic", color="grey"),
            plot.caption =element_text(size=18, hjust=0.5, face="italic", color="grey"),
            plot.background=element_blank(),
            plot.margin = margin(2,2, 2, 4, "cm")) +
      transition_states(Anio, transition_length = 4, state_length = 1, wrap = FALSE) +
      view_follow(fixed_x = TRUE)  +
      labs(title = paste('Casos de', gdp_formatted_example$Tipo_delito[1], 'por Año : {closest_state}'),  
           subtitle  =  "Top 10 Estados",
           caption  = "Violencia en México | Fuente de datos: Gobierno de México") 
    
    anim_save("./outfile.gif", animate(anim, 150,fps = 6, width = 1200)) # New
    
    list(src = "./outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )
  })   
  
  #Gráfico de barras 2
  output$plot_graph_bar2 <- renderPlotly({
    
    library(tidyverse)
    library(hrbrthemes)
    library(kableExtra)
    
    AnioEstadoDelito <- read_csv("./csv/AnioEstadoDelito.csv")
    
    AnioEstadoDelito <- mutate(AnioEstadoDelito, Fecha = as.numeric(Anio))
    vio_mexico <- filter(AnioEstadoDelito, Fecha == input$anios_an2 & (Tipo_delito == input$tipo_delito_an2)) # Seleccionamos solo a Gto
    
    # Plot 
    ggplotly(vio_mexico %>%
      ggplot( aes(x=Estado, y=Casos) ) +
      geom_segment( aes(x=Estado ,xend=Estado, y=0, yend=Casos), color="grey") +
      geom_point(size=3, color="#69b3a2") +
      coord_flip() +
      theme_ipsum() +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none"
      ) +
      xlab("Estados")
    )
    
    
    
  })
  
  
  #Gráfico de barras 3
  output$plot_graph_bar3 <- renderPlotly({
    
    library(tidyverse)
    library(hrbrthemes)
    library(kableExtra)
    
    AnioEstadoDelito <- read_csv("./csv/AnioEstadoDelito.csv")
    
    AnioEstadoDelito <- mutate(AnioEstadoDelito, Fecha = as.numeric(Anio))
    vio_mexico <- filter(AnioEstadoDelito, Fecha == input$anios_an2 & (Tipo_delito == input$tipo_delito_an2)) # Seleccionamos solo a Gto
    
    ggplotly(vio_mexico %>%
      filter(!is.na(Casos)) %>%
      arrange(Casos) %>%
      mutate(Estado=factor(Estado, Estado)) %>%
      ggplot( aes(x=Estado, y=Casos) ) +
      geom_segment( aes(x=Estado ,xend=Estado, y=0, yend=Casos), color="blue") +
      geom_point(size=3, color="#69b3a2") +
      coord_flip() +
      theme_ipsum() +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none"
      ) +
      xlab("")
    )
    
  })
  
  
  #Gráfico de barras 4
  output$plot_graph_bar4 <- renderPlotly({
    
    library(tidyverse)
    library(hrbrthemes)
    library(kableExtra)
    
    AnioEstadoDelito <- read_csv("./csv/AnioEstadoDelito.csv")
    
    AnioEstadoDelito <- mutate(AnioEstadoDelito, Fecha = as.numeric(Anio))
    vio_mexico <- filter(AnioEstadoDelito, Fecha == input$anios_an2 & (Tipo_delito == input$tipo_delito_an2)) # Seleccionamos solo a Gto
    
    
    # Barplot
    ggplotly(vio_mexico %>%
      filter(!is.na(Casos)) %>%
      arrange(Casos) %>%
      tail(20) %>%
      mutate(Estado=factor(Estado, Estado)) %>%
      ggplot( aes(x=Estado, y=Casos, fill=Casos) ) +
      geom_bar(stat="identity") +
      viridis::scale_fill_viridis() +
      coord_flip() +
      theme_ipsum() +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none"
      ) +
      xlab("") +
      ylab(paste("Casos de",vio_mexico$Tipo_delito[1],"en México en el año",vio_mexico$Anio[1]))
    )
    
    
  })
  
  
  # Series de tiempo
  output$plot_graph_time_series <- renderPlot({ 
    estado <- filter(violencia_mexico, Estado == input$estado_ts & (Tipo_delito == input$tipo_delito_ts)) # Busca un estado y un delito
    estado <- mutate(estado, Fecha = as.Date(Fecha, "%Y-%m-%d"),Cve_estado = as.numeric(Cve_estado), Casos = as.numeric(Casos))
    
    estado.ts <- ts(estado[, 5],start = 2015, freq = 12)
    estado.decom.A <- decompose(estado.ts)
    Tendencia <- estado.decom.A$trend
    
    line_trend = input$line_trend
    New.series <- window(estado.ts, start = c(2015, 1), end = c(2022, 1)) 
    New.time <- time(New.series)
    
    plot(estado.ts, ylab = "Número de casos", xlab = "Tiempo", 
       main = paste("Casos de", estado$Tipo_delito[1], "en", estado$Estado[1]), 
       sub = "Casos en el periodo 2015-2022")
    
    
    if(line_trend){
      abline(reg = lm(New.series ~ New.time))
      lines(Tendencia, lwd = 2, col = "blue")
    }
    
  })   
  
  output$plot_graph_time_series_decom <- renderPlot({ 
    estado <- filter(violencia_mexico, Estado == input$estado_ts & (Tipo_delito == input$tipo_delito_ts)) # Busca un estado y un delito
    estado <- mutate(estado, Fecha = as.Date(Fecha, "%Y-%m-%d"),Cve_estado = as.numeric(Cve_estado), Casos = as.numeric(Casos))
    
    estado.ts <- ts(estado[, 5],start = 2015, freq = 12)
    estado.decom.A <- decompose(estado.ts)
    plot(estado.decom.A, xlab = "Tiempo", 
         sub = "Descomposición de los datos de producción de electricidad")
    
    
  })   
  
  #Data Table
  output$data_table <- renderDataTable( {mtcars}, 
                                        options = list(aLengthMenu = c(5,25,50),
                                                       iDisplayLength = 5)
  )
  
})