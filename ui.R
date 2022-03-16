#Condicional
library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)
library(dplyr)
library(plotly)



#setwd("C:/Users/isai_/Documents/DataScience/Modulo 2 R/Proyecto/www/Github-Proyecto/")
violencia_mexico <-  read.csv("./csv/violencia_mexico.csv", header = T)
estados <- unique(violencia_mexico$Estado)   # Estados que contiene la BDD
violencia_mexico <- mutate(violencia_mexico, Fecha = as.Date(Fecha, "%Y-%m-%d"),Cve_estado = as.numeric(Cve_estado), Casos = as.numeric(Casos))
anios <- unique( format(violencia_mexico$Fecha, "%Y"))   # Estados que contiene la BDD

tipo_delito <- unique(violencia_mexico$Tipo_delito)   # Estados que contiene la BDD
tipo_delito
fluidPage(
  
  dashboardPage(
    
    dashboardHeader(title = "Dashboard Violencia México"),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Histograma", tabName = "Dashboard", icon = icon("dashboard")),
        menuItem("Graficos lineales", tabName = "graph_line", icon = icon("line-chart")),
        menuItem("Graficos de Barras Animado", tabName = "graph_bar_an", icon = icon("bar-chart")),
        menuItem("Graficos de Barras", tabName = "graph_bar", icon = icon("bar-chart")),
        menuItem("Series de tiempo", tabName = "graph_time_series", icon = icon("area-chart"))
        #menuItem("Dispersión", tabName = "graph", icon = icon("area-chart")),
        #menuItem("Data Table", tabName = "data_table", icon = icon("table"))
      )
      
    ),
    
    dashboardBody(
      
      tabItems(
        
        # Histograma
        tabItem(tabName = "Dashboard",
                
                fluidPage(
                  fluidRow(
                    titlePanel("Histograma de violencia en México"), 
                    selectInput("estado", "Seleccione el estado",
                                choices = estados),
                    
                    selectInput("tipo_delito", "Selecciona el tipo de delito", 
                                
                                choices = tipo_delito),
                    box(plotlyOutput("plot_histograma", height = 250)),
                    
                    box(
                      title = "Controles",
                      sliderInput("bins", "Número de observaciones:", 1, 30, 15)
                    )
                  )
                )
        ),
        
        # Grafico Lineal
        tabItem(tabName = "graph_line", 
                fluidPage(
                  fluidRow(
                    titlePanel(h3("Gráficos lineales")),
                    selectInput("tipo_delito_gl", "Selecciona el tipo de delito", 
                                
                                choices = tipo_delito),
                    box(plotlyOutput("plot_graph_line", width = 1200, height = 600))
                  )
                  
                )
        ),
        
        # Grafico de Barras Animado
        tabItem(tabName = "graph_bar_an", 
                
                
                fluidPage(
                  fluidRow(
                    titlePanel(h3("Gráficos de barras animado")),
                    
                    selectInput("tipo_delito_an", "Selecciona el tipo de delito", 
                                choices = tipo_delito),
                    imageOutput("plot_graph_bar")
                    
                  )
                  
                )
        ),
        
        
        
        # Grafico de Barras
        tabItem(tabName = "graph_bar", 
                
                # Show a plot of the generated distribution
                fluidPage(
                  mainPanel(
                    
                    titlePanel(h3("Gráficos de barras")),
                    
                    selectInput("tipo_delito_an2", "Selecciona el tipo de delito", 
                                choices = tipo_delito),
                    selectInput("anios_an2", "Selecciona el año", 
                                choices = anios),
                    
                    tabsetPanel(
                      
                      tabPanel("Gráfico",
                               
                         fluidPage(
                           fluidRow(
                             box(plotlyOutput("plot_graph_bar2", width = 1200, height = 600))
                             
                           )
                           
                         )
                      ),
                      
                      tabPanel("Gráfico Ordenado", 
                         fluidPage(
                           fluidRow(
                             box(plotlyOutput("plot_graph_bar3", width = 1200, height = 600))
                             
                           )
                           
                         )
                               
                      ),
                      
                      tabPanel("Gráfico Ordenado Colores", 
                               fluidPage(
                                 fluidRow(
                                   box(plotlyOutput("plot_graph_bar4", width = 1200, height = 600))
                                   
                                 )
                                 
                               )
                               
                      )
                      
                      
                  )
                )
              )
        ),
        
        
        
        # Series de tiempo
        tabItem(tabName = "graph_time_series", 
                fluidPage(
                  fluidRow(
                    column(12,
                      titlePanel(h3("Series de tiempo")),
                      selectInput("estado_ts", "Seleccione el estado",
                                  choices = estados),
                      
                      selectInput("tipo_delito_ts", "Selecciona el tipo de delito", 
                                  choices = tipo_delito),
                      
                      checkboxInput("line_trend", "Línea de tendencia", FALSE),
                      verbatimTextOutput("value"),
                      
                      box(plotOutput("plot_graph_time_series", width = 1200))
                    ),
                    column(12,
                      box(plotOutput("plot_graph_time_series_decom", width = 1200))
                    )
                  )
                  
                )
        ),
        # Dispersión
        tabItem(tabName = "graph", 
                fluidRow(
                  titlePanel(h3("Gráficos de dispersión")),
                  selectInput("a", "Selecciona el valor de x",
                              choices = names(mtcars)),
                  selectInput("y", "Seleccione el valor de y",
                              choices = names(mtcars)),
                  selectInput("z", "Selecciona la variable del grid", 
                              choices = c("cyl", "vs", "gear", "carb")),
                  box(plotOutput("output_plot", height = 300, width = 460) )
                  
                )
        ),
        
        
        
        tabItem(tabName = "data_table",
                fluidRow(        
                  titlePanel(h3("Data Table")),
                  dataTableOutput ("data_table")
                )
        )
      )
    )
  )
)
