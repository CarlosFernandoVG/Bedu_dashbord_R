library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- fluidPage(
    dashboardPage(
      dashboardHeader(title = "Elaboración y descripción de un modelo predictivo para partidos de fútbol soccer"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Rendimiento del equipo", tabName = "bar_graph", icon = icon("dashboard")),
          menuItem("Probabilidades de goles e casa", tabName = "phs_aws", icon = icon("area-chart")),
          menuItem("Partidos", tabName = "data_table", icon = icon("table")),
          menuItem("Ganancias promedio y máximo", tabName = "img", icon = icon("file-picture-o"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "bar_graph",
                  fluidRow(
                    titlePanel("Gráfico de barras para el equipo local"), 
                    selectInput("x", "Seleccione el equipo local",
                                choices = names(mtcars)),
                    box(plotOutput("plot1", height = 250)),
                    box(
                      title = "Controls",
                      sliderInput("bins", "Number of observations:", 1, 30, 15)
                    )
                  )
          ),
          
          
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
          ), 
          tabItem(tabName = "img",
                  fluidRow(
                    titlePanel(h3("Imágen de calor para la correlación de las variables")),
                    img(src = "cor_mtcars.png", 
                        height = 350, width = 350)
                  )
          )
          
        )
      )
    )
  )



server <- function(input, output) {
  library(ggplot2)
  output$plot1 <- renderPlot({
    x <- mtcars[,input$x]
    bin <- seq(min(x), max(x), length.out = input$bins + 1)
    ggplot(mtcars, aes(x, fill = mtcars[,input$zz])) + 
      geom_histogram( breaks = bin) +
      labs( xlim = c(0, max(x))) + 
      theme_light() + 
      xlab(input$x) + ylab("Frecuencia") + 
      facet_grid(input$zz)
  })
  
  
  output$output_plot <- renderPlot({ 
    ggplot(mtcars, aes(x =  mtcars[,input$a] , y = mtcars[,input$y], 
                       colour = mtcars[,input$z] )) + 
      geom_point() +
      ylab(input$y) +
      xlab(input$a) + 
      theme_linedraw() + 
      facet_grid(input$z)
    
  })   
  
  
  output$data_table <- renderDataTable( {mtcars}, 
                                        options = list(aLengthMenu = c(5,25,50),
                                                       iDisplayLength = 5)
  )
  
}

shinyApp(ui, server)
