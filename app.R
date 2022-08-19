library(shiny)
library(tidyverse)
library(shinythemes)

#Cargamos los datos necesarios
md <- read.csv("match.data.csv")

ui <- navbarPage(theme = shinytheme("superhero"),
                 title = "Elaboración y descripción de un modelo predictivo para partidos de fútbol soccer",
                 tabPanel("Rendimiento del equipo", 
                          sidebarLayout(sidebarPanel(
                            p(h1("¡Hola!"), "Aquí podras encontrar algunas conclusiones del uso de un modelo predictivo de goles en partidos de futbol utilizado para una serie de apuestas. 
                            Para comenzar, la gráfica de esta sección muestra la frecuencia de goles que ha marcado el equipo que tu elijas contra todos los equipos (visitantes) con los que ha jugado."),
                            selectInput(inputId = "teamChoice",
                                        label = "Selecciona el equipo local",
                                        choices = sort(unique(md$Home.team))
                            ),
                            p("Comenzamos con el análisis descripritvo de los datos, en ésta ventana podemos observar la síntesis de la información mediante ésta tabla de frecuencias absolutas. Por medio de la barra de selección es posible elegir el equipo local para así tener una comparativa de su desempeño respecto al equipo visitante.
Ésto nos permite tener una primera aproximación hacia la comprensión del desempeño  de los  equipos.  A simple vista podemos observar que la mayoría de los equipos cuando juegan de local, han anotado al menos un gol; lo cual no sucede cuando el equipo es visitante") 
                          ),
                          mainPanel(
                            plotOutput("bar_LocalTeam", width = "100%")
                          )
                          ), 
                 ), 
                 tabPanel("Probabilidades de goles", 
                          fluidRow(column(width = 6,
                                          wellPanel(img(src='MarginalProbLocal.png', height="80%", width="100%", align = "center"))),
                                   column(width = 6, 
                                          wellPanel("En ésta pestaña podemos observar la estimación empírica de cada una de las probabilidades de anotación para las funciones de probabilidad conjunta y marginal. En los gráficos de la izquierda podemos observar la probabilidad de que el equipo de casa anote  una cantidad específica de goles, eligiendo un partido al azar.   
                                                    Realizando una prueba de hipótesis de medias, se comprueba estadísticamente, a un nivel de confianza del 99%, que cuando el equipo es local, en promedio anotará más goles que el equipo visitante (p-value = 5.16e-14)."),
                                          wellPanel("El mapa de calor de abajo, nos muestra las probabilidades conjuntas respecto al número de goles que puede anotar unn equipo de acuedo a su clasificación. Esto refuerza la hipótesis anterior, ya que la mayor probabilidad se encuentra cuando el equipo local anota un gol y el equipo visitante ningún gol.")
                                   )),
                          fluidRow(column(width = 6,
                                          wellPanel(img(src='MarginalProb.png', height="30%", width="100%", align = "center"))),
                                   column(width = 6,
                                          wellPanel(img(src='jointProb.png', height="30%", width="100%", align = "center")))
                          ),
                 ), 
                 tabPanel("Partidos", 
                          wellPanel("Aquí podras explorar toda la información de cada partido involucrado en este estudio."),
                          dataTableOutput ("data_table")),
                 tabPanel("Ganancias promedio y máximo", 
                          fluidRow(column(width = 4),
                                   column(width = 6, offset = 2,
                                          wellPanel(img(src='CuotaMaxima.png', height="20%", width="100%", align = "center")))),
                          fluidRow(column(width = 4),
                                   column(width = 6, offset = 2,
                                          wellPanel(img(src='CuotaPromedio.png', height="20%", width="100%", align = "center"))))
                          ))


server <- function(input, output) {
  output$bar_LocalTeam <- renderPlot({
    md %>% filter(Home.team == input$teamChoice) %>%
      pivot_longer(cols = c(Home.score, Away.score), 
                   names_to = "home_away", values_to = "Goles") %>% 
      mutate(home_away = factor(home_away, levels = c("Home.score", "Away.score"), 
                                labels = c("Local", "Visitante"))) %>% 
      ggplot(aes(x = home_away, y = Goles, fill = home_away)) + 
      facet_wrap("Away.team") + 
      geom_bar(stat = "identity") + 
      labs(x = NULL, title = paste(c("Número de goles totales del equipo ", input$teamChoice, " como local y del equipo visitante"), collapse = "")) + 
      scale_fill_manual(values = c("#FC471F", "black")) + 
      theme(plot.title = element_text(size=12, hjust = 0.5, face = "bold"),
            axis.text.x = element_blank(),
            axis.text.y = element_text(face = "bold", size = 10, colour = "black"),
            axis.text = element_text(colour = "gray82", face = "bold"),
            axis.title = element_text(face = "bold"),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_line(colour = "gray97"), 
            panel.grid.minor = element_line(colour = "gray85"),
            panel.background = element_rect(fill = "gray97", linetype = "solid"), 
            plot.background = element_rect(colour = "aliceblue")) + 
      guides(fill = guide_legend(title="Goles del \nequipo ..."))
  }, height = 500)
  
  output$data_table <- renderDataTable({md %>% rename(Fecha = Date, 
                                                      "Equipo local" = Home.team, 
                                                      "Goles del equipo local" = Home.score,
                                                      "Equipo visitante" = Away.team, 
                                                      "Goles del equipo visitante" = Away.score)}, 
                                       options = list(aLengthMenu = c(5,25,50),
                                                       iDisplayLength = 10)
                                       )
}

shinyApp(ui, server)