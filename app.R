library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)

#Cargamos los datos necesarios
md <- read.csv("match.data.csv")

ui <- navbarPage(theme = shinytheme("superhero"),
                 title = p(style=" color: white; font-size: 30px","Análisis de los partidos de fútbol soccer de la liga española"),
                 tabPanel(p(style=" color: #fc7060; font-weight: bold; font-size: 15px", "Rendimiento del equipo"), 
                          sidebarLayout(sidebarPanel(
                            h1("¡Hola!", ),
                            p(style="text-align: justify;", "Aquí podras encontrar algunas conclusiones del uso de un modelo predictivo de goles en partidos de futbol utilizado para una serie de apuestas. 
                            Para comenzar, la gráfica de esta sección muestra la frecuencia de goles que ha marcado el equipo que tu elijas contra todos los equipos (visitantes) con los que ha jugado."),
                            selectInput(inputId = "teamChoice",
                                        label = "Selecciona el equipo local",
                                        choices = sort(unique(md$Home.team)),
                                        selected = "Barcelona"
                            ),
                            p(style="text-align: justify;", "Comenzando con el análisis descripritvo de los datos, en ésta ventana se puede observar la síntesis de información acerca de los goles del equipo local y visitante de los partidos de futbol de la liga española entre los años 2010 y 2020. Por medio de la barra de selección, es posible elegir el equipo local para así tener una comparativa de su desempeño respecto al equipo visitante.
Esto nos permite tener una primera aproximación hacia la comprensión del desempeño  de los  equipos.  A simple vista, se puede observar que la mayoría de los equipos, cuando juegan de local, han anotado al menos un gol, lo cual no sucede cuando el equipo es visitante."),
                            p(style="text-align: justify;", "También es notorio el desempeño del equipo Barcelona, el cual ha anotado 1,037 goles en los años considerados obteniendo un 75% de juegos ganados, ya sea que haya jugado de manera local o de visitante."),
                            img(src='bedu.png', height="30%", width="30%", align = "right")
                          ),
                          mainPanel(
                            plotlyOutput("bar_LocalTeam", width = "100%", height = "700px")
                          )
                          ), 
                 ), 
                 tabPanel(title = p(style=" color: #fc7060; font-weight: bold; font-size: 15px", "Probabilidades de goles"), 
                          fluidRow(column(width = 6,
                                          wellPanel(img(src='MarginalProbLocal.png', height="80%", width="100%", align = "center"))),
                                   column(width = 6, 
                                          wellPanel(p(style="text-align: justify;", "En ésta pestaña puede observarse la estimación empírica de cada una de las probabilidades de anotación entre los años 2017 y 2020 para las funciones de probabilidad conjunta y marginal. En los gráficos de la izquierda se muestra la probabilidad de que el equipo de casa anote  una cantidad específica de goles, eligiendo un partido al azar.   
                                                    Realizando una prueba de hipótesis de medias, se comprueba estadísticamente, a un nivel de confianza del 99%, que cuando el equipo es local, en promedio anotará más goles que el equipo visitante (p-value = 5.16e-14)."),
                                                    p(style="text-align: justify;", "Considerando los partidos entre el 2010 y 2020; se puede concluir, estadísticamente, que el equipo local anotará más goles, en promedio, cuando juega de manera local (p-value < 2.2e-16)."), img(src='bedu.png', height="15%", width="15%", align = "right")),
                                          wellPanel(p(style="text-align: justify;", "El mapa de calor de abajo, muestra las probabilidades conjuntas respecto al número de goles que puede anotar un equipo de acuedo a su clasificación (local o visitante). Esto refuerza la hipótesis anterior, ya que la mayor probabilidad se encuentra cuando el equipo local anota un gol y el equipo visitante ningún gol."))
                                   )),
                          fluidRow(column(width = 6,
                                          wellPanel(img(src='MarginalProb.png', height="30%", width="100%", align = "center"))),
                                   column(width = 6,
                                          wellPanel(img(src='jointProb.png', height="30%", width="100%", align = "center")))
                          ),
                 ), 
                 tabPanel(title = p(style=" color: #fc7060; font-weight: bold; font-size: 15px","Partidos"), 
                          wellPanel("Aquí podras explorar toda la información de cada partido involucrado en este estudio.", img(src='bedu.png', height="5%", width="5%", align = "right")),
                          dataTableOutput ("data_table")),
                 tabPanel(p(style=" color: #fc7060; font-weight: bold; font-size: 15px", "Comportamiento de las apuestas"), 
                          fluidRow(column(width = 6,
                                          wellPanel(p(style="text-align: justify;", "Las gráficas de la derecha muestran la evolución de las ganancias o pérdidas, comenzando con 50,000 unidades monetarias y siguiendo una serie de estrategias basadas en un modelo probabilísitico para entrar en una apuesta o no."),
                                                    p(style="text-align: justify;", "Las apuestas consideradas son sobre el número total de goles en los partidos, los cuales se eligieron si se tienen probabilidades favorables de ganarle a la casa de apuestas y de obtener una predicción positiva sobre el número de goles."),
                                                    p(style="text-align: justify;", "En las mismas gráficas se rescatan las tendencias que llevan las apuestas (la primera considerando los momios máximos de las casas de apuestas y la segunda tomando en cuenta los momios promedio de más/menos de 2,5 goles). Considerando los momios máximos de las casas de apuestas, se tiene una tendencia a la alza en la ganancia; caso contrario al apostar considerando los momios promedio."),
                                                    p(style="text-align: justify;", "Realizando un análisis de series de tiempo, las ganancias se comportan como una clásica caminata aleatoria, lo cual sugiere a tener atención continua sobre el comportamiento de las ganancias para evitar posibles perdidas drásticas respecto al capital inicial.")
                                          )),
                                   column(width = 6,
                                          wellPanel(img(src='ApuestasMomiosMaximos.png', height="20%", width="100%", align = "center")))),
                          fluidRow(column(width = 6,
                                          wellPanel(
                                            h3("Conclusiones", img(src='bedu.png', height="10%", width="10%", align = "right")),
                                            tags$div(
                                              tags$ul(
                                                tags$li("Si se desea apostar por el equipo ganador en algún juego donde participe el equipo Barcelona, considerar este como el equipo más probable a ser el ganador del encuentro."),
                                                tags$li("Es más probable que el equipo de local gane el partido."),
                                                tags$li("Apostar contra la casa de apuestas de acuerdo a los momios máximos ha generado mejores ganancias."),
                                                tags$li("Se recomienda evitar, al menos por la tendencia que ha seguido esta estrategia, apostar contra la casa de apuestas tomando en cuenta los momios promedio."),
                                                tags$li("Seguir con la estrategia de selección de partidos para entrar a una apuesta o no y evaluar continuamente el comportamiento de la inversión."),
                                                tags$li("Actualizar continuamente el modelo de selección de partidos para mejorar la exactitud de este y obtener las mejores estrategias en apuestas futuras.")
                                              )
                                            )
                                          )),
                                   column(width = 6,
                                          wellPanel(img(src='ApuestasMomiosPromedio.png', height="20%", width="100%", align = "center"))))
                          ))


server <- function(input, output) {
  output$bar_LocalTeam <- renderPlotly({
    ggplotly(md %>% filter(Home.team == input$teamChoice) %>%
               pivot_longer(cols = c(Home.score, Away.score), 
                            names_to = "Equipo", values_to = "Goles") %>% 
               mutate(Equipo = factor(Equipo, levels = c("Home.score", "Away.score"), 
                                      labels = c("Local", "Visitante"))) %>% 
               group_by(Home.team, Away.team, Equipo) %>% summarise(Goles = sum(Goles)) %>% ungroup() %>% 
               ggplot(aes(x = Equipo, y = Goles, fill = Equipo)) + 
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
               guides(fill = guide_legend(title="Goles del \nequipo ...")))
  })
  
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