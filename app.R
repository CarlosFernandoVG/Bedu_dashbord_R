library(shiny)
library(shinythemes)

#Cargamos los datos necesarios
md <- read.csv("match.data.csv")

ui <- navbarPage(theme = shinytheme("superhero"),
                 title = "Elaboración y descripción de un modelo predictivo para partidos de fútbol soccer",
                 tabPanel("Rendimiento del equipo", 
                          sidebarLayout(sidebarPanel(
                            selectInput(inputId = "teamChoice",
                                        label = "Selecciona el equipo local",
                                        choices = sort(unique(md$Home.team))
                            )
                          ),
                          mainPanel(
                            plotOutput("bar_LocalTeam", width = "100%")
                          )
                          )
                 ), 
                 tabPanel("Probabilidades de goles en casa", 
                          fluidRow(column(width = 4),
                                   column(width = 6, offset =2,
                                          wellPanel(img(src='MarginalProbabilities.png', height="30%", width="100%", align = "center")))),
                          fluidRow(column(width = 4),
                                   column(width = 6, offset =2,
                                          wellPanel(img(src='jointProbabilities.png', height="30%", width="100%", align = "center"))))
                          ), 
                 tabPanel("Partidos", "contents",
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
      labs(x = NULL, title = paste(c("Número de goles del equipo ", input$teamChoice, " como local y del equipo visitante"), collapse = "")) + 
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
  
  output$data_table <- renderDataTable({md}, 
                                       options = list(aLengthMenu = c(5,25,50),
                                                       iDisplayLength = 10)
                                       )
}

shinyApp(ui, server)