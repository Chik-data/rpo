#' tibble UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import ggplot2
mod_tibble_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, titlePanel("Morts par responsable a chaque saison"), #Titre
                #Liste deroulante pour le choix de l'annee à representer
                selectInput(inputId= ns("saison"), label="Choisir une saison", choices=death$season),
                #actionButton("gel", "Geler la saison!"),
                selectInput(inputId= ns("responsable"), label = "Choisir un meutrier(e)", choices = death$responsible)

                # navbarPage("App Title",
                #         tabPanel("test image",
                #            tags$img(src="icons8-walter-white-32.png", align = "topleft",height='32px',width='32px')),
                #         tabPanel("test son", tags$audio(src="Walter_is_the_Danger.mp3", width="180", type = "audio/mp3", autoplay =TRUE, controls = "preload")),
                #         tabPanel("Plot", ),
                #         tabPanel("Summary", verbatimTextOutput("summary"), icon = icon("list-alt")),
                #         tabPanel("Table", tableOutput("table"), icon = icon("table"))
                # )
             ),
      fluidRow(
        column(5,tableOutput(outputId = ns("tib_death")), tableOutput("tab_view"), tableOutput("director"), tableOutput("writters"), tableOutput("jointure"))
      ),

    #   ,
    #
    # #sliderInput("min", "Limit (minimum)", value = 50, min = 0, max = 100),
    fluidRow(
      column(4, plotOutput(outputId = ns("plot_death"))),
      column(4, plotOutput(outputId = ns("plot_view"))),
      column(4, plotOutput(outputId = ns("plot_facette"))),
      column(4, plotOutput(outputId = ns("plot_resp"))),
      column(4, plotOutput(outputId = ns("plot_smooth")))
    )
    ,
     #dateInput("date", "Date de diffusion de la serie", value= epi1$original_air_date),
     #checkboxGroupInput("character", "What's your favourite character?", unique(epi2$characters) ),
     radioButtons("rb", "Choose one:",
                 choiceNames = list( icon("angry"), icon("smile"), icon("face-grin-tears") ),
                 choiceValues = list("angry", "happy", "sad")
               )
    #  dateRangeInput("daterange1", "Date range:", start ="2008-01-20", end ="2013-09-29"),
    # submitButton("Update View", icon = icon("face-grin-tears")),

  )
)
}

#' tibble Server Functions
#'
#' @noRd
mod_tibble_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #Création de l'output réactif aux modifications de la liste déroulante
     dataset0 <- reactive({
       death %>% group_by(responsible, season) %>% summarise(nb_deaths_per_responsible=sum(number_of_deaths)) %>%
         filter(season== input$saison) %>% filter(responsible %in% input$responsable)
                        })
     # dataset1 <- eventReactive(input$gel, {
     #             dataset0() %>% filter(responsible %in% input$responsable)
     #                          })

    output$tib_death <- renderTable({# ou renderDataTable() pr gros volume de données
      dataset0()
                                     })
    output$tab_view <- renderTable({ #nb vus moy. (aux us) par sem pour chaque saison:
      reactive({ merge(tmp, tmp.vus) %>% mutate(nb_vu_wk_each_seas= nb_us_viewers_per_season/duration_season_in_weeks)
               })
                                   })
    output$director <- renderTable({# / director:
      epi1 %>% group_by(written_by, episode_num_in_season, season) %>% summarise(nb_us_view_per_writter= sum(us_viewers)) #%>% arrange(desc(nb_us_view_per_writter))
                                  })

    output$writters <- renderTable({#nb de vus aux usa/episodes:
      epi1 %>% select(episode_num_in_season, season , us_viewers, written_by) %>% arrange(desc(us_viewers))
                                 })
    output$jointure <- renderTable({# nb de morts cumulé par saison et temps passé depuis la 1ere saison episode 1:
      (jointure1=list(tmp, tmp.vus, season.d) %>% reduce(full_join, by='season') %>%
         mutate(nb_cum_death_season=cumsum(nb_deaths_per_season), t.cum.in.weeks.from.s1= cumsum(duration_season_in_weeks)))
                                })

    output$plot_death <- renderPlot({
      ggplot(episodes.meurtrier.by.season, aes(episode, nb_deaths_per_epiANDseason, colour = season)) + geom_point()+
        xlab("episode")+ ylab("nb morts")+ ggtitle("Nombre de mort par episode selon la saison")
                                    })
    output$plot_view <- renderPlot({
      ggplot(jointure1, aes(season, nb_deaths_per_season, size = nb_us_viewers_per_season)) + geom_point()+
        xlab("saison")+ ylab("nb morts")+ ggtitle("Nombre de mort par saison selon le nb d'audience aux usa")+
        theme_bw()#switch le fond par 1 fond blanc
    })
    output$plot_facette <- renderPlot({
      ggplot(deaths.by.resp.by.season, aes(season, nb_deaths_per_responsible)) + geom_point(aes(colour = "pink")) + facet_wrap(~responsible)
    })
    output$plot_resp <- renderPlot({
      ggplot(deaths.by.resp.by.season, aes(season, nb_deaths_per_responsible, colour = responsible)) + geom_point()+
        xlab("saison")+ ylab("nb morts")+ ggtitle("Nombre de mort par saison selon les responsables")+ theme_bw()#switch le fond par 1 fond blanc
    })
    output$plot_smooth <- renderPlot({
      ggplot(epi1, aes(episode_num_overall, us_viewers, colour= season)) + geom_point() + geom_smooth(method = "lm")+xlab("num d'episode")+
        ylab("nb d'audience aux usa")+ ggtitle("Nombre d'audience aux usa par episode selon la saison")+
        theme_bw()#switch le fond par 1 fond blanc
    })


})
}

## To be copied in the UI
# mod_tibble_ui("tibble_1")

## To be copied in the server
# mod_tibble_server("tibble_1")
