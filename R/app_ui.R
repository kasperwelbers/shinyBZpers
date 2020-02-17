#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    
    dashboardPage(title = 'Persberichten tracker',
      dashboardHeader(title = 'Methode'),
      dashboardSidebar(width = 300, collapsed=T,
                       sidebar_style(),
                       
                       sidebarMenu(
                         div(
                             prepare_data_ui(),
                             prepare_data_button()
                             )
                       )),
      dashboardBody(

                      fluidRow(
                        #box(width=4, height=600, 
                        #    radioButtons('pers_of_nieuws', label = '', inline = T, choices=list('Persberichten'='pers', 'Nieuwsberichten'='nieuws'), selected = 'pers'),
                        #    dateRangeInput('daterange', 'Selecteer periode',  start = '1900-01-01', end = '2100-01-01'),
                        #    shinyWidgets::pickerInput('media', multiple=T, label = 'Selecteer bronnen', choices=list(), options = list(`actions-box` = TRUE))
                        #    ),
                        box(width=12, height=600,
                            fluidRow(
                              column(width=2, selectInput('pers_of_nieuws', width='100%', label = 'Bekijk pers- of nieuwsberichten', choices=list('Persberichten'='pers', 'Nieuwsberichten'='nieuws'), selected = 'pers')),
                              column(width=4, shinyWidgets::pickerInput('media', width='100%', multiple=T, label = 'Filter op medium', choices=list(), options = list(`actions-box` = TRUE))),
                              column(width=2, selectInput('aggregate', label = 'Datum per', width = '100%', choices=list('Per dag'='day', 'Per week'='week', 'Per maand'='month'), selected = 'week')),
                              column(width=2, selectInput('dateselect', label = 'Datum selectie', width = '100%', choices=list('Afgelopen week'='week', 'Afgelopen maand'='maand', 'Afgelopen jaar'='jaar', 'Hele periode'='alles', 'Vrije selectie'='vrij'), selected = 'alles'))
                            ),
                            div(align='center',
                                dygraphs::dygraphOutput("dategraph", height='420px', width = '90%')
                            )
                        )                           
                      ),
                      
                      fluidRow(
                        box(width=4, height=1000,
                          h2(textOutput('articlelist_header')),
                          br(),
                          
                          div(align='center', 
                              #actionButton('create_articlelist', label = 'Bekijk artikelen'),
                              #actionLink('use_graph_daterange', label='Gebruik periode uit grafiek'),
                              checkboxInput('only_matches', 'Minstens 1 match', value=TRUE)
                          ),
                          
                          #div(radioButtons('persselect', label=NULL, choiceNames='', choiceValues='', selected=character()),
                          #    style = "overflow-y: scroll; height: 500px"),
                          div(DT::dataTableOutput('articlelist', height = 'auto', width='100%'))
                          ),
                        
                        box(width=8, height=1000,
                          shiny::fluidRow(
                            column(width=6, height=900,
                                h3('Persberichten', align='center'),
                                br(),
                                div(class='textbox', 
                                    htmlOutput('txt_x'),  
                                    style = "overflow-y: scroll; height: 750px")
                                ),
                            column(width=6, height=900,
                                h3('Nieuwsberichten', align='center'),
                                br(),
                                div(class='textbox', 
                                    htmlOutput('txt_y'), 
                                    style = "overflow-y: scroll; height: 750px"), 
                            )
                          )
                          )
                      ))
    )
  )
}

prepare_data_ui <- function() {
  list(
    #selectInput('method', 'Vergelijkingsmethode', 
    #            list('Soortgelijk onderwerp' = 'event','Letterlijke kopie' = 'verbatim'), 
    #            multiple=F, selected='verbatim'),
    #br(),
    sliderInput('min_similarity', label = 'similarity threshold', value=0.2, min=0.1, max=1, step=0.01),
    br(),
    sliderInput('hour_window', label='Time window in hours', value=c(0, 7*24), min=-3*24, max=7*24, step=1)
    #br(),
    #div(sliderInput('ngrams', "N-grams text overlap", min = 1, max=5, value = 3))
  )
}

prepare_data_button <- function(){
  list(
    HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
    div(align='center', actionButton('prepare', '', icon = icon('circle'), width=120, style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'shinyBZpers')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
