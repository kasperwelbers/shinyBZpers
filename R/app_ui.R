#' @import shiny
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
                        box(width=4, height=800,
                          div(dateRangeInput('daterange', "Persbericht publicatie", 
                                           start = '2000-01-01', end = '2000-02-01', min = '2000-01-01', max = '2000-01-02')),
                          
                          div(radioButtons('persselect', label=NULL, choiceNames='', choiceValues='', selected=character()),
                              style = "overflow-y: scroll; height: 700px")
                          ),
                        
                        
                        box(width=8, height=800,
                          shiny::fluidRow(
                            column(width=6, height=800,
                                div(class='textbox', 
                                    htmlOutput('txt_x'),  
                                    style = "overflow-y: scroll; height: 750px")
                                ),
                            column(width=6, height=800, 
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
    selectInput('method', 'Vergelijkingsmethode', 
                list('Soortgelijk onderwerp' = 'event','Letterlijke kopie' = 'verbatim'), 
                multiple=F, selected='verbatim'),
    br(),
    sliderInput('min_similarity', label = 'similarity threshold', value=0.1, min=0.1, max=1, step=0.01),
    br(),
    sliderInput('hour_window', label='Time window in hours', value=c(0, 7*24), min=-3*24, max=7*24, step=1)
  )
}

prepare_data_button <- function(){
  list(
    HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
    actionButton('prepare', '', icon = icon('circle'), width=120, style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
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
