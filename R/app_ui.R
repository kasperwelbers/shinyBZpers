#' @import shiny
#' @import shinydashboard
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader(title = 'Change settings'),
      dashboardSidebar(width = 300, collapsed=T,
                       sidebar_style(),
                       
                       sidebarMenu(
                         prepare_data_ui(),
                         prepare_data_button()
                       )),
      dashboardBody(
                      fluidRow(
                        #box(div(DT::dataTableOutput('from'), 
                        #        style = "overflow-y: auto; height: 550px", 
                        #        options = list(rownames= FALSE)), width=4, height=600),
                        box(width=4, height=600,
                          div(dateRangeInput('daterange', "Persbericht publicatie", 
                                           start = '2000-01-01', end = '2000-02-01', min = '2000-01-01', max = '2000-01-02')),
                          div(radioButtons('persselect', label=NULL, choices = list("Choice 1" = 1), selected=1),
                              style = "overflow-y: scroll; height: 450px")
                          ),
                        
                        
                        box(width=8, height=800,
                          #shiny::fluidRow(height=200, 
                          #  shiny::column(offset = 4, width=8, 'test')
                          #                ),
                          shiny::fluidRow(
                            column(width=6, height=600,
                                div(htmlOutput('txt_x'), 
                                    style = "overflow-y: scroll; height: 550px")
                                ),
                            column(width=6, height=600, 
                                div(htmlOutput('txt_y'), 
                                    style = "overflow-y: scroll; height: 550px"), 
                            )
                          )
                          )
                      ))
    )
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
