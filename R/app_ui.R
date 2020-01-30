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
                        box(div(DT::dataTableOutput('from'), 
                                style = "overflow-y: auto; height: 550px", 
                                options = list(rownames= FALSE)), width=4, height=600),
                        box(div(htmlOutput('txt_x'), 
                                style = "overflow-y: scroll; height: 550px"), width=4, height=600),
                        box(div(htmlOutput('txt_y'), 
                                style = "overflow-y: scroll; height: 550px"), width=4, height=600)
                      ))
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'shinycopy')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
  tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
}
