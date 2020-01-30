#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
shinycopy <- function() {
  
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list()
  )
}

#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
bz_app <- function(bz_data, ...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, options = list(...)), 
    golem_opts = list(bz_data = bz_data)
  )
}

function() {
  library(amcatr)
  conn = amcat.connect('https://amcat.nl')
  bz_data = create_bz_data(conn, project=1916, pers_set=79431, nieuws_set=79457, 
                           min.similarity=0.2, ngrams=3)
  bz_app(bz_data, port = 6171)
  class(bz_data$tc$meta$date)
}