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
bz_app <- function(bz_data, port=6171, ...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, options = list(port=port, ...)), 
    golem_opts = list(bz_data = bz_data)
  )
}

function() {
  library(shinyBZpers)
  library(amcatr)
  conn = amcat.connect('https://amcat.nl')
  bz_data = create_bz_data(conn, project=1916, pers_set=79431, nieuws_set=79457, deduplicate=0.9)
  bz_app(bz_data, port = 6171)
  
  bz_data = create_bz_data(conn, project=29, pers_set=80229, nieuws_set=80227, deduplicate=0.9)
  bz_app(bz_data, port = 6171)
  bz_data$conn
  bz_data$pers_index
    
  ####
  library(amcatr)
  conn = amcat.connect('https://amcat.nl')
  
  pers = amcatr::amcat.hits(conn, queries='*', project=project, sets=pers_set, col = c('doc_id','date','medium','headline','text'))  
  nieuws = amcatr::amcat.hits(conn, queries='*', project=project, sets=nieuws_set, col = c('doc_id','date','medium','headline','text'))  
  
  pers$from = 1
  nieuws$from = 0
  d = rbind(pers, nieuws)
  d = d[order(d$id),]
  d$title = d$headline
  
  db_conn = tc_db(d)
  
  bz_data = prepare_data2(db_conn, unique(d$id), deduplicate)
  
  bz_app(bz_data)
  format(object.size(bz_data), 'Mb')
}