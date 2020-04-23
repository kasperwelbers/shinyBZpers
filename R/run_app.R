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
#' @param bz_data the data, prepared with create_bz_data()
#' @param port    The port
#' @param token_auth If True, read the .valid_tokens file (create with \code{\link{set_tokens}}) to authenticate tokens. 
#'                   The token should be passed to the app with the ?token URL parameter
#'                   
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
bz_app <- function(bz_data, token_auth=F, port=6171, ...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, options = list(port=port, ...)), 
    golem_opts = list(bz_data = bz_data, token_auth=token_auth)
  )
}

#' Create .valid_tokens file
#' 
#' Creates the .valid_tokens file. Previous versions will be overwritten 
#' (so only the most recently created tokens are valid)
#'
#' @param tokens 
#'
#' @return safety
#' @export
set_tokens <- function(tokens) {
  writeLines(paste(tokens, collapse='\n'), con = '.valid_tokens')
}

function() {
  library(shinyBZpers)
  library(amcatr)
  conn = amcat.connect('https://amcat.nl')
  bz_data = create_bz_data(conn, project=1916, pers_set=79431, nieuws_set=79457, deduplicate=0.9)
  bz_app(bz_data, port = 6171)
  

  bz_data = create_bz_data(conn, project=1916, pers_set=79457, nieuws_set=79457, deduplicate=0.9)
  bz_app(bz_data, port = 6171)
  
  library(shinyBZpers)
  library(amcatr)
  conn = amcat.connect('http://bzk.nieuwsmonitor.org')
  bz_data = create_bz_data(conn, project=3, pers_set=4564, nieuws_set=4585, deduplicate=0.9, pers_medium_col='publisher', pers_headline_col='title', nieuws_medium_col='medium', nieuws_headline_col='title')
  bz_app(bz_data, port = 6171)
  
  ## this way, the app can only be used if a valid token (in .valid_tokens) 
  ## is provided with the ?token url parameter. 
  set_tokens(c('token1','token2'))   ## creates/overwrites .valid_tokens
  bz_app(bz_data, token_auth=T, port = 6171)
  ## open with http://127.0.0.1:6171?token=token1
  
  
  ##
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