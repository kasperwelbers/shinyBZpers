# Module UI
  
#' @title   mod_viewer_ui and mod_viewer_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_viewer
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}
    
# Module Server
    
#' @rdname mod_viewer
#' @export
#' @keywords internal
    
mod_viewer_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_viewer_ui("viewer_ui_1")
    
## To be copied in the server
# callModule(mod_viewer_server, "viewer_ui_1")
 
