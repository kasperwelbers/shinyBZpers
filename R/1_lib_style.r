sidebar_style <- function(sidebarheight='200vh', inputcontainer_height='65px'){
  list(tags$head(tags$style(HTML(  ## add scroll to sidebar (because conditinalpanels mess up the height)
    sprintf(".sidebar {height: %s; overflow-y: auto;}", sidebarheight)
  ))),  
  tags$head(tags$style(HTML(  ## reduce height of input containers
    sprintf('.shiny-input-container {height:%s;}', inputcontainer_height)
  ))),
  tags$head(tags$style(HTML(  
    sprintf(".shiny-plot-output {overflow: scroll;}", sidebarheight)
  )))  
  )
}
