#' @import shiny
app_server <- function(input, output, session) {
  data = golem::get_golem_options('bz_data')
  
  state = reactiveValues()
  state$sim = subset(data$verbatim, weight >= 0.1 & hourdiff >= 0 & hourdiff <= 7*24)
  state$from_table = make_table(input, data, subset(data$verbatim, weight >= 0.1 & hourdiff >= 0 & hourdiff <= 7*24))
  set_default_settings(session, data)
  
  observeEvent(input$prepare, {
    state$sim = update_similarity_data(input, data)
    state$from_table = make_table(input, data, state$sim)
    update_persselect(session, input, state)
    highlight_text(input, output, data, state)
    updateActionButton(session, 'prepare', '', icon = icon('circle'))
  })
  
  observeEvent({input$min_similarity; input$method; input$hour_window}, {
    updateActionButton(session = session, inputId = 'prepare', label = 'Bevestig', icon = icon('play-circle'))
  }, ignoreInit=T)

  observeEvent(input$daterange, {
    update_persselect(session, input, state)
  })

  observeEvent(input$persselect,
               highlight_text(input, output, data, state))

}


set_default_settings <- function(session, data) {
  mindate = min(as.Date(data$pers_index$date))
  maxdate = max(as.Date(data$pers_index$date))
  updateDateRangeInput(session, 'daterange', start= maxdate - 14, end=maxdate, min=mindate, max=maxdate)
}

update_persselect <- function(session, input, state) {
  m = subset(state$from_table, date >= input$daterange[1] & date <= input$daterange[2])
  if (nrow(m) == 0) {
    updateRadioButtons(session, 'persselect', choices = NULL, selected=NULL)
  } else {
    data.table::setorderv(m, 'date', -1)
    n = paste0('<b>', m$date, '</b>&emsp;', m$headline)
    n = ifelse(is.na(m$matches), paste0('<span style="color:grey">',n,'<span>'), paste0(n, '(', m$matches, ')'))
    print(m$doc_id)
    print(n)
    updateRadioButtons(session, 'persselect', choiceNames = lapply(n, HTML), choiceValues = m$doc_id, selected=m$doc_id[1])
  }
}

make_table <- function(input, data, sim) {
  d = data$pers_index
  d$date = as.Date(d$date)
  
  if (nrow(sim) > 0) {
    from=NULL
    sim_count = sim[, list(matches = length(unique(to))), by='from']
    d = merge(d, sim_count, by.x='doc_id', by.y='from', all.x=T)
  } else {
    d$matches = 0
  }
  d
}

update_similarity_data <- function(input, data) {
  sim = if (input$method == 'verbatim') data$verbatim else data$event
  subset(sim, weight >= input$min_similarity & hourdiff >= input$hour_window[1] & hourdiff <= input$hour_window[2])
}
