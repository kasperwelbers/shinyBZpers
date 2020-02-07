#' @import shiny
app_server <- function(input, output, session) {
  bz_data = golem::get_golem_options('bz_data')
  tc = bz_data$tc
  default_sim = bz_data$default_sim

  settings = default_sim$settings
  state = reactiveValues(sim = default_sim$sim, from_table = make_table(tc, default_sim$sim, settings))
  
  set_default_settings(session, settings, tc)
  
  observeEvent(input$prepare, {
    state$sim = update_similarity_data(session, input, output, tc, settings)
  })
  
  observeEvent(input$daterange, {
    update_persselect(session, input, settings, tc)
  })

  output$from = DT::renderDataTable({
    state$from_table = make_table(tc, state$sim, settings)
    state$from_table[,c('date','headline','matches')]
  }, server=F, selection='single')

  
  observeEvent(input$persselect,
               highlight_text(input, output, tc, state$sim, state$from_table))
  
}


set_default_settings <- function(session, settings, tc) {
  updateSelectInput(session, 'measure', selected = settings$measure)
  updateSelectInput(session, 'weight', selected = settings$weight)
  updateSliderInput(session, 'min_similarity', value=settings$min_similarity, min=0.05, max=1)  
  ngrams = if (is.na(settings$ngrams)) 1 else settings$ngrams
  updateSliderInput(session, 'ngrams', value=ngrams, min=1, max=3)
  updateSliderInput(session, 'hour_window', value=settings$hour_window, min=-7*24, max=7*24)
  
  mindate = min(as.Date(tc$meta$date))
  maxdate = max(as.Date(tc$meta$date))
  updateDateRangeInput(session, 'daterange', start= maxdate - 7, end=maxdate, min=mindate, max=maxdate)
}

update_persselect <- function(session, input, settings, tc) {
  m = tc$meta[settings$from_subset,]
  m$date = as.Date(m$date)
  m = subset(m, date >= input$daterange[1] & date <= input$daterange[2])
  if (nrow(m) == 0) {
    updateRadioButtons(session, 'persselect', choices = NULL, selected=NULL)
  } else {
    data.table::setorderv(m, 'date', -1)
    n = paste0('<b>', m$date, '</b>&emsp;', m$headline)
    updateRadioButtons(session, 'persselect', choiceNames = lapply(n, HTML), choiceValues = m$doc_id, selected=m$doc_id[1])
  }
}
