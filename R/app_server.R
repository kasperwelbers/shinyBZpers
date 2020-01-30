#' @import shiny
app_server <- function(input, output, session) {
  bz_data = golem::get_golem_options('bz_data')
  tc = bz_data$tc
  default_sim = bz_data$default_sim

  settings = default_sim$settings
  state = reactiveValues(sim = default_sim$sim, from_table = make_table(tc, default_sim$sim, settings))
  
  set_sim_settings(session, settings)
  
  observeEvent(input$prepare, {
    state$sim = update_similarity_data(session, input, output, tc, settings)
  })

  output$from = DT::renderDataTable({
    state$from_table = make_table(tc, state$sim, settings)
    state$from_table[,c('date','headline','matches')]
  }, server=F, selection='single')

  
  observeEvent(input$from_rows_selected,
               highlight_text(input, output, tc, state$sim, state$from_table))
  
}


set_sim_settings <- function(session, settings) {
  updateSelectInput(session, 'measure', selected = settings$measure)
  updateSelectInput(session, 'weight', selected = settings$weight)
  updateSliderInput(session, 'min_similarity', value=settings$min_similarity, min=0.05, max=1)  
  ngrams = if (is.na(settings$ngrams)) 1 else settings$ngrams
  updateSliderInput(session, 'ngrams', value=ngrams, min=1, max=3)
  updateSliderInput(session, 'hour_window', value=settings$hour_window, min=-7*24, max=7*24)
}


