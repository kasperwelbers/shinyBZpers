#' @import shiny
#' @import shinyWidgets
app_server <- function(input, output, session) {
  data = golem::get_golem_options('bz_data')
  
  state = reactiveValues(doc_ids=c())
  state$sim = data$sim
  state$from_table = make_from_table(input, data, subset(data$sim, weight >= 0.1 & hourdiff >= 0 & hourdiff <= 7*24))
  state$to_table = make_to_table(input, data, subset(data$sim, weight >= 0.1 & hourdiff >= 0 & hourdiff <= 7*24))
  
  set_default_settings(session, output, data)
  

  observeEvent({state$sim; input$pers_of_nieuws; input$aggregate; input$only_matches}, {
    if (input$pers_of_nieuws == 'pers') 
      output$articlelist_header = renderText('Selecteer persbericht')
    else
      output$articlelist_header = renderText('Selecteer nieuwsbericht')
    create_graph_data(session, input, output, data, state)
    output$dategraph = create_graph(input, state$time_graph_data)
    #update_persselect(session, input, output, state)
    empty_persselect(output)
  })
  
  observeEvent(input$media, ignoreNULL = F, {
    if (input$pers_of_nieuws == 'pers') {
      ## pers data requires update, because it can't just select columns based on media filter
      state$graph_data = create_graph_data(session, input, output, data, state)
    }
    output$dategraph = create_graph(input, state$time_graph_data)
    empty_persselect(output)
  })
  
  observeEvent(input$prepare, {
    state$sim = update_similarity_data(input, data)
    state$from_table = make_from_table(input, data, state$sim)
    state$to_table = make_to_table(input, data, state$sim)
    update_persselect(session, input, output, state)
    highlight_text(input, output, data, state)
    updateActionButton(session, 'prepare', '', icon = icon('circle'))
  })
  
  observeEvent({input$min_similarity; input$hour_window}, {
    updateActionButton(session = session, inputId = 'prepare', label = 'Bevestig', icon = icon('play-circle'))
  }, ignoreInit=T)

  daterange <- debounce(reactive({
    daterange = input$dategraph_date_window
  }), 500)
  
  
  observeEvent(daterange(), {
    if (!is.null(daterange())) {
      daterange = as.Date(input$dategraph_date_window)
      updateDateRangeInput(session, 'daterange', 
                           start=  max(c(min(data$pers_index$date), daterange[1])), 
                           end= min(c(max(data$pers_index$date), daterange[2])))
    }
  })
  
  observeEvent({input$create_articlelist}, ignoreInit = T, {
    update_persselect(session, input, output, state)
  })
  
  observeEvent({input$daterange}, ignoreInit = T, {
    output$dategraph = create_graph(input, state$time_graph_data, input$daterange)
  })
  

  observeEvent(input$articlelist_rows_selected,
               highlight_text(input, output, data, state))

 
}

create_graph_data <- function(session, input, output, data, state) {
  if (input$pers_of_nieuws == 'pers') {
    if (input$aggregate == 'day') state$from_table$agg_date = state$from_table$date
    if (input$aggregate == 'week') state$from_table$agg_date = as.Date(cut(state$from_table$date, breaks = 'weeks'))
    if (input$aggregate == 'month') state$from_table$agg_date = as.Date(cut(state$from_table$date, breaks = 'months'))
    
    if (length(input$media) > 0) {
      state$from_table$has_match = rowSums(state$from_table[,intersect(input$media, colnames(state$from_table)), with=F]) > 0
      
    } else 
      state$from_table$has_match = state$from_table$TOTAL > 0
    
    #if (input$only_matches)
    #  agg = state$from_table[,list(`Aantal overgenomen` = sum(has_match)), by=c('agg_date')]
    #else
    #  agg = state$from_table[,list(`Aantal verstuurd` = length(has_match)), by=c('agg_date')]
    agg = state$from_table[,list(`Overgenomen` = sum(has_match), `Niet overgenomen` = sum(!has_match)), by=c('agg_date')]
    
    #print(state$from_table)
    #agg = state$from_table[, lapply(data.table::.SD, function(x) sum(x>0)), by='agg_date']
    #print(agg)
    #agg = data.table::as.xts.data.table(agg)

  } else {
    if (input$aggregate == 'day') state$to_table$agg_date = state$to_table$date
    if (input$aggregate == 'week') state$to_table$agg_date = as.Date(cut(state$to_table$date, breaks = 'weeks'))
    if (input$aggregate == 'month') state$to_table$agg_date = as.Date(cut(state$to_table$date, breaks = 'months'))
    
    agg = state$to_table[,list(N = length(has_match), matches = sum(has_match)), by=c('agg_date','medium')]
    
    #if (input$only_matches)
      agg = data.table::dcast(agg, agg_date ~ medium, value.var='matches')
    #else 
    #  agg = data.table::dcast(agg, agg_date ~ medium, value.var='N')
    agg = data.table::as.xts.data.table(agg)
    agg[is.na(agg)] = 0
  }
  
  state$time_graph_data = agg
}

create_graph <- function(input, graph_data, datewindow=NULL) {
  if (input$pers_of_nieuws == 'pers') {
    main = 'Persberichten'
    d = graph_data
    
    dygraphs::renderDygraph({
      dygraphs::dygraph(d, main = "Persberichten") %>%
        dygraphs::dyRangeSelector(retainDateWindow=T, height=30, dateWindow = datewindow,  
                                  fillColor = " #A7B1C4", strokeColor = "#808FAB", keepMouseZoom = T) %>%
        dygraphs::dyStackedBarChart() %>%
        dygraphs::dyUnzoom() %>%
        dygraphs::dyStackedBarChart() %>%
        dygraphs::dyOptions(useDataTimezone = TRUE)
    })
  } else {
    main = 'Nieuwsberichten met sporen van persbericht'
    if (length(input$media) > 0) {
      d = subset(graph_data, select=input$media)
    } else
      d = graph_data
    
    dygraphs::renderDygraph({
      dygraphs::dygraph(d, main = "Persberichten") %>%
        dygraphs::dyRangeSelector(retainDateWindow=T, height=30, dateWindow = datewindow,
                                  fillColor = " #A7B1C4", strokeColor = "#808FAB", keepMouseZoom = T) %>%
        dygraphs::dyStackedBarChart() %>%
        dygraphs::dyUnzoom() %>%
        dygraphs::dyLegend(show='onmouseover', showZeroValues = F, labelsSeparateLines = T) %>%
        dygraphs::dyStackedBarChart() %>%
        dygraphs::dyOptions(useDataTimezone = TRUE)
    })
  }
}

empty_persselect <- function(output) {
  output$txt_x = shiny::renderText('')
  output$txt_y = shiny::renderText('')
  output$articlelist = DT::renderDataTable(data.table::data.table())
}

#' @import shiny
set_default_settings <- function(session, output, data) {
  mindate = min(as.Date(data$pers_index$date))
  maxdate = max(as.Date(data$pers_index$date))
  output$articlelist_header = renderText('Selecteer persbericht')
  #updateDateRangeInput(session, 'daterange', min= mindate, max=maxdate) ## this completely messes up shiny somehow
  updateDateRangeInput(session, 'daterange', start= mindate, end=maxdate)
  
  media = sort(unique(data$media_index$medium))
  shinyWidgets::updateMultiInput(session, inputId = 'media', choices = as.list(media))
}

#' @import shiny
update_persselect <- function(session, input, output, state, daterange) {
  output$txt_x = shiny::renderText('')
  output$txt_y = shiny::renderText('')
  
  if (input$pers_of_nieuws == 'pers') {
    m = state$from_table
    if (length(input$media) > 0) {
      m$hits = rowSums(m[,intersect(input$media, colnames(m)), with=F])
    } else 
      m$hits = m$TOTAL
    m$has_match = m$hits > 0
    cols = c('date','headline','hits')  
  } else {
    m = state$to_table
    if (length(input$media) > 0) {
      m = m[list(medium=input$media),,on='medium',nomatch=0]
    }
    cols = c('date','medium','headline')  
  }
  daterange =input$daterange
  date_seq = seq.Date(daterange[1], daterange[2], by = 1)
  date_seq = rev(date_seq)   ## lastest date first
  m = m[list(date=date_seq),on='date', nomatch=0]
  
  if (nrow(m) == 0) {
    output$articlelist = DT::renderDataTable(NULL)
  } else {
    if (input$only_matches) m = subset(m, subset = has_match)
    state$doc_ids = m$doc_id
    output$articlelist = DT::renderDataTable(subset(m, select = cols), options=list(dom = 'tp'), rownames=F, selection='single')
  }

}

make_from_table <- function(input, data, sim) {
  d = data$pers_index
  d$date = as.Date(d$date)
  
  if (nrow(sim) > 0) {
    from=NULL
    #sim_count = sim[, list(hits = length(unique(to))), by=c('from', 'medium')]
    sim_count = data.table::dcast(sim, from ~ medium, value.var='to', fun.aggregate=length)
    sim_count$TOTAL = rowSums(sim_count[,-1])
    d = merge(d, sim_count, by.x='doc_id', by.y='from', all.x=T)
    #d$hits[is.na(d$hits)] = 0
  } else {
    d$TOTAL = 0
  }
  #d$has_match = d$hits > 0
  data.table::setindexv(d, 'date')
  data.table::setorderv(d, 'date', -1)
  d[is.na(d)] = 0
  d
}

make_to_table <- function(input, data, sim) {
  d = data$media_index
  d$date = as.Date(d$date)
  d[,has_match := F]
  if (nrow(sim) > 0) {
    d[list(doc_id=sim$to), has_match := T, on='doc_id']
  } 
  data.table::setindexv(d, 'date')
  data.table::setindexv(d, 'medium')
  data.table::setorderv(d, 'date', -1)
  d
}


update_similarity_data <- function(input, data) {
  #sim = if (input$method == 'verbatim') data$verbatim else data$event
  subset(data$sim, weight >= input$min_similarity & hourdiff >= input$hour_window[1] & hourdiff <= input$hour_window[2])

}
