


make_table <- function(tc, sim, settings) {
  d = tc$meta[settings$from_subset, c('doc_id','date','headline')]
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


prepare_data_button <- function(){
  list(
    HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
    actionButton('prepare', 'Compute similarity', icon = icon('play-circle'), width = 200, style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  )
}

update_similarity_data <- function(session, input, output, tc, settings){
  #output$from = DT::renderDataTable({data.table::data.table(doc_id=numeric(), headline=character(), matches=numeric())})
  #output$txt_x = shiny::renderText('')
  #output$txt_y = shiny::renderText('')
  
  comp = corpustools::compare_documents(tc, 
                                        min_similarity = input$min_similarity, 
                                        measure = input$measure, 
                                        ngrams = if (input$ngrams == 1) NA else input$ngrams,
                                        weight = input$weight, 
                                        return_igraph=F,
                                        from_subset = settings$from_subset, 
                                        to_subset = settings$to_subset,
                                        date = settings$date_col)
  sim = comp$d
  if (is.null(sim)) return(data.table::data.table(doc_id=numeric(), headline=character(), matches=numeric()))
  data.table::setindexv(sim, cols='from')
  sim
}


prepare_data_ui <- function() {
  list(
    selectInput('measure', 'Similarity measure', 
                list('Overlap percentage' = 'overlap_pct','Cosine similarity' = 'cosine'), 
                multiple=F, selected='overlap_pct'),
    br(),
    selectInput('weight', 'Weighting scheme', 
                list("tf-idf with normalization" = 'norm_tfidf', "tf-idf" = 'tfidf', "term frequency" = 'termfreq', "document frequency" = 'docfreq'), 
                multiple=F, selected='norm_tfidf'),
    br(),
    sliderInput('min_similarity', label = 'similarity threshold', value=0.2, min=0.05, max=1, step=0.01),
    br(),
    sliderInput('ngrams', label='ngrams', value=1, min=1, max=4, step=1),
    br(),
    sliderInput('hour_window', label='Time window in hours', value=c(0, 1*24), min=-7*24, max=7*24, step=1)
  )
}

