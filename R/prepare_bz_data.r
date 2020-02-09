
set_default_params <- function(min_similarity=0.05, measure='overlap_pct', weight='norm_tfidf', hour_window=c(0,7*24), ngrams=1) {
  list(min_similarity=min_similarity, hour_window=hour_window, ngrams=ngrams, weight=weight, measure=measure)
}

#' Prepare similarity data
#' 
#' @param tc          a tCorpus
#' @param from_subset a subset expression for selecting 'from' which documents to compare
#' @param to_subset   a subset expression for selecting 'to' which documents to compare
#' @param date_col    The column in tc$meta that contains a date (has to be in POSIXct)
#' @param ...         Optionally, default values for min_similarity, hour_window, ngrams, measure and weight
prepare_data <- function(tc, from_subset, to_subset, date_col, ...) {
  params = set_default_params(...)
  params$from_subset = tc$eval_meta(substitute(from_subset), parent.frame())
  params$to_subset = tc$eval_meta(substitute(to_subset), parent.frame())
  params$date_col = date_col
  if (params$ngrams == 1) params$ngrams = NA
  
  comp = do.call(corpustools::compare_documents, args = c(list(tc=tc, return_igraph=F), params))
  
  
  default_sim = list(sim = comp$d, settings = params)
  data.table::setindexv(default_sim$sim, cols='from')
  
  list(tc=tc, default_sim=default_sim)
  #usethis::use_data(tc, default_sim, overwrite=T)
}

#' Run the Shiny Application
#'
#' @param conn         An amcatr connection
#' @param project      AmCAT project
#' @param pers_set     AmCAT articleset with BZ press articles
#' @param nieuws_set   AmCAT articleset with BZ news articles
#' @param deduplicate  Optionally, a similarity threshold for duplicates (only for articles in same medium within 24 hour diffence)
#' @param ... 
#'
#' @export
create_bz_data <- function(conn, project=1916, pers_set=79431, nieuws_set=79457, deduplicate=NA, ...) {
  pers = amcatr::amcat.hits(conn, queries='*', project=project, sets=pers_set, col = c('doc_id','date','medium','headline','text'))  
  nieuws = amcatr::amcat.hits(conn, queries='*', project=project, sets=nieuws_set, col = c('doc_id','date','medium','headline','text'))  
  
  pers$from = 1
  nieuws$from = 0
  d = rbind(pers, nieuws)
  d = d[order(d$id),]
  d$title = d$headline

  tc = corpustools::create_tcorpus(d, doc_col='id', text_columns = c('title','text'), remember_spaces=T)
  tc$meta$date = as.POSIXct(tc$meta$date)
  
  if (!is.na(deduplicate)) tc$deduplicate('token', meta_cols = 'medium', keep='last', hour_window=24, date_col = 'date', similarity = deduplicate)

  
  prepare_data(tc, 
               from_subset = from == 1,
               to_subset = from == 0,
               date_col='date',
               ...)
}
  



prepare_data2 <- function(conn, ids, deduplicate) {
  tc = get_tc(conn, ids)
  tc$meta$date = as.POSIXct(tc$meta$date)
  
  if (!is.na(deduplicate)) {
    message("\nDeduplicating")
    tc$deduplicate('token', meta_cols = 'medium', keep='last', hour_window=24, date_col = 'date', similarity = deduplicate)
  }
  
  message('\nCompute event level similarity')
  #event_idf = tc$tokens[, list(event_idf = length(unique(doc_id))), by=c('event_feature')]
  #event_idf$event_idf = log2(tc$n_meta / event_idf$event_idf)
  comp_event = corpustools::compare_documents(tc, from_subset = from == 1, to_subset = from == 0, date_col = 'date', feature = 'event_feature', min_similarity=0.1, measure='overlap_pct', hour_window = c(-3*24, 7*24), return_igraph=F)
  comp_event = comp_event$d
  data.table::setindexv(comp_event, cols='from')

  message('\nCompute verbatim level similarity')
  #verbatim_idf = tc$tokens[, list(verbatim_idf = length(unique(doc_id))), by=c('verbatim_feature')]
  #verbatim_idf$verbatim_idf = log2(tc$n_meta / verbatim_idf$verbatim_idf)
  comp_verbatim = corpustools::compare_documents(tc, from_subset = from == 1, to_subset = from == 0, date_col = 'date', feature = 'verbatim_feature', min_similarity = 0.1, measure = 'overlap_pct', hour_window = c(-3*24, 7*24), return_igraph=F)
  comp_verbatim = comp_verbatim$d
  data.table::setindexv(comp_verbatim, cols='from')
  
  pers_index = subset(tc$meta, from==1, select=c('date','headline','doc_id'))
  pers_index$date = as.Date(pers_index$date)
  list(conn=conn, pers_index=pers_index, event=comp_event, verbatim=comp_verbatim)
}


#' Run the Shiny Application
#'
#' @param conn         An amcatr connection
#' @param project      AmCAT project
#' @param pers_set     AmCAT articleset with BZ press articles
#' @param nieuws_set   AmCAT articleset with BZ news articles
#' @param deduplicate  Optionally, a similarity threshold for duplicates (only for articles in same medium within 24 hour diffence)
#'
#' @export
create_bz_data2 <- function(conn, project=1916, pers_set=79431, nieuws_set=79457, deduplicate=NA) {
  pers = amcatr::amcat.hits(conn, queries='*', project=project, sets=pers_set, col = c('doc_id','date','medium','headline','text'))  
  nieuws = amcatr::amcat.hits(conn, queries='*', project=project, sets=nieuws_set, col = c('doc_id','date','medium','headline','text'))  
  
  pers$from = 1
  nieuws$from = 0
  d = rbind(pers, nieuws)
  d = d[order(d$id),]
  d$title = d$headline
  
  db_conn = tc_db(d)

  prepare_data2(db_conn, unique(d$id), deduplicate)
}

