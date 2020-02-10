prepare_data <- function(conn, ids, deduplicate) {
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
create_bz_data <- function(conn, project=1916, pers_set=79431, nieuws_set=79457, deduplicate=NA) {
  pers = amcatr::amcat.hits(conn, queries='*', project=project, sets=pers_set, col = c('doc_id','date','medium','headline','text'))  
  nieuws = amcatr::amcat.hits(conn, queries='*', project=project, sets=nieuws_set, col = c('doc_id','date','medium','headline','text'))  
  
  pers$from = 1
  nieuws$from = 0
  d = rbind(pers, nieuws)
  d = d[order(d$id),]
  d$title = d$headline
  
  db_conn = tc_db(d)

  prepare_data(db_conn, unique(d$id), deduplicate)
}

