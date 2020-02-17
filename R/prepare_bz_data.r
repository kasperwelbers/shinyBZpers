prepare_data <- function(ids, deduplicate, db_file) {
  tc = get_tc(db_file, ids)
  tc$meta$date = as.POSIXct(tc$meta$date)
  
  if (!is.na(deduplicate)) {
    message("\nDeduplicating")
    tc$deduplicate('token', meta_cols = 'medium', keep='last', hour_window=24, date_col = 'date', similarity = deduplicate)
  }

  sim = corpustools::compare_documents(tc, from_subset = from == 1, to_subset = from == 0, date_col = 'date', feature = 'event_feature', min_similarity = 0.1, measure = 'cosine', hour_window = c(-3*24, 7*24), return_igraph=F)
  sim = sim$d
  
  data.table::setindexv(sim, cols='from')
  data.table::setindexv(sim, cols='to')
  
  pers_index = subset(tc$meta, from==1, select=c('date','headline','doc_id'))
  data.table::setkeyv(pers_index, 'doc_id')
  data.table::setindexv(pers_index, cols=c('date'))
  pers_index$date = as.Date(pers_index$date)
  
  media_index = subset(tc$meta, from==0, select=c('date','headline','medium','doc_id'))
  data.table::setkeyv(media_index, 'doc_id')
  data.table::setindexv(media_index, cols=c('date','medium'))
  media_index$date = as.Date(media_index$date)
  
  sim = merge(sim, media_index[,c('doc_id','medium')], by.x='to', by.y='doc_id', all.x=T)
  
  list(db_file=db_file, pers_index=pers_index, media_index=media_index, sim=sim)
}

rename_cols <- function(d, from, to) {
  for (i in 1:length(from)) {
    colnames(d)[colnames(d) == from[i]] = to[i]
  }
  d
}

#' Run the Shiny Application
#'
#' @param conn         An amcatr connection
#' @param project      AmCAT project
#' @param pers_set     AmCAT articleset with BZ press articles
#' @param nieuws_set   AmCAT articleset with BZ news articles
#' @param deduplicate  Optionally, a similarity threshold for duplicates (only for articles in same medium within 24 hour diffence)
#' @param pers_headline_col    The name of the headline column in the pers articleset
#' @param pers_medium_col   The name of the medium column in the pers articleset
#' @param nieuws_headline_col    The name of the headline column in the nieuws articleset
#' @param nieuws_medium_col   The name of the medium column in the nieuws articleset
#' 
#' @export
create_bz_data <- function(conn, project=1916, pers_set=79431, nieuws_set=79457, deduplicate=NA, 
                           pers_headline_col='headline', pers_medium_col='medium', nieuws_headline_col='headline', nieuws_medium_col='medium', db_path=getwd()) {
  pers = amcatr::amcat.hits(conn, queries='*', project=project, sets=pers_set, col = c('id','date',pers_medium_col,pers_headline_col,'text'))  
  nieuws = amcatr::amcat.hits(conn, queries='*', project=project, sets=nieuws_set, col = c('id','date',nieuws_medium_col,nieuws_headline_col,'text'))   
  pers = rename_cols(pers, from=c(pers_medium_col, pers_headline_col), to=c('medium','headline'))
  nieuws = rename_cols(nieuws, from=c(nieuws_medium_col, nieuws_headline_col), to=c('medium','headline'))

  pers$from = 1
  nieuws$from = 0
  d = rbind(pers, nieuws)
  d = d[order(d$id),]
  d$title = d$headline
  
  db_file = file.path(db_path, 'shinyBZpers.db')
  tc_db(d, db_file=db_file)

  prepare_data(unique(d$id), deduplicate, db_file)
}
