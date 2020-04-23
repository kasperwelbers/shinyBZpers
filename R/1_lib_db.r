
write_to_table <- function(conn, name, value, indices=c(), new=F) {
  if (RSQLite::dbExistsTable(conn, name) && new) RSQLite::dbRemoveTable(conn, name)
  if (!RSQLite::dbExistsTable(conn, name)) {
    RSQLite::dbCreateTable(conn, name, value)
    for (i in indices) {
      RSQLite::dbExecute(conn, sprintf('DROP INDEX IF EXISTS %s_%s', name, i))
      RSQLite::dbExecute(conn, sprintf('CREATE INDEX %s_%s ON %s (%s);', name, i, name, i))
    }
  }
  RSQLite::dbAppendTable(conn, name, value)
}

tc_to_db <- function(conn, tc) {  
  message("\nSaving tokens to DB")
  
  d = tc_to_json(tc)
  suppressWarnings({
    write_to_table(conn, 'tc', d, indices='doc_id')
  })
}

doc_exists <- function(conn, doc_ids) {
  if (RSQLite::dbExistsTable(conn, 'tc'))
    in_db = RSQLite::dbGetQuery(conn, sprintf('select doc_id from tc where doc_id in (%s);', paste(doc_ids, collapse=',')))$doc_id
  else 
    in_db = c()
  !as.character(doc_ids) %in% as.character(in_db)
}

get_tc <- function(db_file, doc_ids=NULL) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)
  
  if (is.null(doc_ids)) {
    tc = RSQLite::dbReadTable(conn, 'tc')
  } else {
    tc = RSQLite::dbGetQuery(conn, sprintf('select * from tc where doc_id in (%s);', paste(doc_ids, collapse=',')))
  }

  tokens = lapply(tc$tokens_json, jsonlite::fromJSON)
  for (i in 1:length(tokens)) tokens[[i]]$doc_id = tc$doc_id[i] 
  meta = lapply(tc$meta_json, jsonlite::fromJSON)
  for (i in 1:length(meta)) meta[[i]]$doc_id = tc$doc_id[i] 
  
  RSQLite::dbDisconnect(conn)
  corpustools::tokens_to_tcorpus(tokens = data.table::rbindlist(tokens, use.names=T, fill=T), 
                                 meta = data.table::rbindlist(meta, use.names=T, fill=T))
}


add_comparison_features <- function(tc) {
  tc$preprocess('lemma', new_column = 'event_feature', lowercase = F, as_ascii = T)
  tc$feature_subset('event_feature', subset = POS %in% c('PROPN','NOUN'))
  #tc$preprocess('lemma', new_column = 'verbatim_feature', lowercase = T, as_ascii = T, ngrams=3)
  #tc$tokens$verbatim_feature = as.numeric(tc$tokens$verbatim_feature)
  tc
}

tc_db <- function(d, db_file='shinyBZpers.db', udpipe_cores=1) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)
  
  to_do = doc_exists(conn, unique(d$id))
  if (any(to_do)) {
    message(sprintf('\nNeed to parse %s documents', sum(to_do)))
    if (udpipe_cores > 1)
      tc = corpustools::create_tcorpus(d[to_do,], doc_col='id', text_columns = c('title','text'), remember_spaces=T, udpipe_model='dutch-alpino', udpipe_cores=udpipe_cores)
    else
      tc = corpustools::create_tcorpus(d[to_do,], doc_col='id', text_columns = c('title','text'), remember_spaces=T, udpipe_model='dutch-alpino')
    
    tc$meta$date = as.POSIXct(tc$meta$date)
    tc$meta$date = as.character(tc$meta$date)
    tc$delete_columns(c('xpos','feats'))
    tc = add_comparison_features(tc)
    tc_to_db(conn, tc)
  }
  RSQLite::dbDisconnect(conn)
  return(NULL)
}


tc_to_json <- function(tc) {
  .SD = NULL
  tokens = tc$tokens[,list(tokens_json=jsonlite::toJSON(as.data.frame(.SD))), by='doc_id']
  meta = tc$meta[,list(meta_json=jsonlite::toJSON(as.data.frame(.SD))), by='doc_id']
  merge(tokens,meta,by='doc_id')
}
