remove.md <- function(x)
{
  x = gsub("<.*?>", " ", x)
  return(x)
}

remove.non.ascii <- function(x)
{
  x  =  iconv(x, "latin1", "ASCII", sub="")
  return(x)
}

remove.non.alnum <- function(x)
{
  x  =  gsub("[^[:alnum:]]", " ", x)
  return(x)
}

remove.whitespace <- function(x)
{
  x  =  tm::stripWhitespace(x)
  x  =  gsub("^\\s+|\\s+$", "", x)
  return(x)
}

phrase_detector <- function(doc_df, noun=TRUE){ 
  
  if (!("pos_tag" %in% colnames(doc_df))) {doc_df = py.annotate(doc_df)}	
  
  # defining phrase components
  verb_phrase = c("RB", "RBR", "RBS", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "PRP", "PRP$")
  noun_phrase = c("JJ", "JJR", "JJS", "NN", "NNP", "NNS", "NNPS")
  
  # evaluate condn on phrase type required.  
  if (noun == "FALSE"){phrase_compts = verb_phrase} else {phrase_compts = noun_phrase}
  
  # index the tokens, drop tokens with irrelevant POSTags, etc.  
  serial_num = seq(1:nrow(doc_df))
  phrase_index = rep(0, nrow(doc_df))
  doc_df1 = data.frame(serial_num, phrase_index, doc_df)
  logical_vec = (doc_df$pos_tag %in% phrase_compts)
  doc_df_subsetted = doc_df1[logical_vec,]
  
  # drop all rows with F-T-F pattern
  n1 = nrow(doc_df_subsetted)
  try(if(n1 < 2) stop("not enough df rows"))
  if (n1 > 5) { 
    current = doc_df_subsetted$serial_num
    before = c(0, doc_df_subsetted$serial_num[1:(n1-1)])  
    after = c(doc_df_subsetted$serial_num[2:n1], 0)
    drop = ((current - before) != 1)*((after-current) != 1); # sum(drop)
    
    doc_df_subsetted = doc_df_subsetted[(!drop),]
  }
  
  # exception handling for when the DF is too small.
  try(if(nrow(doc_df_subsetted) <3) stop("not enough df rows"))
  if(nrow(doc_df_subsetted) <3)  {b0 = NULL} else {
    
    # build loop for detecting phrases    
    index = 1
    for (i1 in 2:nrow(doc_df_subsetted)){
      if ((doc_df_subsetted$serial_num[i1] - doc_df_subsetted$serial_num[i1-1]) == 1) {
        doc_df_subsetted$phrase_index[i1] = index;  
        doc_df_subsetted$phrase_index[i1-1] = index} else {index = index+1}
    }
    
    
    b0 = sapply(seq(2:max(doc_df_subsetted$phrase_index)),
                function(x) {
                  paste0(doc_df_subsetted$token[(doc_df_subsetted$phrase_index == x)], collapse = " ") }) # 0.02 secs
    
  }
  return(b0)
}

py.annotate <- function(corpus, ner = FALSE){
  
  require(reticulate)
  nltk = import("nltk")   # Import nltk
  
  clean_corpus = clean_text(corpus)
  
  if (ner == "TRUE") {text_list = lapply(clean_corpus, function(x) {py.ner(x)})} else { 
    text_list = lapply(clean_corpus, function(x) {py.postag(x)})}
  
  for (doc in 1:length(text_list)){ text_list[[doc]]$doc_num = doc    }
  text_df = bind_rows(text_list)
  text_annotated_df = text_df %>% postag_desc(penn_treebank)
  
  return(text_annotated_df) }

build_wordcloud <- function(label,count,scalex,scaley,max.words,title)
{
  wordcloud::wordcloud(label, count,     # words, their freqs 
                       scale = c(scalex, scaley),     # range of word sizes
                       min.freq=0,                     # min.freq of words to consider
                       max.words = 150,
                       random.order=FALSE,
                       rot.per=0.35,
                       colors = brewer.pal(10, "Dark2"))    # Plot results in a word cloud 
  title(sub = title)     # title for the wordcloud display
} 

clean_text <- function(text)
{
  text  =  stringr::str_replace_all(text, "<.*?>", " ")   # drop html junk
  
  text = text %>%   
  stringr::str_replace_all("\\\\s+", " ")  
  
  return(text) 
}