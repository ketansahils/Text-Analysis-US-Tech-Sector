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

build_wordcloud <- function(dtm, 
                            max.words1=150,     # max no. of words to accommodate
                            min.freq=5,       # min.freq of words to consider
                            plot.title="wordcloud"){          # write within double quotes
  
  require(wordcloud)
  if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
    
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)      } # i loop ends
    
    tsum = ss.col
    
  } else { tsum = apply(dtm, 2, sum) }
  
  tsum = tsum[order(tsum, decreasing = T)]       # terms in decreasing order of freq
  head(tsum);    tail(tsum)
  
  # windows()  # Opens a new plot window when active
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(3.5, 0.5),     # range of word sizes
            min.freq,                     # min.freq of words to consider
            max.words = max.words1,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = plot.title)     # title for the wordcloud display
  
}