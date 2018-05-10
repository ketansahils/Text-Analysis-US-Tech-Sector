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