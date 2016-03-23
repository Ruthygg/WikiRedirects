#' @title Functions for Redirects
#' @name WikiRedirects
#' @docType package
#'@seealso \code{\link{getAllRedirects}} for finding all possible redirects for a given wikipedia title


#get_redirects_from_wikipedia_page <- function(page)
getAllRedirects<- function(page)
{
  json_url<-paste("https://en.wikipedia.org/w/api.php?action=query&list=backlinks&blfilterredir=redirects&bltitle=",page,"&bllimit=max&format=json", sep="")
  json_data <- fromJSON(file=json_url)
  redirects<-json_data[['query']][['backlinks']]
  temp <-  rep(NA, length(redirects))
  if (length(redirects) >0 )
    for (i in 1: length(redirects))
      temp[i]<-redirects[[i]][['title']]
  
  
  return(temp)
}

