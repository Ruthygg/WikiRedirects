#' @title Functions for Redirects
#' @name WikiRedirects
#' @docType package
#'@seealso \code{\link{getAllRedirects}} for finding all possible redirects for a given wikipedia title
#'@seealso \code{\link{solveRedirect}} Finds the latest version title of a page title (in case it is redirect)


solveRedirect <- function (title, wiki="en") {
  #  Returns the current title page
  #' Args:
  #'  title: The page title in wikipedia
  #'  wiki: language of the page
  #' Returns:
  #'  The current title of the page 
  title <-  gsub(" ","_",title)
  title<- URLencode(title, reserved = TRUE)
  page_title_url<- paste("https://", wiki,".wikipedia.org/w/api.php?action=query&titles=",title, "&redirects&format=json", sep="")
  json_data <- rjson::fromJSON(file=page_title_url)
  
  if (length(json_data$query$redirects) > 1)
    print(sprintf("Page %s has more than 1 redirect. Will take the first.", title))
  
  redirect <-  gsub(" ","_",json_data$query$redirects[[1]]$to)
  
  if(length(redirect)<=0){
    redirect <- title
    redirect<-URLdecode(redirect) 
  }
  return(redirect)
}


#get_redirects_from_wikipedia_page <- function(page)
getAllRedirects<- function(page, wiki="en")
{
  #  Returns all the redirects of a page
  #' Args:
  #'  page: The page title in wikipedia (it needs to be the latest title, not a redirect)
  #'  wiki: language of the page
  #' Returns:
  #'  All the redirects for that language
  
  
  json_url<-paste("https://",wiki,".wikipedia.org/w/api.php?action=query&list=backlinks&blfilterredir=redirects&bltitle=",page,"&bllimit=max&format=json", sep="")
  json_data <- rjson::fromJSON(file=json_url)
  redirects<-json_data[['query']][['backlinks']]
  temp <-  rep(NA, length(redirects))
  if (length(redirects) >0 )
    for (i in 1: length(redirects))
      temp[i]<-redirects[[i]][['title']]
  
  
  return(temp)
}

