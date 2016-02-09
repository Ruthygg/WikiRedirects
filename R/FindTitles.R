#'@title Contains several functions that use wikipedia pages 
#'by www.ruthygarcia.com
#'@description \code{get_redirect_wikipedia} returns the corresponding redirect of a wikipedia page if it exists otherwise it returns 
#'the same value
#'@description \code{cleanQuotes} Some titles have quotes which interfere the query. They need to be
#'properly encoded
#' @param title corresponds to the title of the wikipedia page.
#'it needs to be properly clean, all spaces should be 
#'properly encoded and the quotes cleaned
#' @param wiki needs to be in the format of : en, es, etc.
#' 
#' @examples 
#' # retrieve redirects of title
#' title <- cleanQuotes(gsub(" ", "_", "Agenda 2010"))
#' redirect <- get_redirects(title, "dewiki")
#' # results as of October 5 2015 Jens_Ammoser\tAgenda_2010 Agenda2010\tAgenda_2010 Ammoser\tAgenda_2010

cleanQuotes<- function(word)
{
  word<-gsub("'", "\\'",word, fixed=TRUE)  
  word<-gsub("\"", "\\\"", word, fixed =TRUE)
  return(word)
}



get_redirect_wikipedia <- function (title, wiki, ...)
{
  result <- tryCatch({
    title<-gsub("_"," ",title)
    # extract the content of the page
    content<- page_content(wiki,"wikipedia", page_name =title,  as_wikitext=TRUE)    
    
    # check if the title is a redirect
    if (length(grep("^\\#redirect", tolower(content$parse$wikitext$`*`))) > 0) {
      print (sprintf("Found a redirect from %s", title))
      title <- sub(".*\\[\\[(.*?)\\]\\].*", "\\1", content$parse$wikitext$`*`) 
    }
  },
  error = function(e){
    return (e)
  })
  
  if (is(result,"error")) { 
    #print(sprintf("There was an error with title: %s ",title))
    return (NA)
  }
  else
    return(title)  
}



retrieve_wikidata_info <- function (title,wiki, ...)
{
  complete_term <- NA
  flag <- TRUE
  count <- 0
  title<-gsub("_"," ",title)
  
  if (length(title)>0)
    #retrieve results for query in wikidata
    result<-tryCatch({
      found_term <- find_item(title)
    }, 
    error = function(e) {
      e
    }) 
  
  
  
  if (!is(result,"error")) {
   
     while ( flag & count < length(found_term))
        result<-tryCatch({
        #Move from one result to other  
          count <- count + 1
        
        if (count)
            complete_term <- get_item(found_term[[count]]$id)
        
        if(
        #  tolower(title)==tolower(as.character(unlist(complete_term$sitelinks$enwiki)[2])) | #if it is not the same as the corresponding link in English
          
          tolower(title)==tolower(complete_term$label$en$value) # And also the title is not the same as the wikidata page
          ){
        
            flag <- FALSE
          }
        }, 
        error = function(e) {
          e
          
        })
  
 
  }
  
  if (flag)
  {
    print(sprintf("There was no match in wikidata for title %s or there was an error", title )) 
    complete_term <- NA
  }
  
  return(complete_term) 
}



