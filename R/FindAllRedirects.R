#'@title Retrieves all possible redirects
#'by www.ruthygarcia.com
#'@description \code{get_redirects} allows you to retrieve the redirects associated 
#'with the individual page_title. It returns NA if there are no redirects, and returns
#'the main page even of the titatle given is already a redirect
#'This needs to be run in the cluster of Wikimedia, to have access to the sql server
#'@description \code{cleanQuotes} Some titles have quotes which interfere the query. They need to be
#'properly encoded

#' @param title corresponds to the title of the wikipedia page.
#'it needs to be properly clean, all spaces should be 
#'properly encoded and the quotes cleaned

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



get_redirects <- function (title, wiki, ...)
{
        result = tryCatch({
          redirect<- system(
            sprintf("
                    mysql --defaults-file=\"${HOME}\"/replica.my.cnf -N -h %s.labsdb %s_p -e \"
                    SELECT           
                    p.page_title as stitle,
                    t.page_title as ttitle
                    FROM redirect s  
                    JOIN page p 
                    ON (s.rd_from = p.page_id) 
                    JOIN page t ON (s.rd_namespace = t.page_namespace AND s.rd_title = t.page_title) 
                    WHERE s.rd_title = '%s' AND s.rd_namespace=0; 
                    \"
                    ",
                     wiki, wiki, title
                    ), intern=TRUE )
          
          return (redirect)
        }, warning = function(war) {
          # warning handler picks up where error was generated
          print(paste("MY_WARNING:  ",war))
          return(war)
        }, error = function(e) {
          print(paste("MY_ERROR:  ",e))
          return(e)
        }
        )
}



