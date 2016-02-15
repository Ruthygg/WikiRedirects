#'@title Uses the function of the class GetPageviews to save results
#'by www.ruthygarcia.com
#'@description \code{save_pageviews_no_redirects} saves the pageviews of a list of titles within a certain timewindow in years


#' @param file_list_titles is a file that contains all Wikipedia titles in one column with no tabs.
#' each title has to be in one row and it corresponds to the title of the wikipedia page.
#' each title needs to be properly clean, all spaces should be
#'properly encoded and the quotes cleaned. For example "Aristotle"

#' @param dir  Directory where all results need to be saved
#' @param wiki The language of the Wikipedia. Most common is English :"en"

#' @param year_start  The year the crawling starts (it will automatically start from January 2008). There is no previous data.
#' @param year_end  The year the crawling ends (it will automatically crawl all 2016). No data will retrieve 0.

#' @examples
#' #
#' Save all the pageviews of my list in /Users/ruthgarcia/test





save_pageviews_no_redirects <- function (file_list_titles,  dir,wiki="en", year_start=2008, year_end=2016 )
{

  titles <- read.table(file_list_titles)
  for (title in titles){
    title <- gsub(" ", "_", title)
    file_name2<- paste(dir,"/",name_to_save_file(title),".txt", sep = "")
    file_name<- paste(dir,"/",name_to_save_file(title), sep = "")
    results<- get_pageviews(year_start, year_end, title, wiki )
    if (nrow(results) >0)
      write.table(results[complete.cases(results),], file = file_name, sep = "\t", row.names = FALSE)
    else
      print(sprintf(" No results to save for %s", title) )

}
}


#
#
#
# args <- commandArgs(TRUE)
# file_list_titles <- sprintf("%s",args[1])
# dir <- sprintf("%s",args[2])
# wiki <- sprintf("%s",args[3])
# year_start <- as.numeric(sprintf("%s",args[4]))
# year_end <- as.numeric(sprintf("%s",args[5]))
# save_pageviews_no_redirects(file_list_titles,  dir,wiki, year_start, year_end)
