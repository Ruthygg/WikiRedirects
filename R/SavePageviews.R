#'@title Uses the function of the class GetPageviews to save results
#'by www.ruthygarcia.com
#'@description \code{save_pageviews_no_redirects} saves the pageviews of a list of titles within a certain timewindow in years.
#' It does not consider redirects

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



#'@param title  the name of the file
#'@param year_start starting year of time window
#'@param year_end last year of time window
#'@param wiki The language of the wikipedia version
#'@param dir The dir to save the file
save_pageviews <- function (title, year_start, year_end, wiki, dir)
{
  
  
  
  file_name<- paste(dir,"/",name_to_save_file(title),"_",year_start,"_", year_end,".txt", sep = "")
  
  if (!file.exists(file_name) ) {
    results<- get_pageviews(year_start, year_end, title, wiki )
    if (nrow(results) >0)
      write.table(results[complete.cases(results),], file = file_name, sep = "\t", row.names = FALSE)
    else
      print(" No results to save")
  }
  else
    print(sprintf("File %s already exists", file_name))
  
  
  
  
}


storePageviewsAndRedirects <- function (titles_df, year.start, year.end, output.folder ) {

########## Finding the pageviews of the page and all redirects

for (i in 1: nrow(titles_df) )
{
  
  target <- lapply(complete_df[i,1],function (x) solve_redirect(x))
  target<- gsub(" ", "_", target)
  dir <- paste(output.folder,"/",name_to_save_file(target), sep = "")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  save_pageviews(target, year.start, year.end, "en", dir)
  titles <- getAllRedirects(target)
  titles <- gsub(" ", "_", titles)
  if ( length(titles) >0)
    for (j in 1:  length(titles))
      save_pageviews(titles[j], year.start, year.end, "en", dir)
  
  
}


}


save_pageviews_no_redirects <- function (file_list_titles,  dir,wiki="en", year_start=2008, year_end=2016 )
{

  titles <- read.table(file_list_titles)
  colnames(titles) <- c("title")
  for (title in titles$title){
    title <- gsub(" ", "_", title)
    results<- get_pageviews(year_start, year_end, title, wiki )
    if (nrow(results) >0){
      folder<- paste(dir,"/",name_to_save_file(title), sep = "")
      file_name<- paste(folder,"/",name_to_save_file(title),".txt", sep = "")
      dir.create(dirname(file_name), showWarnings = FALSE, recursive = TRUE)
      write.table(results[complete.cases(results),], file = file_name, sep = "\t", row.names = FALSE)
    }
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
