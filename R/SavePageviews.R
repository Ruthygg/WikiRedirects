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

library(pageviews)


#'@param title  the name of the file
#'@param year_start starting year of time window
#'@param year_end last year of time window
#'@param wiki The language of the wikipedia version
#'@param dir The dir to save the file
#'
#'
save_pageviews <- function (title, year_start, year_end, wiki, dir)
{
  
  
  
  file_name<- paste(dir,"/",nameToSaveFile(title),"_",year_start,"_", year_end,".txt", sep = "")
  
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



complementPageviews <- function (df, source.folder, end.date, platform.code="all")
{
  
  
  
  ########## Finding the pageviews of the page and all redirects
  #  Returns all the pageviews per date of all the redirects of a Wikipedia article (including latest version)
  #' Args:
  #'  df: The dataframe with the titles in the first column
  #'  source.folder : the folder where to obtain the pageviews sum e.g pageviews_sum
  #'  end.date : the max date up to where we will complement the views
  #'  platform.code : The platform the pageviews came from; one of "all", "desktop", "mobile-web" and "mobile-app". Set to "all" by default (same as pageviews package)
  #'  returns:
  #'  Nothing, it saves in the same file the complemented version
 
  for (i in 1: nrow(df) )
  {
    
    ifelse(flag, target <- lapply(df[i,1],function (x) solveRedirect(x, wiki)), target <- df[i,1])
    target<- gsub(" ", "_", target)
    file <- paste(source.folder,nameToSaveFile(target), "total.txt"  ,sep = "/")
    page.views <- read.table( file, header=T, sep="\t", stringsAsFactors=FALSE)
    page.views$date <- as.Date(page.views$date)  
    max.date <- page.views$date[nrow(page.views)]
    if( max.date < end.date ){
    max.date<- as.character(max.date)
    max.date<-paste(gsub("-", "", max.date), "00", sep = "")
    end.date<- as.character(end.date)
    end.date<-paste(gsub("-", "", end.date), "00", sep = "")
    missing<-article_pageviews(article = target,  start= max.date, end =end.date , platform=platform.code)
    missing <- subset(missing, select=c(timestamp, views) )
    colnames(missing) <- c("date", "rd.views")
    missing$date <- as.Date(substr(missing$date, 1, 8), format="%Y%m%d")
    page.views <- rbind(page.views, missing)
    write.table( page.views, file = file, sep = "\t", row.names = FALSE)
    print(sprintf("Saved additional pageviews in %s" , file) )
    }
  
    }
  
  
  
  
}



nameToSaveFile<- function(term, ...) {
  #'@param term the name of file to be saved and removes all possible signs that can cause interference 
  #'#'@param \dots Arguments to be passed to methods
  #'
  term<-  gsub("/","%2F",term, fixed = TRUE)
  return (term)
}


storePageviewsAndRedirects <- function (df, year.start, year.end, output.folder, wiki="en" ) {

########## Finding the pageviews of the page and all redirects
  #  Returns all the pageviews per date of all the redirects of a Wikipedia article (including latest version)
  #' Args:
  #'  df: The list of titles in a data frame with one column
  #'  year.start : the starting year to collect info (note that it is available from 2008 onwards)
  #'  year.end   : the ending year to collect info (note that it is available from 2008 onwards)
  #'  output.folder : the dir where each folder with the Wikipedia article title name will be created
  #'  wiki: language of the page
  #' Returns:
  #'  Folders containing in one file all the viwership data per redirect
for (i in 1: nrow(df) )
{
  
  target <- lapply(df[i,1],function (x) solveRedirect(x, wiki))
  target<- gsub(" ", "_", target)
  dir <- paste(output.folder,"/",nameToSaveFile(target), sep = "")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  save_pageviews(target, year.start, year.end, wiki, dir)
  titles <- getAllRedirects(target, wiki)
  titles <- gsub(" ", "_", titles)
  if ( length(titles) >0)
    for (j in 1:  length(titles))
      save_pageviews(titles[j], year.start, year.end, "en", dir)
  
  
}


}


addAndStoredAllRedirects <- function (df, limit, source.folder, output.folder, flag, wiki ) {
  ########## Add all the viwership data files per subfolder 
  #  Returns a directory with the addition of viwership data per subdirectory (a total.txt in each subdirectory)
  #' Args:
  #'  df: The list of titles in a data frame with one column
  #'  limit : The date limiting the views
  #'  output.folder : the dir where each folder with the Wikipedia article title name will be created
  #'  wiki: language of the page
  #' Returns:
  #' A folder with subdirectories, each containing the addition in a total.txt of all redirects
########## Adding all views into a single file per item
for (i in 1: nrow(df) ) {
  ifelse(flag, target <- lapply(df[i,1],function (x) solveRedirect(x, wiki)), target <- df[i,1])
  target<- gsub(" ", "_", target)
  dir <- paste(source.folder,"/",nameToSaveFile(target), sep = "")
  
  views_sum <- get_totalViewsinFolder(dir, limit, "enwiki" )
  if(nrow(views_sum) > 0) {
    dir<- paste(output.folder, "/", basename(dir), sep="")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file <- paste(dir,"/total.txt", sep="/")
    write.table(views_sum, file = file, sep = "\t", row.names = FALSE)
  }else
    print(sprintf(" The page %s did not provide any result", dir) )
}

}

storeNormalizedViews<- function (df,df2, pageviews.sum.folder, pageviews.output.dir, flag, wiki) {

  ########## Normalizes the pageviews according to a file containing the max views of a wikipedia page
  #  Returns a directory with normalized pageviews
  #' Args:
  #'  df: The list of titles in a data frame with one column (or considering only the first column)
  #'  pageviews.sum.folder  : the dir with subdirectories containing the files to be normalized that go in accordance to df
  #'  pageviews.output.dir  : where the normaliztion will be stored, it is a replica of pageviews.sum.folder with normalized data
  #' Returns:
  #' Subfolders and files containing normalized views stored in pageviews.output.dir
  ########## Adding all views into a single file per item
  for (i in 1: nrow(df) ) {
    ifelse(flag, target <- lapply(df[i,1],function (x) solveRedirect(x, wiki)), target <- df[i,1])
    target<- gsub(" ", "_", target)
    dir <- paste(pageviews.sum.folder,"/",nameToSaveFile(target), sep = "")
    file <- paste(dir,"total.txt", sep="/")
    total_views <- read.table(file, header=T, sep="\t", stringsAsFactors=FALSE, encoding="UTF-8")
    total_views$date2<- substr(as.character(total_views$date),1,7)
    
    total_views<- merge(total_views, df2, by.x='date2', by.y='date')
    total_views$rd.views <- total_views$rd.views/total_views$daily_views
    total_views <- subset(total_views, select=c(date,rd.views))
    total_views<-total_views[with(total_views,order(as.Date(date), decreasing=FALSE)),]
    output.file <- paste(pageviews.output.dir,nameToSaveFile(target),"total.txt", sep="/")
    
    if(nrow(total_views) >0 ){
      dir.create(dirname(output.file), recursive=TRUE,  showWarnings = FALSE)
      write.table(total_views,file=output.file, sep = "\t", row.names = FALSE)
    }else
      print(sprintf(" The page %s did not provide any result", file) )
    
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
      folder<- paste(dir,"/",nameToSaveFile(title), sep = "")
      file_name<- paste(folder,"/",nameToSaveFile(title),".txt", sep = "")
      dir.create(dirname(file_name), showWarnings = FALSE, recursive = TRUE)
      write.table(results[complete.cases(results),], file = file_name, sep = "\t", row.names = FALSE)
    }
    else
      print(sprintf(" No results to save for %s", title) )
    }
}

