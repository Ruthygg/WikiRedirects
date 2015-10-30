#'@title Finds Pageviews of wikipedia pages
#'by www.ruthygarcia.com
#'@description \code{get_pageviews} retrieves the pageviews within a certain timewindow for a certain term
#'@description \code{get_totalViewsinFolder} function to add all files with pageviews within a folder
#'@description \code{get_Urls} function to build a list of url to call for a certain even within two years 
#'@description \code{getData} saves the pageviews from a url 

#' @param title corresponds to the title of the wikipedia page.
#'it needs to be properly clean, all spaces should be 
#'properly encoded and the quotes cleaned

#' @examples 
#' # retrieve pageviews of a term 
#' results<- get_pageviews(2008, 2015, "Agenda_2010", "de" )   
#' write.table(results[complete.cases(results),], file = "Agenda_2010/Agenda_2010.txt", sep = "\t", row.names = FALSE)
#' #retrieves a data frame of all the pageviews for this page in dewiki
#' get_totalViewsinFolder("Agenda_2010", as.Date("2015-10-01"))
#' # Saves a file called tota.txt in Agenda_2010.txt





#'@param year_start first year of time window
#'@param year_end last year of time window
#'@param term The wikipage page title (redirect or not)
#'@param wiki The language of the wikipedia version
#'#'@param \dots Arguments to be passed to methods
get_pageviews <- function(year_start,year_end,term,wiki, ...){
  
  library(methods)
      
    urls <- get_Urls(year_start,year_end,term,wiki)
    results <- NULL
    
    for (url in urls){
      
      flag <-TRUE
      result <- NULL
      count_flag <- 0
      while (is(result,"error") | flag)
      {
        
        flag <- FALSE  
        count_flag <- count_flag +1
        result <- tryCatch({
        t<-getData(url)
        results <- rbind(results,t)
        print(sprintf("Added to time series : %s",url))
        },
        error=function(e) {
          print(sprintf("There is an error with %s", url ))
          print(sprintf("Attept [%s]", count_flag))
        })
        

      }  

  }
    
    return(results)
    
  }
  

#'@param input_dir the directory containing files with pageviews
#'@param max_date The maximum date to consider the addition of pageviews
#'#'@param \dots Arguments to be passed to methods
get_totalViewsinFolder <- function(input_dir, max_date, ...){
  
  
  
  files <- list.files(input_dir, full.names = TRUE)
  total_views <- NULL
  
  files <- list.files(input_dir, full.names = TRUE)
  t<- which(basename(files)=="total.txt" )
  
  while(length(t) > 0)
  {
    print(sprintf("There is a total file in %s! Erase the file!", input_dir))
    file.remove(files[t])
    files <- list.files(input_dir, full.names = TRUE)
    t<- which(basename(files)=="total.txt" )
  }
 
  for (file in files) {
    
    if (file.info(file)$size>0 ){
      
      views <- read.table(file, header =T, sep="\t")
      
      views<- subset(views, as.Date(date) < max_date)
      views<-unique(views)
      if(is.null(total_views)){
        total_views<- views
      }else
      {
        temp <-merge(total_views, views, by= "date", all=TRUE) 
        temp[is.na(temp)] <- 0
        temp$rd.views <- temp[,2] + temp[,3]
        total_views<-temp[,c("date","rd.views")]
      }
      
      
    } else  {
      print(sprintf("File %s does not have data. We delete it!", file))
      file.remove(file)
    }
    
  }
  total_views<- total_views[with(total_views,order(as.Date(date), decreasing=FALSE)),]
  #write.table(total_views, file = paste(input_dir,"total.txt", sep = "/"), sep = "\t", row.names = FALSE)
  #print(sprintf("File %s saved",paste(input_dir,"total.txt", sep = "/") ))
  
  return(total_views)
}



#'@param year_start first year of time window
#'@param year_end last year of time window
#'@param term The wikipage page title (redirect or not)
#'@param wiki The language of the wikipedia version
get_Urls <- function(year_start,year_end,term,wiki){
  
  root <- paste(paste("http://stats.grok.se/json/",wiki,sep=""),"/", sep="")
  urls <- NULL
  for (year in year_start:year_end){
    for (month in 1:9){
      urls <- c(urls,(paste(root,year,0,month,"/",term,sep="")))
    }
    
    for (month in 10:12){
      urls <- c(urls,(paste(root,year,month,"/",term,sep="")))
    }
  }
  return(urls)
}


#'@param term the name of file to be saved and removes all possible signs that can cause interference 
#'#'@param \dots Arguments to be passed to methods
name_to_save_file<- function(term, ...)
{
  term<-  gsub("/","%2F",term, fixed = TRUE)
  return (term)
}


#'@param url extracts the pageviews data in json format and converts it in readable format 
getData <- function(url){
  require(rjson)
  raw.data <- readLines(url, warn="F") 
  rd  <- fromJSON(raw.data)
  
  if(length(rd$daily_views)==0) {
    print("d is NA")
    return (NULL)
  }
  rd.views <- rd$daily_views 
  rd.views <- unlist(rd.views)
  rd <- as.data.frame(rd.views)
  rd$date <- rownames(rd)
  rownames(rd) <- NULL
  newdata <- rd[order(rd$date),]
  newdata$date <- as.Date(newdata$date)
  
  return(newdata)
}

  