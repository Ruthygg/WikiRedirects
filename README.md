WikiRedirects
=========

R functions to find redirects and pageviews 

__Author:__ Ruth Garcia-Gavilanes<br/> 
__License:__ [None]<br/> 
__Status:__ Dont know

![downloads](http://cranlogs.r-pkg.org/badges/grand-total/WikidataR)

Description
======
WikiRedirects contains functions that allow to extract all redirects of a Wikipedia title in any language within Wikimedia. 
It also contains functions to crawl pageviews of a title or list of titles and save  the pageviews locally
It is written in R.
Example (to be run in R)

rm(list=ls())

library(devtools)
source_url('https://raw.githubusercontent.com/Ruthygg/WikiRedirects/master/R/GetPageviews.R')
source_url('https://raw.githubusercontent.com/Ruthygg/WikiRedirects/master/R/FindAllRedirects.R')
source_url('https://raw.githubusercontent.com/Ruthygg/WikiRedirects/master/R/SavePageviews.R')

save_pageviews_no_redirects("/Users/ruthgarcia/Documents/test_folder/list.txt", "/Users/ruthgarcia/Documents/test_folder/pageviews/")




Installation
======

Not in CRAN yet:

    
    
Dependencies
======
* [methods](http://cran.r-project.org/web/packages/methods/index.html) and its dependencies.
* [devtools](http://cran.r-project.org/web/packages/devtools/index.html) if you want to source these scripts directly.

