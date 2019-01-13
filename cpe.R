#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#              Arnau Sangra Rocamora - Data Driven Securty                     #
#                                                                              #
#******************************************************************************#

install.packages("xml2",repos='http://cran.us.r-project.org')
install.packages("rvest",repos='http://cran.us.r-project.org')

library(xml2)
library(rvest)

compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
cpes_filename <- "cpes.zip"
download.file(compressed_cpes_url, cpes_filename)
unzip(zipfile = cpes_filename)
cpe.file <- "./official-cpe-dictionary_v2.3.xml"

GetCPEItems <- function(cpe.raw) {
  #cpe <- NewCPEItem()

  # transform the list to data frame
  cpe.items <- rvest::xml_nodes(cpe.raw,xpath="//d1:cpe-item")
  aux <- character(0)
  cpe.items.aux <- cpe.items[1:10]
  titles <- rbind()
  references <- rbind()
  names <- rbind()
  for(cpe.item in cpe.items.aux){
    title <- rvest::html_text(rvest::xml_nodes(cpe.item,xpath=".//d1:title"))
    reference <- rvest::html_text(rvest::xml_nodes(cpe.item,xpath=".//d1:references/d1:reference/@href"))
    name <- rvest::html_text(rvest::xml_nodes(cpe.item,xpath="./@name"))
    print(name)
    if(identical(aux,title)){
      titles <- rbind(titles,NA)
    }else{
      titles <- rbind(titles,title)
    }

    if(identical(aux,reference)){
      references <- rbind(references,NA)
    }else{
      references <- rbind(references,reference)
    }

    if(identical(aux,name)){
      names <- rbind(names,NA)
    }else{
      names <- rbind(names,name)
    }


  }
  data.cpe <- data.frame(title=titles,reference=references[,1], name=names)

  # data manipulation

  return(data.cpe)
}

CleanCPEs <- function(cpes){
  return(cpes)
}

ParseCPEData <- function(cpe.file) {

  # load cpes as xml file
  cpes <- tempfile(fileext = ".xml")
  cpes <- xml2::read_xml(x = cpe.file)

  # get CPEs
  cpes <- GetCPEItems(cpes)

  # transform, clean, arrange parsed cpes as data frame
  df <- CleanCPEs(cpes)

  # return data frame
  return(df)
}
