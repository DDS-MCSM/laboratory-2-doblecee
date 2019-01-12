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
  cpe.reference <- rvest::html_text(rvest::xml_nodes(cpe.raw,xpath="//d1:cpe-item/d1:references/d1:reference/@href"))
  cpe.title <- rvest::html_text(rvest::xml_nodes(cpe.raw,xpath="//d1:cpe-item/d1:title"))
  cpe.id <- rvest::html_text(rvest::xml_nodes(cpe.raw,xpath="//d1:cpe-item/@name"))

  # return data frame
  aux.data.frame <- data.frame(title=cpe.title
                              )

  # data manipulation

  return(aux.data.frame)
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
