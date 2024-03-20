library('tibble')
library('tidyr')
library('stringr')
library('shiny')
library('shinyTime')
library("magrittr")
library('dplyr')
#library("tidyverse")
library('readr')
library('shinydashboard')
library('htmlwidgets')
library('shinyBS')
library('shinyjs')
library('shinycssloaders') #for loading spinners
library('DT')
library('shinybusy')
library('shinyalert')
library('leaflet')
library('shinyBS')
library('shinybusy')
library('shinyFiles')
library('shinythemes')

source('./helper.R')
#source('.python_metadata_writer.py')

#added W to the lookup years list 2.24.22
lookup_years = list("W"="2022-", "V"= "2021-", "U" = "2020-","T" = "2019-","S" =  "2018-","R" = "2017-","Q" = "2016-","P" = "2015-","O" = "2014-","N" = "2013-",
                    "M" = "2012-","L" = "2011-","K" = "2010-","J" = "2009-","I" = "2008-","H" = "2007-","G" = "2006-","F" = "2005-",
                    "E" = "2004-","D" = "2003-","C" = "2002-","B" = "2001-","A" = "2000-")

#list to reference when assessing coded values in file name
lookup_months = list("C" = "DEC","B" = "NOV","A" = "OCT","9" = "09-","8" = "08-","7" = "07-","6" = "06-",
                     "5" = "05-","4" = "04-","3" = "03-","2" = "02-","1" = "01-")

js <- '.nav-tabs-custom .nav-tabs li.active {
border-top-color: #d73925;
}"'

js2 <- '.nav-tabs-custom .nav-tabs li.active {
border-top-color: #d71925;
}"'

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
var element = document.documentElement,
enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
enterFS.call(element);
} else {
exitFS.call(document);
}
}'

jsnavbar <- '.navbar-default {
  background-color: #45698D !important;
}'

#create an empty data frame with named columns to store audioFiles RV
audioData<-data.frame(matrix(ncol = 2))
colnames(audioData)<-c('Orginal Filenames', 'New Filenames')



