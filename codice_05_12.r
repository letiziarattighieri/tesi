# Sto modificando il codice mandato da Ludovico
# Problemi riscontrati nell'importazione del materiale usando:. pathfolder <- "data1", in alternativa ho fatto il setwd("C:/Letizia_R/layer")

#Library----
library(terra)
library(pROC)
library(caret)
library(sf)
library(purrr)
library(tools)
library(tidyverse)
library(rsample)
library(furrr)
library(pbapply)
library(ranger)
library(tidymodels)
library(e1071)

#Functions----

clean_name <- function(file) {
  var_name <- tools::file_path_sans_ext(basename(file))
  var_name <- gsub("[^[:alnum:]_]", "", var_name)
  if (grepl("^[0-9]", var_name)) {
    var_name <- paste0("x", var_name)
  }
  var_name
}


#Load data----


setwd("C:/Letizia_R/layer") 
#non so se equivale a pathfolder <- "data1" ma altrimenti non riesco a importare

#importo gli ortomosaici
ortho <- list.files(pattern = "ortho", 
                    full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~rast(.))

#importo i buffer
buffers <- list.files(pattern = "buffer.*shp",
                      full.names = TRUE )%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))


