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

#importo gli ortomosaici (con ortho mi trova le aree intere, non ho selezionato solo il transetto)
ortho <- list.files(pattern = "ortho", 
                    full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~rast(.))

#importo i buffer
buffers <- list.files(pattern = "buffer.*shp",
                      full.names = TRUE )%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))


ortho<-map2(ortho, map(buffers, ~ ext(.x)), ~ crop(.x, .y))
ortho<-map2(ortho, buffers, ~ mask(.x, .y))
# quando ho usato questa parte di codice è uscito questo avviso, CRS è il sistema di riferimento

# warnings()
# Messaggi di avvertimento:
# 1: [mask] CRS do not match
# 2: [mask] CRS do not match
# 3: [mask] CRS do not match
# 4: [mask] CRS do not match
# 5: [mask] CRS do not match
# 6: [mask] CRS do not match
# 7: [mask] CRS do not match
# 8: [mask] CRS do not match
# 9: [mask] CRS do not match
# 10: [mask] CRS do not match
# 11: [mask] CRS do not match
# 12: [mask] CRS do not match
# 13: [mask] CRS do not match
# 14: [mask] CRS do not match
# 15: [mask] CRS do not match
# 16: [mask] CRS do not match






