# 14/12/23
# Uso i transetti già tagliati su qgis
# Riprendo la parte del codice con anche la parte che era impostata come commento
# c'è solo una linea di codice che mi dà errore

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

# pathfolder<-"data1" non funziona
# ortho <- list.files(path=pathfolder,
#                    pattern = "Ortho", 
#                    full.names = TRUE)%>% 
#  set_names(nm = map(., clean_name)) %>% 
#  map(~rast(.))


setwd("C:/Letizia_R/layer") 


#importo gli ortomosaici (per ora funziona se tolgo dalla cartella il file aux)
ortho <- list.files(pattern = "rit", 
                    full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~rast(.))

#importo i buffer
buffers <- list.files(pattern = "buffer.*shp",
                      full.names = TRUE )%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))

# Reading layer `buffer_el1' from data source `C:\Letizia_R\layer\buffer_el1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 282759.5 ymin: 5636708 xmax: 282884.1 ymax: 5636774
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_ey1' from data source `C:\Letizia_R\layer\buffer_ey1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 283599.1 ymin: 5635049 xmax: 283699.8 ymax: 5635189
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_ey2' from data source `C:\Letizia_R\layer\buffer_ey2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 283679.8 ymin: 5634389 xmax: 283750.4 ymax: 5634518
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_ey3' from data source `C:\Letizia_R\layer\buffer_ey3.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 282700.6 ymin: 5635458 xmax: 282893 ymax: 5635498
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_fr2' from data source `C:\Letizia_R\layer\buffer_fr2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280927.9 ymin: 5637477 xmax: 281042 ymax: 5637616
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_fr3' from data source `C:\Letizia_R\layer\buffer_fr3.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 281569.8 ymin: 5637094 xmax: 281752.1 ymax: 5637108
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_gu4' from data source `C:\Letizia_R\layer\buffer_gu4.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 281377.6 ymin: 5634776 xmax: 281563.1 ymax: 5634858
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_pa2' from data source `C:\Letizia_R\layer\buffer_pa2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 284167.5 ymin: 5633449 xmax: 284369.3 ymax: 5633480
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_ra2' from data source `C:\Letizia_R\layer\buffer_ra2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 281326.7 ymin: 5638770 xmax: 281439.7 ymax: 5638840
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_ra4' from data source `C:\Letizia_R\layer\buffer_ra4.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 282534 ymin: 5639232 xmax: 282646.9 ymax: 5639306
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_sg3' from data source `C:\Letizia_R\layer\buffer_sg3.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280358 ymin: 5638039 xmax: 280495.7 ymax: 5638184
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_si2' from data source `C:\Letizia_R\layer\buffer_si2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 276861.9 ymin: 5638338 xmax: 276912.8 ymax: 5638489
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_va2' from data source `C:\Letizia_R\layer\buffer_va2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 278200 ymin: 5640113 xmax: 278211.4 ymax: 5640318
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_wa1' from data source `C:\Letizia_R\layer\buffer_wa1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 278856.3 ymin: 5638878 xmax: 278928.8 ymax: 5638955
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_wy1' from data source `C:\Letizia_R\layer\buffer_wy1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280809.5 ymin: 5636083 xmax: 280838.3 ymax: 5636195
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `buffer_wy2' from data source `C:\Letizia_R\layer\buffer_wy2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 1 feature and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280822.3 ymin: 5635171 xmax: 280883.1 ymax: 5635301
# Projected CRS: WGS 84 / UTM zone 32N

ortho<-map2(ortho, map(buffers, ~ ext(.x)), ~ crop(.x, .y))
ortho<-map2(ortho, buffers, ~ mask(.x, .y))

fiori <- list.files(pattern = "fiori.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

# Reading layer `fiori_el1' from data source `C:\Letizia_R\layer\fiori_el1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 81 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 282761.6 ymin: 5636709 xmax: 282878.4 ymax: 5636770
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_ey1' from data source `C:\Letizia_R\layer\fiori_ey1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 76 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 283610.6 ymin: 5635064 xmax: 283699.2 ymax: 5635188
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_ey2' from data source `C:\Letizia_R\layer\fiori_ey2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 123 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 283681.3 ymin: 5634391 xmax: 283749.9 ymax: 5634515
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_ey3' from data source `C:\Letizia_R\layer\fiori_ey3.shp' using driver `ESRI Shapefile'
# Simple feature collection with 104 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 282703.2 ymin: 5635458 xmax: 282879 ymax: 5635494
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_fr2' from data source `C:\Letizia_R\layer\fiori_fr2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 70 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280930.9 ymin: 5637479 xmax: 281039.6 ymax: 5637614
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_fr3' from data source `C:\Letizia_R\layer\fiori_fr3.shp' using driver `ESRI Shapefile'
# Simple feature collection with 7 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 281594.6 ymin: 5637098 xmax: 281717.3 ymax: 5637106
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_gu4' from data source `C:\Letizia_R\layer\fiori_gu4.shp' using driver `ESRI Shapefile'
# Simple feature collection with 140 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 281445.7 ymin: 5634806 xmax: 281562.1 ymax: 5634857
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_pa2' from data source `C:\Letizia_R\layer\fiori_pa2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 100 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 284167.7 ymin: 5633449 xmax: 284369.3 ymax: 5633479
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_ra2' from data source `C:\Letizia_R\layer\fiori_ra2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 86 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 281329.2 ymin: 5638771 xmax: 281437.8 ymax: 5638838
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_ra4' from data source `C:\Letizia_R\layer\fiori_ra4.shp' using driver `ESRI Shapefile'
# Simple feature collection with 143 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 282534.2 ymin: 5639236 xmax: 282635.2 ymax: 5639305
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_sg3' from data source `C:\Letizia_R\layer\fiori_sg3.shp' using driver `ESRI Shapefile'
# Simple feature collection with 160 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280360.2 ymin: 5638040 xmax: 280494.9 ymax: 5638182
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_si2' from data source `C:\Letizia_R\layer\fiori_si2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 121 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 276864.2 ymin: 5638342 xmax: 276908.1 ymax: 5638480
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_va2' from data source `C:\Letizia_R\layer\fiori_va2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 71 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 278201.6 ymin: 5640121 xmax: 278209.9 ymax: 5640288
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_wa1' from data source `C:\Letizia_R\layer\fiori_wa1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 121 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 278857.4 ymin: 5638884 xmax: 278926 ymax: 5638954
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_wy1' from data source `C:\Letizia_R\layer\fiori_wy1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 88 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280809.9 ymin: 5636089 xmax: 280836.4 ymax: 5636194
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `fiori_wy2' from data source `C:\Letizia_R\layer\fiori_wy2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 150 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280828.1 ymin: 5635175 xmax: 280882.3 ymax: 5635299
# Projected CRS: WGS 84 / UTM zone 32N

erba<-list.files(pattern = "erba.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

# Reading layer `erba_el1' from data source `C:\Letizia_R\layer\erba_el1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 30 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 282766.2 ymin: 5636713 xmax: 282883.9 ymax: 5636774
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_ey1' from data source `C:\Letizia_R\layer\erba_ey1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 32 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 283632.3 ymin: 5635096 xmax: 283699.1 ymax: 5635189
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_ey2' from data source `C:\Letizia_R\layer\erba_ey2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 31 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 283728.8 ymin: 5634407 xmax: 283750.3 ymax: 5634458
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_ey3' from data source `C:\Letizia_R\layer\erba_ey3.shp' using driver `ESRI Shapefile'
# Simple feature collection with 34 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 282702.4 ymin: 5635459 xmax: 282892.5 ymax: 5635497
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_fr2' from data source `C:\Letizia_R\layer\erba_fr2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 40 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280928.1 ymin: 5637477 xmax: 281036.4 ymax: 5637609
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_fr3' from data source `C:\Letizia_R\layer\erba_fr3.shp' using driver `ESRI Shapefile'
# Simple feature collection with 31 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 281570.4 ymin: 5637096 xmax: 281732.9 ymax: 5637108
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_gu4' from data source `C:\Letizia_R\layer\erba_gu4.shp' using driver `ESRI Shapefile'
# Simple feature collection with 30 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 281380.9 ymin: 5634778 xmax: 281558.2 ymax: 5634855
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_pa2' from data source `C:\Letizia_R\layer\erba_pa2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 33 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 284167.7 ymin: 5633450 xmax: 284369.1 ymax: 5633480
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_ra2' from data source `C:\Letizia_R\layer\erba_ra2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 30 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 281327 ymin: 5638779 xmax: 281436.3 ymax: 5638840
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_ra4' from data source `C:\Letizia_R\layer\erba_ra4.shp' using driver `ESRI Shapefile'
# Simple feature collection with 31 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 282535.9 ymin: 5639243 xmax: 282618.2 ymax: 5639295
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_sg3' from data source `C:\Letizia_R\layer\erba_sg3.shp' using driver `ESRI Shapefile'
# Simple feature collection with 39 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280359.5 ymin: 5638041 xmax: 280493.3 ymax: 5638182
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_si2' from data source `C:\Letizia_R\layer\erba_si2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 41 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 276873 ymin: 5638357 xmax: 276911.7 ymax: 5638489
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_va2' from data source `C:\Letizia_R\layer\erba_va2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 32 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 278200.3 ymin: 5640135 xmax: 278210.3 ymax: 5640318
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_wa1' from data source `C:\Letizia_R\layer\erba_wa1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 34 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 278860.9 ymin: 5638885 xmax: 278927.6 ymax: 5638955
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_wy1' from data source `C:\Letizia_R\layer\erba_wy1.shp' using driver `ESRI Shapefile'
# Simple feature collection with 30 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280815.3 ymin: 5636084 xmax: 280838.2 ymax: 5636176
# Projected CRS: WGS 84 / UTM zone 32N
# Reading layer `erba_wy2' from data source `C:\Letizia_R\layer\erba_wy2.shp' using driver `ESRI Shapefile'
# Simple feature collection with 30 features and 1 field
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 280842.8 ymin: 5635181 xmax: 280883 ymax: 5635270
# Projected CRS: WGS 84 / UTM zone 32N


#ordiniamo i nomi in modo che coincidano
names(ortho) <- sort(names(ortho))
names(fiori) <- sort(names(fiori))
names(erba) <- sort(names(erba))

#Values extraction----
df_fiori <- map2(ortho, fiori, ~terra::extract(.x, .y))
df_erba <- map2(ortho, erba, ~terra::extract(.x, .y))

#nomi delle colonne di ogni dataframe
df_fiori <- map(df_fiori, ~{
  colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1)))
  .
})

df_erba <- map(df_erba, ~{
  colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1)))
  .
})

#label
df_fiori <- map(df_fiori, ~{
  .$label <- "fiore"
  .
})

df_erba <- map(df_erba, ~{
  .$label <- "erba"
  .
})


#bind dataframes

#unisci tutti i dataframe in una lista
df_fiori_unified <- bind_rows(df_fiori, .id = "origin")
df_erba_unified <- bind_rows(df_erba,.id = "origin")

set.seed(123) # Per riproducibilità
size_of_fiori <- nrow(df_fiori_unified)

# Numero di righe da campionare da ciascun gruppo

# Crea un campione bilanciato
df_erba_unified <- df_erba_unified %>%
  group_by(origin) %>%
  sample_n(size = size_of_fiori, replace = TRUE) %>%
  ungroup()

df_tot <- rbind(df_fiori_unified, df_erba_unified) %>% 
  rowid_to_column(.) %>%
  rename(ID_pixel = rowid)

#Splitting into train and test----
#Seleziono le bande

set.seed(123)

# Crea un nuovo dataframe con solo le colonne che vuoi usare
df_selected <- dplyr::select(df_tot, banda_1, banda_2, banda_3, label)

# Dividi il dataframe selezionato in set di addestramento e di test
data_split <- initial_split(df_selected, prop = 0.7, strata = "label")
training_data <- training(data_split)
test_data <- testing(data_split)

# Convertiamo in un fattore-----
training_data$label <- as.factor(training_data$label)
test_data$label <- as.factor(test_data$label)

#RF Model----
train_control <- trainControl(
  method = "cv",             # Cross-validation
  number = 10,               # Numero di fold per la cross-validation
  savePredictions = "final", # Salva le predizioni per ogni modello
  classProbs = TRUE,         # Calcola le probabilità di appartenenza alle classi
  summaryFunction = twoClassSummary # Funzione di riepilogo per classificazione binaria
)

tuneGrid <- expand.grid(
  mtry = c(1,2,3),       # Numero di variabili considerate a ogni split
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 3, 5)
)

# Rimuovi eventuali righe con valori NA
training_data <- training_data %>% na.omit()
test_data <- test_data %>% na.omit()

# Addestra il modello utilizzando i pesi delle classi
set.seed(123)

RF <- train(
  label ~ .,
  data = training_data,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuneGrid,
  metric = "ROC",
  importance = 'impurity', # Qui puoi impostare 'importance' per 'ranger'
  weights = ifelse(training_data$label == "fiore", 10, 1) # Pesi per affrontare lo squilibrio dei dati
)

print(RF)
# Random Forest 
#128222 samples
#     3 predictor
#     2 classes: 'erba', 'fiore' 

# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 115400, 115400, 115399, 115399, 115400, 115400, ... 
# Resampling results across tuning parameters:
#  mtry  splitrule   min.node.size  ROC        Sens       Spec     
#  1     gini        1              0.9999653  0.9993540  0.9926394
#  1     gini        3              0.9999634  0.9993789  0.9923716
#  1     gini        5              0.9999608  0.9993623  0.9923716
#  1     extratrees  1              0.9999702  0.9994286  0.9926394
#  1     extratrees  3              0.9999651  0.9994037  0.9925055
#  1     extratrees  5              0.9999672  0.9994037  0.9931748
#  2     gini        1              0.9996977  0.9992961  0.9921039
#  2     gini        3              0.9996947  0.9992961  0.9922378
#  2     gini        5              0.9998310  0.9992878  0.9923716
#  2     extratrees  1              0.9999042  0.9994203  0.9927732
#  2     extratrees  3              0.9999057  0.9993789  0.9926394
#  2     extratrees  5              0.9999051  0.9994037  0.9927732
#  3     gini        1              0.9994807  0.9991553  0.9921039
#  3     gini        3              0.9994803  0.9991636  0.9918363
#  3     gini        5              0.9994883  0.9991553  0.9921039
#  3     extratrees  1              0.9999010  0.9994120  0.9926395
#  3     extratrees  3              0.9999043  0.9993789  0.9927734
#  3     extratrees  5              0.9999044  0.9993540  0.9927734
# ROC was used to select the optimal model using the largest value.
# The final values used for the model were mtry = 1, splitrule = extratrees and min.node.size = 1.

#RF model test----
predizioni <- predict(RF, newdata= test_data)
test_accuracy <- mean(predizioni== test_data$label)
print(paste("Test Accuracy:", test_accuracy))            # "Test Accuracy: 0.998871763143049"

matrice_confusione <- confusionMatrix(as.factor(predizioni), test_data$label, positive = "fiore")
print(matrice_confusione)
# Confusion Matrix and Statistics
#          Reference
# Prediction  erba fiore
#     erba  51618    29
#     fiore    33  3273                                       
#         Accuracy : 0.9989          
#                 95% CI : (0.9986, 0.9991)
#    No Information Rate : 0.9399          
#    P-Value [Acc > NIR] : <2e-16                                                 
#                  Kappa : 0.99                                                     
# Mcnemar's Test P-Value : 0.7032           ###################                                        
#            Sensitivity : 0.99122         
#            Specificity : 0.99936         
#         Pos Pred Value : 0.99002         
#         Neg Pred Value : 0.99944         
#             Prevalence : 0.06009         
#         Detection Rate : 0.05956         
#   Detection Prevalence : 0.06016         
#      Balanced Accuracy : 0.99529                                                  
#       'Positive' Class : fiore           
                                    
accuracy <- matrice_confusione$overall['Accuracy']
recall <- matrice_confusione$byClass["Sensitivity"]
f1 <- matrice_confusione$byClass["F1"]
auc_roc<- roc(test_data$label, as.numeric(predizioni))
auc <- auc(auc_roc)
precision <- matrice_confusione$byClass["Pos Pred Value"]

print(paste("Accuracy: ", accuracy))                     # "Accuracy:  0.998871763143049"
print(paste("Recall: ", recall))                         # "Recall:  0.991217443973349"
print(paste("F1-Score: ", f1))                           # "F1-Score:  0.990617433414044"
print(paste("AUC-ROC: ", auc))                           # "AUC-ROC:  0.995289270281964"
print(paste("Precision: ", precision))                   # "Precision:  0.990018148820326"

#Valutiamo l'overfitting----

#Confronto tra Prestazioni su Addestramento e Test:
# Calcola le prestazioni sul set di addestramento
train_predictions <- predict(RF, newdata = training_data)
train_conf_matrix <- confusionMatrix(as.factor(train_predictions), training_data$label, positive = "fiore")
print(train_conf_matrix)
# Confusion Matrix and Statistics
# Reference
# Prediction   erba  fiore
#     erba  120735      0
#     fiore     14   7473                                          
# Accuracy : 0.9999          
#                 95% CI : (0.9998, 0.9999)
#    No Information Rate : 0.9417          
#    P-Value [Acc > NIR] : < 2.2e-16                                                
#                  Kappa : 0.999           
# Mcnemar's Test P-Value : 0.000512                                                 
#            Sensitivity : 1.00000         
#            Specificity : 0.99988         
#         Pos Pred Value : 0.99813         
#         Neg Pred Value : 1.00000         
#             Prevalence : 0.05828         
#         Detection Rate : 0.05828         
#   Detection Prevalence : 0.05839         
#      Balanced Accuracy : 0.99994                                              
#       'Positive' Class : fiore    



#Cross-Validation Esterna
# Esegui k-fold cross-validation

df_selected <- na.omit(df_selected)

set.seed(123)

cv_results <- train(
  label ~ .,
  data = df_selected, # Usa l'intero dataset prima della divisione
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  tuneGrid = tuneGrid,
  metric = "ROC"
)

print(cv_results)
#Random Forest 
# 183175 samples
#     3 predictor
#     2 classes: 'erba', 'fiore' 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 164857, 164858, 164857, 164858, 164858, 164857, ... 
# Resampling results across tuning parameters:
# mtry  splitrule   min.node.size  ROC        Sens       Spec     
#  1     gini        1              0.9999639  0.9996636  0.9896058
#  1     gini        3              0.9999680  0.9996636  0.9894202
#  1     gini        5              0.9999664  0.9996578  0.9896058
#  1     extratrees  1              0.9999734  0.9997216  0.9886776
#  1     extratrees  3              0.9999732  0.9997158  0.9888630
#  1     extratrees  5              0.9999712  0.9996926  0.9887703
#  2     gini        1              0.9996525  0.9995824  0.9897915
#  2     gini        3              0.9996982  0.9995882  0.9896987
#  2     gini        5              0.9996971  0.9995882  0.9900700
#  2     extratrees  1              0.9999314  0.9996694  0.9896057
#  2     extratrees  3              0.9999308  0.9996752  0.9896057
#  2     extratrees  5              0.9998846  0.9996636  0.9896057
#  3     gini        1              0.9997835  0.9995534  0.9896059
#  3     gini        3              0.9996472  0.9995592  0.9896987
#  3     gini        5              0.9996409  0.9995244  0.9896987
#  3     extratrees  1              0.9998399  0.9996346  0.9898842
#  3     extratrees  3              0.9998393  0.9996404  0.9900698
#  3     extratrees  5              0.9998844  0.9996230  0.9896056
# ROC was used to select the optimal model using the largest value.
# The final values used for the model were mtry = 1, splitrule = extratrees and min.node.size = 1.


#Verifichiamo la robustezza----

importance <- varImp(RF)
print(importance)
#ranger variable importance
#        Overall
# banda_2  100.00
# banda_1   91.98
# banda_3    0.00

#Confronto delle Prestazioni su Set Diversi:
print(cv_results$results)
#   mtry  splitrule min.node.size       ROC      Sens      Spec        ROCSD       SensSD      SpecSD
#1     1       gini             1 0.9999639 0.9996636 0.9896058 1.783686e-05 1.120756e-04 0.002306347
#2     1       gini             3 0.9999680 0.9996636 0.9894202 1.753138e-05 1.153630e-04 0.002195633
#3     1       gini             5 0.9999664 0.9996578 0.9896058 1.728851e-05 1.109020e-04 0.002178103
#4     1 extratrees             1 0.9999734 0.9997216 0.9886776 1.179006e-05 1.305641e-04 0.001738939
#5     1 extratrees             3 0.9999732 0.9997158 0.9888630 1.291916e-05 1.266398e-04 0.001751372
#6     1 extratrees             5 0.9999712 0.9996926 0.9887703 1.425116e-05 1.473769e-04 0.001414511
#7     2       gini             1 0.9996525 0.9995824 0.9897915 3.184593e-04 1.276688e-04 0.002186402
#8     2       gini             3 0.9996982 0.9995882 0.9896987 3.279975e-04 1.142232e-04 0.002117735
#9     2       gini             5 0.9996971 0.9995882 0.9900700 3.293247e-04 1.266398e-04 0.002188372
#10    2 extratrees             1 0.9999314 0.9996694 0.9896057 1.479732e-04 9.491831e-05 0.001896599
#11    2 extratrees             3 0.9999308 0.9996752 0.9896057 1.483662e-04 1.258996e-04 0.001946360
#12    2 extratrees             5 0.9998846 0.9996636 0.9896057 2.004835e-04 1.441713e-04 0.001897152
#13    3       gini             1 0.9997835 0.9995534 0.9896059 3.326563e-04 1.523657e-04 0.002220855
#14    3       gini             3 0.9996472 0.9995592 0.9896987 3.840517e-04 1.228944e-04 0.002071837
#15    3       gini             5 0.9996409 0.9995244 0.9896987 3.224351e-04 1.541949e-04 0.001928150
#16    3 extratrees             1 0.9998399 0.9996346 0.9898842 2.257465e-04 1.129064e-04 0.001977853
#17    3 extratrees             3 0.9998393 0.9996404 0.9900698 2.265639e-04 9.782762e-05 0.001958516
#18    3 extratrees             5 0.9998844 0.9996230 0.9896056 1.993608e-04 1.318463e-04 0.001503397

#Analisi della Curva ROC:
# Calcola e traccia la curva ROC per il set di test
test_probabilities <- predict(RF, test_data, type = "prob")
roc_test <- roc(response = test_data$label, test_probabilities[, "fiore"])

# Calcola l'AUC
auc(roc_test)        # Area under the curve: 0.9999

# Aggiungi l'AUC al grafico
plot(roc_test, main = paste("ROC Curve (AUC =", auc(roc_test), ")"))


ggplot(df_selected, aes(x = label, y = banda_1, fill = label)) +
  geom_boxplot() +
  labs(title = "Distribuzione della Banda 1 per Classe", x = "Classe", y = "Valore Banda 1")

# ggplot(df_selected, aes(x = label, y = banda_2, fill = label)) +
#  geom_boxplot() +
#  labs(title = "Distribuzione della Banda 2 per Classe", x = "Classe", y = "Valore Banda 2")


# ggplot(df_selected, aes(x = label, y = banda_3, fill = label)) +
#  geom_boxplot() +
#  labs(title = "Distribuzione della Banda 3 per Classe", x = "Classe", y = "Valore Banda 3")


saveRDS(RF, file = "C:/Letizia_R/model_RF.rds")

#per caricarlo 
RF_loaded <- readRDS(file = "C:/Letizia_R/model_rf.rds")


#Funzione a tutti gli elementi in ortho
ortho <- map(ortho, function(x) {
  # Seleziona solo le prime tre bande
  x <- terra::subset(x, 1:3)
  # Rinomina queste bande
  names(x) <- colnames(df_selected)[1:3]
  return(x)
})

#Application of model and save----
t0<-Sys.time()
classificazione_RF_list <- pbapply::pblapply(names(ortho), function(name) {
  terra::predict(ortho[[name]], RF, na.rm=TRUE) #gli NA sono dello sfondo, quindi possiamo escluderli
})
t1<-Sys.time()
t1-t0                   # Time difference of 1.774917 hours


classificazione_RF_list <- future_map(ortho[[1]], ~predict(., RF),.progress = TRUE)      # Questa parte del codice non va
#Error in (function (.x, .f, ..., .progress = FALSE)  : 
#  ℹ In index: 1.
#ℹ With name: banda_1.
#Caused by error in `UseMethod()`:
#! no applicable method for 'predict' applied to an object of class "c('integer', 'numeric')"

#Continamo i pixel per area----

# Funzione per conteggiare i pixel per ogni categoria
countCategoryPixels <- function(classification, category) {
  sum(values(classification) == category, na.rm = TRUE)
}


# Calcola il conteggio per ogni orthomosaico e per ogni categoria
results <- lapply(classificazione_RF_list, function(classification) {
  # Assumiamo che 1 sia il valore per "fiore" e 2 per "erba"
  count_flowers <- countCategoryPixels(classification, 2)
  count_grass <- countCategoryPixels(classification, 1)
  
  return(data.frame(Flowers = count_flowers, Grass = count_grass))
})


# Combina i risultati per ottenere il totale
total_counts <- do.call(rbind, results)
row.names(total_counts) <- names(classificazione_RF_list) # Opzionale: aggiungi i nomi

# Visualizza o salva i risultati
print(total_counts)
#    Flowers   Grass
# 1    11588 1740930
# 2    43400 2014928
# 3    46670 2167036
# 4    74307 2952721
# 5    15227 2929564
# 6    30407 2429773
# 7   177662 3615646
# 8    14036 4574250
# 9    62810 4769646
# 10   20470 2165780
# 11   57416 5073313
# 12   54823 3522085
# 13    6302 2559245
# 14   36354 2772581
# 15   93497 2362796
# 16   53896 2022263







