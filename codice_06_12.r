# Per provare a risolvere il problema di importazione degli ortomosaici (sistema di riferimento non corrispondente) 
# ho provato così:


library(raster) # insieme ai pacchetti già caricati precedentemente

# Definisci la funzione per pulire i nomi
clean_name <- function(x) gsub("[^[:alnum:]]", "_", tools::file_path_sans_ext(basename(x)))

#quella nel codice originale è:----------------------------------------------------
clean_name <- function(file) {
  var_name <- tools::file_path_sans_ext(basename(file))
  var_name <- gsub("[^[:alnum:]_]", "", var_name)
  if (grepl("^[0-9]", var_name)) {
    var_name <- paste0("x", var_name)
  }
  var_name
}
# ------------------------------------------------------------------------------------

# Funzione per leggere il raster con controllo del CRS
read_raster_with_crs <- function(file_path) {
  raster_obj <- raster(file_path)
  
  # Verifica se il CRS è definito
  if (is.null(proj4string(raster_obj))) {
    warning("Il sistema di riferimento non è definito per il file: ", file_path)
  }
  
  return(raster_obj)
}

# Applica la funzione alle immagini orto
ortho <- list.files(pattern = "ortho", full.names = TRUE) %>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~read_raster_with_crs(.))

st_crs(ortho) #esce ancora NA ma almeno non dà più errore

#------------------------------------- da qui seguo il codice originale per vedere se funziona----------------------------------------------------





#----------------------------------------------------------------------------------- da qui ho chiesto a chatgpt di spiegarmi le funzioni: di seguito il codice in cui ho usato gli orto interi
# Funzione per pulire i nomi dei file
clean_name <- function(file) {
  # Implementazione della pulizia del nome
}

# Funzione per leggere il raster con controllo del CRS
read_raster_with_crs <- function(file_path) {
  # Implementazione della lettura del raster con controllo del CRS
}

# Imposta la directory di lavoro
setwd("C:/Letizia_R/layer")

# Leggi gli ortomosaici e verifica il sistema di riferimento
ortho <- list.files(pattern = "ortho", full.names = TRUE) %>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~read_raster_with_crs(.))

# Leggi i buffer
buffers <- list.files(pattern = "buffer.*shp", full.names = TRUE ) %>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))

# Ritaglia gli ortomosaici con i buffer
ortho <- map2(ortho, map(buffers, ~ ext(.x)), ~ crop(.x, .y))
ortho <- map2(ortho, buffers, ~ mask(.x, .y))

# Leggi i dati vettoriali (fiori e erba)
fiori <- list.files(pattern = "fiori.*shp", full.names = TRUE) %>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.)) %>% 
  map(~.[!st_is_empty(.), ])

erba <- list.files(pattern = "erba.*shp", full.names = TRUE) %>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.)) %>% 
  map(~.[!st_is_empty(.), ])

# Estrai i pixel per fiori ed erba
df_fiori <- map2(ortho, fiori, ~terra::extract(.x, .y))
df_erba <- map2(ortho, erba, ~terra::extract(.x, .y))

# Rinomina e etichetta i dataset
df_fiori <- map(df_fiori, ~{ colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1))); . })
df_erba <- map(df_erba, ~{ colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1))); . })
df_fiori <- map(df_fiori, ~{ .$label <- "fiore"; . })
df_erba <- map(df_erba, ~{ .$label <- "erba"; . })

# Unisci i dataset
df_fiori_unified <- bind_rows(df_fiori, .id = "origin")
df_erba_unified <- bind_rows(df_erba, .id = "origin")

# Imposta i seed per la riproducibilità
set.seed(123)

# Suddividi il dataset
df_selected <- dplyr::select(df_tot, banda_1, banda_2, banda_3, label)
data_split <- initial_split(df_selected, prop = 0.7, strata = "label")
training_data <- training(data_split)
test_data <- testing(data_split)

# Alcune operazioni di pulizia e conversione
training_data$label <- as.factor(training_data$label)
test_data$label <- as.factor(test_data$label)

# Imposta i parametri per il modello RandomForest
train_control <- trainControl(
  method = "cv",             
  number = 10,              
  savePredictions = "final", 
  classProbs = TRUE,         
  summaryFunction = twoClassSummary 
)

tuneGrid <- expand.grid(
  mtry = c(1,2,3),       
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 3, 5)
)

# Addestra il modello RandomForest
RF <- train(
  label ~ .,
  data = training_data,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuneGrid,
  metric = "ROC",
  importance = 'impurity', 
  weights = ifelse(training_data$label == "fiore", 10, 1) 
)

# Visualizza i risultati
print(RF)

# Valuta il modello
predizioni <- predict(RF, newdata = test_data)
# ... Altre operazioni di valutazione del modello

# Salva il modello
saveRDS(RF, file = "C:/Letizia_R/model_RF.rds")

# Carica il modello
RF_loaded <- readRDS(file = "C:/Letizia_R/model_rf.rds")

# Preprocessamento degli ortomosaici
ortho <- map(ortho, function(x) {
  x <- terra::subset(x, 1:3)
  names(x) <- colnames(df_selected)[1:3]
  return(x)
})

# Applica il modello alle immagini raster
classificazione_RF_list <- pbapply::pblapply(names(ortho), function(name) {
  terra::predict(ortho[[name]], RF, na.rm = TRUE) 
})

# Calcola e visualizza i risultati
results <- lapply(classificazione_RF_list, function(classification) {
  count_flowers <- countCategoryPixels(classification, 2)
  count_grass <- countCategoryPixels(classification, 1)
  return(data.frame(Flowers = count_flowers, Grass = count_grass))
})

total_counts <- do.call(rbind, results)
row.names(total_counts) <- names(classificazione_RF_list) 

print(total_counts)






