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

pathfolder<-"data1" 
ortho <- list.files(path=pathfolder,
                    pattern = "Ortho", 
                    full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~rast(.))



buffers <- list.files(path=pathfolder,
                      pattern = "Buffer.*shp",
                      full.names = TRUE )%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))


ortho<-map2(ortho, map(buffers, ~ ext(.x)), ~ crop(.x, .y))
ortho<-map2(ortho, buffers, ~ mask(.x, .y))

#Fiori ed Erba


fiori<- list.files(path=pathfolder, pattern = "fiori.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

erba<-list.files(path=pathfolder,pattern = "erba.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

#Ordiniamo i nomi in modo che coincidano
names(ortho) <- sort(names(ortho))
names(fiori) <- sort(names(fiori))
names(erba) <- sort(names(erba))

#Values extraction----
df_fiori <- map2(ortho, fiori, ~terra::extract(.x, .y))
df_erba <- map2(ortho, erba, ~terra::extract(.x, .y))


#Nomi delle colonne di ogni dataframe
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

#Bind dataframes----

# Unisci tutti i dataframe in una lista
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

#RF model test----
predizioni <- predict(RF, newdata= test_data)
test_accuracy <- mean(predizioni== test_data$label)
print(paste("Test Accuracy:", test_accuracy))

matrice_confusione <- confusionMatrix(as.factor(predizioni), test_data$label, positive = "fiore")
print(matrice_confusione)

accuracy <- matrice_confusione$overall['Accuracy']
recall <- matrice_confusione$byClass["Sensitivity"]
f1 <- matrice_confusione$byClass["F1"]
auc_roc<- roc(test_data$label, as.numeric(predizioni))
auc <- auc(auc_roc)
precision <- matrice_confusione$byClass["Pos Pred Value"]

print(paste("Accuracy: ", accuracy))
print(paste("Recall: ", recall))
print(paste("F1-Score: ", f1))
print(paste("AUC-ROC: ", auc))
print(paste("Precision: ", precision))



#Valutiamo l'overfitting----

#Confronto tra Prestazioni su Addestramento e Test:
# Calcola le prestazioni sul set di addestramento
train_predictions <- predict(RF, newdata = training_data)
train_conf_matrix <- confusionMatrix(as.factor(train_predictions), training_data$label, positive = "fiore")
print(train_conf_matrix)



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

#Verifichiamo la robustezza----

importance <- varImp(RF)
print(importance)


#Confronto delle Prestazioni su Set Diversi:
print(cv_results$results)

#Analisi della Curva ROC:
# Calcola e traccia la curva ROC per il set di test
test_probabilities <- predict(RF, test_data, type = "prob")
roc_test <- roc(response = test_data$label, test_probabilities[, "fiore"])

# Calcola l'AUC
auc(roc_test)

# Aggiungi l'AUC al grafico
plot(roc_test, main = paste("ROC Curve (AUC =", auc(roc_test), ")"))



# Le caratteristiche che catturano meglio le differenze di colore 
# (probabilmente le bande 1 e 2, a seconda di come sono state misurate) 
# saranno le più informative per il modello. Se la banda 3 non cambia tra fiori 
# gialli e bianchi o non riflette differenze significative che aiutano a 
# discriminare tra le due classi, allora è ragionevole che il modello le assegni 
# un'importanza bassa o nulla.
# 
# Nel contesto del machine learning applicato all'analisi delle immagini, 
# soprattutto in applicazioni come la classificazione di fiori dove il colore è
# un fattore distintivo importante, le bande specifiche che catturano
# l'informazione rilevante (come quelle che corrispondono alle lunghezze 
# d'onda in cui i fiori hanno il maggiore contrasto) saranno molto più utili
# per il modello.
# 
# Questo può essere esaminato in termini di:


#2:Caratteristiche Spettrali:

# Per esempio, crea un boxplot per confrontare le distribuzioni delle bande
ggplot(df_selected, aes(x = label, y = banda_1, fill = label)) +
  geom_boxplot() +
  labs(title = "Distribuzione della Banda 1 per Classe", x = "Classe", y = "Valore Banda 1")



# Il boxplot che abbiamo generato mostra la distribuzione dei valori della banda 1 
# per le due classi: "erba" e "fiore". Dall'immagine, sembra che ci sia una 
# differenza significativa nella distribuzione dei valori tra le due classi, 
# il che potrebbe spiegare perché la banda 1 è stata identificata come la più
# importante dal nostro modello di classificazione.
# 
# La classe "fiore" mostra valori più alti nella banda 1 rispetto alla classe 
# "erba", il che suggerisce che questa banda cattura una caratteristica distintiva
# che aiuta il modello a discriminare tra le due classi. Questa differenza 
# potrebbe corrispondere alla riflettanza o all'assorbenza luminosa specifica dei
# fiori gialli e bianchi nella banda 1 dello spettro visibile, presumibilmente
# legata al colore giallo.




# saveRDS(RF, file = "/home/PERSONALE/ludovico.chieffallo2/Showcase_proj/model_RF.rds")
# 
# 
# #per caricarlo RF_loaded <- readRDS(file = "/home/PERSONALE/ludovico.chieffallo2/Showcase_proj/model_rf.rds")
# 
# 
# 
# 
# #Funzione a tutti gli elementi in ortho
# ortho <- map(ortho, function(x) {
#   # Seleziona solo le prime tre bande
#   x <- terra::subset(x, 1:3)
#   # Rinomina queste bande
#   names(x) <- colnames(df_selected)[1:3]
#   return(x)
# })
# 
# 
# 
# #Application of model and save----
# t0<-Sys.time()
# classificazione_RF_list <- pbapply::pblapply(names(ortho), function(name) {
#   terra::predict(ortho[[name]], RF, na.rm=TRUE) #gli NA sono dello sfondo, quindi possiamo escluderli
# })
# t1<-Sys.time()
# t1-t0
# 
# #Time difference of 30.24887 mins
# 
# #QUI
# 
# #classificazione_RF_list <- future_map(ortho[[1]], ~predict(., RF),.progress = TRUE)
# 
# 
# # Salva la classificazione come un file GeoTIFF
# #output_filename <- "classificazione_RF.tif"
# #writeRaster(classificazione_RF, output_filename, overwrite = TRUE)
# 
# 
# names(classificazione_RF_list) <- names(ortho)  # Assicuriamoci che i nomi corrispondano
# 
# 
# walk(names(classificazione_RF_list), function(name) {
#   terra::writeRaster(classificazione_RF_list[[name]], 
#                      filename = paste0(name, ".tif"), 
#                      overwrite=TRUE)
# })
# 


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

