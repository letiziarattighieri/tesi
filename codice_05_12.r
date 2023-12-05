# Sto modificando il codice mandato da Ludovico
# Problemi riscontrati nell'importazione del materiale usando:. pathfolder <- "data1", in alternativa ho fatto il setwd("C:/Letizia_R/layer")

# in questo codice ho usato il ritaglio del buffer, non ho usato l'orto intero perché pesano troppo

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

ortho<-map2(ortho, map(buffers, ~ ext(.x)), ~ crop(.x, .y))
ortho<-map2(ortho, buffers, ~ mask(.x, .y))

fiori <- list.files(pattern = "fiori.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

erba<-list.files(pattern = "erba.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

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


ggplot(df_selected, aes(x = label, y = banda_1, fill = label)) +
  geom_boxplot() +
  labs(title = "Distribuzione della Banda 1 per Classe", x = "Classe", y = "Valore Banda 1")


# provo a fare la stessa cosa con la banda 2 perché
# print(importance)
# ranger variable importance
# Overall
# banda_2  100.00
# banda_1   91.98
# banda_3    0.00

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
 t1-t0

 # time difference of: 27.95613 mins
 
# qui mi dà problemi---------------------------------------------------------------------------
classificazione_RF_list <- future_map(ortho[[1]], ~predict(., RF),.progress = TRUE)
# ---------------------------------------------------------------------------------------------
 
 
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

# Flowers   Grass
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


# load("C:/Letizia_R/script/codice_05.12.RData")


  



