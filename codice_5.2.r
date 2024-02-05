#Codice al 5 febbraio. Potrei dover cambiare il nome della cartella da cui prendere i layer

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
library(kernlab)

clean_name <- function(file) {
   var_name <- tools::file_path_sans_ext(basename(file))
   var_name <- gsub("[^[:alnum:]_]", "", var_name)
   if (grepl("^[0-9]", var_name)) {
     var_name <- paste0("x", var_name)
   }
   var_name
 }

pathfolder<-"C:/Letizia_R/layer"
ortho <- list.files(pattern = "ortomosaico", 
                    full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~rast(.))

buffers <- list.files(pattern = "buffer.*shp",
                      full.names = TRUE )%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))

ortho<-map2(ortho, map(buffers, ~ ext(.x)), ~ crop(.x, .y))
ortho<-map2(ortho, buffers, ~ mask(.x, .y))

fiori<- list.files(pattern = "fiori.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

erba<-list.files(pattern = "erba.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

names(ortho) <- sort(names(ortho))
names(fiori) <- sort(names(fiori))
names(erba) <- sort(names(erba))

df_fiori <- map2(ortho, fiori, ~terra::extract(.x, .y))
df_erba <- map2(ortho, erba, ~terra::extract(.x, .y))

df_fiori <- map(df_fiori, ~{
  colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1)))
  .
})

df_erba <- map(df_erba, ~{
  colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1)))
  .
})

df_fiori <- map(df_fiori, ~{
  .$label <- "fiore"
  .
})

df_erba <- map(df_erba, ~{
  .$label <- "erba"
  .
})

df_fiori_unified <- bind_rows(df_fiori, .id = "origin")
df_erba_unified <- bind_rows(df_erba,.id = "origin")

set.seed(123)
size_of_fiori <- nrow(df_fiori_unified)

df_erba_unified <- df_erba_unified %>%
  group_by(origin) %>%
  sample_n(size = size_of_fiori, replace = TRUE) %>%
  ungroup()

df_tot <- rbind(df_fiori_unified, df_erba_unified) %>%
rowid_to_column(.) %>%
rename(ID_pixel = rowid)

set.seed(123)
df_selected <- dplyr::select(df_tot, banda_1, banda_2, banda_3, label)

data_split <- initial_split(df_selected, prop = 0.7, strata = "label")
training_data <- training(data_split)
test_data <- testing(data_split)

training_data$label <- as.factor(training_data$label)
test_data$label <- as.factor(test_data$label)

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

training_data <- training_data %>% na.omit()
test_data <- test_data %>% na.omit()

set.seed(123)
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

print(RF)

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

train_predictions <- predict(RF, newdata = training_data)
train_conf_matrix <- confusionMatrix(as.factor(train_predictions), training_data$label, positive = "fiore")
print(train_conf_matrix)

df_selected <- na.omit(df_selected)

set.seed(123)
cv_results <- train(
label ~ .,
data = df_selected,
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

importance <- varImp(RF)
print(importance)

print(cv_results$results)

test_probabilities <- predict(RF, test_data, type = "prob")
roc_test <- roc(response = test_data$label, test_probabilities[, "fiore"])

plot(roc_test, main

= paste("ROC Curve (AUC =", auc(roc_test), ")"))

saveRDS(RF, file = "C:/Letizia_R/model_RF.rds")

ortho <- map(ortho, function(x) {
x <- terra::subset(x, 1:3)
names(x) <- colnames(df_selected)[1:3]
return(x)
})

t0 <- Sys.time()
classificazione_RF_list <- pbapply::pblapply(names(ortho), function(name) {
terra::predict(ortho[[name]], RF, na.rm=TRUE)
})
t1 <- Sys.time()
print(t1-t0)

names(classificazione_RF_list) <- names(ortho)

walk(names(classificazione_RF_list), function(name) {
terra::writeRaster(classificazione_RF_list[[name]],
filename = paste0(name, ".tif"),
overwrite=TRUE)
})
