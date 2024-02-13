# Libraries
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

# Supporting function to clean file names
clean_name <- function(file) {
   var_name <- tools::file_path_sans_ext(basename(file))
   var_name <- gsub("[^[:alnum:]_]", "", var_name)
   if (grepl("^[0-9]", var_name)) {
     var_name <- paste0("x", var_name)
   }
   var_name
 }

# Load data

# Orthomosaics
setwd("C:/Letizia_R/layer")
ortho <- list.files(pattern = "ortho", 
                    full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~rast(.))

# Buffers
buffers <- list.files(pattern = "buffer.*shp",
                      full.names = TRUE )%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))

# Orthomosaics are cropped and masked according to the corresponding buffer
ortho <- map2(ortho, map(buffers, ~ ext(.x)), ~ crop(.x, .y))
ortho <- map2(ortho, buffers, ~ mask(.x, .y))

# Flowers
fiori <- list.files(pattern = "fiori.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

# Grass/soil
erba <- list.files(pattern = "erba.*shp", full.names = TRUE)%>% 
  set_names(nm = map(., clean_name)) %>% 
  map(~st_read(.))%>% 
  map(~.[!st_is_empty(.), ])

# Orthomosaics, flowers and grass/soil polygons are sorted alphabetically
names(ortho) <- sort(names(ortho))
names(fiori) <- sort(names(fiori))
names(erba) <- sort(names(erba))

# Values extraction 
df_fiori <- map2(ortho, fiori, ~terra::extract(.x, .y))
df_erba <- map2(ortho, erba, ~terra::extract(.x, .y))

# Column names are changed
# Flowers
df_fiori <- map(df_fiori, ~{
  colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1)))
  .
})

# Grass/soil
df_erba <- map(df_erba, ~{
  colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1)))
  .
})

# Label which identifies the class:
# Flowers
df_fiori <- map(df_fiori, ~{
  .$label <- "fiore"
  .
})

# Grass/soil
df_erba <- map(df_erba, ~{
  .$label <- "erba"
  .
})

# Bind dataframes 

# All the dataframes are merged into a list
df_fiori_unified <- bind_rows(df_fiori, .id = "origin")
df_erba_unified <- bind_rows(df_erba,.id = "origin")

set.seed(123)  # To create reproducible results
size_of_fiori <- nrow(df_fiori_unified)
# Number of rows to sample for each group

# Create a balanced sample
df_erba_unified <- df_erba_unified %>%
  group_by(origin) %>%
  sample_n(size = size_of_fiori, replace = TRUE) %>%
  ungroup()

# The dataframes are merged in a new dataframe
df_tot <- rbind(df_fiori_unified, df_erba_unified) %>%
rowid_to_column(.) %>%
rename(ID_pixel = rowid)



# Splitting data into train and test sets
# Selecting the bands
# Creation of a new dataframe with the necessary columns
set.seed(123)
df_selected <- dplyr::select(df_tot, banda_1, banda_2, banda_3, label)

# The dataframe is splitted in training and testing sets
data_split <- initial_split(df_selected, prop = 0.7, strata = "label")
training_data <- training(data_split)
test_data <- testing(data_split)

# Labels are converted to factors
training_data$label <- as.factor(training_data$label)
test_data$label <- as.factor(test_data$label)



# RF model
train_control <- trainControl(
method = "cv",             # Cross-validation
number = 10,               # Number of k-fold for cross-validation
savePredictions = "final", # Saving final predictions for each model
classProbs = TRUE,         # Class probabilities are calculated
summaryFunction = twoClassSummary 
)
# Summary function for binary classification


# Tuning parameters
tuneGrid <- expand.grid(
mtry = c(1,2,3),           # Number of features in each split
splitrule = c("gini", "extratrees"),
min.node.size = c(1, 3, 5) 
)
# Minimum number of observations required to allow further divisions


# Rows with NA values are removed
training_data <- training_data %>% na.omit()
test_data <- test_data %>% na.omit()

# Train the model using class weight
set.seed(123)
RF <- train(
label ~ .,
data = training_data,
method = "ranger",
trControl = train_control,
tuneGrid = tuneGrid,
metric = "ROC",         # Model performance evaluation metric
importance = 'impurity',
weights = ifelse(training_data$label == "fiore", 10, 1) 
)
# Handling imbalance in class distribution 
# by assigning more importance to flower class

print(RF)


# RF model test
predizioni <- predict(RF, newdata= test_data)
test_accuracy <- mean(predizioni== test_data$label)
print(paste("Test Accuracy:", test_accuracy))

# Confusion matrix
matrice_confusione <- confusionMatrix(as.factor(predizioni), 
                                      test_data$label, 
                                      positive = "fiore")
print(matrice_confusione)

# Metrics analyzed to evaluate the model
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



# Overfitting evaluation

# Performance comparison on training and testing sets
# Performance is calculated on training set
train_predictions <- predict(RF, newdata = training_data)
train_conf_matrix <- confusionMatrix(as.factor(train_predictions), 
                                     training_data$label, 
                                     positive = "fiore")
print(train_conf_matrix)


# External cross-validation
# k-fold cross-validation
df_selected <- na.omit(df_selected)

set.seed(123)
cv_results <- train(
label ~ .,
data = df_selected, # Entire dataset before division
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

# Robustness check

importance <- varImp(RF)
print(importance)

# Performance comparison on different sets
print(cv_results$results)

# ROC curve (Receiver Operating Characteristic) analysis:
# ROC curve is calculated for the test set
test_probabilities <- predict(RF, test_data, type = "prob")
roc_test <- roc(response = test_data$label, 
                test_probabilities[, "fiore"])

# AUC is calculated (Area Under Curve)
plot(roc_test, main

= paste("ROC Curve (AUC =", auc(roc_test), ")"))

saveRDS(RF, file = "C:/Letizia_R/model_RF.rds")


# Data preparation for classification
ortho <- map(ortho, function(x) {
x <- terra::subset(x, 1:3)
names(x) <- colnames(df_selected)[1:3]  
# Bands are renamed
  return(x)
})

# Application of model and save
t0 <- Sys.time()
classificazione_RF_list <- pbapply::pblapply(names(ortho), 
                                             function(name) {
terra::predict(ortho[[name]], RF, na.rm=TRUE)  
})
# NAs can be removed because they are in the background 
t1 <- Sys.time()
print(t1-t0)

# Names need to match
names(classificazione_RF_list) <- names(ortho)

# Classification maps are saved as .tif files
walk(names(classificazione_RF_list), function(name) {
terra::writeRaster(classificazione_RF_list[[name]],
filename = paste0(name, ".tif"),
overwrite=TRUE)
})

# Pixels for each area are counted

# Function to count pixels for each category
countCategoryPixels <- function(classification, category) {
  sum(values(classification) == category, na.rm = TRUE)
}

# Pixels for each cateogory are counted for every orthomosaic
results <- lapply(classificazione_RF_list, function(classification) {
# We assume that 1 represents the value for "flowers" and 2 for "grass"
  count_flowers <- countCategoryPixels(classification, 2)
  count_grass <- countCategoryPixels(classification, 1)
  
  return(data.frame(Flowers = count_flowers, Grass = count_grass))
})

# The results are combined to obtain the total count 
total_counts <- do.call(rbind, results)
row.names(total_counts) <- names(classificazione_RF_list) 
# Names can be added

print(total_counts)
