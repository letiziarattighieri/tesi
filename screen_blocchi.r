# Qui metti le parti di codice da mettere come screen nella tesi, cambia interfaccia in modo da renderlo chiaro
# capisci se mettere il 17.02 o il 19.02, hanno lo stesso numero di poligoni ma il 19.02 ha un'area in meno (FR3) perché avevo solo 6 poligoni di fiori


# 19 febbraio, sempre meno poligoni come il 17.02 però qui ho tolto FR3


# Supporting function to clean file names
 clean_name <- function(file) {
   var_name <- tools::file_path_sans_ext(basename(file))
   var_name <- gsub("[^[:alnum:]_]", "", var_name)
   if (grepl("^[0-9]", var_name)) {
     var_name <- paste0("x", var_name)
   }
   var_name
 }
# Orthomosaics
 setwd("C:/Letizia_R/Random_Forest_multi_senza_fr3")
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

 # All the dataframes are merged into a list
 df_fiori_unified <- bind_rows(df_fiori, .id = "origin")
 df_erba_unified <- bind_rows(df_erba,.id = "origin")

 set.seed(123)  # To create reproducible results
 size_of_fiori <- nrow(df_fiori_unified)
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

 # Tuning parameters
 tuneGrid <- expand.grid(
   mtry = c(1,2,3),           # Number of features in each split
   splitrule = c("gini", "extratrees"),
   min.node.size = c(1, 3, 5) 
 )
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

 print(RF)
Random Forest 

82936 samples
    3 predictor
    2 classes: 'erba', 'fiore' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 74641, 74641, 74643, 74643, 74643, 74643, ... 
Resampling results across tuning parameters:

  mtry  splitrule   min.node.size  ROC        Sens       Spec     
  1     gini        1              0.9998221  0.9977513  0.9804363
  1     gini        3              0.9998288  0.9977770  0.9806316
  1     gini        5              0.9998231  0.9976999  0.9814144
  1     extratrees  1              0.9998312  0.9978027  0.9810230
  1     extratrees  3              0.9998445  0.9976228  0.9818057
  1     extratrees  5              0.9998446  0.9976871  0.9820011
  2     gini        1              0.9996079  0.9977642  0.9804366
  2     gini        3              0.9996117  0.9977256  0.9800449
  2     gini        5              0.9997102  0.9977256  0.9798496
  2     extratrees  1              0.9997294  0.9978541  0.9812194
  2     extratrees  3              0.9997338  0.9978156  0.9804363
  2     extratrees  5              0.9998348  0.9976999  0.9808280
  3     gini        1              0.9993719  0.9975586  0.9804363
  3     gini        3              0.9995700  0.9975201  0.9802402
  3     gini        5              0.9995815  0.9974687  0.9796539
  3     extratrees  1              0.9997327  0.9978413  0.9800449
  3     extratrees  3              0.9997258  0.9978284  0.9804366
  3     extratrees  5              0.9997320  0.9977770  0.9810233

ROC was used to select the optimal model using the largest value.
The final values used for the model were mtry = 1, splitrule = extratrees and min.node.size = 5.

 # RF model test
 predizioni <- predict(RF, newdata= test_data)
 test_accuracy <- mean(predizioni== test_data$label)
 print(paste("Test Accuracy:", test_accuracy))
[1] "Test Accuracy: 0.996989646635156"

 # Confusion matrix
 matrice_confusione <- confusionMatrix(as.factor(predizioni), 
                                       test_data$label, 
                                       positive = "fiore")

 print(matrice_confusione)
         Confusion Matrix and Statistics

                    Reference
          Prediction  erba fiore
               erba  33178    34
              fiore     73  2259
                                          
               Accuracy : 0.997           
                 95% CI : (0.9964, 0.9975)
    No Information Rate : 0.9355          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9753          
                                          
 Mcnemar's Test P-Value : 0.0002392       
                                          
            Sensitivity : 0.98517         
            Specificity : 0.99780         
         Pos Pred Value : 0.96870         
         Neg Pred Value : 0.99898         
             Prevalence : 0.06451         
         Detection Rate : 0.06356         
   Detection Prevalence : 0.06561         
      Balanced Accuracy : 0.99149         
                                          
       'Positive' Class : fiore           
                                          
# Metrics analyzed to evaluate the model
 accuracy <- matrice_confusione$overall['Accuracy']
 recall <- matrice_confusione$byClass["Sensitivity"]
 f1 <- matrice_confusione$byClass["F1"]
 auc_roc<- roc(test_data$label, as.numeric(predizioni))
Setting levels: control = erba, case = fiore
Setting direction: controls < cases
 auc <- auc(auc_roc)
 precision <- matrice_confusione$byClass["Pos Pred Value"]
 print(paste("Accuracy: ", accuracy))
[1] "Accuracy:  0.996989646635156"
 print(paste("Recall: ", recall))
[1] "Recall:  0.985172263410379"
 print(paste("F1-Score: ", f1))
[1] "F1-Score:  0.976864864864865"
 print(paste("AUC-ROC: ", auc))
[1] "AUC-ROC:  0.991488420358163"
 print(paste("Precision: ", precision))
[1] "Precision:  0.968696397941682"
  Performance comparison on training and testing sets
  Performance is calculated on training set
 train_predictions <- predict(RF, newdata = training_data)
 train_conf_matrix <- confusionMatrix(as.factor(train_predictions), 
                                      training_data$label, 
                                      positive = "fiore")
 print(train_conf_matrix)
       Confusion Matrix and Statistics

                    Reference
        Prediction  erba fiore
             erba  77780     0
            fiore     44  5112
                                          
               Accuracy : 0.9995          
                 95% CI : (0.9993, 0.9996)
    No Information Rate : 0.9384          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9954          
                                          
 Mcnemar's Test P-Value : 9.022e-11       
                                          
            Sensitivity : 1.00000         
            Specificity : 0.99943         
         Pos Pred Value : 0.99147         
         Neg Pred Value : 1.00000         
             Prevalence : 0.06164         
         Detection Rate : 0.06164         
   Detection Prevalence : 0.06217         
      Balanced Accuracy : 0.99972         
                                          
       'Positive' Class : fiore           
                                          
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
Random Forest 

118480 samples
     3 predictor
     2 classes: 'erba', 'fiore' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 106632, 106632, 106633, 106632, 106631, 106632, ... 
Resampling results across tuning parameters:

  mtry  splitrule   min.node.size  ROC        Sens       Spec     
  1     gini        1              0.9998387  0.9988296  0.9743406
  1     gini        3              0.9998510  0.9988296  0.9744755
  1     gini        5              0.9998532  0.9987846  0.9744762
  1     extratrees  1              0.9998576  0.9989106  0.9739364
  1     extratrees  3              0.9998592  0.9989647  0.9743416
  1     extratrees  5              0.9998602  0.9989467  0.9735307
  2     gini        1              0.9995954  0.9987756  0.9738000
  2     gini        3              0.9996609  0.9987576  0.9742049
  2     gini        5              0.9997284  0.9987486  0.9736652
  2     extratrees  1              0.9997299  0.9988026  0.9743413
  2     extratrees  3              0.9997327  0.9988476  0.9740706
  2     extratrees  5              0.9998029  0.9988746  0.9747460
  3     gini        1              0.9993840  0.9987396  0.9739346
  3     gini        3              0.9993217  0.9987486  0.9724494
  3     gini        5              0.9994511  0.9987126  0.9732598
  3     extratrees  1              0.9996656  0.9988206  0.9735307
  3     extratrees  3              0.9997311  0.9988026  0.9739359
  3     extratrees  5              0.9998009  0.9987846  0.9750160

ROC was used to select the optimal model using the largest value.
The final values used for the model were mtry = 1, splitrule = extratrees and min.node.size = 5.
 importance <- varImp(RF)
 print(importance)
ranger variable importance

        Overall
banda_2  100.00
banda_1   74.52
banda_3    0.00
 # Performance comparison on different sets
 print(cv_results$results)
   mtry  splitrule min.node.size       ROC      Sens      Spec        ROCSD       SensSD      SpecSD
1     1       gini             1 0.9998387 0.9988296 0.9743406 7.245556e-05 0.0003819692 0.004682773
2     1       gini             3 0.9998510 0.9988296 0.9744755 4.249309e-05 0.0004451209 0.005154830
3     1       gini             5 0.9998532 0.9987846 0.9744762 4.179230e-05 0.0004142045 0.004099685
4     1 extratrees             1 0.9998576 0.9989106 0.9739364 4.195760e-05 0.0004124618 0.003430090
5     1 extratrees             3 0.9998592 0.9989647 0.9743416 4.283849e-05 0.0004142119 0.002845690
6     1 extratrees             5 0.9998602 0.9989467 0.9735307 4.070592e-05 0.0004202532 0.003781883
7     2       gini             1 0.9995954 0.9987756 0.9738000 3.665195e-04 0.0004269466 0.004987283
8     2       gini             3 0.9996609 0.9987576 0.9742049 3.404088e-04 0.0004527433 0.005274677
9     2       gini             5 0.9997284 0.9987486 0.9736652 2.991315e-04 0.0004080724 0.005526261
10    2 extratrees             1 0.9997299 0.9988026 0.9743413 2.983776e-04 0.0003797276 0.004410928
11    2 extratrees             3 0.9997327 0.9988476 0.9740706 3.019998e-04 0.0004797843 0.004217855
12    2 extratrees             5 0.9998029 0.9988746 0.9747460 2.212480e-04 0.0004098397 0.004323163
13    3       gini             1 0.9993840 0.9987396 0.9739346 4.690804e-04 0.0003866581 0.005912672
14    3       gini             3 0.9993217 0.9987486 0.9724494 5.381801e-04 0.0003562318 0.005069330
15    3       gini             5 0.9994511 0.9987126 0.9732598 3.556989e-04 0.0003749496 0.005248167
16    3 extratrees             1 0.9996656 0.9988206 0.9735307 3.383669e-04 0.0004124694 0.004282645
17    3 extratrees             3 0.9997311 0.9988026 0.9739359 2.996401e-04 0.0004329083 0.003930252
18    3 extratrees             5 0.9998009 0.9987846 0.9750160 2.288607e-04 0.0004009562 0.004092528
 # ROC curve (Receiver Operating Characteristic) analysis:
 # ROC curve is calculated for the test set
 test_probabilities <- predict(RF, test_data, type = "prob")
 roc_test <- roc(response = test_data$label, 
                 test_probabilities[, "fiore"])
Setting levels: control = erba, case = fiore
Setting direction: controls < cases
 # AUC is calculated (Area Under Curve)
 plot(roc_test, main
      
      = paste("ROC Curve (AUC =", auc(roc_test), ")"))
 saveRDS(RF, file = "C:/Letizia_R/model_RF_noFR3_19.02.rds")
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
  |+++++++++++++++++++++++++++++++++++++             | 73% ~06m 58s      Aggregating predictions.. Progress: 71%. Estimated remaining time: 12 seconds.
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=24m 40s
 # NAs can be removed because they are in the background 
 t1 <- Sys.time()
 print(t1-t0)
Time difference of 24.77929 mins
 # Names need to match
 names(classificazione_RF_list) <- names(ortho)
 # Classification maps are saved as .tif files
 walk(names(classificazione_RF_list), function(name) {
   terra::writeRaster(classificazione_RF_list[[name]],
                      filename = paste0(name, ".tif"),
                      overwrite=TRUE)
 })
                                          
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
 print(total_counts)
          Flowers   Grass
ortho_el1    9820 1742698
ortho_ey1   48617 2009711
ortho_ey2   53778 2159928
ortho_ey3   30054 2996974
ortho_fr2   18901 2925149

ortho_gu4  153698 3637926
ortho_pa2   14195 4573350
ortho_ra2   44730 4787726
ortho_ra4   17136 2169114
ortho_sg3   45944 5084751
ortho_si2   60806 3515739
ortho_va2    4115 2561035
ortho_wa1   18459 2786989
ortho_wy1   12484 2443656
ortho_wy2   49280 2026879
