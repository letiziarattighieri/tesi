> # 17 febbraio - orto fatti con sun sensor e ho diminuito il numero di poligoni
> # Libraries

> # Supporting function to clean file names
> clean_name <- function(file) {
+   var_name <- tools::file_path_sans_ext(basename(file))
+   var_name <- gsub("[^[:alnum:]_]", "", var_name)
+   if (grepl("^[0-9]", var_name)) {
+     var_name <- paste0("x", var_name)
+   }
+   var_name
+ }
> # Orthomosaics
> setwd("C:/Letizia_R/layer")
> # Orthomosaics
> setwd("C:/Letizia_R/Random_Forest_multi")

> ortho <- list.files(pattern = "ortho", 
+                     full.names = TRUE)%>% 
+   set_names(nm = map(., clean_name)) %>% 
+   map(~rast(.))
> # Buffers
> buffers <- list.files(pattern = "buffer.*shp",
+                       full.names = TRUE )%>% 
+   set_names(nm = map(., clean_name)) %>% 
+   map(~st_read(.))

> # Orthomosaics are cropped and masked according to the corresponding buffer
> ortho <- map2(ortho, map(buffers, ~ ext(.x)), ~ crop(.x, .y))
> ortho <- map2(ortho, buffers, ~ mask(.x, .y))
                                          
> # Flowers
> fiori <- list.files(pattern = "fiori.*shp", full.names = TRUE)%>% 
+   set_names(nm = map(., clean_name)) %>% 
+   map(~st_read(.))%>% 
+   map(~.[!st_is_empty(.), ])
Reading layer `fiori_el1' from data source `C:\Letizia_R\Random_Forest_multi\fiori_el1.shp' using driver `ESRI Shapefile'
Simple feature collection with 70 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 282761.6 ymin: 5636709 xmax: 282878.4 ymax: 5636770
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_ey1' from data source `C:\Letizia_R\Random_Forest_multi\fiori_ey1.shp' using driver `ESRI Shapefile'
Simple feature collection with 74 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 283610.6 ymin: 5635064 xmax: 283699.2 ymax: 5635188
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_ey2' from data source `C:\Letizia_R\Random_Forest_multi\fiori_ey2.shp' using driver `ESRI Shapefile'
Simple feature collection with 90 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 283681.3 ymin: 5634391 xmax: 283749.9 ymax: 5634515
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_ey3' from data source `C:\Letizia_R\Random_Forest_multi\fiori_ey3.shp' using driver `ESRI Shapefile'
Simple feature collection with 86 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 282703.2 ymin: 5635458 xmax: 282879 ymax: 5635494
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_fr2' from data source `C:\Letizia_R\Random_Forest_multi\fiori_fr2.shp' using driver `ESRI Shapefile'
Simple feature collection with 70 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 280931 ymin: 5637479 xmax: 281039.6 ymax: 5637614
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_fr3' from data source `C:\Letizia_R\Random_Forest_multi\fiori_fr3.shp' using driver `ESRI Shapefile'
Simple feature collection with 6 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 281594.6 ymin: 5637098 xmax: 281717.3 ymax: 5637106
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_gu4' from data source `C:\Letizia_R\Random_Forest_multi\fiori_gu4.shp' using driver `ESRI Shapefile'
Simple feature collection with 80 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 281428.8 ymin: 5634799 xmax: 281561.7 ymax: 5634857
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_pa2' from data source `C:\Letizia_R\Random_Forest_multi\fiori_pa2.shp' using driver `ESRI Shapefile'
Simple feature collection with 70 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 284168 ymin: 5633449 xmax: 284369.3 ymax: 5633479
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_ra2' from data source `C:\Letizia_R\Random_Forest_multi\fiori_ra2.shp' using driver `ESRI Shapefile'
Simple feature collection with 72 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 281329.2 ymin: 5638771 xmax: 281437.8 ymax: 5638838
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_ra4' from data source `C:\Letizia_R\Random_Forest_multi\fiori_ra4.shp' using driver `ESRI Shapefile'
Simple feature collection with 76 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 282535 ymin: 5639236 xmax: 282635.1 ymax: 5639305
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_sg3' from data source `C:\Letizia_R\Random_Forest_multi\fiori_sg3.shp' using driver `ESRI Shapefile'
Simple feature collection with 78 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 280359.3 ymin: 5638040 xmax: 280494.9 ymax: 5638182
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_si2' from data source `C:\Letizia_R\Random_Forest_multi\fiori_si2.shp' using driver `ESRI Shapefile'
Simple feature collection with 74 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 276864.2 ymin: 5638342 xmax: 276909.6 ymax: 5638480
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_va2' from data source `C:\Letizia_R\Random_Forest_multi\fiori_va2.shp' using driver `ESRI Shapefile'
Simple feature collection with 70 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 278201.6 ymin: 5640121 xmax: 278209.9 ymax: 5640288
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_wa1' from data source `C:\Letizia_R\Random_Forest_multi\fiori_wa1.shp' using driver `ESRI Shapefile'
Simple feature collection with 75 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 278857.4 ymin: 5638884 xmax: 278926 ymax: 5638953
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_wy1' from data source `C:\Letizia_R\Random_Forest_multi\fiori_wy1.shp' using driver `ESRI Shapefile'
Simple feature collection with 70 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 280809.9 ymin: 5636089 xmax: 280836.4 ymax: 5636194
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `fiori_wy2' from data source `C:\Letizia_R\Random_Forest_multi\fiori_wy2.shp' using driver `ESRI Shapefile'
Simple feature collection with 80 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 280825.1 ymin: 5635173 xmax: 280882.3 ymax: 5635299
Projected CRS: WGS 84 / UTM zone 32N
> # Grass/soil
> erba <- list.files(pattern = "erba.*shp", full.names = TRUE)%>% 
+   set_names(nm = map(., clean_name)) %>% 
+   map(~st_read(.))%>% 
+   map(~.[!st_is_empty(.), ])
Reading layer `erba_el1' from data source `C:\Letizia_R\Random_Forest_multi\erba_el1.shp' using driver `ESRI Shapefile'
Simple feature collection with 28 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 282761.4 ymin: 5636710 xmax: 282882.7 ymax: 5636774
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_ey1' from data source `C:\Letizia_R\Random_Forest_multi\erba_ey1.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 283630.3 ymin: 5635093 xmax: 283698.7 ymax: 5635188
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_ey2' from data source `C:\Letizia_R\Random_Forest_multi\erba_ey2.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 283687.5 ymin: 5634392 xmax: 283748.3 ymax: 5634502
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_ey3' from data source `C:\Letizia_R\Random_Forest_multi\erba_ey3.shp' using driver `ESRI Shapefile'
Simple feature collection with 35 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 282703.9 ymin: 5635459 xmax: 282891.1 ymax: 5635497
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_fr2' from data source `C:\Letizia_R\Random_Forest_multi\erba_fr2.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 280929 ymin: 5637478 xmax: 281040.8 ymax: 5637615
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_fr3' from data source `C:\Letizia_R\Random_Forest_multi\erba_fr3.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 281571 ymin: 5637096 xmax: 281734.2 ymax: 5637107
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_gu4' from data source `C:\Letizia_R\Random_Forest_multi\erba_gu4.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 281380.2 ymin: 5634777 xmax: 281558.9 ymax: 5634855
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_pa2' from data source `C:\Letizia_R\Random_Forest_multi\erba_pa2.shp' using driver `ESRI Shapefile'
Simple feature collection with 35 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 284170.6 ymin: 5633450 xmax: 284366.6 ymax: 5633479
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_ra2' from data source `C:\Letizia_R\Random_Forest_multi\erba_ra2.shp' using driver `ESRI Shapefile'
Simple feature collection with 32 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 281328.3 ymin: 5638772 xmax: 281438.6 ymax: 5638839
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_ra4' from data source `C:\Letizia_R\Random_Forest_multi\erba_ra4.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 282535.2 ymin: 5639235 xmax: 282643.8 ymax: 5639295
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_sg3' from data source `C:\Letizia_R\Random_Forest_multi\erba_sg3.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 280358.5 ymin: 5638041 xmax: 280493.7 ymax: 5638182
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_si2' from data source `C:\Letizia_R\Random_Forest_multi\erba_si2.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 276876.6 ymin: 5638364 xmax: 276910.1 ymax: 5638487
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_va2' from data source `C:\Letizia_R\Random_Forest_multi\erba_va2.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 278200.2 ymin: 5640114 xmax: 278211.2 ymax: 5640317
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_wa1' from data source `C:\Letizia_R\Random_Forest_multi\erba_wa1.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 278857.2 ymin: 5638885 xmax: 278927.5 ymax: 5638955
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_wy1' from data source `C:\Letizia_R\Random_Forest_multi\erba_wy1.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 280810.2 ymin: 5636086 xmax: 280836.9 ymax: 5636192
Projected CRS: WGS 84 / UTM zone 32N
Reading layer `erba_wy2' from data source `C:\Letizia_R\Random_Forest_multi\erba_wy2.shp' using driver `ESRI Shapefile'
Simple feature collection with 30 features and 1 field
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 280825 ymin: 5635173 xmax: 280883 ymax: 5635274
Projected CRS: WGS 84 / UTM zone 32N
> # Orthomosaics, flowers and grass/soil polygons are sorted alphabetically
> names(ortho) <- sort(names(ortho))
> names(fiori) <- sort(names(fiori))
> names(erba) <- sort(names(erba))
> # Values extraction 
> df_fiori <- map2(ortho, fiori, ~terra::extract(.x, .y))
> df_erba <- map2(ortho, erba, ~terra::extract(.x, .y))
> # Column names are changed
> # Flowers
> df_fiori <- map(df_fiori, ~{
+   colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1)))
+   .
+ })
> # Grass/soil
> df_erba <- map(df_erba, ~{
+   colnames(.) <- c("ID_poly", paste0("banda_", 1:(ncol(.)-1)))
+   .
+ })
> # Label which identifies the class:
> # Flowers
> df_fiori <- map(df_fiori, ~{
+   .$label <- "fiore"
+   .
+ })
> # Grass/soil
> df_erba <- map(df_erba, ~{
+   .$label <- "erba"
+   .
+ })
> # All the dataframes are merged into a list
> df_fiori_unified <- bind_rows(df_fiori, .id = "origin")
> df_erba_unified <- bind_rows(df_erba,.id = "origin")
> set.seed(123)  # To create reproducible results
> size_of_fiori <- nrow(df_fiori_unified)
> # Create a balanced sample
> df_erba_unified <- df_erba_unified %>%
+   group_by(origin) %>%
+   sample_n(size = size_of_fiori, replace = TRUE) %>%
+   ungroup()
> # The dataframes are merged in a new dataframe
> df_tot <- rbind(df_fiori_unified, df_erba_unified) %>%
+   rowid_to_column(.) %>%
+   rename(ID_pixel = rowid)
> # Splitting data into train and test sets
> # Selecting the bands
> # Creation of a new dataframe with the necessary columns
> set.seed(123)
> df_selected <- dplyr::select(df_tot, banda_1, banda_2, banda_3, label)
> # The dataframe is splitted in training and testing sets
> data_split <- initial_split(df_selected, prop = 0.7, strata = "label")
> training_data <- training(data_split)
> test_data <- testing(data_split)
> # Labels are converted to factors
> training_data$label <- as.factor(training_data$label)
> test_data$label <- as.factor(test_data$label)
> # RF model
> train_control <- trainControl(
+   method = "cv",             # Cross-validation
+   number = 10,               # Number of k-fold for cross-validation
+   savePredictions = "final", # Saving final predictions for each model
+   classProbs = TRUE,         # Class probabilities are calculated
+   summaryFunction = twoClassSummary 
+ )
> # Tuning parameters
> tuneGrid <- expand.grid(
+   mtry = c(1,2,3),           # Number of features in each split
+   splitrule = c("gini", "extratrees"),
+   min.node.size = c(1, 3, 5) 
+ )
> # Rows with NA values are removed
> training_data <- training_data %>% na.omit()
> test_data <- test_data %>% na.omit()
> # Train the model using class weight
> set.seed(123)
> RF <- train(
+   label ~ .,
+   data = training_data,
+   method = "ranger",
+   trControl = train_control,
+   tuneGrid = tuneGrid,
+   metric = "ROC",         # Model performance evaluation metric
+   importance = 'impurity',
+   weights = ifelse(training_data$label == "fiore", 10, 1) 
+ )
> print(RF)
Random Forest 

88417 samples
    3 predictor
    2 classes: 'erba', 'fiore' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 79574, 79576, 79576, 79575, 79575, 79575, ... 
Resampling results across tuning parameters:

  mtry  splitrule   min.node.size  ROC        Sens       Spec     
  1     gini        1              0.9996069  0.9976211  0.9760841
  1     gini        3              0.9996167  0.9975730  0.9758914
  1     gini        5              0.9996442  0.9975130  0.9756991
  1     extratrees  1              0.9996777  0.9979215  0.9764698
  1     extratrees  3              0.9997002  0.9979215  0.9762775
  1     extratrees  5              0.9997026  0.9978253  0.9770497
  2     gini        1              0.9991167  0.9973448  0.9739639
  2     gini        3              0.9993109  0.9973808  0.9737716
  2     gini        5              0.9994142  0.9973688  0.9739643
  2     extratrees  1              0.9994282  0.9979575  0.9747350
  2     extratrees  3              0.9996189  0.9979695  0.9753138
  2     extratrees  5              0.9996222  0.9979335  0.9758918
  3     gini        1              0.9977469  0.9971285  0.9726133
  3     gini        3              0.9976420  0.9970925  0.9733851
  3     gini        5              0.9979517  0.9970925  0.9728063
  3     extratrees  1              0.9995209  0.9978013  0.9743492
  3     extratrees  3              0.9996245  0.9979094  0.9753130
  3     extratrees  5              0.9995332  0.9977292  0.9751207

ROC was used to select the optimal model using the largest value.
The final values used for the model were mtry = 1, splitrule = extratrees and min.node.size = 5.
> # RF model test
> predizioni <- predict(RF, newdata= test_data)
> test_accuracy <- mean(predizioni== test_data$label)
> print(paste("Test Accuracy:", test_accuracy))
[1] "Test Accuracy: 0.996833188187792"
> # Confusion matrix
> matrice_confusione <- confusionMatrix(as.factor(predizioni), 
+                                       test_data$label, 
+                                       positive = "fiore")
> print(matrice_confusione)
Confusion Matrix and Statistics

          Reference
Prediction  erba fiore
     erba  35587    59
     fiore    61  2186
                                          
               Accuracy : 0.9968          
                 95% CI : (0.9962, 0.9974)
    No Information Rate : 0.9408          
    P-Value [Acc > NIR] : <2e-16          
                                          
                  Kappa : 0.9716          
                                          
 Mcnemar's Test P-Value : 0.9273          
                                          
            Sensitivity : 0.97372         
            Specificity : 0.99829         
         Pos Pred Value : 0.97285         
         Neg Pred Value : 0.99834         
             Prevalence : 0.05925         
         Detection Rate : 0.05769         
   Detection Prevalence : 0.05930         
      Balanced Accuracy : 0.98600         
                                          
       'Positive' Class : fiore           
                                          
> # Metrics analyzed to evaluate the model
> accuracy <- matrice_confusione$overall['Accuracy']
> recall <- matrice_confusione$byClass["Sensitivity"]
> f1 <- matrice_confusione$byClass["F1"]
> auc_roc<- roc(test_data$label, as.numeric(predizioni))
Setting levels: control = erba, case = fiore
Setting direction: controls < cases
> auc <- auc(auc_roc)
> precision <- matrice_confusione$byClass["Pos Pred Value"]
> print(paste("Accuracy: ", accuracy))
[1] "Accuracy:  0.996833188187792"
> print(paste("Recall: ", recall))
[1] "Recall:  0.973719376391982"
> print(paste("F1-Score: ", f1))
[1] "F1-Score:  0.973285841495993"
> print(paste("AUC-ROC: ", auc))
[1] "AUC-ROC:  0.986004100224716"
> print(paste("Precision: ", precision))
[1] "Precision:  0.972852692478861"
> # Performance comparison on training and testing sets
> # Performance is calculated on training set
> train_predictions <- predict(RF, newdata = training_data)
> train_conf_matrix <- confusionMatrix(as.factor(train_predictions), 
+                                      training_data$label, 
+                                      positive = "fiore")
> print(train_conf_matrix)
Confusion Matrix and Statistics

          Reference
Prediction  erba fiore
     erba  83183     0
     fiore    49  5185
                                          
               Accuracy : 0.9994          
                 95% CI : (0.9993, 0.9996)
    No Information Rate : 0.9414          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.995           
                                          
 Mcnemar's Test P-Value : 7.025e-12       
                                          
            Sensitivity : 1.00000         
            Specificity : 0.99941         
         Pos Pred Value : 0.99064         
         Neg Pred Value : 1.00000         
             Prevalence : 0.05864         
         Detection Rate : 0.05864         
   Detection Prevalence : 0.05920         
      Balanced Accuracy : 0.99971         
                                          
       'Positive' Class : fiore           
                                          
> # External cross-validation
> # k-fold cross-validation
> df_selected <- na.omit(df_selected)
> set.seed(123)
> cv_results <- train(
+   label ~ .,
+   data = df_selected, # Entire dataset before division
+   method = "ranger",
+   trControl = trainControl(
+     method = "cv",
+     number = 10,
+     classProbs = TRUE,
+     summaryFunction = twoClassSummary
+   ),
+   tuneGrid = tuneGrid,
+   metric = "ROC"
+ )
> print(cv_results)
Random Forest 

126310 samples
     3 predictor
     2 classes: 'erba', 'fiore' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 113679, 113679, 113679, 113679, 113679, 113679, ... 
Resampling results across tuning parameters:

  mtry  splitrule   min.node.size  ROC        Sens       Spec     
  1     gini        1              0.9997180  0.9988139  0.9672948
  1     gini        3              0.9997249  0.9988812  0.9666218
  1     gini        5              0.9997334  0.9989065  0.9667564
  1     extratrees  1              0.9997453  0.9990579  0.9646030
  1     extratrees  3              0.9997470  0.9990495  0.9644684
  1     extratrees  5              0.9997637  0.9990831  0.9628533
  2     gini        1              0.9990317  0.9988055  0.9672948
  2     gini        3              0.9991759  0.9988055  0.9670256
  2     gini        5              0.9995007  0.9988392  0.9674293
  2     extratrees  1              0.9993757  0.9988980  0.9681023
  2     extratrees  3              0.9994432  0.9989065  0.9678331
  2     extratrees  5              0.9996429  0.9989401  0.9675639
  3     gini        1              0.9986300  0.9987298  0.9658143
  3     gini        3              0.9988955  0.9987466  0.9660834
  3     gini        5              0.9988993  0.9988139  0.9655451
  3     extratrees  1              0.9991787  0.9989065  0.9687752
  3     extratrees  3              0.9993108  0.9988812  0.9675639
  3     extratrees  5              0.9995770  0.9989401  0.9672948

ROC was used to select the optimal model using the largest value.
The final values used for the model were mtry = 1, splitrule = extratrees and min.node.size = 5.
> importance <- varImp(RF)
> print(importance)
ranger variable importance

        Overall
banda_2   100.0
banda_1    73.8
banda_3     0.0
> # Performance comparison on different sets
> print(cv_results$results)
   mtry  splitrule min.node.size       ROC      Sens      Spec        ROCSD       SensSD      SpecSD
1     1       gini             1 0.9997180 0.9988139 0.9672948 1.826282e-04 0.0002952811 0.004621124
2     1       gini             3 0.9997249 0.9988812 0.9666218 1.246837e-04 0.0003123603 0.004477335
3     1       gini             5 0.9997334 0.9989065 0.9667564 1.016000e-04 0.0002718528 0.005272138
4     1 extratrees             1 0.9997453 0.9990579 0.9646030 8.866486e-05 0.0002470022 0.005953462
5     1 extratrees             3 0.9997470 0.9990495 0.9644684 8.894554e-05 0.0002446033 0.006897849
6     1 extratrees             5 0.9997637 0.9990831 0.9628533 9.123234e-05 0.0002552979 0.006157868
7     2       gini             1 0.9990317 0.9988055 0.9672948 7.627637e-04 0.0002853973 0.003861882
8     2       gini             3 0.9991759 0.9988055 0.9670256 6.143631e-04 0.0003014733 0.004172516
9     2       gini             5 0.9995007 0.9988392 0.9674293 3.687474e-04 0.0003240956 0.004198962
10    2 extratrees             1 0.9993757 0.9988980 0.9681023 6.640246e-04 0.0002899071 0.005496425
11    2 extratrees             3 0.9994432 0.9989065 0.9678331 3.667632e-04 0.0002444425 0.004506460
12    2 extratrees             5 0.9996429 0.9989401 0.9675639 2.987706e-04 0.0002068090 0.005933143
13    3       gini             1 0.9986300 0.9987298 0.9658143 8.555626e-04 0.0003108464 0.003428438
14    3       gini             3 0.9988955 0.9987466 0.9660834 6.614404e-04 0.0002952811 0.003521116
15    3       gini             5 0.9988993 0.9988139 0.9655451 7.905466e-04 0.0002979318 0.004022703
16    3 extratrees             1 0.9991787 0.9989065 0.9687752 8.120849e-04 0.0002747297 0.003463483
17    3 extratrees             3 0.9993108 0.9988812 0.9675639 5.670699e-04 0.0002380880 0.004133746
18    3 extratrees             5 0.9995770 0.9989401 0.9672948 3.438972e-04 0.0002418557 0.004875452
> # ROC curve (Receiver Operating Characteristic) analysis:
> # ROC curve is calculated for the test set
> test_probabilities <- predict(RF, test_data, type = "prob")
> roc_test <- roc(response = test_data$label, 
+                 test_probabilities[, "fiore"])
Setting levels: control = erba, case = fiore
Setting direction: controls < cases
> # AUC is calculated (Area Under Curve)
> plot(roc_test, main
+      
+      = paste("ROC Curve (AUC =", auc(roc_test), ")"))
> saveRDS(RF, file = "C:/Letizia_R/model_RF.rds")
> saveRDS(RF, file = "C:/Letizia_R/model_RF_17.02.rds")
> # Data preparation for classification
> ortho <- map(ortho, function(x) {
+   x <- terra::subset(x, 1:3)
+   names(x) <- colnames(df_selected)[1:3]  
+   # Bands are renamed
+   return(x)
+ })
> # Application of model and save
> t0 <- Sys.time()
> classificazione_RF_list <- pbapply::pblapply(names(ortho), 
+                                              function(name) {
+                                                terra::predict(ortho[[name]], RF, na.rm=TRUE)  
+                                              })
  |++++++++++++++++                                  | 31% ~13m 55s      Aggregating predictions.. Progress: 79%. Estimated remaining time: 8 seconds.
  |++++++++++++++++++++++++++++++++++++++            | 75% ~06m 58s      Aggregating predictions.. Progress: 82%. Estimated remaining time: 6 seconds.
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=26m 24s
> # NAs can be removed because they are in the background 
> t1 <- Sys.time()
> print(t1-t0)
Time difference of 1.102771 hours
> # Names need to match
> names(classificazione_RF_list) <- names(ortho)
> # Classification maps are saved as .tif files
> walk(names(classificazione_RF_list), function(name) {
+   terra::writeRaster(classificazione_RF_list[[name]],
+                      filename = paste0(name, ".tif"),
+                      overwrite=TRUE)
+ })
                                          
> # Function to count pixels for each category
> countCategoryPixels <- function(classification, category) {
+   sum(values(classification) == category, na.rm = TRUE)
+ }
> # Pixels for each cateogory are counted for every orthomosaic
> results <- lapply(classificazione_RF_list, function(classification) {
+   # We assume that 1 represents the value for "flowers" and 2 for "grass"
+   count_flowers <- countCategoryPixels(classification, 2)
+   count_grass <- countCategoryPixels(classification, 1)
+   
+   return(data.frame(Flowers = count_flowers, Grass = count_grass))
+ })
> # The results are combined to obtain the total count 
> total_counts <- do.call(rbind, results)
> row.names(total_counts) <- names(classificazione_RF_list) 
> print(total_counts)
          Flowers   Grass
ortho_el1    9028 1743490
ortho_ey1   50389 2007939
ortho_ey2   53028 2160678
ortho_ey3   26397 3000631
ortho_fr2   13676 2930374
ortho_fr3   53671 2400011
ortho_gu4  151079 3640545
ortho_pa2   12623 4574922
ortho_ra2   28718 4803738
ortho_ra4   16288 2169962
ortho_sg3   53876 5076819
ortho_si2   73944 3502601
ortho_va2    4384 2560766
ortho_wa1   17974 2787474
ortho_wy1   13095 2443045
ortho_wy2   47749 2028410
