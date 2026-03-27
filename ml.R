
  library(caret)
  library(pROC)
  library(ranger)
  library(xgboost)


set.seed(42)

train_path <- "adult_train.csv"
test_path  <- "adult_test.csv"

train_data <- read.csv(train_path, stringsAsFactors = FALSE, na.strings = c("", "NA", "?", " ?"))
test_data  <- read.csv(test_path,  stringsAsFactors = FALSE, na.strings = c("", "NA", "?", " ?"))

cat("Original train shape:", dim(train_data), "\n")
cat("Original test shape :", dim(test_data), "\n")


trim_all_char <- function(df) {
  for (nm in names(df)) {
    if (is.character(df[[nm]])) {
      df[[nm]] <- trimws(df[[nm]])
    }
  }
  df
}

clean_target <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\.", "", x)
  x <- ifelse(x == "<=50K", "Low",
              ifelse(x == ">50K", "High", NA))
  factor(x, levels = c("Low", "High"))
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

mode_value <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_impute <- function(x) {
  mv <- mode_value(x)
  x[is.na(x)] <- mv
  x
}


train_data <- trim_all_char(train_data)
test_data  <- trim_all_char(test_data)

num_cols <- c("Age", "fnlwgt", "Education_Num", "Capital_Gain", "Capital_Loss", "Hours_per_week")
for (col in num_cols) {
  train_data[[col]] <- safe_numeric(train_data[[col]])
  test_data[[col]]  <- safe_numeric(test_data[[col]])
}

train_data$Target <- clean_target(train_data$Target)
test_data$Target  <- clean_target(test_data$Target)

train_data <- train_data[!is.na(train_data$Age) & !is.na(train_data$Target), , drop = FALSE]
test_data  <- test_data[!is.na(test_data$Age)  & !is.na(test_data$Target),  , drop = FALSE]

cat("Train shape:", dim(train_data), "\n")
cat("Test shape :", dim(test_data), "\n")


predictor_names <- setdiff(names(train_data), "Target")
char_cols <- predictor_names[sapply(train_data[predictor_names], is.character)]

for (col in char_cols) {
  train_data[[col]] <- factor(train_data[[col]])
  test_data[[col]]  <- factor(test_data[[col]])
}

for (col in char_cols) {
  all_levels <- union(levels(train_data[[col]]), levels(test_data[[col]]))
  train_data[[col]] <- factor(train_data[[col]], levels = all_levels)
  test_data[[col]]  <- factor(test_data[[col]],  levels = all_levels)
}

train_data$Target <- factor(train_data$Target, levels = c("Low", "High"))
test_data$Target  <- factor(test_data$Target,  levels = c("Low", "High"))

for (col in names(train_data)) {
  if (col == "Target") next
  
  if (is.numeric(train_data[[col]])) {
    med <- median(train_data[[col]], na.rm = TRUE)
    if (is.na(med)) med <- 0
    train_data[[col]][is.na(train_data[[col]])] <- med
    test_data[[col]][is.na(test_data[[col]])]   <- med
  } else {
    train_data[[col]] <- mode_impute(train_data[[col]])
    mv <- mode_value(train_data[[col]])
    test_data[[col]][is.na(test_data[[col]])] <- mv
    test_data[[col]] <- factor(test_data[[col]], levels = levels(train_data[[col]]))
  }
}


train_y <- factor(train_data$Target, levels = c("Low", "High"))
test_y  <- factor(test_data$Target,  levels = c("Low", "High"))

train_x_raw <- train_data[, setdiff(names(train_data), "Target"), drop = FALSE]
test_x_raw  <- test_data[,  setdiff(names(test_data),  "Target"), drop = FALSE]

dmy <- dummyVars(~ ., data = train_x_raw, fullRank = TRUE)

train_x <- as.data.frame(predict(dmy, newdata = train_x_raw))
test_x  <- as.data.frame(predict(dmy, newdata = test_x_raw))

# Make test columns match train columns
missing_cols <- setdiff(names(train_x), names(test_x))
if (length(missing_cols) > 0) {
  for (col in missing_cols) {
    test_x[[col]] <- 0
  }
}

extra_cols <- setdiff(names(test_x), names(train_x))
if (length(extra_cols) > 0) {
  test_x <- test_x[, setdiff(names(test_x), extra_cols), drop = FALSE]
}

test_x <- test_x[, names(train_x), drop = FALSE]

# Remove near-zero variance based on train only
nzv_idx <- nearZeroVar(train_x)
if (length(nzv_idx) > 0) {
  train_x <- train_x[, -nzv_idx, drop = FALSE]
  test_x  <- test_x[,  -nzv_idx, drop = FALSE]
}

cat("Encoded train matrix:", dim(train_x), "
")
cat("Encoded test matrix :", dim(test_x), "
")


ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = FALSE
)


cat("\nTraining Logistic Regression...\n")
set.seed(42)
model_glm <- train(
  x = train_x,
  y = train_y,
  method = "glm",
  family = binomial(),
  trControl = ctrl,
  metric = "ROC"
)

cat("Training KNN...\n")
pre_knn <- preProcess(train_x, method = c("center", "scale"))
train_knn_x <- predict(pre_knn, train_x)
test_knn_x  <- predict(pre_knn, test_x)

set.seed(42)
model_knn <- train(
  x = train_knn_x,
  y = train_y,
  method = "knn",
  trControl = ctrl,
  metric = "ROC",
  tuneLength = 12
)

cat("Training Random Forest (ranger)...\n")
rf_grid <- expand.grid(
  mtry = unique(pmax(1, round(c(sqrt(ncol(train_x)), ncol(train_x) * 0.15, ncol(train_x) * 0.25)))),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)
rf_grid$mtry <- pmin(rf_grid$mtry, ncol(train_x))
rf_grid <- unique(rf_grid)

set.seed(42)
model_rf <- train(
  x = train_x,
  y = train_y,
  method = "ranger",
  trControl = ctrl,
  metric = "ROC",
  tuneGrid = rf_grid,
  importance = "impurity",
  num.trees = 400
)

cat("Training XGBoost...\n")
xgb_grid <- expand.grid(
  nrounds = c(150, 250),
  max_depth = c(4, 6),
  eta = c(0.05, 0.1),
  gamma = c(0, 1),
  colsample_bytree = c(0.7, 0.9),
  min_child_weight = c(1, 5),
  subsample = c(0.8, 1.0)
)
set.seed(42)
xgb_grid <- xgb_grid[sample(seq_len(nrow(xgb_grid)), min(24, nrow(xgb_grid))), , drop = FALSE]

set.seed(42)
model_xgb <- train(
  x = train_x,
  y = train_y,
  method = "xgbTree",
  trControl = ctrl,
  metric = "ROC",
  tuneGrid = xgb_grid,
  verbose = FALSE
)


evaluate_model <- function(model, x_test, y_test, model_name, positive_class = "High") {
  pred_class <- predict(model, x_test)
  pred_prob  <- predict(model, x_test, type = "prob")[, positive_class]
  
  pred_class <- factor(pred_class, levels = levels(y_test))
  cm <- confusionMatrix(pred_class, y_test, positive = positive_class)
  roc_obj <- roc(response = y_test, predictor = pred_prob, levels = rev(levels(y_test)), quiet = TRUE)
  
  metrics <- data.frame(
    Model = model_name,
    Accuracy = as.numeric(cm$overall["Accuracy"]),
    Kappa = as.numeric(cm$overall["Kappa"]),
    Precision = as.numeric(cm$byClass["Pos Pred Value"]),
    Recall = as.numeric(cm$byClass["Sensitivity"]),
    F1 = as.numeric(cm$byClass["F1"]),
    AUC = as.numeric(auc(roc_obj)),
    stringsAsFactors = FALSE
  )
  
  list(metrics = metrics, cm = cm, roc = roc_obj, pred = pred_class, prob = pred_prob)
}


res_glm <- evaluate_model(model_glm, test_x, test_y, "Logistic Regression")
res_knn <- evaluate_model(model_knn, test_knn_x, test_y, "KNN")
res_rf  <- evaluate_model(model_rf,  test_x, test_y, "Random Forest")
res_xgb <- evaluate_model(model_xgb, test_x, test_y, "XGBoost")

results_df <- rbind(
  res_glm$metrics,
  res_knn$metrics,
  res_rf$metrics,
  res_xgb$metrics
)
results_df <- results_df[order(-results_df$AUC, -results_df$Accuracy), ]
row.names(results_df) <- NULL

print(results_df)
write.csv(results_df, "model_comparison_results.csv", row.names = FALSE)

capture.output(res_glm$cm, file = "confusion_glm.txt")
capture.output(res_knn$cm, file = "confusion_knn.txt")
capture.output(res_rf$cm,  file = "confusion_rf.txt")
capture.output(res_xgb$cm, file = "confusion_xgb.txt")


png("model_comparison_base.png", width = 1400, height = 900, res = 140)
metrics_mat <- t(as.matrix(results_df[, c("Accuracy", "Kappa", "Precision", "Recall", "F1", "AUC")]))
barplot(
  metrics_mat,
  beside = TRUE,
  legend.text = rownames(metrics_mat),
  args.legend = list(x = "topright", bty = "n", inset = 0.02),
  las = 2,
  main = "Model Comparison on Test Set",
  ylab = "Score",
  cex.names = 0.9
)
dev.off()

# 11.2 ROC curves
png("roc_curves_base.png", width = 1200, height = 900, res = 140)
plot(res_glm$roc, main = "ROC Curves", lwd = 2)
plot(res_knn$roc, add = TRUE, lwd = 2)
plot(res_rf$roc,  add = TRUE, lwd = 2)
plot(res_xgb$roc, add = TRUE, lwd = 2)
abline(a = 0, b = 1, lty = 2)
legend(
  "bottomright",
  legend = c(
    paste0("Logistic Regression (AUC=", round(res_glm$metrics$AUC, 3), ")"),
    paste0("KNN (AUC=", round(res_knn$metrics$AUC, 3), ")"),
    paste0("Random Forest (AUC=", round(res_rf$metrics$AUC, 3), ")"),
    paste0("XGBoost (AUC=", round(res_xgb$metrics$AUC, 3), ")")
  ),
  lwd = 2,
  bty = "n"
)
dev.off()

rf_imp <- varImp(model_rf)$importance
rf_imp$Feature <- rownames(rf_imp)
rf_imp <- rf_imp[order(-rf_imp$Overall), , drop = FALSE]
rf_imp_top <- head(rf_imp, 15)
write.csv(rf_imp_top, "rf_importance.csv", row.names = FALSE)

png("rf_importance_base.png", width = 1200, height = 900, res = 140)
par(mar = c(5, 12, 4, 2))
barplot(
  rev(rf_imp_top$Overall),
  names.arg = rev(rf_imp_top$Feature),
  horiz = TRUE,
  las = 1,
  main = "Top 15 Random Forest Features",
  xlab = "Importance",
  cex.names = 0.85
)
dev.off()

xgb_imp <- varImp(model_xgb)$importance
xgb_imp$Feature <- rownames(xgb_imp)
xgb_imp <- xgb_imp[order(-xgb_imp$Overall), , drop = FALSE]
xgb_imp_top <- head(xgb_imp, 15)
write.csv(xgb_imp_top, "xgb_importance.csv", row.names = FALSE)

png("xgb_importance_base.png", width = 1200, height = 900, res = 140)
par(mar = c(5, 12, 4, 2))
barplot(
  rev(xgb_imp_top$Overall),
  names.arg = rev(xgb_imp_top$Feature),
  horiz = TRUE,
  las = 1,
  main = "Top 15 XGBoost Features",
  xlab = "Importance",
  cex.names = 0.85
)
dev.off()
par(mar = c(5, 4, 4, 2) + 0.1)


saveRDS(model_glm, "model_glm.rds")
saveRDS(model_knn, "model_knn.rds")
saveRDS(model_rf,  "model_rf.rds")
saveRDS(model_xgb, "model_xgb.rds")

cat("\nBest model by AUC:\n")
print(results_df[1, ])
cat("\nAll outputs saved in working directory.\n")
