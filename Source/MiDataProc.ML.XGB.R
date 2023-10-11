library(xgboost)
library(SHAPforxgboost)
library(caret)
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(gridExtra)
library(grid)
library(ggplotify)
library(reshape2)

source("Source/MiDataProc.ML.Models.R")

# Extreme Gradient Boosting ----------------------

xgb.cla <- function(data, sam.dat.na, y.name, eta = 0.001, nrounds = 500, nfold = c(5, 10), alpha = 0, lambda = 1, stratified = TRUE,
                    loss.func = c("error", "auc", "logloss"), name, p = 0.8){
  xgb.list <- list()  
  X = as.matrix(data[[name]])
  y = sam.dat.na[[y.name]]
  cat.names <- category.names(sam.dat.na, y.name)
  
  set.seed(578)
  tr.ind <- y %>% createDataPartition(p = p, list = FALSE)
  train_X <- X[tr.ind,]
  train_Y <- y[tr.ind]
  
  test_X <- X[-tr.ind,]
  test_Y <- y[-tr.ind]
  
  dtrain <- xgb.DMatrix(data = train_X, label = train_Y)
  dtest <- xgb.DMatrix(data = test_X, label = test_Y)
  dwhole <- xgb.DMatrix(data = X, label = y)
  
  ind <- 1
  xgb.cv.list <- list()
  output <- data.frame()
  
  # CV to find the optimal parameters
  for(currentMaxDepth in seq(4, 10, 2)){
    for(currentMCW in seq(2, 6, 2)){
      for(currentCbT in seq(0.5, 0.75, 1)){
        for(currentGamma in seq(0, 0.6, 0.2)){
          set.seed(578)
          xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = nrounds, nfold = nfold, showsd = TRUE, stratified = stratified, tree_method = "exact",
                                   metrics = loss.func, verbose = TRUE, objective = "binary:logistic", max_depth = currentMaxDepth, colsample_bytree = currentCbT,
                                   min_child_weight = currentMCW ,eta = eta, print_every_n = 10, booster = "gbtree", gamma = currentGamma, alpha = alpha, lambda = lambda,
                                   early_stopping_rounds = 150, nthread = 1)
          xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
          niter <- xgboostModelCV$best_iteration
          test <- as.numeric(tail(xvalidationScores[,4], 1))
          train <- as.numeric(tail(xvalidationScores[,2],1))

          xgb.cv.list[[ind]] <- xgboostModelCV
          ind <- ind + 1
          output <- rbind(output, c(niter, train, test, currentMaxDepth, currentMCW, currentCbT, currentGamma))
        }
      }
    }
  }
  varnames <- c("Optimal_trees", "Train", "Test", "max_depth", "min_child_weight", "colsample_bytree", "gamma")
  names(output) <- varnames
  
  if(loss.func == "auc"){
    cv.ind <- which.max(output$Test)
    cv.model <- xgb.cv.list[[cv.ind]]
    opt.tree <- output[cv.ind,][[1]]
    max_depth <- output[cv.ind,][[4]]
    min_child_weight <- output[cv.ind,][[5]]
    colsample_bytree <- output[cv.ind,][[6]]
    gamma <- output[cv.ind,][[7]]
  }
  else{
    cv.ind <- which.min(output$Test)
    cv.model <- xgb.cv.list[[cv.ind]]
    opt.tree <- output[cv.ind,][[1]]
    max_depth <- output[cv.ind,][[4]]
    min_child_weight <- output[cv.ind,][[5]]
    colsample_bytree <- output[cv.ind,][[6]]
    gamma <- output[cv.ind,][[7]]
  }
  
  # Model with train data
  set.seed(578)
  xgboostFit <- xgb.train(data = dtrain, nrounds = opt.tree, objective = "binary:logistic", tree_method = "exact", verbose = TRUE, eval_metric = loss.func,
                          max_depth = max_depth, colsample_bytree = colsample_bytree, eta = eta, min_child_weight = min_child_weight, print_every_n = 10,
                          booster = "gbtree", gamma = gamma, alpha = alpha, lambda = lambda, nthread = 1)
  
  # Model with the whole data
  set.seed(578)
  final.xgboostFit <- xgb.train(data = dwhole, nrounds = opt.tree, objective = "binary:logistic", tree_method = "exact", verbose = TRUE, eval_metric = loss.func,
                          max_depth = max_depth, colsample_bytree = colsample_bytree, eta = eta, min_child_weight = min_child_weight, print_every_n = 10,
                          booster = "gbtree", gamma = gamma, alpha = alpha, lambda = lambda, nthread = 1)
  
  xgb.list[["model"]] <- xgboostFit              # Model with test set
  xgb.list[["final.model"]] <- final.xgboostFit  # Model with whole data set
  xgb.list[["cv.model"]] <- cv.model
  xgb.list[["loss"]] <- loss.func
  xgb.list[["tuning.result"]] <- output
  xgb.list[["train"]] <- dtrain
  xgb.list[["test"]] <- dtest
  xgb.list[["data"]] <- dwhole
  xgb.list[["train.ind"]] <- list(x = train_X, y = train_Y)
  xgb.list[["test.ind"]] <- list(x = test_X, y = test_Y)
  xgb.list[["data.ind"]] <- list(x = X, y = y)
  return(xgb.list)
}

xgb.reg <- function(data, sam.dat.na, y.name, eta = 0.001, nrounds = 500, nfold = c(5, 10), alpha = 0, lambda = 1,
                    loss.func = c("huber", "rss"), name, p = 0.8){
  xgb.list <- list()
  X = as.matrix(data[[name]])
  y = sam.dat.na[[y.name]]
  
  set.seed(578)
  tr.ind <- y %>% createDataPartition(p = p, list = FALSE)
  train_X <- X[tr.ind,]
  train_Y <- y[tr.ind]
  
  test_X <- X[-tr.ind,]
  test_Y <- y[-tr.ind]
  
  dtrain <- xgb.DMatrix(data = train_X, label = train_Y)
  dtest <- xgb.DMatrix(data = test_X, label = test_Y)
  dwhole <- xgb.DMatrix(data = X, label = y)
  
  if(loss.func == "huber"){
    # loss <- "reg:pseudohubererror"
    loss <- "reg:squarederror"
    eval.metric <- "mphe"
  }
  else if(loss.func == "rss"){
    loss <- "reg:squarederror"
    eval.metric <- "rmse"
  }
  
  ind <- 1
  xgb.cv.list <- list()
  output <- data.frame()
  
  for(currentMaxDepth in seq(4, 10, 2)){
    for(currentMCW in seq(2, 6, 2)){
      for(currentCbT in seq(0.5, 0.75, 1)){
        for(currentGamma in seq(0, 0.6, 0.2)){
          set.seed(578)
          xgboostModelCV <- xgb.cv(data = dtrain, nrounds = nrounds, nfold = nfold, showsd = TRUE, tree_method = "exact",
                                   metrics = eval.metric, verbose = TRUE, objective = loss, max_depth = currentMaxDepth, colsample_bytree = currentCbT,
                                   min_child_weight = currentMCW ,eta = eta, print_every_n = 10, booster = "gbtree", gamma = currentGamma, alpha = alpha, lambda = lambda,
                                   early_stopping_rounds = 150, nthread = 1)
          
          xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
          niter <- xgboostModelCV$best_iteration
          test <- as.numeric(tail(xvalidationScores[,4], 1))
          train <- as.numeric(tail(xvalidationScores[,2],1))
          
          xgb.cv.list[[ind]] <- xgboostModelCV
          ind <- ind + 1
          output <- rbind(output, c(niter, train, test, currentMaxDepth, currentMCW, currentCbT, currentGamma))
        }
      }
    }
  }
  varnames <- c("Optimal_trees", "Train", "Test", "max_depth", "min_child_weight", "colsample_bytree", "gamma")
  names(output) <- varnames
  
  cv.ind <- which.min(output$Test)
  cv.model <- xgb.cv.list[[cv.ind]]
  opt.tree <- output[cv.ind,][[1]]
  max_depth <- output[cv.ind,][[4]]
  min_child_weight <- output[cv.ind,][[5]]
  colsample_bytree <- output[cv.ind,][[6]]
  gamma <- output[cv.ind,][[7]]
  
  # Model with train data
  set.seed(578)
  xgboostFit <- xgb.train(data = dtrain, nrounds = opt.tree, objective = loss, tree_method = "exact", verbose = TRUE, eval_metric = eval.metric,
                          max_depth = max_depth, colsample_bytree = colsample_bytree, eta = eta, min_child_weight = min_child_weight, print_every_n = 10,
                          booster = "gbtree", gamma = gamma, alpha = alpha, lambda = lambda, nthread = 1)
  
  # Model with the whole data
  set.seed(578)
  final.xgboostFit <- xgb.train(data = dwhole, nrounds = opt.tree, objective = loss, tree_method = "exact", verbose = TRUE, eval_metric = eval.metric,
                          max_depth = max_depth, colsample_bytree = colsample_bytree, eta = eta, min_child_weight = min_child_weight, print_every_n = 10,
                          booster = "gbtree", gamma = gamma, alpha = alpha, lambda = lambda, nthread = 1)
  
  xgb.list[["model"]] <- xgboostFit              # Model with test set
  xgb.list[["final.model"]] <- final.xgboostFit  # Model with whole data set
  xgb.list[["cv.model"]] <- cv.model
  xgb.list[["loss"]] <- loss.func
  xgb.list[["tuning.result"]] <- output
  xgb.list[["train"]] <- dtrain
  xgb.list[["test"]] <- dtest
  xgb.list[["data"]] <- dwhole
  xgb.list[["train.ind"]] <- list(x = train_X, y = train_Y)
  xgb.list[["test.ind"]] <- list(x = test_X, y = test_Y)
  xgb.list[["data.ind"]] <- list(x = X, y = y)
  return(xgb.list)
}

xgb.error.plot.2 <- function(xgb.list, rank.name){
  options(scipen = 999)
  eval.log <- xgb.list[[rank.name]][["cv.model"]]$evaluation_log
  std <- names(eval.log[,2]) %>% gsub("train_", "",.) %>% gsub("_mean", "",.)
  if(std == "logloss"){
    std <- "Cross entropy"
  }
  else if(std == "error"){
    std <- "Error rate"
  }
  else if(std == "auc"){
    std <- "AUC"
  }
  else if(std == "rmse"){
    std <- "Mean Squared Error"
    eval.log[,2] <- eval.log[,2]^2
    eval.log[,4] <- eval.log[,4]^2
  }
  else if(std == "mphe"){
    std <- "Mean Pseudo-Huber error"
  }
  data.frame(error = c(unlist(eval.log[,2]), unlist(eval.log[,4])),
             class = c(rep("Train Error", nrow(eval.log)),
                       rep("CV Error", nrow(eval.log))),
             nround = rep(1:nrow(eval.log), 2)) %>%
    ggplot(aes(nround, error, col = class)) +
    # geom_point(alpha = 0.5) +
    geom_line(linetype = "solid", linewidth = 1.2) +
    # geom_smooth(alpha = 0.1, se = FALSE, method = "gam") +
    theme_bw() +
    ggtitle("XGBoost Final Model",
            subtitle = sprintf("Level : %s  Iterations : %i", str_to_title(rank.name), dim(eval.log)[1])) +
    xlab("# Iterations") +
    ylab(std) +
    theme(axis.text = element_text(size = 11),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14, vjust = +2),
          plot.title = element_text(size = 17),
          plot.subtitle = element_text(size = 15),
          legend.title = element_blank())
}

xgb.shap.imp <- function(xgb.list, level.names, n = 20){
  imp.list <- list()
  for(name in level.names){
    X <- data.matrix(xgb.list[[name]]$data.ind$x)
    shap <- shap.prep(xgb.list[[name]]$final.model, X_train = X, top_n = n)
    imp.list[[name]] <- data.frame(shap)
  }
  return(imp.list)
}

xgb.shap.imp.var <- function(xgb.list, rank.name, colnames.list, n = 20){
  X <- data.matrix(xgb.list[[rank.name]]$data.ind$x)
  shap <- shap.prep(xgb.list[[rank.name]]$final.model, X_train = X, top_n = n)
  if(n > ncol(xgb.list[[rank.name]]$data.ind$x)){
    n <- ncol(xgb.list[[rank.name]]$data.ind$x)
  }
  var <- levels(shap$variable)[1:n]
  nm <- colnames.df(colnames.list, rank.name)
  ind <- integer()
  for(v in var){
    ind <- c(ind, which(v == rownames(nm)))
  }
  # new.ind <- sort(ind)
  nm <- nm[ind,, drop = FALSE]
  return(nm)
}

xgb.shap.summary <- function(xgb.list, rank.name, n = 20){
  X <- data.matrix(xgb.list[[rank.name]]$data.ind$x)
  shap <- shap.prep(xgb.list[[rank.name]]$final.model, X_train = X, top_n = n)
  shap.plot.summary(shap, scientific = FALSE) +
    theme(
      axis.line.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      legend.position = "bottom", 
      legend.title = element_text(size = 11), 
      legend.text = element_text(size = 9),
      axis.title.x = element_text(size = 13),
      axis.text.y = element_text(size = 13)
    )
}

xgb.imp.var <- function(xgb.importance, rank.name, n = 10){
  imp.var <- (xgb.importance[[rank.name]] %>% top_n(n, Gain))$Feature
  return(imp.var)
}

xgb.pdp.bin <- function(xgb.list, n, rank.name, data.type, cat.name){
  X <- xgb.list[[rank.name]]$data.ind$x
  fit <- xgb.list[[rank.name]]$final.model
  xgb.importance <- shap.prep(fit, X_train = X, top_n = n) %>% 
    group_by(variable) %>% 
    summarise(mean_value = mean(mean_value)) %>%
    data.frame
  n <- min(nrow(xgb.importance), n)
  feature <- xgb.importance$variable[1:n] %>% as.character
  result <- data.frame()
  
  if(data.type == "clr"){
    type = "CLR"
  }
  else if(data.type == "prop"){
    type = "Proportion"
  }
  else if(data.type == "rare.count"){
    type = "Rarefied Count"
  }
  else if(data.type == "arcsin"){
    type = "Arcsine-Root"
  }
  
  for(taxon.name in feature){
    val <- numeric()
    p1 <- numeric()
    p2 <- numeric()
    taxon.val <- seq(min(X[,taxon.name]), max(X[,taxon.name]),len = 100)
    
    for(i in 1:length(taxon.val)){
      newX <- X
      newX[,taxon.name] <- rep(taxon.val[i], nrow(newX))
      y_pred_prob <- predict(fit, newX) # Probability that y = 1, not 0.
      val <- c(val, taxon.val[i])
      p1 <- c(p1, mean(1-y_pred_prob))
      p2 <- c(p2, mean(y_pred_prob))
    }
    prob.result <- data.frame(val, p1, p2) %>%
      reshape2::melt(id.vars = "val", variable.name = "Category", value.name = "Prob")
    prob.result$title <- rep(taxon.name, nrow(prob.result))
    result <- rbind(result, prob.result)
  }
  result$title <- factor(result$title, levels = feature)
  
  p <- ggplot(result, aes(val, Prob)) + 
    geom_line(aes(color = Category), size = 0.8) +
    theme_light() +
    xlab(type) + 
    ylab("Predicted Value") +
    scale_color_discrete(name = "Category", labels = cat.name) +
    theme(
      axis.text.x = element_text(size = 7.5),
      axis.text.y = element_text(size = 7.5),
      strip.text = element_text(size=12),
      panel.spacing.x = unit(1, "lines"),
      legend.title = element_blank()
    ) +
    facet_wrap(~ title, scales = "free_x", nrow = 5, dir = "v")
  p
}

xgb.pdp.reg <- function(xgb.list, n, rank.name, data.type){
  X <- xgb.list[[rank.name]]$data.ind$x
  fit <- xgb.list[[rank.name]]$final.model
  xgb.importance <- shap.prep(fit, X_train = X, top_n = n) %>% 
    group_by(variable) %>% 
    summarise(mean_value = mean(mean_value)) %>%
    data.frame
  n <- min(nrow(xgb.importance), n)
  feature <- xgb.importance$variable[1:n] %>% as.character
  result <- data.frame()
  
  if(data.type == "clr"){
    type = "CLR"
  }
  else if(data.type == "prop"){
    type = "Proportion"
  }
  else if(data.type == "rare.count"){
    type = "Rarefied Count"
  }
  else if(data.type == "arcsin"){
    type = "Arcsine-Root"
  }
  
  for(taxon.name in feature){
    val <- numeric()
    y_hat <- numeric()
    taxon.val <- seq(min(X[,taxon.name]), max(X[,taxon.name]),len = 100)
    
    for(i in 1:length(taxon.val)){
      newX <- X
      newX[,taxon.name] <- rep(X[,taxon.name][i], nrow(newX))
      y_pred <- predict(fit, newX)
      val <- c(val, X[,taxon.name][i])
      y_hat <- c(y_hat, mean(y_pred))
    }
    predict.result <- data.frame(val, y_hat)
    predict.result$title <- rep(taxon.name, nrow(predict.result))
    result <- rbind(result, predict.result)
  }
  result$title <- factor(result$title, levels = feature)
  
  p <- ggplot(result, aes(val, y_hat)) + 
    geom_line() +
    theme_light() +
    xlab(type) + 
    ylab("Predicted Value") +
    theme(
      axis.text.x = element_text(size = 7.5),
      axis.text.y = element_text(size = 7.5),
      strip.text = element_text(size=11),
      panel.spacing.x = unit(1, "lines"),
      legend.title = element_blank()
    ) +
    facet_wrap(~ title, scales = "free_x", nrow = 5, dir = "v")
  p
}
