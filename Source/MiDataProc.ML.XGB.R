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

source("Source/MiDataProc.ML.Models.R")

# Extreme Gradient Boosting ----------------------

xgb.cla <- function(data, sam.dat.na, y.name, eta = 0.001, nrounds = 500, nfold = c(5, 10), alpha = 0, lambda = 1, stratified = TRUE,
                    loss.func = c("error", "auc", "logloss"), name){
  xgb.list <- list()  
  X = as.matrix(data[[name]])
  y = sam.dat.na[[y.name]]
  cat.names <- category.names(sam.dat.na, y.name)
  data.dmatrix <- xgb.DMatrix(data = X, label = y)
  
  ind <- 1
  xgb.cv.list <- list()
  output <- data.frame()
  
  for(currentMaxDepth in seq(4, 10, 2)){
    for(currentMCW in seq(2, 6, 2)){
      for(currentCbT in seq(0.5, 0.75, 1)){
        for(currentGamma in seq(0, 0.6, 0.2)){
          set.seed(578)
          xgboostModelCV <- xgb.cv(data =  data.dmatrix, nrounds = nrounds, nfold = nfold, showsd = TRUE, stratified = stratified, tree_method = "exact",
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
  
  set.seed(578)
  xgboostFit <- xgb.train(data = data.dmatrix, nrounds = opt.tree, objective = "binary:logistic", tree_method = "exact", verbose = TRUE, eval_metric = loss.func,
                          max_depth = max_depth, colsample_bytree = colsample_bytree, eta = eta, min_child_weight = min_child_weight, print_every_n = 10,
                          booster = "gbtree", gamma = gamma, alpha = alpha, lambda = lambda, nthread = 1)
  
  xgb.list[["model"]] <- xgboostFit
  xgb.list[["cv.model"]] <- cv.model
  xgb.list[["loss"]] <- loss.func
  xgb.list[["tuning.result"]] <- output
  xgb.list[["data"]] <- list(x = X, y = y)
  return(xgb.list)
}

xgb.reg <- function(data, sam.dat.na, y.name, eta = 0.001, nrounds = 500, nfold = c(5, 10), alpha = 0, lambda = 1,
                    loss.func = c("huber", "rss"), name){
  xgb.list <- list()
  X = as.matrix(data[[name]])
  y = sam.dat.na[[y.name]]
  data.dmatrix <- xgb.DMatrix(data = X, label = y)
  
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
          xgboostModelCV <- xgb.cv(data = data.dmatrix, nrounds = nrounds, nfold = nfold, showsd = TRUE, tree_method = "exact",
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
  
  set.seed(578)
  xgboostFit <- xgb.train(data = data.dmatrix, nrounds = opt.tree, objective = loss, tree_method = "exact", verbose = TRUE, eval_metric = eval.metric,
                          max_depth = max_depth, colsample_bytree = colsample_bytree, eta = eta, min_child_weight = min_child_weight, print_every_n = 10,
                          booster = "gbtree", gamma = gamma, alpha = alpha, lambda = lambda, nthread = 1)
  
  xgb.list[["model"]] <- xgboostFit
  xgb.list[["cv.model"]] <- cv.model
  xgb.list[["loss"]] <- loss.func
  xgb.list[["tuning.result"]] <- output
  xgb.list[["data"]] <- list(x = X, y = y)
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
    # geom_point(alpha = 0.2) +
    geom_line(linetype = "solid", linewidth = 1.2) +
    # geom_smooth(alpha = 0.4, se = FALSE) +
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

xgb.imp.list <- function(xgb.list, level.names){
  imp.list <- list()
  for(name in level.names){
    imp.list[[name]] <- xgb.importance(model = xgb.list[[name]]$model)
  }
  return(imp.list)
}

xgb.imp.list.ma <- function(xgb.list, level.names, data){
  xgb.importance <- xgb.imp.list(xgb.list, level.names)
  
  d <- subset(as.data.frame(xgb.importance[[level.names]]), select = c("Feature", "Gain"))
  ma <- colMeans(data[[level.names]])
  d$MeanAbundance <- rep(NA, nrow(d))
  
  for(v in names(ma)){
    if(v %in% d$Feature){
      ind <- which(v == d$Feature)
      d$MeanAbundance[ind] <- ma[[v]]
    }
  }
  return(d)
}

xgb.imp.plot <- function(xgb.list, level.names, data, data.type, n = 30, is.cat = TRUE){
  imp.df <- xgb.imp.list.ma(xgb.list, level.names, data)
  
  if(data.type == "clr"){
    data.type <- "CLR"
  }
  else if(data.type == "prop"){
    data.type <- "Proportion"
  }
  else if(data.type == "rare.count"){
    data.type <- "Rarefied Count"
  }
  else if(data.type == "arcsin"){
    data.type <- "Arcsine-Root"
  }
  
  yl <- ifelse(is.cat, "Decrease in Gini Impurity", "Decrease in Mean Squared Error")
  
  imp.df <- imp.df %>% 
    arrange(desc(Gain)) %>%
    top_n(n, Gain)
  
  imp.df <- imp.df %>%
    ggplot(aes(x=reorder(Feature, Gain), y=Gain)) +
    geom_segment( aes(x=reorder(Feature, Gain), xend=reorder(Feature, Gain), y=0, yend=Gain), color="grey") +
    geom_point(aes(color = MeanAbundance), size=5, alpha=0.8) +
    scale_color_gradient(low = "blue", high = "red", name = sprintf("Mean\nAbundance\n(%s)", data.type)) +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 13),
      axis.title.x = element_text(size = 13)
    ) +
    xlab(element_blank()) +
    ylab(yl)

  imp.df
}

xgb.shap.summary <- function(xgb.list, rank.name, n = 20){
  X <- data.matrix(xgb.list[[rank.name]]$data$x)
  shap <- shap.prep(xgb.list[[rank.name]]$model, X_train = X, top_n = n)
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

xgb.shap.prep <- function(xgb.list, rank.name, n = 20){
  X <- data.matrix(xgb.list[[rank.name]]$data$x)
  shap <- shap.prep(xgb.list[[rank.name]]$model, X_train = X, top_n = n)
  return(shap)
}

xgb.shap.dependence <- function(xgb.list, rank.name, n = 20){
  shap <- xgb.shap.prep(xgb.list, rank.name, n = n)
  
  plot.list <- list()
  for (v in shap.importance(shap, names_only = TRUE)) {
    p <- shap.plot.dependence(shap, x = v, color_feature = v, alpha = 0.8) +
      labs(color = "") +
      theme(
        plot.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = grid::unit(c(0, 0.75, 0.2, 0.75), unit = "cm")
      ) +
      ggtitle(v)
    plot.list[[v]] <- p
  }
  grid.arrange(grobs = plot.list, nrow = 5, left = textGrob("SHAP", rot = 90), bottom = textGrob("Feature Value", rot = 0), as.table = FALSE)
}

xgb.prediction <- function(xgb.list, level.names, is.cat = TRUE){
  train <- numeric()
  test <- numeric()
  OT <- numeric()
  Depth <- numeric()
  mcw <- numeric()
  cbt <- numeric()
  gamma <- numeric()
  
  if(is.cat){
    for(name in level.names){
      train <- c(train, mean(ifelse(predict(xgb.list[[name]]$model, xgb.list[[name]]$train$x, type = "prob") > 0.5, 1, 0) == xgb.list[[name]]$train$y))
      test <- c(test, mean(ifelse(predict(xgb.list[[name]]$model, xgb.list[[name]]$test$x, type = "prob") > 0.5, 1, 0) == xgb.list[[name]]$test$y))
      OT <- c(OT, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[1]])
      Depth <- c(Depth, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[4]])
      mcw <- c(mcw, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[5]])
      cbt <- c(cbt, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[6]])
      gamma <- c(gamma, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[7]])
    }
  }
  else{
    for(name in level.names){
      train <- c(train, sqrt(mean((predict(xgb.list[[name]]$model, xgb.list[[name]]$train$x) - xgb.list[[name]]$train$y)^2)))
      test <- c(test, sqrt(mean((predict(xgb.list[[name]]$model, xgb.list[[name]]$test$x) - xgb.list[[name]]$test$y)^2)))
      OT <- c(OT, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[1]])
      Depth <- c(Depth, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[4]])
      mcw <- c(mcw, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[5]])
      cbt <- c(cbt, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[6]])
      gamma <- c(gamma, xgb.list[[name]]$tuning.result[which.min(xgb.list[[name]]$tuning.result$Test),][[7]])
    }
  }
  
  
  outcome <- data.frame(Train_Error = train, Test_Error = test, Optimal_Tree = OT, Maximum_Depth = Depth, Min_child_weight = mcw,
                        Colsample_bytree = cbt, Gamma = gamma)
  rownames(outcome) <- str_to_title(level.names)
  return(outcome)
}

xgb.imp.var <- function(xgb.importance, rank.name, n = 10){
  imp.var <- (xgb.importance[[rank.name]] %>% top_n(n, Gain))$Feature
  return(imp.var)
}
