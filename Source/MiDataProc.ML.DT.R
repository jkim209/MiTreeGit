library(rpart)
library(rpart.plot)
library(caret)
library(stringr)
library(tidyverse)
library(ggplot2)

source("Source/MiDataProc.ML.Models.R")

dt.cla <- function(data, sam.dat.na, y.name, split.method = c("gini", "info"), minsplit = 20, minbucket = round(minsplit/3), nfold, name, p = 0.75){
  dt.list <- list()
  X <- data[[name]]
  y <- as.factor(sam.dat.na[[y.name]])
  X <- remove.symb(X)
  
  set.seed(578)
  tr.ind <- y %>% createDataPartition(p = p, list = FALSE)

  # Train / Test Split
  train_X = X[tr.ind,]
  train_Y = y[tr.ind]

  test_X = X[-tr.ind,]
  test_Y = y[-tr.ind]
  
  # Modeling Method
  if(split.method == "gini"){
    split <- "gini"
  }
  else{ # Information gain
    split <- "information"
  }
  
  # CV to get the optimal complex parameter
  new.dat <- cbind(train_X, train_Y)
  colnames(new.dat)[dim(new.dat)[2]] <- y.name
  new.dat[[y.name]] <- as.factor(new.dat[[y.name]])
  
  f1 <<- as.formula(paste(y.name, "~", ".", sep=" "))
  set.seed(578)
  control <- rpart.control(minsplit = minsplit, minbucket = minbucket, cp = 1e-5, xval = nfold)
  fit <- rpart(f1, data = new.dat, method = "class", control = control, parms = list(split = split))
  
  if(identical(class(fit$cptable[-1,]), c("matrix", "array"))){
    cp <- fit$cptable[-1,][which.min(fit$cptable[-1,][,"xerror"]), "CP"]
  }
  else{
    cp <- fit$cptable[-1,][[1]]
  }
  
  # Model with train data
  set.seed(578)
  best.fit <- prune(fit, cp = cp)
  
  # Model with the whole data
  new.dat <- cbind(X, y)
  colnames(new.dat)[dim(new.dat)[2]] <- y.name
  new.dat[[y.name]] <- as.factor(new.dat[[y.name]])
  
  set.seed(578)
  control <- rpart.control(minsplit = minsplit, minbucket = minbucket, cp = 1e-5, xval = nfold)
  final.best.fit <- rpart(f1, data = new.dat, method = "class", control = control, parms = list(split = split))
  final.best.fit <- prune(final.best.fit, cp = cp)
  
  dt.list[["model"]] <- fit
  dt.list[["best.tuned"]] <- cp
  dt.list[["final.model"]] <- best.fit
  dt.list[["final.model.whole.data"]] <- final.best.fit
  dt.list[["data"]] <- list(x = X, y = y)
  dt.list[["train"]] <- list(x = train_X, y = train_Y)
  if(p != 1) dt.list[["test"]] <- list(x = test_X, y = test_Y)
  return(dt.list)
}

dt.reg <- function(data, sam.dat.na, y.name, minsplit = 20, minbucket = round(minsplit/3), nfold, name, p = 0.75){
  dt.list <- list()
  X <- data[[name]]
  y <- as.numeric(sam.dat.na[[y.name]])
  X <- remove.symb(X)
  
  set.seed(578)
  tr.ind <- y %>% createDataPartition(p = p, list = FALSE)

  # Train / Test Split
  train_X = X[tr.ind,]
  train_Y = y[tr.ind]

  test_X = X[-tr.ind,]
  test_Y = y[-tr.ind]
  
  # CV to get the optimal complex parameter
  new.dat <- cbind(train_X, train_Y)
  colnames(new.dat)[dim(new.dat)[2]] <- y.name
  
  f1 <<- as.formula(paste(y.name, "~", ".", sep=" "))
  set.seed(578)
  control <- rpart.control(minsplit = minsplit, minbucket = minbucket, cp = 1e-5, xval = nfold)
  fit <- rpart(f1, data = new.dat, method = "anova", control = control)
  if(identical(class(fit$cptable[-1,]), c("matrix", "array"))){
    cp <- fit$cptable[-1,][which.min(fit$cptable[-1,][,"xerror"]), "CP"]
  }
  else{
    cp <- fit$cptable[-1,][[1]]
  }
  
  # Model with train data
  best.fit <- prune(fit, cp = cp)
  
  # Model with the whole data
  new.dat <- cbind(X, y)
  colnames(new.dat)[dim(new.dat)[2]] <- y.name
  
  set.seed(578)
  control <- rpart.control(minsplit = minsplit, minbucket = minbucket, cp = 1e-5, xval = nfold)
  final.best.fit <- rpart(f1, data = new.dat, method = "anova", control = control)
  final.best.fit <- prune(final.best.fit, cp = cp)
  
  dt.list[["model"]] <- fit
  dt.list[["best.tuned"]] <- cp
  dt.list[["final.model"]] <- best.fit
  dt.list[["final.model.whole.data"]] <- final.best.fit
  dt.list[["data"]] <- list(x = X, y = y)
  dt.list[["train"]] <- list(x = train_X, y = train_Y)
  if(p != 1) dt.list[["test"]] <- list(x = test_X, y = test_Y)
  return(dt.list)
}

dt.plotcp <- function(dt.list, name, minline = TRUE, lty = 3, col = 1, upper = c("size","splits", "none"), ...){
  x <- dt.list[[name]]$model
  dots <- list(...)
  if (!inherits(x, "rpart")) 
    stop("Not a legitimate \"rpart\" object")
  upper <- match.arg(upper)
  p.rpart <- x$cptable
  if (ncol(p.rpart) < 5L) 
    stop("'cptable' does not contain cross-validation results")
  xstd <- p.rpart[, 5L]
  xerror <- p.rpart[, 4L]
  nsplit <- p.rpart[, 2L]
  ns <- seq_along(nsplit)
  cp0 <- p.rpart[, 1L]
  cp <- sqrt(cp0 * c(Inf, cp0[-length(cp0)]))
  if (!"ylim" %in% names(dots)) 
    dots$ylim <- c(min(xerror - xstd) - 0.1, max(xerror + 
                                                   xstd) + 0.1)
  do.call(plot, c(list(ns, xerror, axes = FALSE, xlab = "Complexity Parameter", 
                       ylab = "CV Error", type = "o"), dots))
  box()
  axis(2, ...)
  segments(ns, xerror - xstd, ns, xerror + xstd)
  axis(1L, at = ns, labels = as.character(signif(cp, 2L)), 
       ...)
  switch(upper, size = {
    axis(3L, at = ns, labels = as.character(nsplit + 1), 
         ...)
    mtext("# Leaves", side = 3, line = 3)
  }, splits = {
    axis(3L, at = ns, labels = as.character(nsplit), ...)
    mtext("Numbe of Splits", side = 3, line = 3)
  })
  minpos <- min(seq_along(xerror)[xerror == min(xerror)])
  if (minline) 
    abline(h = (xerror + xstd)[minpos], lty = lty, col = col)
  invisible()
}

dt.pruned <- function(dt.list, name){
  plotcp(dt.list[[name]]$model, minline = TRUE)
}

dt.fancy.plot <- function(dt.list, name, type = "cla"){
  if(type == "cla"){
    rpart.plot(dt.list[[name]]$final.model.whole.data,
               type = 4,
               roundint = FALSE, 
               extra = 104, 
               under = TRUE, 
               fallen.leaves = TRUE,
               digits = 3,
               faclen = 3,
               cex = 0.85,
               tweak = 1.45,
               clip.right.labs = FALSE,
               box.palette = "BuOr")
  }
  else if(type == "reg"){
    rpart.plot(dt.list[[name]]$final.model.whole.data,
               type = 4,
               extra = 100, 
               under = TRUE, 
               fallen.leaves = TRUE,
               digits = 3,
               faclen = 3,
               cex = 0.85,
               tweak = 1.45,
               clip.right.labs = FALSE,
               box.palette = "Blue")
  }
  else if(type == "mult"){
    rpart.plot(dt.list[[name]]$final.model.whole.data,
               type = 4,
               roundint = FALSE, 
               extra = 104, 
               under = TRUE, 
               fallen.leaves = TRUE,
               digits = 3,
               faclen = 3,
               cex = 0.85,
               tweak = 1.45,
               clip.right.labs = FALSE)
  }
  
}

dt.used.var <- function(dt.list, colnames.list, rank.name){
  var.used.list <- dt.list[[rank.name]]$final.model.whole.data$frame
  var.used.ind <- which(var.used.list$var != "<leaf>")
  var <- unique(var.used.list[var.used.ind,]$var)
  nm <- colnames.df(colnames.list, rank.name)
  ind <- integer()
  for(v in var){
    ind <- c(ind, which(v == rownames(nm)))
  }
  new.ind <- sort(ind)
  nm <- nm[new.ind,, drop = FALSE]
  return(nm)
}

dt.summary.table <- function(dt.list, name, type = "cla", y.var){
  dt.frame <- dt.list[[name]]$final.model.whole.data$frame
  leaf.dt.frame <- dt.frame[which(dt.frame$var == "<leaf>"),]
  
  nrj <- leaf.dt.frame$n
  n <- length(nrj)
  
  if(type == "cla"){
    overally <- round((rep(length(which(y.var == names(table(y.var))[1]))/length(y.var), length(nrj)))*100, digits = 3)
    suby <- numeric()
    for(i in 1:nrow(leaf.dt.frame$yval2)){
      suby[i] <- sum(leaf.dt.frame$yval2[,c(4,5)][i,] * c(0, 1))
    }
    suby <- round(suby*100, digits = 3)
    diff <- round(suby - overally, digits = 3)
    outcome <- as.data.frame(rbind(nrj, diff))
    colnames(outcome) <- sprintf("L%s",seq(1:n))
    rownames(outcome) <- c("# Units", "Predicted Output (Leaf) - Predicted Output (Overall)")
    return(outcome)
  }
  else if(type == "reg"){
    overally <- round((rep(mean(y.var), n)), digits = 3)
    suby <- round(leaf.dt.frame$yval, digits = 3)
    diff <- round(suby - overally, digits = 3)
    outcome <- as.data.frame(rbind(nrj, diff))
    colnames(outcome) <- sprintf("L%s",seq(1:n))
    rownames(outcome) <- c("# Units", "Predicted Output (Leaf) - Predicted Output (Overall)")
    return(outcome)
  }
}

dt.importance.df <- function(dt.list, name){
  df <- as.data.frame(dt.list[[name]]$final.model.whole.data$variable.importance)
  colnames(df) <- "Importance"
  return(df)
}

dt.imp.plot <- function(dt.list, rank.name){
  sorted.imp.var <- sort(dt.list[[rank.name]]$final.model.whole.data$variable.importance)
  par(mar=c(5,15,6,3))
  if(length(sorted.imp.var) >= 10){
    barplot(sorted.imp.var[1:10], horiz = TRUE, beside = FALSE, las = 2, space = 0.5, main = sprintf("Coefficients (%s)", str_to_title(rank.name)),
            col = ifelse(sorted.imp.var[1:10] > 0, "lightblue", "pink2"), xlab = "Coefficients", width = 2)
  } else{
    barplot(sorted.imp.var, horiz = TRUE, beside = FALSE, las = 2, space = 0.5, main = sprintf("Variable Importance (%s)", str_to_title(rank.name)),
            col = ifelse(sorted.imp.var > 0, "lightblue", "pink2"), xlab = "Variable Importance", width = 2)
  }
}

dt.used.var.names <- function(dt.list, rank.name, colnames.list){
  tax.name <- dt.used.var(dt.list, colnames.list, rank.name)[,1]
  if(length(tax.name) > 10) tax.name <- tax.name[1:10]
  return(tax.name)
}
