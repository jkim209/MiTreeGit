# MiTreeGit

Title: MiTree: A unified web cloud analytic platform for user-friendly and interpretable microbiome data mining using tree-based methods


Version: 1.0.1

Maintainer: Jihun Kim <toujours209@gmail.com>

Description: MiTree is a unified web cloud analytic platform for user-friendly and interpretable microbiome data mining using tree-based methods. Machine learning is a promising approach to help such an effort especially due to the high complexity of microbiome data. However, many of the current machine learning algorithms are in a “black box”. They are hard to understand and interpret. Clinicians, public health practitioners or biologists are not also usually skilled at computer programming, and they do not always have a high-end computing device. MiTree employs tree-based learning methods, including 1) decision tree, 2) random forest and 3) gradient boosting, that are both well understood and suited to human microbiome studies, for both classification and regression problems through covariate-adjusted or unadjusted analysis. 

NeedsCompilation: No

Depends: R(≥ 4.1.0)

Imports: Bioconductor ('BiocManager', 'biomformat', 'phyloseq'); CRAN ('ape', 'bio2mds', 'caret', 'chatgpt', 'compositions', 'data.table', 'DT', 'ecodist', 'fontawesome', 'fossil', 'ggplot2', 'ggplotify', 'googleVis', 'grid', 'htmltools', 'phangorn', 'picante', 'plotly', 'randomForest', 'ROCR', 'rpart', 'rpart.plot', 'seqinr', 'SHAPforxgboost', 'shiny', 'shinydashboard', 'shinyjs', 'shinyWidgets', 'stringr', 'tidyverse', 'xgboost', 'xtable', 'zCompositions'**(1.4.0.1)**, 'zip'); GitHub: ('chatgpt', 'dashboardthemes, 'edarf')

License: GPL 1, GPL 2 

## URLs

* Web application (online implementation): http://mitree.micloud.kr
* GitHub repository (local implementation): https://github.com/jkim209/MiTreeGit

## References

* Kim J, Koh H. MiTree: A unified web cloud analytic platform for user-friendly and interpretable microbiome data mining using tree-based methods. Microorganisms. 2023;11(2816):1-14.


## Prerequites

#### Notice: For the local implementation, you do not need to install all the pre-requite R packages individually. You only need to install the 'shiny' package, and then run a simple command in 'Launch App' below. Then, all the pre-requisite R packages will be installed and imported automatically. For MAC users, please make sure that you have to dowload and install xQuartz before the local implementation. (https://www.xquartz.org/)


shiny
```
install.packages('shiny')
```

## Launch App

```
library(shiny)

runGitHub('MiTreeGit', 'jkim209', ref = 'main')
```

## Troubleshooting Tips

If you have any problems for using MiTree, please report in issues (https://github.com/jkim209/MiTreeGit/issues) or email Jihun Kim (jihun.kim.3@stonybrook.edu).
