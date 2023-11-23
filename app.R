ls.pkg <- c('shiny', 'rmarkdown', 'seqinr', 'shinydashboard', 'tidyverse', 'plotly', 'shinyWidgets', 'shinyjs', 'googleVis', 'xtable',
            'DT', 'htmltools', 'phangorn', 'bios2mds', 'zip', 'ape', 'zCompositions', 'compositions', 'stringr', 'rpart', 'rpart.plot', 
            'caret', 'ggplot2', 'randomForest', 'data.table', 'xgboost', 'SHAPforxgboost', 'fontawesome', 'grid', 'ggplotify',
            'BiocManager', 'remotes', 'reshape2', 'fossil', 'ROCR', 'picante', 'ecodist')

new.pkg <- ls.pkg[!(ls.pkg %in% installed.packages()[,"Package"])]
if(length(new.pkg)) install.packages(new.pkg, repos = 'https://cloud.r-project.org/')

if(!require('phyloseq')) BiocManager::install('phyloseq')
if(!require('biomformat')) remotes::install_github('joey711/biomformat')
if(!require('dashboardthemes')) remotes::install_github('nik01010/dashboardthemes', force = TRUE)
if(!require('chatgpt')) remotes::install_github('jcrodriguez1989/chatgpt')
if(!require('edarf')) remotes::install_github('zmjones/edarf', subdir = 'pkg')

library(shiny)
library(BiocManager)
library(seqinr)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(googleVis)
library(xtable)
library(DT)
library(htmltools)
library(phangorn)
library(bios2mds)
library(zip)
library(ape)
# library(zCompositions)
library(compositions)
library(stringr)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(randomForest)
library(data.table)
library(xgboost)
library(SHAPforxgboost)
library(fontawesome)
library(grid)
library(ggplotify)
library(phyloseq)
library(biomformat)
library(dashboardthemes)
library(edarf)
library(chatgpt)
library(reshape2)

library(devtools)
if(packageVersion("zCompositions") != "1.4.0.1") install_version("zCompositions", version = "1.4.0.1", repos = "http://cran.us.r-project.org")
library(zCompositions)

source("Source/MiDataProc.Data.Upload.R")
source("Source/MiDataProc.Data.Input.R")
source("Source/MiDataProc.ML.Models.R")
source("Source/MiDataProc.ML.DT.R")
source("Source/MiDataProc.ML.RF.R")
source("Source/MiDataProc.ML.XGB.R")

# COMMENTS ------
{
  TITLE = p("MiTree: A Unified Web Cloud Analytic Platform for User-Friendly and Interpretable Microbiome Data Mining Using Tree-Based Methods", style = "font-size:18pt")
  HOME_COMMENT = p(strong("MiTree:", style = "font-size:15pt"), "A unified web cloud analytic platform for user-friendly and interpretable microbiome data mining.
                   MiTree employs tree-based learning methods, decision tree, random forest and gradient boosting, that are both well understood and suited to human microbiome studies.
                   We suggest that the random forest or gradient boosting to be used as a main analytic method because of their high prediction accuracy, while the decision tree to be used just for reference.
                   MiTree handles both classification and regression problems through covariate-adjusted or unadjusted analysis. The results from MiTree are also easy to understand and interpret with nice visualizations for important disease predictors and their delicate relationship patterns to the host's health or disease status.
                   It is also engaging that MiTree employs ChatGPT, a popular and well-trained AI language model, as a plug-in to help users easily search for the microbial taxa that are found as important disease predictors.
                   MiTree will provide new insights into microbiome-based diagnostics, treatment and prevention.", style = "font-size:13pt")
  HOME_COMMENT2 = p(strong("URLs:"), "Web server (online implementation):", tags$a(href = "http://mitree.micloud.kr", "http://mitree.micloud.kr"), 
                    "; GitHub repository (local implementation):", 
                    tags$a(href = "https://github.com/jkim209/MiTreeGit", "https://github.com/jkim209/MiTreeGit"), style = "font-size:13pt")
  HOME_COMMENT3 = p(strong("Maintainers:"), "Jihun Kim (", tags$a(href = "toujours209@gmail.com", "toujours209@gmail.com"), ")", style = "font-size:13pt")
  HOME_COMMENT4 = p(strong("Reference:"), "Kim J, Koh H. MiTree: A unified web cloud analytic platform for user-friendly and interpretable microbiome data mining using tree-based methods. Microorganisms. 2023;11(2816):1-14.", style = "font-size:13pt")
  
  INPUT_PHYLOSEQ_COMMENT1 = p("Description:", br(), br(), "This should be an '.Rdata' or '.rds' file, and the data should be in 'phyloseq' format (see ", 
                              htmltools::a(tags$u("https://bioconductor.org/packages/release/bioc/html/phyloseq.html"), style = "color:red3"),
                              "). The phyloseq object should contain three necessary data, feature (OTU or ASV) table, taxonomic table and meta/sample information.",br(), br(),
                              "Details:", br(), br(), 
                              strong("Feature table:"), "It should contain counts, where rows are features (OTUs or ASVs) and columns are units (row names are feature IDs and column names are unit IDs).", br(), br(),
                              strong("Taxonomic table:"),"It should contain taxonomic names, where rows are features and columns are seven taxonomic ranks (row names are feature IDs and column names are 'Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species' or 'Domain', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species').", br(), br(),
                              strong("Metadata/Sample information:"),"It should contain variables for the units about host phenotypes, medical interventions, disease status or environmental/behavioral factors, where rows are units and columns are variables (row names are unit IDs, and column names are variable names).", br(), br(),
                              "* The features should be matched and identical across feature table and taxonomic table. The units should be matched and identical between feature table and metadata/sample information.
                              MiTree will analyze only the matched features and units.", style = "font-size:11pt")
  INPUT_PHYLOSEQ_COMMENT2 = p("You can download example microbiome data 'sub.1.con.biom.Rdata' in 'phyloseq' format. For more details about 'phyloseq', see ", 
                              htmltools::a(tags$u("https://bioconductor.org/packages/release/bioc/html/phyloseq.html"), style = "color:red3"), br(), br(), 
                              "> setwd('/yourdatadirectory/')", br(), br(), 
                              "> load(file = 'sub.1.con.biom.Rdata')", br(), br(), 
                              "> library(phyloseq)", br(), br(), 
                              " > otu.tab <- otu_table(sub.1.con.biom)", br(), 
                              " > tax.tab <- tax_table(sub.1.con.biom)", br(), 
                              " > sam.dat <- sample_data(sub.1.con.biom)", br(), br(), 
                              "You can check if the features are matched and identical across feature table and taxonomic table, and the units are matched and identical between feature table and metadata/sample information using following code.", br(), br(), 
                              " > identical(rownames(otu.tab), rownames(tax.tab))", br(), 
                              " > identical(colnames(otu.tab), rownames(sam.dat))", style = "font-size:11pt", br(), br(),
                              strong("Reference:"), "Park B, Koh H, Patatanian M, Reyes-Caballero H, Zhao N, Meinert J, et al. The mediating roles of the oral microbiome in saliva and subgingival sites between e-cigarette smoking and gingival inflammation. BMC Microbiology. 2023;23(35):1-18.")
  INPUT_INDIVIDUAL_DATA_COMMENT = p("Description:", br(), br(), 
                                    strong("Feature table:"), "It should contain counts, where rows are features (OTUs or ASVs) and columns are units (row names are feature IDs and column names are unit IDs).", br(), br(),
                                    strong("Taxonomic table:"),"It should contain taxonomic names, where rows are features and columns are seven taxonomic ranks (row names are feature IDs and column names are 'Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species' or 'Domain', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species').", br(), br(),
                                    strong("Metadata/Sample information:"),"It should contain variables for the units about host phenotypes, medical interventions, disease status or environmental/behavioral factors, where rows are units and columns are variables (row names are unit IDs, and column names are variable names).", br(), br(),
                                    "* The features should be matched and identical across feature table and taxonomic table. The units should be matched and identical between feature table and metadata/sample information.
                                    MiTree will analyze only the matched features and units.", style = "font-size:11pt")
  INPUT_INDIVIDUAL_DATA_COMMENT2 = p("You can download example microbiome data 'Oral.zip'. This zip file contains three necessary data components, feature table (otu.tab.txt), taxonomic table (tax.tab.txt), and metadata/sample information (sam.dat.txt).", br(), br(),
                                     "> setwd('/yourdatadirectory/')", br(), br(), 
                                     "> otu.tab <- read.table(file = 'sub.1.con.biom.otu.tab.txt', check.names = FALSE)", br(), 
                                     "> tax.tab <- read.table(file = 'sub.1.con.biom.tax.tab.txt', check.names = FALSE)", br(), 
                                     "> sam.dat <- read.table(file = 'sub.1.con.biom.sam.dat.txt', check.names = FALSE)", br(), br(),
                                     "You can check if the features are matched and identical across feature table and taxonomic table, 
                                     and the units are matched and identical between feature table and metadata/sample information using following code.", br(), br(), 
                                     " > identical(rownames(otu.tab), rownames(tax.tab))", br(), 
                                     " > identical(colnames(otu.tab), rownames(sam.dat))", style = "font-size:11pt", br(), br(),
                                     strong("Reference:"), "Park B, Koh H, Patatanian M, Reyes-Caballero H, Zhao N, Meinert J, et al. The mediating roles of the oral microbiome in saliva and subgingival sites between e-cigarette smoking and gingival inflammation. BMC Microbiology. 2023;23(35):1-18.")
  
  QC_KINGDOM_COMMENT = p("A microbial kingdom to be analyzed. Default is 'Bacteria' for 16S data. Alternatively, you can type 'Fungi' for ITS data 
                         or any other kingdom of interest for shotgun metagenomic data.", style = "font-size:11pt")
  QC_LIBRARY_SIZE_COMMENT1 = p("Remove units that have low library sizes (total read counts). Default is 3,000.", style = "font-size:11pt")
  QC_LIBRARY_SIZE_COMMENT2 = p("Library size: The total read count per unit.", style = "font-size:11pt")
  QC_MEAN_PROP_COMMENT1 = p("Remove features (OTUs or ASVs) that have low mean relative abundances (Unit: %). Default is 0.002%.",style = "font-size:11pt")
  QC_MEAN_PROP_COMMENT2 = p("Mean proportion: The average of relative abundances (i.e., proportions) per feature.", style = "font-size:11pt")
  QC_TAXA_NAME_COMMENT1 = p("Remove taxonomic names in the taxonomic table that are completely matched with the specified character strings. 
                            Multiple character strings should be separated by a comma. Default is \"\", \"metagenome\", \"gut metagenome\", \"mouse gut metagenome\".",
                            style = "font-size:11pt")
  QC_TAXA_NAME_COMMENT2 = p("Remove taxonomic names in the taxonomic table that are partially matched with the specified character strings (i.e., taxonomic names that contain 
                            the specified character strings). Multiple character strings should be separated by a comma. Default is \"uncultured\", \"incertae\", \"Incertae\",
                            \"unidentified\", \"unclassified\", \"unknown\".",style = "font-size:11pt")
  QC_BATCH_REFERENCE = p("1. Ling W, Lu J, Zhao N. et al. Batch effects removal for microbiome data via conditional quantile regression. Nat Commun. 2022;13(5418)")
  
  DATA_TRANSFORM_COMMENT = p("Transform the data into four different formats (1) CLR (centered log ratio) (Aitchison, 1982), (2) Count (Rarefied) (Sanders, 1968), (3) Proportion, (4) Arcsine-root 
                             for each taxonomic rank (phylum, class, order, familiy, genus, species).")
  DATA_TRANSFORM_REFERENCE = p("1. Aitchison J. The statistical analysis of compositional data. J R Stat Soc B. 1982;44(2):139-77", br(),
                               "2. Sanders HL. Marine benthic diversity: A comparative study. Am Nat. 1968;102:243-282.")
  
  DT_REFERENCE = p("1. Breiman L, Friedman JH, Olshen RA, Stone CJ. Classification and Regression Trees. CRC Press. 1984.", br())
  DT_REFERENCE_CLR = p("1. Breiman L, Friedman JH, Olshen RA, Stone CJ. Classification and Regression Trees. CRC Press. 1984.", br(),
                       "2. Aitchison J. The statistical analysis of compositional data. J R Stat Soc B. 1982;44(2):139-77")
  DT_REFERENCE_RC = p("1. Breiman L, Friedman JH, Olshen RA, Stone CJ. Classification and Regression Trees. CRC Press. 1984.", br(),
                      "2. Sanders HL. Marine benthic diversity: A comparative study. Am Nat. 1968;102:243-282.")
  RF_REFERENCE = p("1. Breiman L. Random forests. Mach Learn. 2001;45:5-32", br())
  RF_REFERENCE_CLR = p("1. Breiman L. Random forests. Mach Learn. 2001;45:5-32", br(),
                       "2. Aitchison J. The statistical analysis of compositional data. J R Stat Soc B. 1982;44(2):139-77")
  RF_REFERENCE_RC = p("1. Breiman L. Random forests. Mach Learn. 2001;45:5-32", br(),
                      "2. Sanders HL. Marine benthic diversity: A comparative study. Am Nat. 1968;102:243-282.")
  XGB_REFERENCE = p("1. Friedman JH. Greedy function approximation: A gradient boosting machine. Ann Stat. 2001;29(5):1189-1232",br(),
                    "2. Chen T, Guestrin C. XGBoost: A scalable tree boosting system. in Proc the 22nd ACM SIGKDD Int Conf KDD. ACM. 2016;785-794", br(),
                    "3. Lundberg SM, Lee SI. A unified approach to interpreting model predictions. in Proc Adv Neural Inf Process Syst. 2017;4765-4774.")
  XGB_REFERENCE_CLR = p("1. Friedman JH. Greedy function approximation: A gradient boosting machine. Ann Stat. 2001;29(5):1189-1232",br(),
                        "2. Chen T, Guestrin C. XGBoost: A scalable tree boosting system. in Proc the 22nd ACM SIGKDD Int Conf KDD. ACM. 2016;785-794", br(),
                        "3. Lundberg SM, Lee SI. A unified approach to interpreting model predictions. in Proc Adv Neural Inf Process Syst. 2017;4765-4774.",br(),
                        "4. Aitchison J. The statistical analysis of compositional data. J R Stat Soc B. 1982;44(2):139-77")
  XGB_REFERENCE_RC = p("1. Friedman JH. Greedy function approximation: A gradient boosting machine. Ann Stat. 2001;29(5):1189-1232",br(),
                       "2. Chen T, Guestrin C. XGBoost: A scalable tree boosting system. in Proc the 22nd ACM SIGKDD Int Conf KDD. ACM. 2016;785-794", br(),
                       "3. Lundberg SM, Lee SI. A unified approach to interpreting model predictions. in Proc Adv Neural Inf Process Syst. 2017;4765-4774.",br(),
                       "4. Sanders HL. Marine benthic diversity: A comparative study. Am Nat. 1968;102:243-282.")
}

# UI ---------------------------------------------------------------------------
{
  ui = dashboardPage(
    title = "MiTree",
    dashboardHeader(title = span(TITLE, style = "float:left;font-size: 20px"), titleWidth = "100%"),
    dashboardSidebar(
      tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
      sidebarMenu(id = "side_menu",
                  menuItem("Home", tabName = "home"),
                  menuItem("Data Processing",
                           menuSubItem("Data Input", tabName = "step1", 
                                       icon = fontawesome::fa("upload", margin_left = "0.3em", margin_right = "0.1em")),
                           menuSubItem("Quality Control", tabName = "step2", 
                                       icon = fontawesome::fa("chart-bar", margin_left = "0.3em")),
                           menuSubItem("Data Transformation", tabName = "dataTransform", 
                                       icon = fontawesome::fa("calculator", margin_left = "0.3em", margin_right = "0.25em"))),
                  menuItem("Data Mining",
                           menuSubItem("Decision Tree", tabName = "dt", 
                                       icon = fontawesome::fa("tree", margin_left = "0.2em", margin_right = "0.1em")),
                           menuSubItem("Random Forest", tabName = "rf", 
                                       icon = fontawesome::fa("network-wired")),
                           menuSubItem("Gradient Boosting", tabName = "xgb", 
                                       icon = fontawesome::fa("diagram-project"))))),
    dashboardBody(
      # Custom Theme -----
      shinyDashboardThemeDIY(
        
        # general
        appFontFamily = "Arial"
        ,appFontColor = "rgb(0,0,0)"
        ,primaryFontColor = "rgb(0,0,0)"
        ,infoFontColor = "rgb(0,0,0)"
        ,successFontColor = "rgb(0,0,0)"
        ,warningFontColor = "rgb(0,0,0)"
        ,dangerFontColor = "rgb(0,0,0)"
        ,bodyBackColor = "rgb(255,255,255)"
        
        # header
        ,logoBackColor = "rgb(2,123,255)"
        
        ,headerButtonBackColor = "rgb(2,123,255)"
        ,headerButtonIconColor = "rgb(2,123,255)"
        ,headerButtonBackColorHover = "rgb(2,123,255)"
        ,headerButtonIconColorHover = "rgb(0,0,0)"
        
        ,headerBackColor = "rgb(2,123,255)"
        ,headerBoxShadowColor = "#aaaaaa"
        ,headerBoxShadowSize = "0px 0px 0px"
        
        # sidebar
        ,sidebarBackColor = "rgb(24,31,41)"
        ,sidebarPadding = 0
        
        ,sidebarMenuBackColor = "transparent"
        ,sidebarMenuPadding = 0
        ,sidebarMenuBorderRadius = 0
        
        ,sidebarShadowRadius = ""
        ,sidebarShadowColor = "0px 0px 0px"
        
        ,sidebarUserTextColor = "rgb(24,31,41)"
        
        ,sidebarSearchBackColor = "rgb(255, 255, 255)"
        ,sidebarSearchIconColor = "rgb(24,31,41)"
        ,sidebarSearchBorderColor = "rgb(24,31,41)"
        
        ,sidebarTabTextColor = "rgb(210,210,210)"
        ,sidebarTabTextSize = 14
        ,sidebarTabBorderStyle = "none"
        ,sidebarTabBorderColor = "none"
        ,sidebarTabBorderWidth = 0
        
        ,sidebarTabBackColorSelected = "rgb(45,52,63)"
        ,sidebarTabTextColorSelected = "rgb(252,255,255)"
        ,sidebarTabRadiusSelected = "0px"
        
        ,sidebarTabBackColorHover = "rgb(67,75,86)"
        ,sidebarTabTextColorHover = "rgb(252,255,255)"
        ,sidebarTabBorderStyleHover = "none"
        ,sidebarTabBorderColorHover = "none"
        ,sidebarTabBorderWidthHover = 0
        ,sidebarTabRadiusHover = "0px"
        
        # boxes
        ,boxBackColor = "rgb(245,245,245)"
        ,boxBorderRadius = 3
        ,boxShadowSize = "0px 0px 0px"
        ,boxShadowColor = "rgba(0,0,0,0)"
        ,boxTitleSize = 16
        ,boxDefaultColor = "rgb(210,214,220)"
        ,boxPrimaryColor = "rgb(35, 49, 64)"
        ,boxInfoColor = "rgb(2,123,255)"
        ,boxSuccessColor = "rgb(112,173,71)"
        ,boxWarningColor = "rgb(244,156,104)"
        ,boxDangerColor = "rgb(255,88,55)"
        
        ,tabBoxTabColor = "rgb(255,255,255)"
        ,tabBoxTabTextSize = 14
        ,tabBoxTabTextColor = "rgb(0,0,0)"
        ,tabBoxTabTextColorSelected = "rgb(35, 49, 64)"
        ,tabBoxBackColor = "rgb(255,255,255)"
        ,tabBoxHighlightColor = "rgb(2,123,255)"
        ,tabBoxBorderRadius = 0
        
        # inputs
        ,buttonBackColor = "rgb(245,245,245)"
        ,buttonTextColor = "rgb(0,0,0)"
        ,buttonBorderColor = "rgb(24,31,41)"
        ,buttonBorderRadius = 3
        
        ,buttonBackColorHover = "rgb(227,227,227)"
        ,buttonTextColorHover = "rgb(100,100,100)"
        ,buttonBorderColorHover = "rgb(200,200,200)"
        
        ,textboxBackColor = "rgb(255,255,255)"
        ,textboxBorderColor = "rgb(200,200,200)"
        ,textboxBorderRadius = 0
        ,textboxBackColorSelect = "rgb(245,245,245)"
        ,textboxBorderColorSelect = "rgb(200,200,200)"
        
        # tables
        ,tableBackColor = "rgb(255, 255, 255)"
        ,tableBorderColor = "rgb(245, 245, 245)"
        ,tableBorderTopSize = 1
        ,tableBorderRowSize = 1
        
      ),
      
      tags$head(tags$style(HTML(".content { padding-top: 2px;}"))),
      tags$script(src = "fileInput_text.js"),
      tags$head(tags$style(HTML('.progress-bar {background-color: rgb(2,144,255);}'))),
      useShinyjs(),
      tabItems(
        
        ## Home -----
        tabItem(tabName = "home",
                div(id = "homepage", br(), HOME_COMMENT, 
                    p(" ", style = "margin-bottom: 10px;"),
                    div(tags$img(src="MiTree_Home_Img2.png", height = 630, width = 750), style = "text-align: center;"), br(),
                    HOME_COMMENT2, HOME_COMMENT3, HOME_COMMENT4)),
        
        ## DATA INPUT -----
        tabItem(tabName = "step1", br(),
                fluidRow(
                  column(width = 6,
                         box(
                           width = NULL, status = "info", solidHeader = TRUE,
                           title = strong("Data Input", style = "color:white"),
                           selectInput("inputOption", h4(strong("Data type")), c("Choose one" = "", "Phyloseq", "Individual Data"), width = '30%'),
                           div(id = "optionsInfo", tags$p("You can choose phyloseq or individual data.", style = "font-size:11pt"), tags$p("", style = "margin-bottom:-8px"), style = "margin-top: -15px"),
                           uiOutput("moreOptions"))),
                  column(width = 6, style='padding-left:0px', uiOutput("addDownloadinfo"))
                )),
        
        ## QC ----
        tabItem(tabName = "step2", br(),
                fluidRow(column(width = 3,  style = "padding-left:+15px",
                                
                                # Quality Control
                                box(
                                  width = NULL, status = "info", solidHeader = TRUE,
                                  title = strong("Quality Control", style = "color:white"),
                                  textInput("kingdom", h4(strong("Kingdom")), value = "Bacteria"),
                                  QC_KINGDOM_COMMENT,
                                  tags$style(type = 'text/css', '#slider1 .irs-grid-text {font-size: 1px}'),
                                  tags$style(type = 'text/css', '#slider2 .irs-grid-text {font-size: 1px}'),
                                  
                                  sliderInput("slider1", h4(strong("Library size")), 
                                              min=0, max=10000, value = 3000, step = 1000),
                                  QC_LIBRARY_SIZE_COMMENT1,
                                  QC_LIBRARY_SIZE_COMMENT2,
                                  
                                  sliderInput("slider2", h4(strong("Mean proportion")), 
                                              min = 0, max = 0.1, value = 0.002, step = 0.001,  post  = " %"),
                                  QC_MEAN_PROP_COMMENT1,
                                  QC_MEAN_PROP_COMMENT2,
                                  
                                  br(),
                                  p(" ", style = "margin-bottom: -20px;"),
                                  
                                  h4(strong("Errors in taxonomic names")),
                                  textInput("rem.str", label = "Complete match", value = ""),
                                  QC_TAXA_NAME_COMMENT1,
                                  
                                  textInput("part.rem.str", label = "Partial match", value = ""),
                                  QC_TAXA_NAME_COMMENT2,
                                  
                                  actionButton("run", (strong("Run!")), class = "btn-info"), 
                                  p(" ", style = "margin-bottom: +10px;"), 
                                  p(strong("Attention:"),"You have to click this Run button to perform data transformation and further analyses.", style = "margin-bottom:-10px"), br()
                                ),
                                
                                uiOutput("moreControls")
                ),
                
                column(width = 9, style = "padding-left:+10px",
                       
                       box(
                         width = NULL, status = "info", solidHeader = TRUE,
                         fluidRow(width = 12,
                                  status = "info", solidHeader = TRUE, 
                                  valueBoxOutput("sample_Size", width = 3),
                                  valueBoxOutput("OTUs_Size", width = 3),
                                  valueBoxOutput("phyla", width = 3),
                                  valueBoxOutput("classes", width = 3)),
                         fluidRow(width = 12, 
                                  status = "info", solidHeader = TRUE,
                                  valueBoxOutput("orders", width = 3),
                                  valueBoxOutput("families", width = 3),
                                  valueBoxOutput("genera", width = 3),
                                  valueBoxOutput("species", width = 3)),
                         fluidRow(style = "position:relative",
                                  tabBox(width = 6, title = strong("Library Size", style = "color:black"), 
                                         tabPanel("Histogram",
                                                  plotlyOutput("hist"),
                                                  sliderInput("binwidth", "# of Bins:",
                                                              min = 0, max = 100, value = 50, width = "100%"),
                                                  chooseSliderSkin("Round", color = "#112446")),
                                         tabPanel("Box Plot", 
                                                  plotlyOutput("boxplot"))),
                                  tabBox(width = 6, title = strong("Mean Proportion", style = "color:black"), 
                                         tabPanel("Histogram",
                                                  plotlyOutput("hist2"),
                                                  sliderInput("binwidth2", "# of Bins:",
                                                              min = 0, max = 100, value = 50, width = "100%"),
                                                  chooseSliderSkin("Round", color = "#112446")),
                                         tabPanel("Box Plot",
                                                  plotlyOutput("boxplot2"))))
                       ),
                       
                       shinyjs::hidden(
                         shiny::div(id = "pcoa.area",
                                    box(width = NULL, status = "info", solidHeader = TRUE,
                                        title = strong("Batch Effect Correction", style = "color:white"),
                                        plotOutput("batch.pcoa", height = 600))))))),
        
        ## Data Transformation -----
        tabItem(tabName = "dataTransform", br(),
                column(width = 6, style='padding-left:0px',
                       box(title = strong("Data Transformation", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                           DATA_TRANSFORM_COMMENT,
                           actionButton("datTransRun", (strong("Run!")), class = "btn-info"),
                           p(" ", style = "margin-bottom: +10px;"), 
                           p(strong("Attention:"),"You have to click this Run button to perform following taxonomy-level machine learning analyses."),
                           p("", style = "margin-bottom:-8px")),
                       uiOutput("datTransDownload")),
                column(width = 6, style='padding-left:0px', 
                       box(title = strong("References", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                           DATA_TRANSFORM_REFERENCE,
                           p("", style = "margin-bottom:-8px")))),
        
        ## Decision tree ------
        tabItem(tabName = "dt", br(),
                fluidRow(
                  tabBox(width = 12,
                         tabPanel(
                           title = "Classification",
                           sidebarLayout( 
                             position = "left",
                             sidebarPanel(width = 3,
                                          shinyjs::hidden(
                                            uiOutput("dt_cla_data_input"),
                                            uiOutput("dt_cla_covariate"),
                                            uiOutput("dt_cla_train_setting"),
                                            uiOutput("dt_cla_downloadTabUI"),
                                            uiOutput("dt_cla_reference"))),
                             mainPanel(width = 9,
                                       fluidRow(width = 12, uiOutput("dt_cla_results"))))),
                         tabPanel(
                           title = "Regression",
                           sidebarLayout( 
                             position = "left",
                             sidebarPanel(width = 3,
                                          shinyjs::hidden(
                                            uiOutput("dt_reg_data_input"),
                                            uiOutput("dt_reg_covariate"),
                                            uiOutput("dt_reg_train_setting"),
                                            uiOutput("dt_reg_downloadTabUI"),
                                            uiOutput("dt_reg_reference"))),
                             mainPanel(width = 9,
                                       fluidRow(width = 12, uiOutput("dt_reg_results")))))))),
        
        ## Random Forest ------
        tabItem(tabName = "rf", br(),
                fluidRow(
                  tabBox(width = 12,
                         tabPanel(
                           title = "Classification",
                           sidebarLayout( 
                             position = "left",
                             sidebarPanel(width = 3,
                                          shinyjs::hidden(
                                            uiOutput("rf_cla_data_input"),
                                            uiOutput("rf_cla_covariate"),
                                            uiOutput("rf_cla_train_setting"),
                                            uiOutput("rf_cla_downloadTabUI"),
                                            uiOutput("rf_cla_reference"))),
                             mainPanel(width = 9,
                                       fluidRow(width = 12, uiOutput("rf_cla_results"))))),
                         tabPanel(
                           title = "Regression",
                           sidebarLayout( 
                             position = "left",
                             sidebarPanel(width = 3,
                                          shinyjs::hidden(
                                            uiOutput("rf_reg_data_input"),
                                            uiOutput("rf_reg_covariate"),
                                            uiOutput("rf_reg_train_setting"),
                                            uiOutput("rf_reg_downloadTabUI"),
                                            uiOutput("rf_reg_reference"))),
                             mainPanel(width = 9,
                                       fluidRow(width = 12, uiOutput("rf_reg_results")))))))),
        
        ## Gradient Boosting ------
        tabItem(tabName = "xgb", br(),
                fluidRow(
                  tabBox(width = 12,
                         tabPanel(
                           title = "Classification",
                           sidebarLayout( 
                             position = "left",
                             sidebarPanel(width = 3,
                                          shinyjs::hidden(
                                            uiOutput("xgb_cla_data_input"),
                                            uiOutput("xgb_cla_covariate"),
                                            uiOutput("xgb_cla_train_setting"),
                                            uiOutput("xgb_cla_downloadTabUI"),
                                            uiOutput("xgb_cla_reference"))),
                             mainPanel(width = 9,
                                       fluidRow(width = 12, uiOutput("xgb_cla_results"))))),
                         tabPanel(
                           title = "Regression",
                           sidebarLayout( 
                             position = "left",
                             sidebarPanel(width = 3,
                                          shinyjs::hidden(
                                            uiOutput("xgb_reg_data_input"),
                                            uiOutput("xgb_reg_covariate"),
                                            uiOutput("xgb_reg_train_setting"),
                                            uiOutput("xgb_reg_downloadTabUI"),
                                            uiOutput("xgb_reg_reference"))),
                             mainPanel(width = 9,
                                       fluidRow(width = 12, uiOutput("xgb_reg_results"))))))))
      )
    )
  )
}

# SERVER -----------------------------------------------------------------------
server = function(input, output, session){
  options(shiny.maxRequestSize=30*1024^2)

  env <- new.env()
  nm <- load(file = "Data/sub.1.con.biom.Rdata", env)[1]
  sub.1.con.biom <- env[[nm]]
  
  sub.1.con.biom.otu.tab <- otu_table(sub.1.con.biom)
  sub.1.con.biom.tax.tab <- tax_table(sub.1.con.biom)
  sub.1.con.biom.sam.dat <- sample_data(sub.1.con.biom)
  
  output$downloadData.sub.1.con <- downloadHandler(
    filename = function() {
      paste("sub.1.con.biom.Rdata", sep = "")
    },
    content = function(file1) {
      save(sub.1.con.biom, file = file1)
    })
  output$downloadZip.sub.1.con.biom <- downloadHandler(
    filename = function() {
      paste("sub.1.con.biom",".zip", sep = "")
    },
    content <- function(fname) {
      temp <- setwd(tempdir())
      on.exit(setwd(temp))
      dataFiles = c("sub.1.con.biom.otu.tab.txt", "sub.1.con.biom.tax.tab.txt", "sub.1.con.biom.sam.dat.txt")
      write.table(sub.1.con.biom.otu.tab, "sub.1.con.biom.otu.tab.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
      write.table(sub.1.con.biom.tax.tab, "sub.1.con.biom.tax.tab.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
      write.table(sub.1.con.biom.sam.dat, "sub.1.con.biom.sam.dat.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
      zip(zipfile=fname, files=dataFiles)
    })
  
  ## Reactive Variables -------
  infile = reactiveValues(biom = NULL, qc_biom = NULL, rare_biom = NULL, na_omit_biom = NULL)
  batch.infile = reactiveValues(batchid = NULL, bat_biom = NULL, qc_biom = NULL, rare_biom = NULL)
  chooseData = reactiveValues(sam.dat = NULL, mon.sin.rev.bin.con = NULL, prim_vars = NULL, alpha.div = NULL,
                              alpha.div.rare = NULL, alpha.div.qc = NULL, taxa.out = NULL, tax.tab = NULL)
  taxa.results = reactiveValues(bin.var = NULL, cov.var = NULL, id.var = NULL, taxa = NULL, taxa.bin.sum.out = NULL, con.var = NULL, taxa.con.sum.out = NULL, lib.size = NULL)
  dt.model.input.cla <- reactiveValues(split = NULL, tune = NULL)
  dt.model.input.reg <- reactiveValues(split = NULL, tune = NULL)
  xgb.model.input.cla <- reactiveValues(eval = NULL, loss = NULL, cov.eval = NULL, cov.loss = NULL)
  xgb.model.input.reg <- reactiveValues(eval = NULL, loss = NULL)
  rcol = reactiveValues(selected = "lightblue")
  
  ## Input Option -----------
  observeEvent(input$inputOption,{
    observe({
      if (input$inputOption == "Phyloseq") {
        shinyjs::hide(id = "optionsInfo")
        output$moreOptions <- renderUI({
          tagList(
            tags$style("
                       .btn-file {
                       border-top-left-radius: 5px !important; border-bottom-left-radius: 5px !important; border-left-style: solid !important; border-left-width: 1px !important;
                       border-top-right-radius: 0px !important; border-bottom-right-radius: 0px !important; border-right-width: 0px !important;
                       }"
            ),
            fileInput("phyloseqData", strong("Please upload your 'phyloseq' data (.Rdata, .rds)", style = "color:black"), 
                      accept = c(".Rdata", ".rds"), width = '80%'), div(style = "margin-top: -15px"),
            actionButton('Load_Phyloseq_Data', 'Upload', class = "btn-info"), 
            p(" ", style = "margin-bottom: +10px;"), 
            p(strong("Attention:"), "You have to click this Upload button to perform following data processing and downstream data analyses."),br(),
            shinyjs::hidden(
              shiny::div(id = "phyloseqUpload_error",
                         shiny::tags$p("Please upload a Rdata file!!",
                                       style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
            ),
            INPUT_PHYLOSEQ_COMMENT1,
            p("", style = "margin-bottom:-8px")
          )
        })
        
        output$addDownloadinfo <- renderUI({
          tagList(
            box(title = strong("Example Data", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                downloadButton("downloadData.sub.1.con", "Oral Microbiome", width = '30%', style = "color:black; background-color: red2"),
                br(),br(),
                INPUT_PHYLOSEQ_COMMENT2,
                p("", style = "margin-bottom:-8px")
            )
          )
        })
      }
      else if (input$inputOption == "Individual Data") {
        shinyjs::hide(id = "optionsInfo")
        output$moreOptions <- renderUI({
          tagList(
            tags$style("
                       .btn-file {
                       border-top-left-radius: 5px !important; border-bottom-left-radius: 5px !important; border-left-style: solid !important; border-left-width: 1px !important;
                       border-top-right-radius: 0px !important; border-bottom-right-radius: 0px !important; border-right-width: 0px !important;
                       }"
            ),
            fileInput("otuTable", strong("Please upload your feature (OTU or ASV) table (.txt, .csv, .biom)", style = "color:black"), 
                      accept = c(".txt", ".csv", ".biom"), width = '80%'), div(style = "margin-top: -15px"),
            fileInput("taxTable", strong("Please upload your taxonomic table (.txt, .tsv)", style = "color:black"), 
                      accept = c(".txt", ".tsv"), width = '80%'), div(style = "margin-top: -15px"),
            fileInput("samData", strong("Please upload your metadata/sample information (.txt, .csv)", style = "color:black"), 
                      accept = c(".txt", ".csv"), width = '80%'), div(style = "margin-top: -15px"),
            actionButton('Load_Individual_Data', 'Upload', class = "btn-info"), br(),br(),
            shinyjs::hidden(
              shiny::div(id = "textfilesUpload_error",
                         shiny::tags$p("Please upload txt and tre files!!",
                                       style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
            ),
            INPUT_INDIVIDUAL_DATA_COMMENT, 
            p("", style = "margin-bottom:-8px")
          )
        })
        
        output$addDownloadinfo <- renderUI({
          tagList(
            box(title = strong("Example Data", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                downloadButton("downloadZip.sub.1.con.biom", "Oral Microbiome", width = '30%', style = "color:black; background-color: red2"),
                br(),br(),
                INPUT_INDIVIDUAL_DATA_COMMENT2,
                p("", style = "margin-bottom:-8px")
            )
          )
        })
      }
    })
    
  }, ignoreInit = TRUE, once = TRUE, ignoreNULL = TRUE)
  
  observe({
    toggleState("Load_Phyloseq_Data", !is.null(input$phyloseqData))
    toggleState("Load_Individual_Data", 
                !(is.null(input$otuTable) | is.null(input$taxTable) | is.null(input$samData)))
    toggleState("run", !is.null(infile$biom))
    toggleState("skip", !is.null(infile$biom))
    toggleState("slider1", !is.null(infile$biom))
    toggleState("slider2", !is.null(infile$biom))
    toggleState("kingdom", !is.null(infile$biom))
    toggleState("binwidth", !is.null(infile$biom))
    toggleState("binwidth2", !is.null(infile$biom))
    
    toggleState("datTransRun", !is.null(infile$rare_biom))
  })
  
  observeEvent(input$Load_Phyloseq_Data, {
    
    if (!is.null(input$phyloseqData)) {
      dataInfile  = reactive({
        phyloseq.data = input$phyloseqData
        ext <- tools::file_ext(phyloseq.data$datapath)
        
        req(phyloseq.data)
        if (ext == "Rdata") {
          phyloseq.dataPath = phyloseq.data$datapath
          e = new.env()
          name <- load(phyloseq.dataPath, envir = e)
          data <- e[[name]]
          
          if (sum(sapply(sample_data(data),is.factor))!=0) {
            sample_data(data)[,which(sapply(sample_data(data), is.factor))] = lapply(sample_data(data)[,which(sapply(sample_data(data), is.factor))], as.character)
          }
          
          colnames(tax_table(data)) = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
          
          if (sum(colnames(otu_table(data)) %in% rownames(sample_data(data))) < sum(rownames(otu_table(data)) %in% rownames(sample_data(data)))) {
            otu_table(data) = t(otu_table(data))
          }
          
          return(data)
        } else if (ext == "rds") {
          phyloseq.dataPath = phyloseq.data$datapath
          data <- readRDS(phyloseq.dataPath)
          
          if (sum(sapply(sample_data(data),is.factor))!=0) {
            sample_data(data)[,which(sapply(sample_data(data), is.factor))] = lapply(sample_data(data)[,which(sapply(sample_data(data), is.factor))], as.character)
          }
          colnames(tax_table(data)) = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
          
          if (sum(colnames(otu_table(data)) %in% rownames(sample_data(data))) < sum(rownames(otu_table(data)) %in% rownames(sample_data(data)))) {
            otu_table(data) = t(otu_table(data))
          }
          
          return(data)
        } else {
          shinyjs::toggle(id = "phyloseqUpload_error", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(5000, shinyjs::toggle(id = "phyloseqUpload_error", anim = TRUE, time = 1, animType = "fade"))
          return(NULL)
        }
      })
    } else {
      return(NULL)
    }
    
    if (is.null(dataInfile)) {
      infile$biom <- NULL
      infile$qc_biom <- NULL
      infile$rare_biom = NULL
    } else {
      infile$biom <- dataInfile()
      infile$qc_biom <- dataInfile()
      infile$rare_biom = NULL
    }
    
    updateTabsetPanel(session, "side_menu",
                      selected = "step2")
    rcol$selected = "lightblue"
    
    if (!is.null(infile$biom)) QC$resume()
  })
  observeEvent(input$Load_Individual_Data, {
    shinyjs::disable("Load_Individual_Data")
    
    withProgress(
      message = 'Calculation in progress',
      detail = 'This may take a while...', value = 0, {
        incProgress(3/10, message = "File Check")
        if (!is.null(input$otuTable) & !is.null(input$taxTable) & !is.null(input$samData)) {
          dataInfile  = reactive({
            otu.table = input$otuTable
            ext1 <- tools::file_ext(otu.table$datapath)
            
            tax.table = input$taxTable
            ext2 <- tools::file_ext(tax.table$datapath)
            
            sam.data = input$samData
            ext3 <- tools::file_ext(sam.data$datapath)
            
            req(otu.table, tax.table, sam.data)
            if ((ext1 == "txt"| ext1 == "csv" | ext1 == "biom") & (ext2 == "txt" | ext2 == "tsv") &
                (ext3 == "txt" | ext3 == "csv")) {
              otu.table.path = otu.table$datapath
              tax.table.path = tax.table$datapath
              sam.data.path = sam.data$datapath
              
              if (ext1 == "txt") {
                otu.tab <- read.table(otu.table.path, header=TRUE, check.names = FALSE, sep = "\t")
              } else if (ext1 == "csv") {
                otu.tab <- read.csv(otu.table.path, check.names = FALSE)
                rownames(otu.tab) = otu.tab[,1];otu.tab = otu.tab[,-1]
              } else if (ext1 == "biom") {
                biom <- read_biom(otu.table.path)
                otu.tab <- as.matrix(biom_data(biom))
              }
              
              if (ext2 == "txt") {
                tax.tab <- read.table(tax.table.path, header=TRUE, check.names = FALSE, sep = "\t")
              } else if (ext2 == "tsv") {
                tax.tab <- read.table(tax.table.path, header=TRUE, sep="\t")
                tax.tab = preprocess.tax.tab(tax.tab)
              }
              
              if (ext3 == "txt") {
                sam.dat <- read.table(sam.data.path, header=TRUE, check.names = FALSE, sep = "\t")
              } else if (ext3 == "csv") {
                sam.dat <- read.csv(sam.data.path, check.names = FALSE)
                rownames(sam.dat) = sam.dat[,1]
                sam.dat = sam.dat[,-1]
              }
              
              otu.tab <- otu_table(otu.tab, taxa_are_rows = TRUE)
              tax.tab <- tax_table(as.matrix(tax.tab))
              sam.dat <- sample_data(sam.dat)
              
              if (sum(colnames(otu.tab) %in% rownames(sam.dat)) < sum(rownames(otu.tab) %in% rownames(sam.dat))) {
                otu.tab = t(otu.tab)
              }
              
              incProgress(3/10, message = "Validating")
              shiny::validate(
                if (biom.check.samples(otu.tab, sam.dat)) {
                  if (biom.check.otu(otu.tab, tax.tab)) {
                    showNotification(h4("Error: There is no common samples among OTU/feature table and Sample Data. And
                                        there is no common OTUs among OTU/feature table and taxonomic table."),
                                     type = "error")
                  } else {
                    showNotification(h4("Error: There is no common samples among OTU/feature table and Sample Data"),
                                     type = "error")
                  }
                } else if (biom.check.otu(otu.tab, tax.tab)) {
                  showNotification(h4("Error: There is no common OTUs among OTU/feature table and taxonomic table."),
                                   type = "error")
                } else {
                  NULL
                }
              )
              
              incProgress(1/10, message = "Merging")
              biomData <- merge_phyloseq(otu.tab, tax.tab, sam.dat)
              return(biomData)
            } else {
              shinyjs::toggle(id = "textfilesUpload_error", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(5000, shinyjs::toggle(id = "textfilesUpload_error", anim = TRUE, time = 1, animType = "fade"))
              return(NULL)
            }
          })
        } else {
          return(NULL)
        }
        
        if (is.null(dataInfile)) {
          infile$biom <- NULL
          infile$qc_biom <- NULL
          infile$rare_biom = NULL
        } else {
          infile$biom <- dataInfile()
          infile$qc_biom <- dataInfile()
          infile$rare_biom = NULL
        }
        
        updateTabsetPanel(session, "side_menu",
                          selected = "step2")
        rcol$selected = "lightblue"
        
        if (!is.null(infile$biom)) QC$resume()
      })
    shinyjs::enable("Load_Individual_Data")
  })
  
  # Main --------
  
  ## (1) QC & Data Transformation -----------
  # This reactive expression stores the input data from either the individual data or phyloseq data
  QC = observe(suspended = T,{
    taxa.results$lib.size <- lib.size.func(infile$biom)$lib.size
    
    # Plots graphs using example infile data
    output$hist <- renderPlotly({
      lib_size = lib.size.func(infile$qc_biom)$lib.size
      plot_ly(x = ~lib_size, nbinsx = input$binwidth,
              type = "histogram",
              marker = list(color = rcol$selected, line = list(color = "black", width = 2))) %>%
        layout(
          yaxis = list(title = "Frequency", zeroline = FALSE),
          xaxis = list(title = "Library Size", zeroline = FALSE))
    })
    
    output$hist2 <- renderPlotly({
      mean_prop = mean.prop.func(infile$qc_biom)$mean.prop
      plot_ly(x = ~mean_prop, nbinsx = input$binwidth2,
              type = "histogram",
              marker = list(color = rcol$selected, line = list(color = "black", width = 2))) %>%
        layout(
          yaxis = list(title = "Frequency", zeroline = FALSE),
          xaxis = list(title = "Mean Proportion", zeroline = FALSE))
    })
    
    output$boxplot<- renderPlotly({
      lib_size = lib.size.func(infile$qc_biom)$lib.size
      
      plot_ly(x = ~lib_size, type = "box", notched=TRUE, name = "Library Size",
              color = ~"lib_size", colors = rcol$selected, line = list(color = 'black'))%>%
        layout(
          yaxis = list(title = "", zeroline = FALSE),
          xaxis = list(title = "", zeroline = FALSE), showlegend = FALSE)
    })
    
    output$boxplot2<- renderPlotly({
      mean_prop = mean.prop.func(infile$qc_biom)$mean.prop
      
      plot_ly(x = ~mean_prop, type = "box", notched=TRUE, name = "Mean Proportion",
              color = ~"mean_prop", colors = rcol$selected, line = list(color = 'black'))%>%
        layout(
          yaxis = list(title = "", zeroline = FALSE),
          xaxis = list(title = "", zeroline = FALSE), showlegend = FALSE)
    })
    
    ## Number of Taxonomic Rank for biom before QC
    num_tax.rank = reactive({
      tax.tab = tax_table(infile$qc_biom)
      num.tax.rank(tax.tab)
    })
    
    ## Fills value boxes using example biom data
    output$sample_Size <- renderValueBox({
      valueBox(
        value = tags$p(paste0(lib.size.func(infile$qc_biom)$num.sams), style = "font-size: 75%;"),
        "Sample Size", icon = icon("user-circle"), color = "fuchsia")
    })
    
    output$OTUs_Size <- renderValueBox({
      valueBox(
        value = tags$p(paste0(lib.size.func(infile$qc_biom)$num.otus), style = "font-size: 75%;"),
        "Number of Features", icon = icon("dna"), color = "aqua")
    })
    
    output$phyla <- renderValueBox({
      num.phyla = num_tax.rank()[1]
      valueBox(
        value = tags$p(paste0(num.phyla), style = "font-size: 75%;"),
        "Number of Phyla", icon = icon("sitemap"), color = "orange")
    })
    
    output$classes <- renderValueBox({
      num.classes = num_tax.rank()[2]
      valueBox(
        value = tags$p(paste0(num.classes), style = "font-size: 75%;"),
        "Number of Classes", icon = icon("sitemap"), color = "purple")
    })
    
    output$orders <- renderValueBox({
      num.orders = num_tax.rank()[3]
      valueBox(
        value = tags$p(paste0(num.orders), style = "font-size: 75%;"),
        "Number of Orders", icon = icon("sitemap"), color = "blue")
    })
    
    output$families <- renderValueBox({
      num.families = num_tax.rank()[4]
      valueBox(
        value = tags$p(paste0(num.families), style = "font-size: 75%;"),
        "Number of Families", icon = icon("sitemap"), color = "red")
    })
    
    output$genera <- renderValueBox({
      num.genera = num_tax.rank()[5]
      valueBox(
        value = tags$p(paste0(num.genera), style = "font-size: 75%;"),
        "Number of Genera", icon = icon("sitemap"), color = "lime")
    })
    
    output$species <- renderValueBox({
      num.species = num_tax.rank()[6]
      valueBox(
        value = tags$p(paste0(num.species), style = "font-size: 75%;"),
        "Number of Species", icon = icon("sitemap"), color = "teal" )
    })
    
    ## This event handler checks whether there is an input file and updates the slider options accordingly
    maxi.slider1 = as.numeric(lib.size.func(infile$qc_biom)$lib.size.sum["3rd quartile"])
    max.mean.prop = as.numeric(mean.prop.func(infile$qc_biom)$mean.prop.sum["3rd quartile"])
    maxi.slider2 = round(max.mean.prop, digits = 6)
    
    if (maxi.slider2 < 2e-05) {
      maxi.slider2 = 2e-05
    }
    
    updateSliderInput(session, "slider1", min = 0, max = round(maxi.slider1,-3))
    updateSliderInput(session, "slider2", min = 0, max = maxi.slider2*100)
  })
  
  ## (2-1) DT - Classification ---------------------------
  output$dt_cla_data_input <- renderUI({
    tagList(
      prettyRadioButtons("dt_cla_dataType", label = h4(strong("Data format", style = "color:black")), icon = icon("check"), animation = "jelly",
                         c("CLR (Default)", "Count (Rarefied)", "Proportion", "Arcsine-root"), selected = "CLR (Default)",width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Output variable", style = "color:black")),
      p("E.g., a variable on the host's health or disease status.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("dt_cla_response", label = NULL, 
                  choices = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Binary"), error = function(e) return(NULL)), 
                  selected = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Binary")[1], error = function(e) return(NULL)), width = '70%'))
  })
  
  output$dt_cla_covariate <- renderUI({
    tagList(
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Covariate(s)", style = "color:black")),
      p("Potential confounders (e.g., age, gender) to be adjusted for.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      prettyRadioButtons("dt_cla_covariate_yn",label = NULL, icon = icon("check"),
                         animation = "jelly", c("None", "Covariate(s)"), selected = "None", width = '70%'),

      shinyjs::hidden(
        shiny::div(id = "dt_cla_covariate_list", style = "margin-left: 2%",
                   prettyCheckboxGroup("dt_cla_covariate_options"," Please select covariate(s)",
                                       choices = get.cov.col(chooseData$sam.dat)[!get.cov.col(chooseData$sam.dat) %in% c(input$dt_cla_response)], width = '70%')))
    )
  })
  
  output$dt_cla_train_setting <- renderUI({
    tagList(
      prettyRadioButtons("dt_cla_loss", label = h4(strong("Loss function", style = "color:black")), animation = "jelly",
                         c("Cross entropy (Default)", "Gini impurity"), selected = "Cross entropy (Default)", icon = icon("check"), width = '70%'),
      
      prettyRadioButtons("dt_cla_cov_loss", label = h4(strong("Loss function", style = "color:black")), animation = "jelly",
                         c("Mean squared error (Default)"), selected = "Mean squared error (Default)", icon = icon("check"), width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# folds", style = "color:black")),
      p("The number of non-overlapping folds of the data to be used in cross-validations.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("dt_cla_nfold", label = NULL, c("Choose one" = "", c("LOOCV (Default)", 5, 10)), selected = "LOOCV (Default)", width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Min # units for a split", style = "color:black")),
      p("The minimum number of units (study subjects or individuals) in a node for a split to be attempt.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      sliderInput("dt_cla_minsplit", label = NULL, min=1, max=ifelse(lib.size.func(infile$qc_biom)$num.sams >= 50, 20, ifelse(lib.size.func(infile$qc_biom)$num.sams >= 30 & lib.size.func(infile$qc_biom)$num.sams < 50, 10, 5)), value = ifelse(lib.size.func(infile$qc_biom)$num.sams >= 30, 10, 3), step = 1),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Min # units in a leaf", style = "color:black")),
      p("The minimum number of units (study subjects or individuals) to be included in each leaf.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      sliderInput("dt_cla_minbucket", label = NULL, min=1, max=ifelse(lib.size.func(infile$qc_biom)$num.sams >= 50, 20, ifelse(lib.size.func(infile$qc_biom)$num.sams >= 30 & lib.size.func(infile$qc_biom)$num.sams < 50, 10, 5)), value = ifelse(lib.size.func(infile$qc_biom)$num.sams >= 30, 5, 2), step = 1),
      
      prettyRadioButtons("dt_cla_include_species", label = h4(strong("Taxonomic ranks", style = "color:black")), animation = "jelly",
                         c("Phylum - Genus (Default)", "Phylum - Species"), selected = "Phylum - Genus (Default)",
                         icon = icon("check"), width = '80%'),
      
      actionButton("dt_cla_runButton", (strong("Run!")), class = "btn-info")
    )
  })
  
  observeEvent(input$dt_cla_covariate_yn,{
    if (input$dt_cla_covariate_yn == "Covariate(s)") {
      shinyjs::show("dt_cla_covariate_list")
      shinyjs::hide("dt_cla_loss")
      shinyjs::show("dt_cla_cov_loss")
    }
    else if (input$dt_cla_covariate_yn == "None") {
      shinyjs::hide("dt_cla_covariate_list")
      shinyjs::show("dt_cla_loss")
      shinyjs::hide("dt_cla_cov_loss")
    }
  })

  observe({
    toggleState("dt_cla_runButton", (input$dt_cla_covariate_yn == "None" & (input$dt_cla_response != "")) | ((input$dt_cla_covariate_yn == "Covariate(s)") & (length(input$dt_cla_covariate_options) != 0) & (input$dt_cla_response != "")))
  })
  
  ## (2-2) DT - Regression ---------------------------
  output$dt_reg_data_input <- renderUI({
    tagList(
      prettyRadioButtons("dt_reg_dataType", label = h4(strong("Data format", style = "color:black")), icon = icon("check"), animation = "jelly",
                         c("CLR (Default)", "Count (Rarefied)", "Proportion", "Arcsine-root"), selected = "CLR (Default)",width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Output variable", style = "color:black")),
      p("E.g., a variable on the host's health or disease status.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("dt_reg_response", label = NULL, 
                  choices = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Continuous"), error = function(e) return(NULL)), 
                  selected = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Continuous")[1], error = function(e) return(NULL)), width = '70%'))
  })
  
  output$dt_reg_covariate <- renderUI({
    tagList(
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Covariate(s)", style = "color:black")),
      p("Potential confounders (e.g., age, gender) to be adjusted for.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      prettyRadioButtons("dt_reg_covariate_yn",label = NULL, icon = icon("check"),
                         animation = "jelly", c("None", "Covariate(s)"), selected = "None", width = '70%'),

      shinyjs::hidden(
        shiny::div(id = "dt_reg_covariate_list", style = "margin-left: 2%",
                   prettyCheckboxGroup("dt_reg_covariate_options"," Please select covariate(s)",
                                       choices = get.cov.col(chooseData$sam.dat)[!get.cov.col(chooseData$sam.dat) %in% c(input$dt_reg_response)], width = '70%')))
    )
  })
  
  observeEvent(input$dt_reg_covariate_yn,{
    if (input$dt_reg_covariate_yn == "Covariate(s)") {
      shinyjs::show("dt_reg_covariate_list")
      print(input$dt_reg_response)
    }
    else if (input$dt_reg_covariate_yn == "None") {
      shinyjs::hide("dt_reg_covariate_list")
    }
  })
  
  output$dt_reg_train_setting <- renderUI({
    tagList(
      prettyRadioButtons("dt_reg_loss", label = h4(strong("Loss function", style = "color:black")), animation = "jelly",
                         c("Mean squared error (Default)"), selected = "Mean squared error (Default)", icon = icon("check"), width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# folds", style = "color:black")),
      p("The number of non-overlapping folds of the data to be used in cross-validations.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("dt_reg_nfold", label = NULL, c("Choose one" = "", c("LOOCV (Default)", 5, 10)), selected = "LOOCV (Default)", width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Min # units for a split", style = "color:black")),
      p("The minimum number of units (study subjects or individuals) in a node for a split to be attempt.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      sliderInput("dt_reg_minsplit", label = NULL, min=1, max=ifelse(lib.size.func(infile$qc_biom)$num.sams >= 50, 20, ifelse(lib.size.func(infile$qc_biom)$num.sams >= 30 & lib.size.func(infile$qc_biom)$num.sams < 50, 10, 5)), value = ifelse(lib.size.func(infile$qc_biom)$num.sams >= 30, 10, 3), step = 1),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Min # units in a leaf", style = "color:black")),
      p("The minimum number of units (study subjects or individuals) to be included in each leaf.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      sliderInput("dt_reg_minbucket", label = NULL, min=1, max=ifelse(lib.size.func(infile$qc_biom)$num.sams >= 50, 20, ifelse(lib.size.func(infile$qc_biom)$num.sams >= 30 & lib.size.func(infile$qc_biom)$num.sams < 50, 10, 5)), value = ifelse(lib.size.func(infile$qc_biom)$num.sams >= 30, 5, 2), step = 1),
      
      prettyRadioButtons("dt_reg_include_species", label = h4(strong("Taxonomic ranks", style = "color:black")), animation = "jelly",
                         c("Phylum - Genus (Default)", "Phylum - Species"), selected = "Phylum - Genus (Default)",
                         icon = icon("check"), width = '80%'),
      
      actionButton("dt_reg_runButton", (strong("Run!")), class = "btn-info"))
  })
  
  observe({
    toggleState("dt_reg_runButton", (input$dt_reg_covariate_yn == "None" & (input$dt_reg_response != "")) | ((input$dt_reg_covariate_yn == "Covariate(s)") & (length(input$dt_reg_covariate_options) != 0) & (input$dt_reg_response != "")))
  })
  
  ## (3-1) RF - Classification ---------------------------
  output$rf_cla_data_input <- renderUI({
    tagList(
      prettyRadioButtons("rf_cla_dataType", label = h4(strong("Data format", style = "color:black")), icon = icon("check"), animation = "jelly",
                         c("CLR (Default)", "Count (Rarefied)", "Proportion", "Arcsine-root"), selected = "CLR (Default)",width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Output variable", style = "color:black")),
      p("E.g., a variable on the host's health or disease status.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("rf_cla_response", label = NULL, 
                  choices = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Binary"), error = function(e) return(NULL)), 
                  selected = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Binary")[1], error = function(e) return(NULL)), width = '70%'))
  })
  
  output$rf_cla_covariate <- renderUI({
    tagList(
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Covariate(s)", style = "color:black")),
      p("Potential confounders (e.g., age, gender) to be adjusted for.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      prettyRadioButtons("rf_cla_covariate_yn",label = NULL, icon = icon("check"),
                         animation = "jelly", c("None", "Covariate(s)"), selected = "None", width = '70%'),

      shinyjs::hidden(
        shiny::div(id = "rf_cla_covariate_list", style = "margin-left: 2%",
                   prettyCheckboxGroup("rf_cla_covariate_options"," Please select covariate(s)",
                                       choices = get.cov.col(chooseData$sam.dat)[!get.cov.col(chooseData$sam.dat) %in% c(input$rf_cla_response)], width = '70%')))
    )
  })

  observeEvent(input$rf_cla_covariate_yn,{
    if (input$rf_cla_covariate_yn == "Covariate(s)") {
      shinyjs::show("rf_cla_covariate_list")
    }
    else if (input$rf_cla_covariate_yn == "None") {
      shinyjs::hide("rf_cla_covariate_list")
    }
  })
  
  output$rf_cla_train_setting <- renderUI({
    tagList(
      prettyRadioButtons("rf_cla_loss", label = h4(strong("Loss function", style = "color:black")), animation = "jelly",
                         c("Gini impurity (Default)"), selected = "Gini impurity (Default)", icon = icon("check"), width = '70%'),
      prettyRadioButtons("rf_cla_cov_loss", label = h4(strong("Loss function", style = "color:black")), animation = "jelly",
                         c("Mean squared error (Default)"), selected = "Mean squared error (Default)", icon = icon("check"), width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# folds", style = "color:black")),
      p("The number of non-overlapping folds of the data to be used in cross-validations.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("rf_cla_nfold", label = NULL, c("Choose one" = "", c(5, 10)), selected = 5, width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# trees", style = "color:black")),
      p("The number of bagged trees to be aggregated (Default: 5,000).", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("rf_cla_ntree", label = NULL, c("Choose one" = "", c(3000, 5000, 10000)), selected = 5000, width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# taxa to be displayed", style = "color:black")),
      p("The maximum number of taxa to be displayed in importance and partial dependence plots (Default: 20).", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      sliderInput("rf_cla_var_num", label = NULL, min = 5, max = 20, value = 20, step = 5),
      
      prettyRadioButtons("rf_cla_include_species", label = h4(strong("Taxonomic ranks", style = "color:black")), animation = "jelly",
                         c("Phylum - Genus (Default)", "Phylum - Species"), selected = "Phylum - Genus (Default)",
                         icon = icon("check"), width = '80%'),
      
      actionButton("rf_cla_runButton", (strong("Run!")), class = "btn-info"))
  })
  
  observeEvent(input$rf_cla_covariate_yn,{
    if (input$rf_cla_covariate_yn == "Covariate(s)") {
      shinyjs::hide("rf_cla_loss")
      shinyjs::show("rf_cla_cov_loss")
    }
    else if (input$rf_cla_covariate_yn == "None") {
      shinyjs::show("rf_cla_loss")
      shinyjs::hide("rf_cla_cov_loss")
    }
  })

  observe({
    toggleState("rf_cla_runButton", (input$rf_cla_covariate_yn == "None" & (input$rf_cla_response != "")) | ((input$rf_cla_covariate_yn == "Covariate(s)") & (length(input$rf_cla_covariate_options) != 0) & (input$rf_cla_response != "")))
  })
  
  ## (3-2) RF - Regression ---------------------------
  output$rf_reg_data_input <- renderUI({
    tagList(
      prettyRadioButtons("rf_reg_dataType", label = h4(strong("Data format", style = "color:black")), icon = icon("check"), animation = "jelly",
                         c("CLR (Default)", "Count (Rarefied)", "Proportion", "Arcsine-root"), selected = "CLR (Default)",width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Output variable", style = "color:black")),
      p("E.g., a variable on the host's health or disease status.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("rf_reg_response", label = NULL, 
                  choices = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Continuous"), error = function(e) return(NULL)), 
                  selected = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Continuous")[1], error = function(e) return(NULL)), width = '70%'))
  })
  
  output$rf_reg_covariate <- renderUI({
    tagList(
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Covariate(s)", style = "color:black")),
      p("Potential confounders (e.g., age, gender) to be adjusted for.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      prettyRadioButtons("rf_reg_covariate_yn",label = NULL, icon = icon("check"),
                         animation = "jelly", c("None", "Covariate(s)"), selected = "None", width = '70%'),

      shinyjs::hidden(
        shiny::div(id = "rf_reg_covariate_list", style = "margin-left: 2%",
                   prettyCheckboxGroup("rf_reg_covariate_options"," Please select covariate(s)",
                                       choices = get.cov.col(chooseData$sam.dat)[!get.cov.col(chooseData$sam.dat) %in% c(input$rf_reg_response)], width = '70%')))
    )
  })

  observeEvent(input$rf_reg_covariate_yn,{
    if (input$rf_reg_covariate_yn == "Covariate(s)") {
      shinyjs::show("rf_reg_covariate_list")
    }
    else if (input$rf_reg_covariate_yn == "None") {
      shinyjs::hide("rf_reg_covariate_list")
    }
  })
  
  output$rf_reg_train_setting <- renderUI({
    tagList(
      prettyRadioButtons("rf_reg_loss", label = h4(strong("Loss function", style = "color:black")), animation = "jelly",
                         c("Mean squared error (Default)"), selected = "Mean squared error (Default)", icon = icon("check"), width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# folds", style = "color:black")),
      p("The number of non-overlapping folds of the data to be used in cross-validations.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("rf_reg_nfold", label = NULL, c("Choose one" = "", c(5, 10)), selected = 5, width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# trees", style = "color:black")),
      p("The number of bagged trees to be aggregated (Default: 5,000).", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("rf_reg_ntree", label = NULL, c("Choose one" = "", c(3000, 5000, 10000)), selected = 5000, width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# taxa to be displayed", style = "color:black")),
      p("The maximum number of taxa to be displayed in importance and partial dependence plots (Default: 20).", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      sliderInput("rf_reg_var_num", label = NULL, min = 5, max = 20, value = 20, step = 5),
      
      prettyRadioButtons("rf_reg_include_species", label = h4(strong("Taxonomic ranks", style = "color:black")), animation = "jelly",
                         c("Phylum - Genus (Default)", "Phylum - Species"), selected = "Phylum - Genus (Default)",
                         icon = icon("check"), width = '80%'),
      
      actionButton("rf_reg_runButton", (strong("Run!")), class = "btn-info"))
  })
  
  observe({
    toggleState("rf_reg_runButton", (input$rf_reg_covariate_yn == "None" & (input$rf_reg_response != "")) | ((input$rf_reg_covariate_yn == "Covariate(s)") & (length(input$rf_reg_covariate_options) != 0) & (input$rf_reg_response != "")))
  })
  
  ## (4-1) XGB - Classification ---------------------------
  output$xgb_cla_data_input <- renderUI({
    tagList(
      prettyRadioButtons("xgb_cla_dataType", label = h4(strong("Data format", style = "color:black")), icon = icon("check"), animation = "jelly",
                         c("CLR (Default)", "Count (Rarefied)", "Proportion", "Arcsine-root"), selected = "CLR (Default)",width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Output variable", style = "color:black")),
      p("E.g., a variable on the host's health or disease status.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("xgb_cla_response", label = NULL, 
                  choices = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Binary"), error = function(e) return(NULL)), 
                  selected = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Binary")[1], error = function(e) return(NULL)), width = '70%'))
  })
  
  output$xgb_cla_covariate <- renderUI({
    tagList(
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Covariate(s)", style = "color:black")),
      p("Potential confounders (e.g., age, gender) to be adjusted for.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      prettyRadioButtons("xgb_cla_covariate_yn",label = NULL, icon = icon("check"),
                         animation = "jelly", c("None", "Covariate(s)"), selected = "None", width = '70%'),

      shinyjs::hidden(
        shiny::div(id = "xgb_cla_covariate_list", style = "margin-left: 2%",
                   prettyCheckboxGroup("xgb_cla_covariate_options"," Please select covariate(s)",
                                       choices = get.cov.col(chooseData$sam.dat)[!get.cov.col(chooseData$sam.dat) %in% c(input$xgb_cla_response)], width = '70%')))
    )
  })

  observeEvent(input$xgb_cla_covariate_yn,{
    if (input$xgb_cla_covariate_yn == "Covariate(s)") {
      shinyjs::show("xgb_cla_covariate_list")
    }
    else if (input$xgb_cla_covariate_yn == "None") {
      shinyjs::hide("xgb_cla_covariate_list")
    }
  })
  
  output$xgb_cla_train_setting <- renderUI({
    tagList(
      prettyRadioButtons("xgb_cla_loss", label = h4(strong("Loss function", style = "color:black")), animation = "jelly",
                         c("AUC (Gain)", "Cross entropy (Default)", "Error rate"), selected = "Cross entropy (Default)", icon = icon("check"), width = '70%'),

      prettyRadioButtons("xgb_cla_loss_cov", label = h4(strong("Loss function", style = "color:black")), animation = "jelly",c("Mean squared error (Default)"),
                         selected = "Mean squared error (Default)", icon = icon("check"), width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# folds", style = "color:black")),
      p("The number of non-overlapping folds of the data to be used in cross-validations.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("xgb_cla_nfold", label = NULL, c("Choose one" = "", c(5, 10)), selected = 5, width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Max # iterations", style = "color:black")),
      p("The maximum number of iterations (updates) in the boosting process. A large maximum number of iterations (e.g., 5,000) is recommended 
        to boost the tree sufficiently, but it can be at the cost of heavy computation. (Default: 5,000)", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("xgb_cla_nrounds", label = NULL, c("Choose one" = "", c(3000, 5000, 10000)), selected = 5000, width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Learning rate", style = "color:black")),
      p("The rate of a newly fitted tree to be reflected into the aggregation (update). A low learning rate (e.g., 0.001) is recommended to elaborate the boosting process through slow learning, but it is at the cost of heavy computations. (Default: 0.005)", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("xgb_cla_eta", label = NULL, c("Choose one" = "", c(0.001, 0.005, 0.01, 0.05)), selected = 0.005, width = '70%'),
      
      prettyRadioButtons("xgb_cla_penalty", label = h4(strong("Regularization", style = "color:black")), animation = "jelly",
                         c("Yes (Default)", "No"), selected = "Yes (Default)", icon = icon("check"), width = "70%"),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# taxa to be displayed", style = "color:black")),
      p("The maximum number of taxa per taxonomic rank to be displayed in plots.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      sliderInput("xgb_cla_var_num", label = NULL, min = 5, max = 20, value = 20, step = 5),
      
      prettyRadioButtons("xgb_cla_include_species", label = h4(strong("Taxonomic ranks", style = "color:black")), animation = "jelly",
                         c("Phylum - Genus (Default)", "Phylum - Species"), selected = "Phylum - Genus (Default)",
                         icon = icon("check"), width = '80%'),
      actionButton("xgb_cla_runButton", (strong("Run!")), class = "btn-info"))
  })
  
  observeEvent(input$xgb_cla_covariate_yn,{
    if (input$xgb_cla_covariate_yn == "Covariate(s)"){
      shinyjs::show("xgb_cla_loss_cov")
      shinyjs::hide("xgb_cla_loss")
    }
    else{
      shinyjs::hide("xgb_cla_loss_cov")
      shinyjs::show("xgb_cla_loss")
    }
  })

  observe({
    toggleState("xgb_cla_runButton", (input$xgb_cla_covariate_yn == "None" & (input$xgb_cla_response != "")) | ((input$xgb_cla_covariate_yn == "Covariate(s)") & (length(input$xgb_cla_covariate_options) != 0) & (input$xgb_cla_response != "")))
  })
  
  ## (4-2) XGB - Regression ---------------------------
  output$xgb_reg_data_input <- renderUI({
    tagList(
      prettyRadioButtons("xgb_reg_dataType", label = h4(strong("Data format", style = "color:black")), icon = icon("check"), animation = "jelly",
                         c("CLR (Default)", "Count (Rarefied)", "Proportion", "Arcsine-root"), selected = "CLR (Default)",width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Output variable", style = "color:black")),
      p("E.g., a variable on the host's health or disease status.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("xgb_reg_response", label = NULL, 
                  choices = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Continuous"), error = function(e) return(NULL)), 
                  selected = tryCatch(bmc.col.check(chooseData$sam.dat, type = "Continuous")[1], error = function(e) return(NULL)), width = '70%'))
  })
  
  output$xgb_reg_covariate <- renderUI({
    tagList(
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Covariate(s)", style = "color:black")),
      p("Potential confounders (e.g., age, gender) to be adjusted for.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      prettyRadioButtons("xgb_reg_covariate_yn",label = NULL, icon = icon("check"),
                         animation = "jelly", c("None", "Covariate(s)"), selected = "None", width = '70%'),

      shinyjs::hidden(
        shiny::div(id = "xgb_reg_covariate_list", style = "margin-left: 2%",
                   prettyCheckboxGroup("xgb_reg_covariate_options"," Please select covariate(s)",
                                       choices = get.cov.col(chooseData$sam.dat)[!get.cov.col(chooseData$sam.dat) %in% c(input$xgb_reg_response)], width = '70%')))
    )
  })

  observeEvent(input$xgb_reg_covariate_yn,{
    if (input$xgb_reg_covariate_yn == "Covariate(s)") {
      shinyjs::show("xgb_reg_covariate_list")
    }
    else if (input$xgb_reg_covariate_yn == "None") {
      shinyjs::hide("xgb_reg_covariate_list")
    }
  })
  
  output$xgb_reg_train_setting <- renderUI({
    tagList(
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Loss function", style = "color:black")),
      p(" ", style = "margin-bottom: +15px;"),
      prettyRadioButtons("xgb_reg_loss", label = NULL, icon = icon("check"), animation = "jelly",c("Mean squared error (Default)"),
                         selected = "Mean squared error (Default)", width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# folds", style = "color:black")),
      p("The number of non-overlapping folds of the data to be used in cross-validations.", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("xgb_reg_nfold", label = NULL, c("Choose one" = "", c(5, 10)), selected = 5, width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Max # iterations", style = "color:black")),
      p("The maximum number of iterations (updates) in the boosting process. A large maximum number of iterations (e.g., 5,000) is recommended 
        to boost the tree sufficiently, but it can be at the cost of heavy computation. (Default: 5,000)", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("xgb_reg_nrounds", label = NULL, c("Choose one" = "", c(3000, 5000, 10000)), selected = 5000, width = '70%'),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("Learning rate", style = "color:black")),
      p("The rate of a newly fitted tree to be reflected into the aggregation (update). A low learning rate (e.g., 0.001) is recommended
            to elaborate the boosting process through slow learning, but it is at the cost of heavy computations. (Default: 0.005)", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      selectInput("xgb_reg_eta", label = NULL, c("Choose one" = "", c(0.001, 0.005, 0.01, 0.05)), selected = 0.005, width = '70%'),
      
      prettyRadioButtons("xgb_reg_penalty", label = h4(strong("Regularization", style = "color:black")), animation = "jelly",
                         c("Yes (Default)", "No"), selected = "Yes (Default)", icon = icon("check"), width = "70%"),
      
      p(" ", style = "margin-top: 25px;"),
      h4(strong("# taxa to be displayed", style = "color:black")),
      p("The maximum number of taxa per taxonomic rank to be displayed in plots", style = "font-size:10pt"),
      p(" ", style = "margin-bottom: +15px;"),
      sliderInput("xgb_reg_var_num", label = NULL, min = 5, max = 20, value = 20, step = 5),
      
      prettyRadioButtons("xgb_reg_include_species", label = h4(strong("Taxonomic ranks", style = "color:black")), animation = "jelly",
                         c("Phylum - Genus (Default)", "Phylum - Species"), selected = "Phylum - Genus (Default)",
                         icon = icon("check"), width = '80%'),
      actionButton("xgb_reg_runButton", (strong("Run!")), class = "btn-info"))
  })
  
  observe({
    toggleState("xgb_reg_runButton", (input$xgb_reg_covariate_yn == "None" & (input$xgb_reg_response != "")) | ((input$xgb_reg_covariate_yn == "Covariate(s)") & (length(input$xgb_reg_covariate_options) != 0) & (input$xgb_reg_response != "")))
  })
  
  ## Variable Assignment --------------
  observeEvent(input$dt_cla_loss, {
    if(input$dt_cla_loss == "Gini impurity"){
      dt.model.input.cla$split = "gini"
    }
    else{
      dt.model.input.cla$split = "info"
    }
  })
  
  observeEvent(input$dt_tune_reg, {
    if(input$dt_tune_reg == "Maximum Depth"){
      dt.model.input.reg$tune = "depth"
    }
    else{
      dt.model.input.reg$tune = "cp"
    }
  })
  
  observeEvent(input$xgb_cla_loss, {
    if(input$xgb_cla_loss == "Cross entropy (Default)"){
      xgb.model.input.cla$eval = "logloss"
    }
    else if(input$xgb_cla_loss == "Error rate"){
      xgb.model.input.cla$eval = "error"
    }
    else if(input$xgb_cla_loss == "AUC (Gain)"){
      xgb.model.input.cla$eval = "auc"
    }
  })
  
  observeEvent(input$xgb_reg_loss, {
    if(input$xgb_reg_loss == "Huber loss (Default)"){
      xgb.model.input.reg$loss = "huber"
    }
    else if(input$xgb_reg_loss == "Mean squared error (Default)"){
      xgb.model.input.reg$loss = "rss"
    }
  })
  
  ## Run Buttons ------
  ## (1) QC & Data Transformation -----
  observeEvent(input$run, {
    # if(input$batch.yn == "Yes"){
    #   withProgress(
    #     message = 'Calculation in progress', 
    #     detail = 'This may take a while...', value = 0, {
    #       
    #       incProgress(1/10, message = "Data Trimming in progress")
    #       
    #       if (nchar(input$part.rem.str) == 0) {
    #         rem.tax.complete <- rem.tax.d
    #         rem.tax.partial <- rem.tax.str.d
    #       } else {
    #         rem.tax.complete <- unique(c(unlist(strsplit(input$rem.str, split = ",")), rem.tax.d))
    #         rem.tax.partial <- unique(c(unlist(strsplit(input$part.rem.str, split = ",")), rem.tax.str.d))
    #       }
    #       
    #       tax.tab <- tax_table(infile$biom)
    #       
    #       if (input$kingdom != "all") {
    #         ind <- is.element(tax.tab[,1], input$kingdom)
    #         shiny::validate(
    #           if (sum(ind) == 0) {
    #             showNotification(h4(paste("Error: Please select valid Kingdom. Available kingdoms are:", 
    #                                       paste(c(na.omit(unique(tax.tab[,1])) ,"and all"), collapse = ", "))),
    #                              type = "error")
    #           } else {
    #             NULL
    #           }
    #         )
    #       }
    #       
    #       shinyjs::disable("batch.yn")
    #       shinyjs::disable("batch.var")
    #       shinyjs::disable("prim.var")
    #       shinyjs::disable("covar")
    #       shinyjs::disable("lib.size.adj")
    #       shinyjs::disable("run")
    #       shinyjs::disable("slider1")
    #       shinyjs::disable("slider2")
    #       shinyjs::disable("kingdom")
    #       shinyjs::disable("skip")
    #       shinyjs::disable("binwidth")
    #       shinyjs::disable("binwidth2")
    #       shinyjs::disable("rem.str")
    #       shinyjs::disable("part.rem.str")
    #       
    #       rcol$selected = "rgba(255, 0, 0, 0.6)"
    #       
    #       tree.exists <- !is.null(access(infile$biom, "phy_tree"))
    #       
    #       # 1. QC
    #       infile$qc_biom = biom.clean(infile$biom, 
    #                                   input$kingdom, 
    #                                   lib.size.cut.off = input$slider1, 
    #                                   mean.prop.cut.off = input$slider2/100,
    #                                   rem.tax = rem.tax.complete, rem.tax.str = rem.tax.partial,
    #                                   tree.exists = tree.exists)
    #       
    #       incProgress(2/10, message = "Batch effect correction in progress")
    #       
    #       # 2. Remove NA
    #       otu.tab <- as.data.frame(t(otu_table(infile$qc_biom)))
    #       sam.dat <- sample_data(infile$qc_biom)
    #       
    #       remain.samples <- sam.dat[,c(input$batch.var, input$prim.var, input$covar)] %>% na.omit %>% rownames
    #       
    #       infile$na_omit_biom <- prune_samples(remain.samples, infile$qc_biom)
    #       otu.tab <- as.data.frame(t(otu_table(infile$na_omit_biom)))
    #       tax.tab <- tax_table(infile$na_omit_biom)
    #       tree <- phy_tree(infile$na_omit_biom)
    #       sam.dat <- sample_data(infile$na_omit_biom)
    #       
    #       # 3. Check numeric / binary factor / multicategory factor
    #       batchid <<- as.factor(sam.dat[[input$batch.var]])
    #       
    #       prim.var <- input$prim.var
    #       if(!is.null(input$covar)){
    #         cov <- input$covar
    #         df <- data.frame(sam.dat[,prim.var], sam.dat[,cov])
    #       }
    #       else{
    #         df <- data.frame(sam.dat[,prim.var])
    #       }
    #       
    #       for(name in colnames(df)){
    #         type <- col.str.check(sam.dat, name)
    #         if(type == "factor"){
    #           df[[name]] <- as.factor(df[[name]])
    #         }
    #         else if(type == "numeric"){
    #           df[[name]] <- as.numeric(df[[name]])
    #         }
    #         else{
    #           df[[name]] <- df[[name]]
    #         }
    #       }
    #       
    #       f1 <- as.formula(paste("~" ,paste(names(df), collapse = "+"), collapse = ""))
    #       covar <- model.matrix(f1, data = df)[,-1]
    #       
    #       # covar <- model.matrix(~ as.factor(sam.dat[[input$prim.var]]) + as.factor(sam.dat[[input$covar]]))[,-1]
    #       ref.bat <- names(which.max(table(batchid)))
    #       
    #       if(input$lib.size.adj == "No"){
    #         set.seed(578)
    #         conqur.otu.tab <- ConQuR(tax_tab = otu.tab, batchid = batchid,
    #                                  covariates = covar, batch_ref = ref.bat,
    #                                  logistic_lasso = T, quantile_type = "lasso", interplt = T, num_core = 2)
    #       }
    #       else{ # input$lib.size.adj == "Yes"
    #         set.seed(578)
    #         conqur.otu.tab <- ConQuR_libsize(tax_tab = otu.tab, batchid = batchid,
    #                                          covariates = covar, batch_ref = ref.bat, libsize_tune = NULL,
    #                                          logistic_lasso = T, quantile_type = "lasso", interplt = T, num_core = 2)
    #       }
    #       
    #       bat.otu.tab <- otu_table(t(as.data.frame(as.matrix(conqur.otu.tab))), taxa_are_rows = TRUE)
    #       bat.sam.dat <- sample_data(infile$na_omit_biom)
    #       bat.tax.tab <- tax_table(infile$na_omit_biom)
    #       bat.tree <- phy_tree(infile$na_omit_biom)
    #       
    #       batch.infile$bat_biom <- merge_phyloseq(bat.otu.tab, bat.tax.tab, bat.sam.dat, bat.tree)
    #       
    #       # 4. Rarefying
    #       incProgress(2/10, message = "Rarefying in progress")
    #       
    #       # Rarefying original biom data
    #       lib_size.sum = lib.size.func(infile$qc_biom)$lib.size.sum
    #       infile$rare_biom = rarefy.func(infile$qc_biom, 
    #                                      cut.off = lib_size.sum["Minimum"],
    #                                      multi.rarefy = 1,
    #                                      tree.exists = tree.exists)
    #       
    #       # Rarefying corrected biom data
    #       batch.infile$qc_biom <- otu.tab.clean(batch.infile$bat_biom,
    #                                             lib.size.cut.off = input$slider1,
    #                                             mean.prop.cut.off = input$slider2/100,
    #                                             tree.exists = tree.exists)
    #       
    #       lib_size.sum = lib.size.func(batch.infile$qc_biom)$lib.size.sum
    #       batch.infile$rare_biom <- rarefy.func(batch.infile$qc_biom,
    #                                             cut.off = lib_size.sum["Minimum"],
    #                                             multi.rarefy = 1,
    #                                             tree.exists = tree.exists)
    #       
    #       # 5. PCoA Plot code
    #       qc.biom <- infile$qc_biom
    #       rare.biom <- infile$rare_biom
    #       qc.biom.bat <- batch.infile$qc_biom
    #       rare.biom.bat <- batch.infile$rare_biom
    #       
    #       shinyjs::show("pcoa.area")
    #       
    #       incProgress(2/10, message = "Plotting PCoA in progress")
    #       output$batch.pcoa <- renderPlot({
    #         try(batch.correct.PCoA(qc.biom = qc.biom, rare.biom = rare.biom,
    #                                qc.biom.bat = qc.biom.bat, rare.biom.bat = rare.biom.bat,
    #                                batch.var = input$batch.var), silent = TRUE)
    #       })
    #       
    #       incProgress(2/10, message = "Saving File in progress")
    #       
    #       chooseData$sam.dat = sample_data(batch.infile$qc_biom)
    #       chooseData$mon.sin.rev.bin.con = is.mon.sin.rev.bin.con(chooseData$sam.dat)
    #       chooseData$prim_vars = pri.func(chooseData$sam.dat, chooseData$mon.sin.rev.bin.con)
    #       chooseData$tax.tab = tax_table(batch.infile$rare_biom)
    #       
    #       output$moreControls <- renderUI({
    #         tagList(
    #           box(title = strong("Download Data", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
    #               span(textOutput("text"), style="font-size:13pt"),
    #               
    #               h5("Data after Quality Control and Batch Effect Correction"),
    #               downloadButton("downloadData2", "Download", width = '50%', 
    #                              style = "color:black; background-color: red3"),br(),
    #               
    #               h5("Data after Quality Control, Batch Effect Correction and Rarefaction"),
    #               downloadButton("downloadData3", "Download", width = '50%', 
    #                              style = "color:black; background-color: red3"),br(),
    #               
    #               p("For your reference, you can download the data files above for 
    #               (1) the phyloseq object (biom.after.qc.bat) after QC and batch effect correction, and 
    #               (2) the phyloseq object (rare.biom.after.qc.bat) after QC, batch effect correction, and rarefaction.", 
    #                 style = "font-size:11pt")))
    #       })
    #       
    #       output$text <- renderText({"You are all set! You can proceed to data analysis!"})
    #       
    #       biom.after.qc.bat = batch.infile$qc_biom
    #       output$downloadData2 <- downloadHandler(
    #         filename = function() {
    #           paste("biom.after.qc.bat.Rdata")
    #         },
    #         content = function(file1) {
    #           save(biom.after.qc.bat, file = file1)
    #         })
    #       
    #       rare.biom.after.qc.bat = batch.infile$rare_biom
    #       output$downloadData3 <- downloadHandler(
    #         filename = function() {
    #           paste("rare.biom.after.qc.bat.Rdata")
    #         },
    #         content = function(file1) {
    #           save(rare.biom.after.qc.bat, file = file1)
    #         })
    #       
    #       incProgress(1/10, message = "Done")
    #       shinyjs::enable("batch.yn")
    #       shinyjs::enable("batch.var")
    #       shinyjs::enable("prim.var")
    #       shinyjs::enable("covar")
    #       shinyjs::enable("lib.size.adj")
    #       shinyjs::enable("run")
    #       shinyjs::enable("slider1")
    #       shinyjs::enable("slider2")
    #       shinyjs::enable("kingdom")
    #       shinyjs::enable("skip")
    #       shinyjs::enable("binwidth")
    #       shinyjs::enable("binwidth2")
    #       shinyjs::enable("rem.str")
    #       shinyjs::enable("part.rem.str")
    #       
    #       infile$qc_biom <- batch.infile$qc_biom
    #       infile$rare_biom <- batch.infile$rare_biom
    #     })
    # }
    
    withProgress(
      message = 'Calculation in progress', 
      detail = 'This may take a while...', value = 0, {
        
        incProgress(1/10, message = "Data Trimming in progress")
        
        if (nchar(input$part.rem.str) == 0) {
          rem.tax.complete <- rem.tax.d
          rem.tax.partial <- rem.tax.str.d
        } else {
          rem.tax.complete <- unique(c(unlist(strsplit(input$rem.str, split = ",")), rem.tax.d))
          rem.tax.partial <- unique(c(unlist(strsplit(input$part.rem.str, split = ",")), rem.tax.str.d))
        }
        
        tax.tab <- tax_table(infile$biom)
        
        if (input$kingdom != "all") {
          ind <- is.element(tax.tab[,1], input$kingdom)
          shiny::validate(
            if (sum(ind) == 0) {
              showNotification(h4(paste("Error: Please select valid Kingdom. Available kingdoms are:", 
                                        paste(c(na.omit(unique(tax.tab[,1])) ,"and all"), collapse = ", "))),
                               type = "error")
            } else {
              NULL
            }
          )
        }
        
        shinyjs::disable("batch.yn")
        shinyjs::disable("batch.var")
        shinyjs::disable("prim.var")
        shinyjs::disable("covar")
        shinyjs::disable("lib.size.adj")
        shinyjs::disable("run")
        shinyjs::disable("slider1")
        shinyjs::disable("slider2")
        shinyjs::disable("kingdom")
        shinyjs::disable("skip")
        shinyjs::disable("binwidth")
        shinyjs::disable("binwidth2")
        shinyjs::disable("rem.str")
        shinyjs::disable("part.rem.str")
        
        rcol$selected = "rgba(255, 0, 0, 0.6)"
        
        tree.exists <- !is.null(access(infile$biom, "phy_tree"))
        
        infile$qc_biom = biom.clean(infile$biom, 
                                    input$kingdom, 
                                    lib.size.cut.off = input$slider1, 
                                    mean.prop.cut.off = input$slider2/100,
                                    rem.tax = rem.tax.complete, rem.tax.str = rem.tax.partial,
                                    tree.exists = tree.exists)
        
        incProgress(3/10, message = "Rarefying in progress")
        lib_size.sum = lib.size.func(infile$qc_biom)$lib.size.sum
        infile$rare_biom = rarefy.func(infile$qc_biom, 
                                       cut.off = lib_size.sum["Minimum"],
                                       multi.rarefy = 1,
                                       tree.exists = tree.exists)
        
        shinyjs::hide("pcoa.area")
        
        incProgress(2/10, message = "Saving File in progress")
        
        chooseData$sam.dat = sample_data(infile$qc_biom)
        chooseData$mon.sin.rev.bin.con = is.mon.sin.rev.bin.con(chooseData$sam.dat)
        chooseData$prim_vars = pri.func(chooseData$sam.dat, chooseData$mon.sin.rev.bin.con)
        chooseData$tax.tab = tax_table(infile$rare_biom)
        
        output$moreControls <- renderUI({
          tagList(
            box(title = strong("Download Data", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                span(textOutput("text"), style="font-size:13pt"),
                h5("Data after Quality Control"),
                downloadButton("downloadData2", "Download", width = '50%', style = "color:black; background-color: red3"),br(),
                h5("Data after Quality Control and Rarefaction"),
                downloadButton("downloadData3", "Download", width = '50%', style = "color:black; background-color: red3"),br(),
                p("For your reference, you can download the data files above for the phyloseq object (biom.after.qc) after QC and
                    (rare.biom.after.qc) after QC and rarefaction.",
                  style = "font-size:11pt")
            )
          )
        })
        
        output$text <- renderText({"You are all set! You can proceed to data analysis!"})
        
        biom.after.qc = infile$qc_biom
        output$downloadData2 <- downloadHandler(
          filename = function() {
            paste("biom.after.qc.Rdata")
          },
          content = function(file1) {
            save(biom.after.qc, file = file1)
          })
        
        rare.biom.after.qc = infile$rare_biom
        output$downloadData3 <- downloadHandler(
          filename = function() {
            paste("rare.biom.after.qc.Rdata")
          },
          content = function(file1) {
            save(rare.biom.after.qc, file = file1)
          })
        
        incProgress(1/10, message = "Done")
        shinyjs::enable("batch.yn")
        shinyjs::enable("batch.var")
        shinyjs::enable("prim.var")
        shinyjs::enable("covar")
        shinyjs::enable("lib.size.adj")
        shinyjs::enable("run")
        shinyjs::enable("slider1")
        shinyjs::enable("slider2")
        shinyjs::enable("kingdom")
        shinyjs::enable("skip")
        shinyjs::enable("binwidth")
        shinyjs::enable("binwidth2")
        shinyjs::enable("rem.str")
        shinyjs::enable("part.rem.str")
      })
    
  })
  
  observeEvent(input$datTransRun, {
    
    withProgress(
      message = 'Calculation in progress', 
      detail = 'This may take a while...', value = 0, {
        incProgress(3/10, message = "Transformation in progress")
        
        rare.otu.tab <- otu_table(infile$rare_biom)
        rare.tax.tab <- tax_table(infile$rare_biom)
        no.rare.otu.tab <- otu_table(infile$qc_biom)
        no.rare.tax.tab <- tax_table(infile$qc_biom)
        chooseData$taxa.out = tax.trans(no.rare.otu.tab, no.rare.tax.tab, rare.otu.tab, rare.tax.tab)
        chooseData$taxa.names.out = taxa.names.rank(chooseData$taxa.out[[1]])
        chooseData$tax.tab = rare.tax.tab
        
        incProgress(3/10, message = "Transformation in progress")
        
        output$datTransDownload <- renderUI({
          tagList(
            box(title = strong("Download Data", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                span(textOutput("text"), style="font-size:15pt"),
                p("You can download taxonomic abundance data.",
                  style = "font-size:11pt"),
                h5("Count", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), "Count (Rarefied)"),
                downloadButton("taxadataCount", "Download", width = '50%', style = " background-color: red3"), HTML('&emsp;'),
                downloadButton("taxadataRareCount", "Download", width = '50%', style = " background-color: red3"), br(), 
                h5("Proportion", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&nbsp;'), "CLR"),
                downloadButton("taxadataProp", "Download", width = '50%', style = " background-color: red3"), HTML('&emsp;'),
                downloadButton("taxadataCLR", "Download", width = '50%', style = " background-color: red3"), br(),
                h5("Arcsine-root"),
                downloadButton("taxadataArc", "Download", width = '50%', style = " background-color: red3"), br(), p("", style = "margin-bottom:5px")
            )
          )
        })
        
        count_biom = chooseData$taxa.out$count
        rare_biom = chooseData$taxa.out$rare.count
        prop_biom = chooseData$taxa.out$prop
        clr_biom = chooseData$taxa.out$clr
        arc_biom = chooseData$taxa.out$arcsin
        
        output$taxadataCount <- downloadHandler(
          
          filename = function() {
            paste("Count.Data.zip")
          },
          content = function(count.file) {
            temp <- setwd(tempdir())
            on.exit(setwd(temp))
            dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
            write.table(as.data.frame(count_biom$phylum), file = "Phylum.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(count_biom$class), file = "Class.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(count_biom$order), file = "Order.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(count_biom$family), file = "Family.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(count_biom$genus), file = "Genus.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(count_biom$species), file = "Species.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            zip(zipfile=count.file, files=dataFiles)
          }
        )
        
        incProgress(3/10, message = "Saving")
        
        output$taxadataRareCount <- downloadHandler(
          
          filename = function() {
            paste("Rarefied.Count.Data.zip")
          },
          content = function(rare.file) {
            temp <- setwd(tempdir())
            on.exit(setwd(temp))
            dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
            write.table(as.data.frame(rare_biom$phylum), file = "Phylum.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(rare_biom$class), file = "Class.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(rare_biom$order), file = "Order.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(rare_biom$family), file = "Family.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(rare_biom$genus), file = "Genus.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(rare_biom$species), file = "Species.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            zip(zipfile=rare.file, files=dataFiles)
          }
        )
        output$taxadataProp <- downloadHandler(
          filename = function() {
            paste("Proportion.Data.zip")
          },
          content = function(prop.file) {
            temp <- setwd(tempdir())
            on.exit(setwd(temp))
            dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
            write.table(as.data.frame(prop_biom$phylum), file = "Phylum.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(prop_biom$class), file = "Class.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(prop_biom$order), file = "Order.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(prop_biom$family), file = "Family.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(prop_biom$genus), file = "Genus.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(prop_biom$species), file = "Species.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            zip(zipfile=prop.file, files=dataFiles)
          }
        )
        output$taxadataCLR <- downloadHandler(
          filename = function() {
            paste("CLR.Transformed.Data.zip")
          },
          content = function(clr.file) {
            temp <- setwd(tempdir())
            on.exit(setwd(temp))
            dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
            write.table(as.data.frame(clr_biom$phylum), file = "Phylum.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(clr_biom$class), file = "Class.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(clr_biom$order), file = "Order.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(clr_biom$family), file = "Family.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(clr_biom$genus), file = "Genus.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(clr_biom$species), file = "Species.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            zip(zipfile=clr.file, files=dataFiles)
          }
        )
        output$taxadataArc <- downloadHandler(
          filename = function() {
            paste("Arcsin.Transformed.Data.zip")
          },
          content = function(arc.file) {
            temp <- setwd(tempdir())
            on.exit(setwd(temp))
            dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
            write.table(as.data.frame(arc_biom$phylum), file = "Phylum.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(arc_biom$class), file = "Class.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(arc_biom$order), file = "Order.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(arc_biom$family), file = "Family.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(arc_biom$genus), file = "Genus.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            write.table(as.data.frame(arc_biom$species), file = "Species.txt", row.names = TRUE, col.names = TRUE, sep = "\t")
            zip(zipfile=arc.file, files=dataFiles)
          }
        )
        incProgress(1/10, message = "Done")
        
        shinyjs::show("dt_cla_data_input")
        shinyjs::show("dt_cla_covariate")
        shinyjs::show("dt_cla_train_setting")
        shinyjs::show("dt_cla_downloadTabUI")
        shinyjs::show("dt_cla_reference")
        
        shinyjs::show("dt_reg_data_input")
        shinyjs::show("dt_reg_covariate")
        shinyjs::show("dt_reg_train_setting")
        shinyjs::show("dt_reg_downloadTabUI")
        shinyjs::show("dt_reg_reference")
        
        shinyjs::show("rf_cla_data_input")
        shinyjs::show("rf_cla_covariate")
        shinyjs::show("rf_cla_train_setting")
        shinyjs::show("rf_cla_downloadTabUI")
        shinyjs::show("rf_cla_reference")
        
        shinyjs::show("rf_reg_data_input")
        shinyjs::show("rf_reg_covariate")
        shinyjs::show("rf_reg_train_setting")
        shinyjs::show("rf_reg_downloadTabUI")
        shinyjs::show("rf_reg_reference")
        
        shinyjs::show("xgb_cla_data_input")
        shinyjs::show("xgb_cla_covariate")
        shinyjs::show("xgb_cla_train_setting")
        shinyjs::show("xgb_cla_downloadTabUI")
        shinyjs::show("xgb_cla_reference")
        
        shinyjs::show("xgb_reg_data_input")
        shinyjs::show("xgb_reg_covariate")
        shinyjs::show("xgb_reg_train_setting")
        shinyjs::show("xgb_reg_downloadTabUI")
        shinyjs::show("xgb_reg_reference")
      })
  })
  
  ## (2-1) DT - Classification -------------------
  observeEvent(input$dt_cla_runButton, {
    shinyjs::disable("dt_cla_data_input")
    shinyjs::disable("dt_cla_response")
    shinyjs::disable("dt_cla_covariate_yn")
    shinyjs::disable("dt_cla_covariate_options")
    shinyjs::disable("dt_cla_nfold")
    shinyjs::disable("dt_cla_loss")
    shinyjs::disable("dt_cla_cov_loss")
    shinyjs::disable("dt_cla_minsplit")
    shinyjs::disable("dt_cla_minbucket")
    shinyjs::disable("dt_cla_include_species")
    
    shinyjs::disable("dt_cla_runButton")
    shinyjs::disable("dt_reg_runButton")
    shinyjs::disable("rf_cla_runButton")
    shinyjs::disable("rf_reg_runButton")
    shinyjs::disable("xgb_cla_runButton")
    shinyjs::disable("xgb_reg_runButton")
    
    shinyjs::disable("dt_cla_chatgpt_run")
    shinyjs::disable("dt_reg_chatgpt_run")
    shinyjs::disable("rf_cla_chatgpt_run")
    shinyjs::disable("rf_reg_chatgpt_run")
    shinyjs::disable("xgb_cla_chatgpt_run")
    shinyjs::disable("xgb_reg_chatgpt_run")
    
    withProgress(
      message = "Calculation in progress",
      detail = "This may take a while...", value = 0, {
        
        if (input$dt_cla_dataType == "Count (Rarefied)") {
          dt_cla_type = "rare.count"
          dt_cla_ref = DT_REFERENCE_RC
        }
        else if (input$dt_cla_dataType == "Proportion") {
          dt_cla_type = "prop"
          dt_cla_ref = DT_REFERENCE
        }
        else if (input$dt_cla_dataType == "CLR (Default)") {
          dt_cla_type = "clr"
          dt_cla_ref = DT_REFERENCE_CLR
        }
        else if(input$dt_cla_dataType == "Arcsine-root"){
          dt_cla_type = "arcsin"
          dt_cla_ref = DT_REFERENCE
        }
        
        if(input$dt_cla_include_species == "Phylum - Genus (Default)"){
          level.names = get.level.names(include = FALSE)
        }
        else if(input$dt_cla_include_species == "Phylum - Species"){
          level.names = get.level.names(include = TRUE)
        }
        
        taxa.out <- chooseData$taxa.out
        
        for(j in 1:6){
          for(name in level.names){
            names(taxa.out[[j]][[name]]) <- chooseData$taxa.names.out$names[[name]]
          }
        }
        data <- taxa.out[[dt_cla_type]]
        colnames.list.cla <- colnames.to.ind(data)
        data <- change.colnames(data, colnames.list.cla$new)
        cov <- input$dt_cla_covariate_yn
        
        if(cov == "Covariate(s)"){
          input.data <- try(cov.remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$dt_cla_response, cov.name = input$dt_cla_covariate_options, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          
          y.name <- input$dt_cla_response
          sam.dat.na <- try(check.column.class(sam.dat.na), silent = TRUE)
          sam.dat.na <- try(cov.logistic.reg(sam.dat.na, y.name), silent = TRUE)
          y.name <- "resid"
          nfold <- ifelse(input$dt_cla_nfold == "LOOCV (Default)", lib.size.func(infile$qc_biom)$num.sams, as.numeric(input$dt_cla_nfold))
          
          dt.list <- list()
          for(name in level.names){
            incProgress(1/10, message = sprintf("Decision Tree: %s in Progress", str_to_title(name)))
            set.seed(578)
            dt.list[[name]] <- try(dt.reg(data = data,
                                          sam.dat.na = sam.dat.na,
                                          y.name = y.name,
                                          minsplit = as.numeric(input$dt_cla_minsplit),
                                          minbucket = as.numeric(input$dt_cla_minbucket),
                                          nfold = nfold,
                                          name = name,
                                          p = 0.8),
                                   silent = TRUE)
          }
        }
        else{
          input.data <- try(remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$dt_cla_response, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          y.name <- input$dt_cla_response
          nfold <- ifelse(input$dt_cla_nfold == "LOOCV (Default)", lib.size.func(infile$qc_biom)$num.sams, as.numeric(input$dt_cla_nfold))
          
          dt.list <- list()
          for(name in level.names){
            incProgress(1/10, message = sprintf("Decision Tree: %s in Progress", str_to_title(name)))
            set.seed(578)
            dt.list[[name]] <- try(dt.cla(data = data,
                                          sam.dat.na = sam.dat.na,
                                          y.name = y.name,
                                          split.method = dt.model.input.cla$split,
                                          minsplit = as.numeric(input$dt_cla_minsplit),
                                          minbucket = as.numeric(input$dt_cla_minbucket),
                                          nfold = nfold,
                                          name = name,
                                          p = 0.8),
                                   silent = TRUE)
          }
        }
        
        dt.summary.table.list <- list()
        dt.var.used.table.list <- list()
        dt.plot.cp.list <- list()
        
        for(name in level.names){
          if(cov == "Covariate(s)"){
            dt.summary.table.list[[name]] <- try(dt.summary.table(dt.list, name, type = "reg", y.var = sam.dat.na[[y.name]]), silent = TRUE)
          }
          else{
            dt.summary.table.list[[name]] <- try(dt.summary.table(dt.list, name, type = "cla", y.var = sam.dat.na[[y.name]]), silent = TRUE)
          }
          dt.var.used.table.list[[name]] <- try(dt.used.var(dt.list, colnames.list.cla, name), silent = TRUE)
        }
        
        incProgress(2/10, message = "Plotting models in progress")
        output$dt_cla_results <- renderUI({
          tagList(
            do.call(tabsetPanel, lapply(1:length(level.names), function(i) {
              tabPanel(title = str_to_title(level.names[i]), align = "center",
                       tabsetPanel(tabPanel(title = "Tree", align = "center", br(),
                                            plotOutput(paste0("dt_cla_fancy_tree", i), height = 750, width = 850), br(),
                                            dataTableOutput(paste0("dt_cla_column_table", i), height = "auto", width = 600), br(), br(),
                                            dataTableOutput(paste0("dt_cla_summary_table", i), height = 400, width = 900)),
                                   tabPanel(title = "CV Error", align = "center", br(),
                                            plotOutput(paste0("dt_cla_tuned_plot", i), height = 600, width = 500)),
                                   tabPanel(title = "Test Error", align = "left", br(),
                                            uiOutput(paste0("dt_cla_test_error", i)))))})),
            uiOutput("dt_cla_chatgpt_ui")
          )
        })
        
        lapply(1:length(level.names), function(j) {
          output[[paste0("dt_cla_fancy_tree", j)]] <- renderPlot({
            if(cov == "Covariate(s)"){
              tryCatch(dt.fancy.plot(dt.list, level.names[j], type = "reg"), error = function(e){
                message("Visualization not available! Check the input.")
                showModal(modalDialog(div("Visualization not available! Check the input.")))
                return(NULL)
              })
            }
            else{
              tryCatch(dt.fancy.plot(dt.list, level.names[j], type = "cla"), error = function(e){
                message("Visualization not available! Check the input.")
                showModal(modalDialog(div("Visualization not available! Check the input.")))
                return(NULL)
              })
            }
          })
          
          output[[paste0("dt_cla_summary_table", j)]] <- renderDataTable({
            tryCatch(dt.summary.table.list[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
            
          })
          
          output[[paste0("dt_cla_column_table", j)]] <- renderDataTable({
            tryCatch(dt.var.used.table.list[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("dt_cla_tuned_plot", j)]] <- renderPlot({
            tryCatch(dt.plotcp(dt.list, level.names[j], minline = TRUE, lty = 3, col = 2), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          if(cov == "Covariate(s)"){
            output[[paste0("dt_cla_test_error", j)]] <- renderUI({
              dt_cla_mse <- try(get.mse(predict(dt.list[[level.names[j]]]$final.model, dt.list[[level.names[j]]]$test$x), dt.list[[level.names[j]]]$test$y), silent = TRUE)
              dt_cla_mae <- try(get.mae(predict(dt.list[[level.names[j]]]$final.model, dt.list[[level.names[j]]]$test$x), dt.list[[level.names[j]]]$test$y), silent = TRUE)
              tagList(
                tags$ul(
                  tags$li(p(paste("Test MSE:", try(format(round(dt_cla_mse, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt")),
                  tags$li(p(paste("Test MAE:", try(format(round(dt_cla_mae, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt"))
                  , style = "font-size:12pt"),
                p("", style = "margin-bottom: 15pt")
              )
            })
          }
          else{
            output[[paste0("dt_cla_test_error", j)]] <- renderUI({
              dt_cla_err <- try(1-get.acc(predict(dt.list[[level.names[j]]]$final.model, dt.list[[level.names[j]]]$test$x), dt.list[[level.names[j]]]$test$y), silent = TRUE)
              dt_cla_auc <- try(get.auc(predict(dt.list[[level.names[j]]]$final.model, dt.list[[level.names[j]]]$test$x, type = "prob")[,2], dt.list[[level.names[j]]]$test$y), silent = TRUE)
              tagList(
                tags$ul(
                  tags$li(p(paste("Test Misclassification Error:", try(format(round(dt_cla_err, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt")),
                  tags$li(p(paste("Test AUC:", try(format(round(dt_cla_auc, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt"))
                  , style = "font-size:12pt"),
                p("", style = "margin-bottom: 15pt")
              )
            })
          }
          
          output$dt_cla_downloadTabUI <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("Download Output Table", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p("You can download the data analysis outputs.",
                    style = "font-size:11pt"),
                  h5("Importance"),
                  downloadButton("dt_cla_downloadTable_imp", "Download", width = '50%', style = "background-color: red3"),
                  h5("Variable Names"),
                  downloadButton("dt_cla_downloadTable_varnames", "Download", width = '50%', style = "background-color: red3"),
                  h5("Summary Table"),
                  downloadButton("dt_cla_downloadTable_summary", "Download", width = '50%', style = "background-color: red3")
              )
            )
          })
          
          output$dt_cla_downloadTable_imp <- downloadHandler(
            filename = function() {
              paste("DT_Importance.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.importance.df(dt.list, level.names[i]), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.importance.df(dt.list, level.names[i]), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$dt_cla_downloadTable_varnames <- downloadHandler(
            filename = function() {
              paste("DT_Variable_Names.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.var.used.table.list[[level.names[i]]], silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.var.used.table.list[[level.names[i]]], silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$dt_cla_downloadTable_summary <- downloadHandler(
            filename = function() {
              paste("DT_summary.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.summary.table.list[[level.names[i]]], silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.summary.table.list[[level.names[i]]], silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$dt_cla_reference <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("References", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p(dt_cla_ref, style = "font-size:11pt")
              )
            )
          })
        })
        
        dt_cla_imp_var_list <- list()
        for(name in level.names){
          dt_cla_imp_var_list[[name]] <- try(dt.used.var.names(dt.list, name, colnames.list.cla), silent = TRUE)
        }
        output$dt_cla_chatgpt_ui <- renderUI({
          tagList(
            box(id = "dt_cla_chat", title = strong("Ask ChatGPT: Tell me about the roles of (a microbial taxon) on (a human disease).", style = "color:white", side = "left"), width = NULL, 
                solidHeader = TRUE, status = "info",
                column(width = 6.5, 
                       h5(strong("API Key"), style = "color:black;margin-top:-1px;"),
                       p("", style = "margin-bottom: -4px;"),
                       p("You need to create your own secret API key to run ChatGPT. Here is the link to create an API key. (https://platform.openai.com/account/api-keys) ", style = "font-size:10pt;"),
                       p("", style = "margin-bottom: -4px;"),
                       textInput("dt_cla_chatgpt_key", label = NULL, value = NULL, width = '55%'), 
                       p(" ", style = "margin-bottom: +20px;"),
                       selectInput("dt_cla_chatgpt_rank", "Select a taxonomic rank", choices = str_to_title(level.names), selected = str_to_title(level.names)[1], width = '55%'),
                       uiOutput("dt_cla_chatgpt_taxon"),
                       uiOutput("dt_cla_taxon_rename"),
                       uiOutput("dt_cla_response_var_rename"),
                       actionButton("dt_cla_chatgpt_run", (strong("Ask!")), class = "btn-info", style = "margin-bottom: -20px")), br(), 
                column(width = 4.4, uiOutput("dt_cla_chatgpt_vis"))
            )
          )})
        
        output$dt_cla_chatgpt_taxon <- renderUI({
          tagList(
            p(" ", style = "margin-top: +15px;"),
            selectInput("dt_cla_chatgpt_taxon", "Select an important taxon (e.g., disease-predictor in the importance plot).", choices = dt_cla_imp_var_list[[str_to_lower(input$dt_cla_chatgpt_rank)]], selected = NULL, width = '55%')
          )
        })
        
        output$dt_cla_taxon_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +18px;"),
            h5(strong("Rename the taxon"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language, for instance, deleting possible codes, special symbols or numbers (e.g., from 'Saccharibacteria_(TM7)_[O-1]' to 'Saccharibacteria').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("dt_cla_rename_taxa", label = NULL, value = input$dt_cla_chatgpt_taxon, width = '55%'),
            p(" ", style = "margin-bottom: +10px;")
          )
        })
        
        output$dt_cla_response_var_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +23px;"),
            h5(strong("Rename the output variable"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language (e.g., from 'gingival_inflammation' to 'gingival inflammation').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("dt_cla_response_var_rename", label = NULL, value = input$dt_cla_response, width = '55%')
          )
        })
      }
    )
    shinyjs::enable("dt_cla_dataType")
    shinyjs::enable("dt_cla_response")
    shinyjs::enable("dt_cla_covariate_yn")
    shinyjs::enable("dt_cla_covariate_options")
    shinyjs::enable("dt_cla_nfold")
    shinyjs::enable("dt_cla_loss")
    shinyjs::enable("dt_cla_cov_loss")
    shinyjs::enable("dt_cla_minsplit")
    shinyjs::enable("dt_cla_minbucket")
    shinyjs::enable("dt_cla_include_species")
    
    shinyjs::enable("dt_cla_runButton")
    shinyjs::enable("dt_reg_runButton")
    shinyjs::enable("rf_cla_runButton")
    shinyjs::enable("rf_reg_runButton")
    shinyjs::enable("xgb_cla_runButton")
    shinyjs::enable("xgb_reg_runButton")
    
    shinyjs::enable("dt_cla_chatgpt_run")
    shinyjs::enable("dt_reg_chatgpt_run")
    shinyjs::enable("rf_cla_chatgpt_run")
    shinyjs::enable("rf_reg_chatgpt_run")
    shinyjs::enable("xgb_cla_chatgpt_run")
    shinyjs::enable("xgb_reg_chatgpt_run")
  })
  
  observeEvent(input$dt_cla_chatgpt_run, {
    withProgress(
      message = "Asking in progress",
      detail = "This may take a while...", value = 0, {
        incProgress(0.5, message = "Asking Chat GPT")
        
        taxa.name <- input$dt_cla_rename_taxa
        var.name <- input$dt_cla_response_var_rename
        api.key <- input$dt_cla_chatgpt_key
        
        new.taxa.name <- gsub("[[:punct:]]", "+", taxa.name)
        new.taxa.name <- gsub(" ", "+", new.taxa.name)
        
        new.var.name <- gsub("[[:punct:]]", "+", var.name)
        new.var.name <- gsub(" ", "+", new.var.name)
        
        chat_result <- tryCatch(chat_gpt_MiTree(taxa.name, var.name, api.key), error = function(e){
          message("Invalid or incorrect API key. Please check it again.")
          showModal(modalDialog(div("Invalid or incorrect API key. Please check it again.")))
          return(NULL)
        })
        
        if(!is.null(chat_result)){
          output$dt_cla_chatgpt_vis <-renderUI({
            tagList(br(), strong(paste("Tell me about the roles of a", taxa.name, "on a", var.name)), 
                    p(chat_result, style = "margin-top: 10px"), br(),
                    
                    strong("Reference Search Results"), br(), 
                    
                    p("Google Scholar", style = "margin-top: 10px"), 
                    p(tags$a(href = paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), target = "_blank")), 
                    
                    p("PubMed"), 
                    p(tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), target = "_blank")))
          })
        }
        
      }
    )
  })
  
  ## (2-2) DT - Regression -------------------
  observeEvent(input$dt_reg_runButton, {
    shinyjs::disable("dt_reg_dataType")
    shinyjs::disable("dt_reg_response")
    shinyjs::disable("dt_reg_covariate_yn")
    shinyjs::disable("dt_reg_covariate_options")
    shinyjs::disable("dt_reg_loss")
    shinyjs::disable("dt_reg_nfold")
    shinyjs::disable("dt_reg_minsplit")
    shinyjs::disable("dt_reg_minbucket")
    shinyjs::disable("dt_reg_include_species")
    
    shinyjs::disable("dt_cla_runButton")
    shinyjs::disable("dt_reg_runButton")
    shinyjs::disable("rf_cla_runButton")
    shinyjs::disable("rf_reg_runButton")
    shinyjs::disable("xgb_cla_runButton")
    shinyjs::disable("xgb_reg_runButton")
    
    shinyjs::disable("dt_cla_chatgpt_run")
    shinyjs::disable("dt_reg_chatgpt_run")
    shinyjs::disable("rf_cla_chatgpt_run")
    shinyjs::disable("rf_reg_chatgpt_run")
    shinyjs::disable("xgb_cla_chatgpt_run")
    shinyjs::disable("xgb_reg_chatgpt_run")
    
    withProgress(
      message = "Calculation in progress",
      detail = "This may take a while...", value = 0, {
        
        if (input$dt_reg_dataType == "Count (Rarefied)") {
          dt_reg_type = "rare.count"
          dt_reg_ref = DT_REFERENCE_RC
        }
        else if (input$dt_reg_dataType == "Proportion") {
          dt_reg_type = "prop"
          dt_reg_ref = DT_REFERENCE
        }
        else if (input$dt_reg_dataType == "CLR (Default)") {
          dt_reg_type = "clr"
          dt_reg_ref = DT_REFERENCE_CLR
        }
        else if(input$dt_reg_dataType == "Arcsine-root"){
          dt_reg_type = "arcsin"
          dt_reg_ref = DT_REFERENCE
        }
        
        if(input$dt_reg_include_species == "Phylum - Genus (Default)"){
          level.names = get.level.names(include = FALSE)
        }
        else if(input$dt_reg_include_species == "Phylum - Species"){
          level.names = get.level.names(include = TRUE)
        }
        
        taxa.out <- chooseData$taxa.out
        
        for(j in 1:6){
          for(name in level.names){
            names(taxa.out[[j]][[name]]) <- chooseData$taxa.names.out$names[[name]]
          }
        }
        data <- taxa.out[[dt_reg_type]]
        colnames.list.reg <- colnames.to.ind(data)
        data <- change.colnames(data, colnames.list.reg$new)
        
        if (input$dt_reg_covariate_yn == "Covariate(s)"){
          input.data <- try(cov.remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$dt_reg_response, cov.name = input$dt_reg_covariate_options, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          
          y.name <- input$dt_reg_response
          sam.dat.na <- check.column.class(sam.dat.na)
          sam.dat.na <- try(cov.linear.reg(sam.dat.na, y.name), silent = TRUE)
          y.name <- "resid"
        }
        else{
          input.data <- try(remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$dt_reg_response, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          y.name <- input$dt_reg_response
        }
        nfold <- ifelse(input$dt_reg_nfold == "LOOCV (Default)", lib.size.func(infile$qc_biom)$num.sams, as.numeric(input$dt_reg_nfold))
        
        fit <- list()
        for(name in level.names){
          incProgress(1/10, message = sprintf("Decision Tree: %s in Progress", str_to_title(name)))
          set.seed(578)
          fit[[name]] <- try(dt.reg(data = data,
                                    sam.dat.na = sam.dat.na,
                                    y.name = y.name,
                                    minsplit = as.numeric(input$dt_reg_minsplit),
                                    minbucket = as.numeric(input$dt_reg_minbucket),
                                    nfold = nfold,
                                    name = name,
                                    p = 0.8),
                             silent = TRUE)
        }
        
        dt.summary.table.list <- list()
        dt.var.used.table.list <- list()
        dt.plot.cp.list <- list()
        dt.test.error.reg <- list()
        
        for(name in level.names){
          dt.summary.table.list[[name]] <- try(dt.summary.table(fit, name, type = "reg", y.var = sam.dat.na[[y.name]]), silent = TRUE)
          dt.var.used.table.list[[name]] <- try(dt.used.var(fit, colnames.list.reg, name), silent = TRUE)
        }
        
        incProgress(2/10, message = "Plotting models in progress")
        output$dt_reg_results <- renderUI({
          tagList(
            do.call(tabsetPanel, lapply(1:length(level.names), function(i) {
              tabPanel(title = str_to_title(level.names[i]), align = "center",
                       tabsetPanel(tabPanel(title = "Tree", align = "center", br(),
                                            plotOutput(paste0("dt_reg_fancy_tree", i), height = 750, width = 850), br(),
                                            dataTableOutput(paste0("dt_reg_column_table", i), height = "auto", width = 600), br(), br(),
                                            dataTableOutput(paste0("dt_reg_summary_table", i), height = 400, width = 900),
                                            p(" ", style = "margin-bottom: -175px;")),
                                   tabPanel(title = "CV Error", align = "center", br(),
                                            plotOutput(paste0("dt_reg_tuned_plot", i), height = 600, width = 500)),
                                   tabPanel(title = "Test Error", align = "left", br(),
                                            uiOutput(paste0("dt_reg_test_error", i)))))})),
            uiOutput("dt_reg_chatgpt_ui")
          )
        })
        
        lapply(1:length(level.names), function(j) {
          output[[paste0("dt_reg_fancy_tree", j)]] <- renderPlot({
            tryCatch(dt.fancy.plot(fit, level.names[j], type = "reg"), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("dt_reg_summary_table", j)]] <- renderDataTable({
            tryCatch(dt.summary.table.list[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("dt_reg_column_table", j)]] <- renderDataTable({
            tryCatch(dt.var.used.table.list[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("dt_reg_tuned_plot", j)]] <- renderPlot({
            tryCatch(dt.plotcp(fit, level.names[j], minline = TRUE, lty = 3, col = 2), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("dt_reg_test_error", j)]] <- renderUI({
            dt_reg_mse <- try(get.mse(predict(fit[[level.names[j]]]$final.model, fit[[level.names[j]]]$test$x), fit[[level.names[j]]]$test$y), silent = TRUE)
            dt_reg_mae <- try(get.mae(predict(fit[[level.names[j]]]$final.model, fit[[level.names[j]]]$test$x), fit[[level.names[j]]]$test$y), silent = TRUE)
            tagList(
              tags$ul(
                tags$li(p(paste("Test MSE:", try(format(round(dt_reg_mse, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt")),
                tags$li(p(paste("Test MAE:", try(format(round(dt_reg_mae, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt"))
                , style = "font-size:12pt")
            )
          })
          
          output$dt_reg_downloadTabUI <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("Download Output Table", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p("You can download the data analysis outputs.",
                    style = "font-size:11pt"),
                  h5("Importance"),
                  downloadButton("dt_reg_downloadTable_imp", "Download", width = '50%', style = "background-color: red3"),
                  h5("Variable Names"),
                  downloadButton("dt_reg_downloadTable_varnames", "Download", width = '50%', style = "background-color: red3"),
                  h5("Summary Table"),
                  downloadButton("dt_reg_downloadTable_summary", "Download", width = '50%', style = "background-color: red3")
              )
            )
          })
          
          output$dt_reg_downloadTable_imp <- downloadHandler(
            filename = function() {
              paste("DT_Importance.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.importance.df(fit, level.names[i]), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.importance.df(fit, level.names[i]), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$dt_reg_downloadTable_varnames <- downloadHandler(
            filename = function() {
              paste("DT_Variable_Names.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.var.used.table.list[[level.names[j]]], silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.var.used.table.list[[level.names[j]]], silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$dt_reg_downloadTable_summary <- downloadHandler(
            filename = function() {
              paste("DT_summary.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.summary.table.list[[level.names[j]]], silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(dt.summary.table.list[[level.names[j]]], silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$dt_reg_reference <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("References", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p(dt_reg_ref, style = "font-size:11pt")
              )
            )
          })
        })
        
        dt_reg_imp_var_list <- list()
        for(name in level.names){
          dt_reg_imp_var_list[[name]] <- try(dt.used.var.names(fit, name, colnames.list.reg), silent = TRUE)
        }
        output$dt_reg_chatgpt_ui <- renderUI({
          tagList(
            br(),
            box(id = "dt_reg_chat", title = strong("Ask ChatGPT: Tell me about the roles of (a microbial taxon) on (a human disease).", style = "color:white", side = "left"), width = NULL, 
                solidHeader = TRUE, status = "info",
                column(width = 6.5, 
                       h5(strong("API Key"), style = "color:black; margin-top: -1px"),
                       p("", style = "margin-bottom: -4px;"),
                       p("You need to create your own secret API key to run ChatGPT. Here is the link to create an API key. (https://platform.openai.com/account/api-keys) ", style = "font-size:10pt;"),
                       p("", style = "margin-bottom: -4px;"),
                       textInput("dt_reg_chatgpt_key", label = NULL, value = NULL, width = '55%'),
                       p(" ", style = "margin-bottom: +20px;"),
                       selectInput("dt_reg_chatgpt_rank", "Select a taxonomic rank", choices = str_to_title(level.names), selected = str_to_title(level.names)[1], width = '55%'),
                       uiOutput("dt_reg_chatgpt_taxon"),
                       uiOutput("dt_reg_taxon_rename"),
                       uiOutput("dt_reg_response_var_rename"),
                       actionButton("dt_reg_chatgpt_run", (strong("Ask!")), class = "btn-info", style = "margin-bottom: -20px")), br(), 
                column(width = 4.4, uiOutput("dt_reg_chatgpt_vis"))
            )
          )})
        
        output$dt_reg_chatgpt_taxon <- renderUI({
          tagList(
            p(" ", style = "margin-top: +15px;"),
            selectInput("dt_reg_chatgpt_taxon", "Select an important taxon (e.g., disease-predictor in the importance plot).", choices = dt_reg_imp_var_list[[str_to_lower(input$dt_reg_chatgpt_rank)]], selected = NULL, width = '55%')
          )
        })
        
        output$dt_reg_taxon_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +18px;"),
            h5(strong("Rename the taxon"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language, for instance, deleting possible codes, special symbols or numbers (e.g., from 'Saccharibacteria_(TM7)_[O-1]' to 'Saccharibacteria').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("dt_reg_rename_taxa", label = NULL, value = input$dt_reg_chatgpt_taxon, width = '55%'),
            p(" ", style = "margin-bottom: +10px;")
          )
        })
        
        output$dt_reg_response_var_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +23px;"),
            h5(strong("Rename the output variable"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language (e.g., from 'gingival_inflammation' to 'gingival inflammation').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("dt_reg_response_var_rename", label = NULL, value = input$dt_reg_response, width = '55%')
          )
        })
      }
    )
    shinyjs::enable("dt_reg_dataType")
    shinyjs::enable("dt_reg_response")
    shinyjs::enable("dt_reg_covariate_yn")
    shinyjs::enable("dt_reg_covariate_options")
    shinyjs::enable("dt_reg_loss")
    shinyjs::enable("dt_reg_nfold")
    shinyjs::enable("dt_reg_minsplit")
    shinyjs::enable("dt_reg_minbucket")
    shinyjs::enable("dt_reg_include_species")
    
    shinyjs::enable("dt_cla_runButton")
    shinyjs::enable("dt_reg_runButton")
    shinyjs::enable("rf_cla_runButton")
    shinyjs::enable("rf_reg_runButton")
    shinyjs::enable("xgb_cla_runButton")
    shinyjs::enable("xgb_reg_runButton")
    
    shinyjs::enable("dt_cla_chatgpt_run")
    shinyjs::enable("dt_reg_chatgpt_run")
    shinyjs::enable("rf_cla_chatgpt_run")
    shinyjs::enable("rf_reg_chatgpt_run")
    shinyjs::enable("xgb_cla_chatgpt_run")
    shinyjs::enable("xgb_reg_chatgpt_run")
  })
  
  observeEvent(input$dt_reg_chatgpt_run, {
    withProgress(
      message = "Asking in progress",
      detail = "This may take a while...", value = 0, {
        incProgress(0.5, message = "Asking Chat GPT")
        
        taxa.name <- input$dt_reg_rename_taxa
        var.name <- input$dt_reg_response_var_rename
        api.key <- input$dt_reg_chatgpt_key
        
        new.taxa.name <- gsub("[[:punct:]]", "+", taxa.name)
        new.taxa.name <- gsub(" ", "+", new.taxa.name)
        
        new.var.name <- gsub("[[:punct:]]", "+", var.name)
        new.var.name <- gsub(" ", "+", new.var.name)
        
        chat_result <- tryCatch(chat_gpt_MiTree(taxa.name, var.name, api.key), error = function(e){
          message("Invalid or incorrect API key. Please check it again.")
          showModal(modalDialog(div("Invalid or incorrect API key. Please check it again.")))
          return(NULL)
        })
        
        if(!is.null(chat_result)){
          output$dt_reg_chatgpt_vis <-renderUI({
            tagList(br(), strong(paste("Tell me about the roles of a", taxa.name, "on a", var.name)), 
                    p(chat_result, style = "margin-top: 10px"), br(),
                    
                    strong("Reference Search Results"), br(), 
                    
                    p("Google Scholar", style = "margin-top: 10px"), 
                    p(tags$a(href = paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), target = "_blank")), 
                    
                    p("PubMed"), 
                    p(tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), target = "_blank")))
          })
        }
        
      }
    )
  })
  ## (3-1) RF - Classification -------------------
  observeEvent(input$rf_cla_runButton, {
    shinyjs::disable("rf_cla_dataType")
    shinyjs::disable("rf_cla_response")
    shinyjs::disable("rf_cla_covariate_yn")
    shinyjs::disable("rf_cla_covariate_options")
    shinyjs::disable("rf_cla_loss")
    shinyjs::disable("rf_cla_cov_loss")
    shinyjs::disable("rf_cla_nfold")
    shinyjs::disable("rf_cla_ntree")
    shinyjs::disable("rf_cla_var_num")
    shinyjs::disable("rf_cla_include_species")
    
    shinyjs::disable("dt_cla_runButton")
    shinyjs::disable("dt_reg_runButton")
    shinyjs::disable("rf_cla_runButton")
    shinyjs::disable("rf_reg_runButton")
    shinyjs::disable("xgb_cla_runButton")
    shinyjs::disable("xgb_reg_runButton")
    
    shinyjs::disable("dt_cla_chatgpt_run")
    shinyjs::disable("dt_reg_chatgpt_run")
    shinyjs::disable("rf_cla_chatgpt_run")
    shinyjs::disable("rf_reg_chatgpt_run")
    shinyjs::disable("xgb_cla_chatgpt_run")
    shinyjs::disable("xgb_reg_chatgpt_run")
    
    withProgress(
      message = "Calculation in progress",
      detail = "This may take a while...", value = 0, {
        if (input$rf_cla_dataType == "Count (Rarefied)") {
          type = "rare.count"
          ref = RF_REFERENCE_RC
        }
        else if (input$rf_cla_dataType == "Proportion") {
          type = "prop"
          ref = RF_REFERENCE
        }
        else if (input$rf_cla_dataType == "CLR (Default)") {
          type = "clr"
          ref = RF_REFERENCE_CLR
        }
        else if(input$rf_cla_dataType == "Arcsine-root"){
          type = "arcsin"
          ref = RF_REFERENCE
        }
        
        if(input$rf_cla_include_species == "Phylum - Genus (Default)"){
          level.names = get.level.names(include = FALSE)
        }
        else if(input$rf_cla_include_species == "Phylum - Species"){
          level.names = get.level.names(include = TRUE)
        }
        
        taxa.out <- chooseData$taxa.out
        
        for(j in 1:6){
          for(name in level.names){
            names(taxa.out[[j]][[name]]) <- chooseData$taxa.names.out$names[[name]]
          }
        }
        data <- taxa.out[[type]]
        colnames.list.cla <- colnames.to.ind(data)
        data <- change.colnames(data, colnames.list.cla$new)
        
        if(input$rf_cla_covariate_yn == "Covariate(s)"){
          input.data <- try(cov.remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$rf_cla_response, cov.name = input$rf_cla_covariate_options,level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          
          y.name <- input$rf_cla_response
          nfold <- as.numeric(input$rf_cla_nfold)
          ntree <- as.numeric(input$rf_cla_ntree)
          
          sam.dat.na <- try(check.column.class(sam.dat.na), silent = TRUE)
          sam.dat.na <- try(cov.logistic.reg(sam.dat.na, y.name), silent = TRUE)
          y.name <- "resid"
          
          rf.list <- list()
          for(name in level.names){
            incProgress(1/10, message = sprintf("Random Forest: %s in progress", str_to_title(name)))
            set.seed(578)
            rf.list[[name]] <- try(rf.reg.rev(data = data,
                                              sam.dat.na = sam.dat.na,
                                              y.name = y.name,
                                              nfold = nfold,
                                              ntree = ntree,
                                              name = name,
                                              p = 0.8), 
                                   silent = TRUE)
          }
        }
        else{
          input.data <- try(remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$rf_cla_response, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          
          y.name <- input$rf_cla_response
          cat.name <- category.names(sam.dat.na, y.name)
          nfold <- as.numeric(input$rf_cla_nfold)
          ntree <- as.numeric(input$rf_cla_ntree)
          
          rf.list <- list()
          for(name in level.names){
            incProgress(1/10, message = sprintf("Random Forest: %s in progress", str_to_title(name)))
            set.seed(578)
            rf.list[[name]] <- try(rf.cla.rev(data = data,
                                              sam.dat.na = sam.dat.na,
                                              y.name = y.name,
                                              nfold = nfold,
                                              ntree = ntree,
                                              stratified = TRUE,
                                              name = name,
                                              p = 0.8), 
                                   silent = TRUE)
          }
        }
        
        incProgress(2/10, message = "Visualizations in progress")
        
        rf.imp.plot.list <- list()
        pd.plot.list <- list()
        is.cat <- ifelse(input$rf_cla_covariate_yn == "Covariate(s)", FALSE, TRUE)
        
        for(name in level.names){
          if(input$rf_cla_covariate_yn == "Covariate(s)"){
            rf.imp.plot.list[[name]] <- try(rf.imp.plot(rf.list, name, n = as.numeric(input$rf_cla_var_num), type = 1, is.cat = is.cat, data = data, data.type = type), silent = TRUE)
            pd.plot.list[[name]] <- try(rf.pdp.reg(rf.list, n = as.numeric(input$rf_cla_var_num), name = name, data.type = type), silent = TRUE)
          }
          else{
            rf.imp.plot.list[[name]] <- try(rf.imp.plot(rf.list, name, n = as.numeric(input$rf_cla_var_num), type = 2, is.cat = is.cat, data = data, data.type = type), silent = TRUE)
            pd.plot.list[[name]] <- try(rf.pdp.bin(rf.list, n = as.numeric(input$rf_cla_var_num), name = name, data.type = type, cat.name = cat.name), silent = TRUE)
          }
        }
        
        rf.width.list <- list()
        for(name in level.names){
          if(as.numeric(input$rf_cla_var_num) > ncol(data[[name]])){
            n <- ncol(data[[name]])
            if(n %% 5 == 0){
              n <- n / 5
            }
            else{
              n <- (n / 5) + 1
            }
          }
          else{
            n <- as.numeric(input$rf_cla_var_num)
            n <- n / 5
          }
          rf.width.list[[name]] <- n * 160
        }
        
        output$rf_cla_results <- renderUI({
          tagList(
            do.call(tabsetPanel, lapply(1:length(level.names), function(i) {
              tabPanel(title = str_to_title(level.names[i]), align = "center",
                       tabsetPanel(tabPanel(title = "Importance", align = "center", br(),
                                            plotOutput(paste0("rf_cla_imp", i), height = 700, width = 700), br(),
                                            dataTableOutput(paste0("rf_cla_column_table1", i), height = "auto", width = 700)),
                                   tabPanel(title = "Partial Dependence", align = "center", br(),
                                            plotOutput(paste0("rf_cla_pd", i), height = 750, width = rf.width.list[[i]]), br(),
                                            dataTableOutput(paste0("rf_cla_column_table2", i), height = "auto", width = 700)),
                                   tabPanel(title = "CV Error", align = "center", br(),
                                            plotOutput(paste0("rf_cla_cv", i), height = 700, width = 700)),
                                   tabPanel(title = "OOB Error", align = "center", br(),
                                            plotOutput(paste0("rf_cla_oob", i), height = 700, width = 700)),
                                   tabPanel(title = "Test Error", align = "left", br(),
                                            uiOutput(paste0("rf_cla_test_error", i)))))})),
            uiOutput("rf_cla_chatgpt_ui")
          )
        })
        
        lapply(1:length(level.names), function(j) {
          output[[paste0("rf_cla_imp", j)]] <- renderPlot({
            tryCatch(rf.imp.plot.list[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_cla_pd", j)]] <- renderPlot({
            tryCatch(pd.plot.list[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_cla_column_table1", j)]] <- renderDataTable({
            tryCatch(rf.pd.var.used(rf.list, level.names[[j]], colnames.list.cla, n = as.numeric(input$rf_cla_var_num), is.cat = is.cat), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
         
          output[[paste0("rf_cla_column_table2", j)]] <- renderDataTable({
            tryCatch(rf.pd.var.used(rf.list, level.names[[j]], colnames.list.cla, n = as.numeric(input$rf_cla_var_num), is.cat = is.cat), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_cla_cv", j)]] <- renderPlot({
            tryCatch(cv.mtry(rf.list, level.names[j], is.cat = is.cat), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_cla_oob", j)]] <- renderPlot({
            tryCatch(error.plot(rf.list, level.names[j], is.cat = is.cat), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          if(input$rf_cla_covariate_yn == "Covariate(s)"){
            output[[paste0("rf_cla_test_error", j)]] <- renderUI({
              rf_cla_mse <- try(get.mse(predict(rf.list[[level.names[j]]]$fit, rf.list[[level.names[j]]]$test$x), rf.list[[level.names[j]]]$test$y), silent = TRUE)
              rf_cla_mae <- try(get.mae(predict(rf.list[[level.names[j]]]$fit, rf.list[[level.names[j]]]$test$x), rf.list[[level.names[j]]]$test$y), silent = TRUE)
              tagList(
                tags$ul(
                  tags$li(p(paste("Test MSE:", try(format(round(rf_cla_mse, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt")),
                  tags$li(p(paste("Test MAE:", try(format(round(rf_cla_mae, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt"))
                  , style = "font-size:12pt"),
                p("", style = "margin-bottom: 15pt")
              )
            })
          }
          else{
            output[[paste0("rf_cla_test_error", j)]] <- renderUI({
              rf_cla_err <- try(1-get.acc(predict(rf.list[[level.names[j]]]$fit, rf.list[[level.names[j]]]$test$x), rf.list[[level.names[j]]]$test$y), silent = TRUE)
              rf_cla_auc <- try(get.auc(predict(rf.list[[level.names[j]]]$fit, rf.list[[level.names[j]]]$test$x, type = "prob")[,2], rf.list[[level.names[j]]]$test$y), silent = TRUE)
              tagList(
                tags$ul(
                  tags$li(p(paste("Test Misclassification Error:", try(format(round(rf_cla_err, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt")),
                  tags$li(p(paste("Test AUC:", try(format(round(rf_cla_auc, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt"))
                  , style = "font-size:12pt"),
                p("", style = "margin-bottom: 15pt")
              )
            })
          }
          
          output$rf_cla_downloadTabUI <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("Download Output Table", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p("You can download the data analysis outputs.",
                    style = "font-size:11pt"),
                  h5("Feature Importance"),
                  downloadButton("rf_cla_downloadTable", "Download", width = '50%', style = "background-color: red3")
              )
            )
          })
          
          output$rf_cla_downloadTable <- downloadHandler(
            filename = function() {
              paste("RF_Importance.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(rf.imp.df(rf.list, level.names[i], type = 0), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(rf.imp.df(rf.list, level.names[i], type = 0), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$rf_cla_reference <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("References", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p(ref, style = "font-size:11pt")
              )
            )
          })
        })
        
        rf_cla_imp_var_list <- list()
        for(name in level.names){
          rf_cla_imp_var_list[[name]] <- try(rf.imp.df.order(rf.list, name, n = 10, colnames.list.cla, is.cat = is.cat)$names, silent = TRUE)
        }
        output$rf_cla_chatgpt_ui <- renderUI({
          tagList(
            br(),
            box(id = "rf_cla_chat", title = strong("Ask ChatGPT: Tell me about the roles of (a microbial taxon) on (a human disease).", style = "color:white", side = "left"), width = NULL, 
                solidHeader = TRUE, status = "info",
                column(width = 6.5, 
                       h5(strong("API Key"), style = "color:black; margin-top: -1px"),
                       p("", style = "margin-bottom: -4px;"),
                       p("You need to create your own secret API key to run ChatGPT. Here is the link to create an API key. (https://platform.openai.com/account/api-keys) ", style = "font-size:10pt;"),
                       p("", style = "margin-bottom: -4px;"),
                       textInput("rf_cla_chatgpt_key", label = NULL, value = NULL, width = '55%'),
                       p(" ", style = "margin-bottom: +20px;"),
                       selectInput("rf_cla_chatgpt_rank", "Select a taxonomic rank", choices = str_to_title(level.names), selected = str_to_title(level.names)[1], width = '55%'),
                       uiOutput("rf_cla_chatgpt_taxon"),
                       uiOutput("rf_cla_taxon_rename"),
                       uiOutput("rf_cla_response_var_rename"),
                       actionButton("rf_cla_chatgpt_run", (strong("Ask!")), class = "btn-info", style = "margin-bottom: -20px")), br(), 
                column(width = 4.4, uiOutput("rf_cla_chatgpt_vis"))
            )
          )})
        
        output$rf_cla_chatgpt_taxon <- renderUI({
          tagList(
            p(" ", style = "margin-top: +15px;"),
            selectInput("rf_cla_chatgpt_taxon", "Select an important taxon (e.g., disease-predictor in the importance plot).", choices = rf_cla_imp_var_list[[str_to_lower(input$rf_cla_chatgpt_rank)]], selected = NULL, width = '55%')
          )
        })
        
        output$rf_cla_taxon_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +18px;"),
            h5(strong("Rename the taxon"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language, for instance, deleting possible codes, special symbols or numbers (e.g., from 'Saccharibacteria_(TM7)_[O-1]' to 'Saccharibacteria').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("rf_cla_rename_taxa", label = NULL, value = input$rf_cla_chatgpt_taxon, width = '55%'),
            p(" ", style = "margin-bottom: +10px;")
          )
        })
        
        output$rf_cla_response_var_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +23px;"),
            h5(strong("Rename the output variable"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language (e.g., from 'gingival_inflammation' to 'gingival inflammation').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("rf_cla_response_var_rename", label = NULL, value = input$rf_cla_response, width = '55%')
          )
        })
      }
    )
    shinyjs::enable("rf_cla_dataType")
    shinyjs::enable("rf_cla_response")
    shinyjs::enable("rf_cla_covariate_yn")
    shinyjs::enable("rf_cla_covariate_options")
    shinyjs::enable("rf_cla_loss")
    shinyjs::enable("rf_cla_cov_loss")
    shinyjs::enable("rf_cla_nfold")
    shinyjs::enable("rf_cla_ntree")
    shinyjs::enable("rf_cla_var_num")
    shinyjs::enable("rf_cla_include_species")
    
    shinyjs::enable("dt_cla_runButton")
    shinyjs::enable("dt_reg_runButton")
    shinyjs::enable("rf_cla_runButton")
    shinyjs::enable("rf_reg_runButton")
    shinyjs::enable("xgb_cla_runButton")
    shinyjs::enable("xgb_reg_runButton")
    
    shinyjs::enable("dt_cla_chatgpt_run")
    shinyjs::enable("dt_reg_chatgpt_run")
    shinyjs::enable("rf_cla_chatgpt_run")
    shinyjs::enable("rf_reg_chatgpt_run")
    shinyjs::enable("xgb_cla_chatgpt_run")
    shinyjs::enable("xgb_reg_chatgpt_run")
  })
  
  observeEvent(input$rf_cla_chatgpt_run, {
    withProgress(
      message = "Asking in progress",
      detail = "This may take a while...", value = 0, {
        incProgress(0.5, message = "Asking Chat GPT")
        
        taxa.name <- input$rf_cla_rename_taxa
        var.name <- input$rf_cla_response_var_rename
        api.key <- input$rf_cla_chatgpt_key
        
        new.taxa.name <- gsub("[[:punct:]]", "+", taxa.name)
        new.taxa.name <- gsub(" ", "+", new.taxa.name)
        
        new.var.name <- gsub("[[:punct:]]", "+", var.name)
        new.var.name <- gsub(" ", "+", new.var.name)
        
        chat_result <- tryCatch(chat_gpt_MiTree(taxa.name, var.name, api.key), error = function(e){
          message("Invalid or incorrect API key. Please check it again.")
          showModal(modalDialog(div("Invalid or incorrect API key. Please check it again.")))
          return(NULL)
        })
        
        if(!is.null(chat_result)){
          output$rf_cla_chatgpt_vis <-renderUI({
            tagList(br(), strong(paste("Tell me about the roles of a", taxa.name, "on a", var.name)), 
                    p(chat_result, style = "margin-top: 10px"), br(),
                    
                    strong("Reference Search Results"), br(), 
                    
                    p("Google Scholar", style = "margin-top: 10px"), 
                    p(tags$a(href = paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), target = "_blank")), 
                    
                    p("PubMed"), 
                    p(tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), target = "_blank")))
          })
        }
      }
    )
  })
  
  ## (3-2) RF - Regression -------------------
  observeEvent(input$rf_reg_runButton, {
    shinyjs::disable("rf_reg_dataType")
    shinyjs::disable("rf_reg_response")
    shinyjs::disable("rf_reg_covariate_yn")
    shinyjs::disable("rf_reg_covariate_options")
    shinyjs::disable("rf_reg_loss")
    shinyjs::disable("rf_reg_nfold")
    shinyjs::disable("rf_reg_ntree")
    shinyjs::disable("rf_reg_var_num")
    shinyjs::disable("rf_reg_include_species")
    
    shinyjs::disable("dt_cla_runButton")
    shinyjs::disable("dt_reg_runButton")
    shinyjs::disable("rf_cla_runButton")
    shinyjs::disable("rf_reg_runButton")
    shinyjs::disable("xgb_cla_runButton")
    shinyjs::disable("xgb_reg_runButton")
    
    shinyjs::disable("dt_cla_chatgpt_run")
    shinyjs::disable("dt_reg_chatgpt_run")
    shinyjs::disable("rf_cla_chatgpt_run")
    shinyjs::disable("rf_reg_chatgpt_run")
    shinyjs::disable("xgb_cla_chatgpt_run")
    shinyjs::disable("xgb_reg_chatgpt_run")
    
    withProgress(
      message = "Calculation in progress",
      detail = "This may take a while...", value = 0, {
        
        if (input$rf_reg_dataType == "Count (Rarefied)") {
          type = "rare.count"
          ref = RF_REFERENCE_RC
        }
        else if (input$rf_reg_dataType == "Proportion") {
          type = "prop"
          ref = RF_REFERENCE
        }
        else if (input$rf_reg_dataType == "CLR (Default)") {
          type = "clr"
          ref = RF_REFERENCE_CLR
        }
        else if(input$rf_reg_dataType == "Arcsine-root"){
          type = "arcsin"
          ref = RF_REFERENCE
        }
        
        if(input$rf_reg_include_species == "Phylum - Genus (Default)"){
          level.names = get.level.names(include = FALSE)
        }
        else if(input$rf_reg_include_species == "Phylum - Species"){
          level.names = get.level.names(include = TRUE)
        }
        
        taxa.out <- chooseData$taxa.out
        
        for(j in 1:6){
          for(name in level.names){
            names(taxa.out[[j]][[name]]) <- chooseData$taxa.names.out$names[[name]]
          }
        }
        data <- taxa.out[[type]]
        colnames.list.reg <- colnames.to.ind(data)
        data <- change.colnames(data, colnames.list.reg$new)
        
        if (input$rf_reg_covariate_yn == "Covariate(s)"){
          input.data <- try(cov.remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$rf_reg_response, cov.name = input$rf_reg_covariate_options, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          
          y.name <- input$rf_reg_response
          sam.dat.na <- check.column.class(sam.dat.na)
          sam.dat.na <- try(cov.linear.reg(sam.dat.na, y.name), silent = TRUE)
          y.name <- "resid"
        }
        else{
          input.data <- try(remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$rf_reg_response, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          y.name <- input$rf_reg_response
        }
        
        nfold <- as.numeric(input$rf_reg_nfold)
        ntree <- as.numeric(input$rf_reg_ntree)
        
        rf.list.reg <- list()
        for(name in level.names){
          incProgress(1/10, message = sprintf("Random Forest: %s in progress", str_to_title(name)))
          set.seed(578)
          rf.list.reg[[name]] <- try(rf.reg.rev(data = data,
                                                sam.dat.na = sam.dat.na,
                                                y.name = y.name,
                                                nfold = nfold,
                                                ntree = ntree,
                                                name = name,
                                                p = 0.8),
                                     silent = TRUE)
        }
        incProgress(2/10, message = "Visualizations in progress")
        
        rf.imp.plot.list.reg <- list()
        pd.plot.list.reg <- list()
        rf.test.error.reg <- list()
        for(name in level.names){
          rf.imp.plot.list.reg[[name]] <- try(rf.imp.plot(rf.list.reg, name, n = as.numeric(input$rf_reg_var_num), type = 1, is.cat = FALSE, data = data, data.type = type), silent = TRUE)
          pd.plot.list.reg[[name]] <- try(rf.pdp.reg(rf.list.reg, n = as.numeric(input$rf_reg_var_num), name = name, data.type = type), silent = TRUE)
        }
        
        rf.width.list.reg <- list()
        for(name in level.names){
          if(as.numeric(input$rf_reg_var_num) > ncol(data[[name]])){
            n <- ncol(data[[name]])
            if(n %% 5 == 0){
              n <- n / 5
            }
            else{
              n <- (n / 5) + 1
            }
          }
          else{
            n <- as.numeric(input$rf_reg_var_num)
            n <- n / 5
          }
          rf.width.list.reg[[name]] <- n * 160
        }
        shinyjs::show("rf_reg_results")
        output$rf_reg_results <- renderUI({
          tagList(
            do.call(tabsetPanel, lapply(1:length(level.names), function(i) {
              tabPanel(title = str_to_title(level.names[i]), align = "center",
                       tabsetPanel(tabPanel(title = "Importance", align = "center", br(),
                                            plotOutput(paste0("rf_reg_imp", i), height = 700, width = 700), br(),
                                            dataTableOutput(paste0("rf_reg_column_table1", i), height = "auto", width = 700)),
                                   tabPanel(title = "Partial Dependence", align = "center", br(),
                                            plotOutput(paste0("rf_reg_pd", i), height = 750, width = rf.width.list.reg[[i]]), br(),
                                            dataTableOutput(paste0("rf_reg_column_table2", i), height = "auto", width = 700)),
                                   tabPanel(title = "CV Error", align = "center", br(),
                                            plotOutput(paste0("rf_reg_cv", i), height = 700, width = 700)),
                                   tabPanel(title = "OOB Error", align = "center", br(),
                                            plotOutput(paste0("rf_reg_oob", i), height = 700, width = 700)),
                                   tabPanel(title = "Test Error", align = "left", br(),
                                            uiOutput(paste0("rf_reg_test_error", i)))))})),
            uiOutput("rf_reg_chatgpt_ui")
          )
        })
        
        lapply(1:length(level.names), function(j) {
          output[[paste0("rf_reg_imp", j)]] <- renderPlot({
            tryCatch(rf.imp.plot.list.reg[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_reg_pd", j)]] <- renderPlot({
            tryCatch(pd.plot.list.reg[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_reg_column_table1", j)]] <- renderDataTable({
            tryCatch(rf.pd.var.used(rf.list.reg, level.names[[j]], colnames.list.reg, n = as.numeric(input$rf_reg_var_num), is.cat = FALSE), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_reg_column_table2", j)]] <- renderDataTable({
            tryCatch(rf.pd.var.used(rf.list.reg, level.names[[j]], colnames.list.reg, n = as.numeric(input$rf_reg_var_num), is.cat = FALSE), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_reg_cv", j)]] <- renderPlot({
            tryCatch(cv.mtry(rf.list.reg, level.names[j], is.cat = FALSE), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_reg_oob", j)]] <- renderPlot({
            tryCatch(error.plot(rf.list.reg, level.names[j], is.cat = FALSE), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("rf_reg_test_error", j)]] <- renderUI({
            rf_reg_mse <- try(get.mse(predict(rf.list.reg[[level.names[j]]]$fit, rf.list.reg[[level.names[j]]]$test$x), rf.list.reg[[level.names[j]]]$test$y), silent = TRUE)
            rf_reg_mae <- try(get.mae(predict(rf.list.reg[[level.names[j]]]$fit, rf.list.reg[[level.names[j]]]$test$x), rf.list.reg[[level.names[j]]]$test$y), silent = TRUE)
            tagList(
              tags$ul(
                tags$li(p(paste("Test MSE:", try(format(round(rf_reg_mse, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt")),
                tags$li(p(paste("Test MAE:", try(format(round(rf_reg_mae, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt"))
                , style = "font-size:12pt")
            )
          })
          
          output$rf_reg_downloadTabUI <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("Download Output Table", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p("You can download the data analysis outputs.",
                    style = "font-size:11pt"),
                  h5("Feature Importance"),
                  downloadButton("rf_reg_downloadTable", "Download", width = '50%', style = "background-color: red3")
              )
            )
          })
          
          output$rf_reg_downloadTable <- downloadHandler(
            filename = function() {
              paste("RF_Importance.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(rf.imp.df(rf.list.reg, level.names[i], type = 0), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(rf.imp.df(rf.list.reg, level.names[i], type = 0), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$rf_reg_reference <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("References", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p(ref, style = "font-size:11pt")
              )
            )
          })
        })
        
        rf_reg_imp_var_list <- list()
        for(name in level.names){
          rf_reg_imp_var_list[[name]] <- try(rf.imp.df.order(rf.list.reg, name, n = 10, colnames.list.reg, is.cat = FALSE)$names, silent = TRUE)
        }
        output$rf_reg_chatgpt_ui <- renderUI({
          tagList(
            br(),
            box(id = "rf_reg_chat", title = strong("Ask ChatGPT: Tell me about the roles of (a microbial taxon) on (a human disease).", style = "color:white", side = "left"), width = NULL, 
                solidHeader = TRUE, status = "info",
                column(width = 6.5, 
                       h5(strong("API Key"), style = "color:black; margin-top: -1px"),
                       p("", style = "margin-bottom: -4px;"),
                       p("You need to create your own secret API key to run ChatGPT. Here is the link to create an API key. (https://platform.openai.com/account/api-keys) ", style = "font-size:10pt;"),
                       p("", style = "margin-bottom: -4px;"),
                       textInput("rf_reg_chatgpt_key", label = NULL, value = NULL, width = '55%'),
                       p(" ", style = "margin-bottom: +20px;"),
                       selectInput("rf_reg_chatgpt_rank", "Select a taxonomic rank", choices = str_to_title(level.names), selected = str_to_title(level.names)[1], width = '55%'),
                       uiOutput("rf_reg_chatgpt_taxon"),
                       uiOutput("rf_reg_taxon_rename"),
                       uiOutput("rf_reg_response_var_rename"),
                       actionButton("rf_reg_chatgpt_run", (strong("Ask!")), class = "btn-info", style = "margin-bottom: -20px")), br(), 
                column(width = 4.4, uiOutput("rf_reg_chatgpt_vis"))
            )
          )})
        
        output$rf_reg_chatgpt_taxon <- renderUI({
          tagList(
            p(" ", style = "margin-top: +15px;"),
            selectInput("rf_reg_chatgpt_taxon", "Select an important taxon (e.g., disease-predictor in the importance plot).", choices = rf_reg_imp_var_list[[str_to_lower(input$rf_reg_chatgpt_rank)]], selected = NULL, width = '55%')
          )
        })
        
        output$rf_reg_taxon_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +18px;"),
            h5(strong("Rename the taxon"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language, for instance, deleting possible codes, special symbols or numbers (e.g., from 'Saccharibacteria_(TM7)_[O-1]' to 'Saccharibacteria').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("rf_reg_rename_taxa", label = NULL, value = input$rf_reg_chatgpt_taxon, width = '55%'),
            p(" ", style = "margin-bottom: +10px;")
          )
        })
        
        output$rf_reg_response_var_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +23px;"),
            h5(strong("Rename the output variable"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language (e.g., from 'gingival_inflammation' to 'gingival inflammation').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("rf_reg_response_var_rename", label = NULL, value = input$rf_reg_response, width = '55%')
          )
        })
      }
    )
    shinyjs::enable("rf_reg_dataType")
    shinyjs::enable("rf_reg_response")
    shinyjs::enable("rf_reg_covariate_yn")
    shinyjs::enable("rf_reg_covariate_options")
    shinyjs::enable("rf_reg_loss")
    shinyjs::enable("rf_reg_nfold")
    shinyjs::enable("rf_reg_ntree")
    shinyjs::enable("rf_reg_var_num")
    shinyjs::enable("rf_reg_include_species")
    
    shinyjs::enable("dt_cla_runButton")
    shinyjs::enable("dt_reg_runButton")
    shinyjs::enable("rf_cla_runButton")
    shinyjs::enable("rf_reg_runButton")
    shinyjs::enable("xgb_cla_runButton")
    shinyjs::enable("xgb_reg_runButton")
    
    shinyjs::enable("dt_cla_chatgpt_run")
    shinyjs::enable("dt_reg_chatgpt_run")
    shinyjs::enable("rf_cla_chatgpt_run")
    shinyjs::enable("rf_reg_chatgpt_run")
    shinyjs::enable("xgb_cla_chatgpt_run")
    shinyjs::enable("xgb_reg_chatgpt_run")
  })
  
  observeEvent(input$rf_reg_chatgpt_run, {
    withProgress(
      message = "Asking in progress",
      detail = "This may take a while...", value = 0, {
        incProgress(0.5, message = "Asking Chat GPT")
        
        taxa.name <- input$rf_reg_rename_taxa
        var.name <- input$rf_reg_response_var_rename
        api.key <- input$rf_reg_chatgpt_key
        
        new.taxa.name <- gsub("[[:punct:]]", "+", taxa.name)
        new.taxa.name <- gsub(" ", "+", new.taxa.name)
        
        new.var.name <- gsub("[[:punct:]]", "+", var.name)
        new.var.name <- gsub(" ", "+", new.var.name)
        
        chat_result <- tryCatch(chat_gpt_MiTree(taxa.name, var.name, api.key), error = function(e){
          message("Invalid or incorrect API key. Please check it again.")
          showModal(modalDialog(div("Invalid or incorrect API key. Please check it again.")))
          return(NULL)
        })
        
        if(!is.null(chat_result)){
          output$rf_reg_chatgpt_vis <-renderUI({
            tagList(br(), strong(paste("Tell me about the roles of a", taxa.name, "on a", var.name)), 
                    p(chat_result, style = "margin-top: 10px"), br(),
                    
                    strong("Reference Search Results"), br(), 
                    
                    p("Google Scholar", style = "margin-top: 10px"), 
                    p(tags$a(href = paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), target = "_blank")), 
                    
                    p("PubMed"), 
                    p(tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), target = "_blank")))
          })
        }
      }
    )
  })
  
  ## (4-1) XGB - Classification -------------------
  observeEvent(input$xgb_cla_runButton, {
    shinyjs::disable("xgb_cla_dataType")
    shinyjs::disable("xgb_cla_response")
    shinyjs::disable("xgb_cla_covariate_yn")
    shinyjs::disable("xgb_cla_covariate_options")
    shinyjs::disable("xgb_cla_loss_cov")
    shinyjs::disable("xgb_cla_nfold")
    shinyjs::disable("xgb_cla_loss")
    shinyjs::disable("xgb_cla_eta")
    shinyjs::disable("xgb_cla_penalty")
    shinyjs::disable("xgb_cla_nrounds")
    shinyjs::disable("xgb_cla_var_num")
    shinyjs::disable("xgb_cla_include_species")
    
    shinyjs::disable("dt_cla_runButton")
    shinyjs::disable("dt_reg_runButton")
    shinyjs::disable("rf_cla_runButton")
    shinyjs::disable("rf_reg_runButton")
    shinyjs::disable("xgb_cla_runButton")
    shinyjs::disable("xgb_reg_runButton")
    
    shinyjs::disable("dt_cla_chatgpt_run")
    shinyjs::disable("dt_reg_chatgpt_run")
    shinyjs::disable("rf_cla_chatgpt_run")
    shinyjs::disable("rf_reg_chatgpt_run")
    shinyjs::disable("xgb_cla_chatgpt_run")
    shinyjs::disable("xgb_reg_chatgpt_run")
    
    withProgress(
      message = "Calculation in progress",
      detail = "This may take a while...", value = 0, {
        if (input$xgb_cla_dataType == "Count (Rarefied)") {
          type = "rare.count"
          ref = XGB_REFERENCE_RC
        }
        else if (input$xgb_cla_dataType == "Proportion") {
          type = "prop"
          ref = XGB_REFERENCE
        }
        else if (input$xgb_cla_dataType == "CLR (Default)") {
          type = "clr"
          ref = XGB_REFERENCE_CLR
        }
        else if(input$xgb_cla_dataType == "Arcsine-root"){
          type = "arcsin"
          ref = XGB_REFERENCE
        }
        
        if(input$xgb_cla_penalty == "Yes (Default)"){
          alpha = 0
          lambda = 1
        }
        else if(input$xgb_cla_penalty == "No"){
          alpha = 0
          lambda = 0
        }
        
        if(input$xgb_cla_include_species == "Phylum - Genus (Default)"){
          level.names = get.level.names(include = FALSE)
        }
        else if(input$xgb_cla_include_species == "Phylum - Species"){
          level.names = get.level.names(include = TRUE)
        }
        
        taxa.out <- chooseData$taxa.out
        
        for(j in 1:6){
          for(name in level.names){
            names(taxa.out[[j]][[name]]) <- chooseData$taxa.names.out$names[[name]]
          }
        }
        data <- taxa.out[[type]]
        colnames.list <- colnames.to.ind(data)
        data <- change.colnames(data, colnames.list$new)
        
        if(input$xgb_cla_covariate_yn == "Covariate(s)"){
          input.data <- try(cov.remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$xgb_cla_response, cov.name = input$xgb_cla_covariate_options, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          
          y.name <- input$xgb_cla_response
          loss <- xgb.model.input.cla$cov.loss
          eta <- as.numeric(input$xgb_cla_eta)
          nfold <- as.numeric(input$xgb_cla_nfold)
          nrounds <- as.numeric(input$xgb_cla_nrounds)
          
          sam.dat.na <- try(check.column.class(sam.dat.na), silent = TRUE)
          sam.dat.na <- try(cov.logistic.reg(sam.dat.na, y.name), silent = TRUE)
          y.name <- "resid"
          
          xgb.list <- list()
          for(name in level.names){
            incProgress(1/10, message = sprintf("Extreme Gradient Boosting: %s in progress", str_to_title(name)))
            set.seed(578)
            xgb.list[[name]] <- try(xgb.reg(data = data,
                                            sam.dat.na = sam.dat.na,
                                            y.name = y.name,
                                            eta = eta,
                                            nrounds = nrounds,
                                            nfold = nfold,
                                            alpha = alpha,
                                            lambda = lambda,
                                            loss.func = "rss",
                                            name = name,
                                            p = 0.8),
                                    silent = TRUE)
          }
        }
        else{
          input.data <- try(remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$xgb_cla_response, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          
          y.name <- input$xgb_cla_response
          cat.name <- category.names(sam.dat.na, y.name)
          sam.dat.na[[y.name]] <- ifelse(sam.dat.na[[y.name]] == cat.name[1], 0, 1)
          eval <- xgb.model.input.cla$eval
          eta <- as.numeric(input$xgb_cla_eta)
          nfold <- as.numeric(input$xgb_cla_nfold)
          nrounds <- as.numeric(input$xgb_cla_nrounds)
          
          xgb.list <- list()
          for(name in level.names){
            incProgress(1/10, message = sprintf("Extreme Gradient Boosting: %s in progress", str_to_title(name)))
            set.seed(578)
            xgb.list[[name]] <- try(xgb.cla(data = data,
                                            sam.dat.na = sam.dat.na,
                                            y.name = y.name,
                                            eta = eta,
                                            nrounds = nrounds,
                                            nfold = nfold,
                                            alpha = alpha,
                                            lambda = lambda,
                                            stratified = TRUE,
                                            loss.func = eval,
                                            name = name,
                                            p = 0.8),
                                    silent = TRUE)
          }
        }
        
        incProgress(2/10, message = "Visualizations in progress")        
        xgb.importance <- try(xgb.shap.imp(xgb.list, level.names, n = as.numeric(input$xgb_cla_var_num)), silent = TRUE)
        
        xgb.loss <- list()
        xgb.imp.list <- list()
        xgb.shap <- list()
        xgb.dep <- list()
        
        for(name in level.names){
          xgb.loss[[name]] <- try(xgb.error.plot.2(xgb.list, name), silent = TRUE)
          xgb.shap[[name]] <- try(xgb.shap.summary(xgb.list, n = as.numeric(input$xgb_cla_var_num), rank.name = name), silent = TRUE)
          if(input$xgb_cla_covariate_yn == "Covariate(s)"){
            xgb.dep[[name]] <- try(xgb.pdp.reg(xgb.list = xgb.list, rank.name = name, n = as.numeric(input$xgb_cla_var_num), data.type = type), silent = TRUE)
          }
          else{
            xgb.dep[[name]] <- try(xgb.pdp.bin(xgb.list = xgb.list, rank.name = name, n = as.numeric(input$xgb_cla_var_num), data.type = type, cat.name = cat.name), silent = TRUE)
          }
        }
        
        xgb.width.list.cla <- list()
        for(name in level.names){
          if(as.numeric(input$xgb_cla_var_num) > ncol(data[[name]])){
            n <- ncol(data[[name]])
            if(n %% 5 == 0){
              n <- n / 5
            }
            else{
              n <- (n / 5) + 1
            }
          }
          else{
            n <- as.numeric(input$xgb_cla_var_num)
            n <- n / 5
          }
          xgb.width.list.cla[[name]] <- n * 160
        }
        
        output$xgb_cla_results <- renderUI({
          tagList(
            do.call(tabsetPanel, lapply(1:length(level.names), function(i) {
              tabPanel(title = str_to_title(level.names[i]), align = "center",
                       tabsetPanel(tabPanel(title = "Importance", align = "center", br(),
                                            plotOutput(paste0("xgb_cla_SHAP", i), height = 700, width = 700), br(),
                                            dataTableOutput(paste0("xgb_cla_column_table1", i), height = "auto", width = 700)),
                                   tabPanel(title = "Partial Dependence", align = "center", br(),
                                            plotOutput(paste0("xgb_cla_SHAP_dep", i), height = 750, width = xgb.width.list.cla[[i]]), br(),
                                            dataTableOutput(paste0("xgb_cla_column_table2", i), height = "auto", width = 700)),
                                   tabPanel(title = "Error Plot", align = "center", br(),
                                            plotOutput(paste0("xgb_cla_loss", i), height = 700, width = 700)),
                                   tabPanel(title = "Test Error", align = "left", br(),
                                            uiOutput(paste0("xgb_cla_test_error", i)))))})),
            uiOutput("xgb_cla_chatgpt_ui")
          )
        })
        
        lapply(1:length(level.names), function(j) {
          output[[paste0("xgb_cla_SHAP", j)]] <- renderPlot({
            tryCatch(xgb.shap[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("xgb_cla_SHAP_dep", j)]] <- renderPlot({
            tryCatch(xgb.dep[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("xgb_cla_column_table1", j)]] <- renderDataTable({
            tryCatch(xgb.shap.imp.var(xgb.list, level.names[[j]], colnames.list, n = as.numeric(input$xgb_cla_var_num)), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })

          output[[paste0("xgb_cla_column_table2", j)]] <- renderDataTable({
            tryCatch(xgb.shap.imp.var(xgb.list, level.names[[j]], colnames.list, n = as.numeric(input$xgb_cla_var_num)), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("xgb_cla_loss", j)]] <- renderPlot({
            tryCatch(xgb.loss[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          if(input$xgb_cla_covariate_yn == "Covariate(s)"){
            output[[paste0("xgb_cla_test_error", j)]] <- renderUI({
              xgb_cla_mse <- try(get.mse(predict(xgb.list[[level.names[j]]]$model, xgb.list[[level.names[j]]]$test), xgb.list[[level.names[j]]]$test.ind$y), silent = TRUE)
              xgb_cla_mae <- try(get.mae(predict(xgb.list[[level.names[j]]]$model, xgb.list[[level.names[j]]]$test), xgb.list[[level.names[j]]]$test.ind$y), silent = TRUE)
              tagList(
                tags$ul(
                  tags$li(p(paste("Test MSE:", try(format(round(xgb_cla_mse, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt")),
                  tags$li(p(paste("Test MAE:", try(format(round(xgb_cla_mae, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt"))
                  , style = "font-size:12pt")
              )
            })
          }
          else{
            output[[paste0("xgb_cla_test_error", j)]] <- renderUI({
              xgb_cla_err <- try(1-get.acc(as.numeric(predict(xgb.list[[level.names[j]]]$model, xgb.list[[level.names[j]]]$test.ind$x) > 0.5), xgb.list[[level.names[j]]]$test.ind$y), silent = TRUE)
              xgb_cla_auc <- try(get.auc(predict(xgb.list[[level.names[j]]]$model, xgb.list[[level.names[j]]]$test.ind$x), xgb.list[[level.names[j]]]$test.ind$y), silent = TRUE)
              tagList(
                tags$ul(
                  tags$li(p(paste("Test Misclassification Error:", try(format(round(xgb_cla_err, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt")),
                  tags$li(p(paste("Test AUC:", try(format(round(xgb_cla_auc, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt"))
                  , style = "font-size:12pt"),
                p("", style = "margin-bottom: 15pt")
              )
            })
          }
          
          output$xgb_cla_downloadTabUI <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("Download Output Table", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p("You can download the data analysis outputs.",
                    style = "font-size:11pt"),
                  h5("Feature Importance"),
                  downloadButton("xgb_cla_downloadTable1", "Download", width = '50%', style = "background-color: red3")
              )
            )
          })
          
          output$xgb_cla_downloadTable1 <- downloadHandler(
            filename = function() {
              paste("XGB_Importance.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(as.data.frame(xgb.importance[[i]]), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(as.data.frame(xgb.importance[[i]]), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$xgb_cla_reference <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("References", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p(ref, style = "font-size:11pt")
              )
            )
          })
        })
        
        xgb_cla_imp_var_list <- list()
        for(name in level.names){
          xgb_cla_imp_var_list[[name]] <- try(xgb.shap.imp.var(xgb.list, name, colnames.list, n = 10)[,1], silent = TRUE)
        }
        output$xgb_cla_chatgpt_ui <- renderUI({
          tagList(
            br(),
            box(id = "xgb_cla_chat", title = strong("Ask ChatGPT: Tell me about the roles of (a microbial taxon) on (a human disease).", style = "color:white", side = "left"), width = NULL, 
                solidHeader = TRUE, status = "info",
                column(width = 6.5, 
                       h5(strong("API Key"), style = "color:black; margin-top: -1px"),
                       p("", style = "margin-bottom: -4px;"),
                       p("You need to create your own secret API key to run ChatGPT. Here is the link to create an API key. (https://platform.openai.com/account/api-keys) ", style = "font-size:10pt;"),
                       p("", style = "margin-bottom: -4px;"),
                       textInput("xgb_cla_chatgpt_key", label = NULL, value = NULL, width = '55%'),
                       p(" ", style = "margin-bottom: +20px;"),
                       selectInput("xgb_cla_chatgpt_rank", "Select a taxonomic rank", choices = str_to_title(level.names), selected = str_to_title(level.names)[1], width = '55%'),
                       uiOutput("xgb_cla_chatgpt_taxon"),
                       uiOutput("xgb_cla_taxon_rename"),
                       uiOutput("xgb_cla_response_var_rename"),
                       actionButton("xgb_cla_chatgpt_run", (strong("Ask!")), class = "btn-info", style = "margin-bottom: -20px")), br(), 
                column(width = 4.4, uiOutput("xgb_cla_chatgpt_vis"))
            )
          )})
        
        output$xgb_cla_chatgpt_taxon <- renderUI({
          tagList(
            p(" ", style = "margin-top: +15px;"),
            selectInput("xgb_cla_chatgpt_taxon", "Select an important taxon (e.g., disease-predictor in the importance plot).", choices = xgb_cla_imp_var_list[[str_to_lower(input$xgb_cla_chatgpt_rank)]], selected = NULL, width = '55%')
          )
        })
        
        output$xgb_cla_taxon_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +18px;"),
            h5(strong("Rename the taxon"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language, for instance, deleting possible codes, special symbols or numbers (e.g., from 'Saccharibacteria_(TM7)_[O-1]' to 'Saccharibacteria').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("xgb_cla_rename_taxa", label = NULL, value = input$xgb_cla_chatgpt_taxon, width = '55%'),
            p(" ", style = "margin-bottom: +10px;")
          )
        })
        
        output$xgb_cla_response_var_rename <- renderUI({
          tagList(
            p(" ", style = "margin-top: +23px;"),
            h5(strong("Rename the output variable"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language (e.g., from 'gingival_inflammation' to 'gingival inflammation').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("xgb_cla_response_var_rename", label = NULL, value = input$xgb_cla_response, width = '55%')
          )
        })
      }
    )
    
    shinyjs::enable("xgb_cla_dataType")
    shinyjs::enable("xgb_cla_response")
    shinyjs::enable("xgb_cla_covariate_yn")
    shinyjs::enable("xgb_cla_covariate_options")
    shinyjs::enable("xgb_cla_loss_cov")
    shinyjs::enable("xgb_cla_nfold")
    shinyjs::enable("xgb_cla_loss")
    shinyjs::enable("xgb_cla_eta")
    shinyjs::enable("xgb_cla_penalty")
    shinyjs::enable("xgb_cla_nrounds")
    shinyjs::enable("xgb_cla_var_num")
    shinyjs::enable("xgb_cla_include_species")
    
    shinyjs::enable("dt_cla_runButton")
    shinyjs::enable("dt_reg_runButton")
    shinyjs::enable("rf_cla_runButton")
    shinyjs::enable("rf_reg_runButton")
    shinyjs::enable("xgb_cla_runButton")
    shinyjs::enable("xgb_reg_runButton")
    
    shinyjs::enable("dt_cla_chatgpt_run")
    shinyjs::enable("dt_reg_chatgpt_run")
    shinyjs::enable("rf_cla_chatgpt_run")
    shinyjs::enable("rf_reg_chatgpt_run")
    shinyjs::enable("xgb_cla_chatgpt_run")
    shinyjs::enable("xgb_reg_chatgpt_run")
  })
  
  observeEvent(input$xgb_cla_chatgpt_run, {
    withProgress(
      message = "Asking in progress",
      detail = "This may take a while...", value = 0, {
        incProgress(0.5, message = "Asking Chat GPT")
        
        taxa.name <- input$xgb_cla_rename_taxa
        var.name <- input$xgb_cla_response_var_rename
        api.key <- input$xgb_cla_chatgpt_key
        
        new.taxa.name <- gsub("[[:punct:]]", "+", taxa.name)
        new.taxa.name <- gsub(" ", "+", new.taxa.name)
        
        new.var.name <- gsub("[[:punct:]]", "+", var.name)
        new.var.name <- gsub(" ", "+", new.var.name)
        
        chat_result <- tryCatch(chat_gpt_MiTree(taxa.name, var.name, api.key), error = function(e){
          message("Invalid or incorrect API key. Please check it again.")
          showModal(modalDialog(div("Invalid or incorrect API key. Please check it again.")))
          return(NULL)
        })
        
        if(!is.null(chat_result)){
          output$xgb_cla_chatgpt_vis <-renderUI({
            tagList(br(), 
                    strong(paste("Tell me about the roles of a", taxa.name, "on a", var.name)), 
                    p(chat_result, style = "margin-top: 10px"), br(),
                    
                    strong("Reference Search Results"), br(), 
                    
                    p("Google Scholar", style = "margin-top: 10px"), 
                    p(tags$a(href = paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), target = "_blank")), 
                    
                    p("PubMed"), 
                    p(tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), target = "_blank")))
          })
        }
      }
    )
  })
  
  ## (4-2) XGB - Regression -------------------
  observeEvent(input$xgb_reg_runButton, {
    shinyjs::disable("xgb_reg_dataType")
    shinyjs::disable("xgb_reg_response")
    shinyjs::disable("xgb_reg_covariate_yn")
    shinyjs::disable("xgb_reg_covariate_options")
    shinyjs::disable("xgb_reg_nfold")
    shinyjs::disable("xgb_reg_loss")
    shinyjs::disable("xgb_reg_eta")
    shinyjs::disable("xgb_reg_penalty")
    shinyjs::disable("xgb_reg_nrounds")
    shinyjs::disable("xgb_reg_var_num")
    shinyjs::disable("xgb_reg_include_species")
    
    shinyjs::disable("dt_cla_runButton")
    shinyjs::disable("dt_reg_runButton")
    shinyjs::disable("rf_cla_runButton")
    shinyjs::disable("rf_reg_runButton")
    shinyjs::disable("xgb_cla_runButton")
    shinyjs::disable("xgb_reg_runButton")
    
    shinyjs::disable("dt_cla_chatgpt_run")
    shinyjs::disable("dt_reg_chatgpt_run")
    shinyjs::disable("rf_cla_chatgpt_run")
    shinyjs::disable("rf_reg_chatgpt_run")
    shinyjs::disable("xgb_cla_chatgpt_run")
    shinyjs::disable("xgb_reg_chatgpt_run")
    
    withProgress(
      message = "Calculation in progress",
      detail = "This may take a while...", value = 0, {
        if (input$xgb_reg_dataType == "Count (Rarefied)") {
          type = "rare.count"
          ref = XGB_REFERENCE_RC
        }
        else if (input$xgb_reg_dataType == "Proportion") {
          type = "prop"
          ref = XGB_REFERENCE
        }
        else if (input$xgb_reg_dataType == "CLR (Default)") {
          type = "clr"
          ref = XGB_REFERENCE_CLR
        }
        else if(input$xgb_reg_dataType == "Arcsine-root"){
          type = "arcsin"
          ref = XGB_REFERENCE
        }
        
        if(input$xgb_reg_penalty == "Yes (Default)"){
          alpha = 0
          lambda = 1
        }
        else if(input$xgb_reg_penalty == "No"){
          alpha = 0
          lambda = 0
        }
        
        if(input$xgb_reg_include_species == "Phylum - Genus (Default)"){
          level.names = get.level.names(include = FALSE)
        }
        else if(input$xgb_reg_include_species == "Phylum - Species"){
          level.names = get.level.names(include = TRUE)
        }
        
        taxa.out <- chooseData$taxa.out
        
        for(j in 1:6){
          for(name in level.names){
            names(taxa.out[[j]][[name]]) <- chooseData$taxa.names.out$names[[name]]
          }
        }
        data <- taxa.out[[type]]
        colnames.list.reg <- colnames.to.ind(data)
        data <- change.colnames(data, colnames.list.reg$new)
        
        if (input$xgb_reg_covariate_yn == "Covariate(s)"){
          input.data <- try(cov.remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$xgb_reg_response, cov.name = input$xgb_reg_covariate_options, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          
          y.name <- input$xgb_reg_response
          sam.dat.na <- check.column.class(sam.dat.na)
          sam.dat.na <- try(cov.linear.reg(sam.dat.na, y.name), silent = TRUE)
          y.name <- "resid"
        }
        else{
          input.data <- try(remove.na(data = data, sam.dat = chooseData$sam.dat, y.name = input$xgb_reg_response, level.names = level.names), silent = TRUE)
          data <- try(input.data[[1]], silent = TRUE)
          sam.dat.na <- try(input.data[[2]], silent = TRUE)
          y.name <- input$xgb_reg_response
        }
        
        loss <- xgb.model.input.reg$loss
        eval <- xgb.model.input.reg$eval
        eta <- as.numeric(input$xgb_reg_eta)
        nfold <- as.numeric(input$xgb_reg_nfold)
        nrounds <- as.numeric(input$xgb_reg_nrounds)
        
        xgb.list.reg <- list()
        for(name in level.names){
          incProgress(1/10, message = sprintf("Extreme Gradient Boosting: %s in progress", str_to_title(name)))
          set.seed(578)
          xgb.list.reg[[name]] <- try(xgb.reg(data = data,
                                              sam.dat.na = sam.dat.na,
                                              y.name = y.name,
                                              eta = eta,
                                              nrounds = nrounds,
                                              nfold = nfold,
                                              alpha = alpha,
                                              lambda = lambda,
                                              loss.func = loss,
                                              name = name,
                                              p = 0.8),
                                      silent = TRUE)
        }
        
        incProgress(2/10, message = "Visualizations in progress")        
        xgb.importance <- try(xgb.shap.imp(xgb.list.reg, level.names, n = as.numeric(input$xgb_reg_var_num)), silent = TRUE)
        
        # Loss
        xgb.loss.reg <- list()
        xgb.imp.list.reg <- list()
        xgb.shap.reg <- list()
        xgb.dep.reg <- list()
        
        for(name in level.names){
          xgb.loss.reg[[name]] <- try(xgb.error.plot.2(xgb.list.reg, name), silent = TRUE)
          xgb.shap.reg[[name]] <- try(xgb.shap.summary(xgb.list.reg, name, n = as.numeric(input$xgb_reg_var_num)), silent = TRUE)
          xgb.dep.reg[[name]] <- try(xgb.pdp.reg(xgb.list = xgb.list.reg, rank.name = name, n = as.numeric(input$xgb_reg_var_num), data.type = type), silent = TRUE)
        }
        
        xgb.width.list.reg <- list()
        for(name in level.names){
          if(as.numeric(input$xgb_reg_var_num) > ncol(data[[name]])){
            n <- ncol(data[[name]])
            if(n %% 5 == 0){
              n <- n / 5
            }
            else{
              n <- (n / 5) + 1
            }
          }
          else{
            n <- as.numeric(input$xgb_reg_var_num)
            n <- n / 5
          }
          xgb.width.list.reg[[name]] <- n * 160
        }
        
        output$xgb_reg_results <- renderUI({
          tagList(
            do.call(tabsetPanel, lapply(1:length(level.names), function(i) {
              tabPanel(title = str_to_title(level.names[i]), align = "center",
                       tabsetPanel(tabPanel(title = "Importance", align = "center", br(),
                                            plotOutput(paste0("xgb_reg_SHAP", i), height = 700, width = 700), br(),
                                            dataTableOutput(paste0("xgb_reg_column_table1", i), height = "auto", width = 700)),
                                   tabPanel(title = "Partial Dependence", align = "center", br(),
                                            plotOutput(paste0("xgb_reg_SHAP_dep", i), height = 750, width = xgb.width.list.reg[[i]]), br(),
                                            dataTableOutput(paste0("xgb_reg_column_table2", i), height = "auto", width = 700)),
                                   tabPanel(title = "Error Plot", align = "center", br(),
                                            plotOutput(paste0("xgb_reg_loss", i), height = 700, width = 700)),
                                   tabPanel(title = "Test Error", align = "left", br(),
                                            uiOutput(paste0("xgb_reg_test_error", i)))))})),
            uiOutput("xgb_reg_chatgpt_ui")
          )
        })
        
        lapply(1:length(level.names), function(j) {
          output[[paste0("xgb_reg_SHAP", j)]] <- renderPlot({
            tryCatch(xgb.shap.reg[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("xgb_reg_SHAP_dep", j)]] <- renderPlot({
            tryCatch(xgb.dep.reg[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("xgb_reg_column_table1", j)]] <- renderDataTable({
            tryCatch(xgb.shap.imp.var(xgb.list.reg, level.names[[j]], colnames.list.reg, n = as.numeric(input$xgb_reg_var_num)), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("xgb_reg_column_table2", j)]] <- renderDataTable({
            tryCatch(xgb.shap.imp.var(xgb.list.reg, level.names[[j]], colnames.list.reg, n = as.numeric(input$xgb_reg_var_num)), error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("xgb_reg_loss", j)]] <- renderPlot({
            tryCatch(xgb.loss.reg[[level.names[j]]], error = function(e){
              message("Visualization not available! Check the input.")
              showModal(modalDialog(div("Visualization not available! Check the input.")))
              return(NULL)
            })
          })
          
          output[[paste0("xgb_reg_test_error", j)]] <- renderUI({
            xgb_reg_mse <- try(get.mse(predict(xgb.list.reg[[level.names[j]]]$model, xgb.list.reg[[level.names[j]]]$test), xgb.list.reg[[level.names[j]]]$test.ind$y), silent = TRUE)
            xgb_reg_mae <- try(get.mae(predict(xgb.list.reg[[level.names[j]]]$model, xgb.list.reg[[level.names[j]]]$test), xgb.list.reg[[level.names[j]]]$test.ind$y), silent = TRUE)
            tagList(
              tags$ul(
                tags$li(p(paste("Test MSE:", try(format(round(xgb_reg_mse, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt")),
                tags$li(p(paste("Test MAE:", try(format(round(xgb_reg_mae, 4), nsmall = 4), silent = TRUE)), style = "font-size:12pt"))
                , style = "font-size:12pt")
            )
          })

          output$xgb_reg_downloadTabUI <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("Download Output Table", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p("You can download the data analysis outputs.",
                    style = "font-size:11pt"),
                  h5("Feature Importance"),
                  downloadButton("xgb_reg_downloadTable1", "Download", width = '50%', style = "background-color: red3")
              )
            )
          })
          
          output$xgb_reg_downloadTable1 <- downloadHandler(
            filename = function() {
              paste("XGB_Importance.zip")
            },
            content = function(DA.file) {
              temp <- setwd(tempdir())
              on.exit(setwd(temp))
              if (length(level.names) == 5) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(as.data.frame(xgb.importance[[i]]), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              else if(length(level.names) == 6) {
                dataFiles = c("Phylum.txt", "Class.txt", "Order.txt" ,"Family.txt", "Genus.txt", "Species.txt")
                for(i in 1:length(dataFiles)){
                  write.table(try(as.data.frame(xgb.importance[[i]]), silent = TRUE), file = dataFiles[i], sep = "\t")
                }
              }
              zip(zipfile=DA.file, files=dataFiles)
            })
          
          output$xgb_reg_reference <- renderUI({
            tagList(
              p(" ", style = "margin-top: 20px;"),
              box(title = strong("References", style = "color:white"), width = NULL, status = "info", solidHeader = TRUE,
                  p(ref, style = "font-size:11pt")
              )
            )
          })
        })
        
        xgb_reg_imp_var_list <- list()
        for(name in level.names){
          xgb_reg_imp_var_list[[name]] <- try(xgb.shap.imp.var(xgb.list.reg, name, colnames.list.reg, n = 10)[,1], silent = TRUE)
        }
        
        output$xgb_reg_chatgpt_ui <- renderUI({
          tagList(
            br(),
            box(id = "xgb_reg_chat", title = strong("Ask ChatGPT: Tell me about the roles of (a microbial taxon) on (a human disease).", style = "color:white", side = "left"), width = NULL, 
                solidHeader = TRUE, status = "info",
                column(width = 6.5, 
                       h5(strong("API Key"), style = "color:black; margin-top: -1px"),
                       p("", style = "margin-bottom: -4px;"),
                       p("You need to create your own secret API key to run ChatGPT. Here is the link to create an API key. (https://platform.openai.com/account/api-keys) ", style = "font-size:10pt;"),
                       p("", style = "margin-bottom: -4px;"),
                       textInput("xgb_reg_chatgpt_key", label = NULL, value = NULL, width = '55%'),
                       selectInput("xgb_reg_chatgpt_rank", "Select a taxonomic rank", choices = str_to_title(level.names), selected = str_to_title(level.names)[1], width = '55%'),
                       p(" ", style = "margin-bottom: +20px;"),
                       uiOutput("xgb_reg_chatgpt_taxon"),
                       uiOutput("xgb_reg_taxon_rename"),
                       uiOutput("xgb_reg_response_var_rename"),
                       actionButton("xgb_reg_chatgpt_run", (strong("Ask!")), class = "btn-info", style = "margin-bottom: -20px")), br(), 
                column(width = 4.4, uiOutput("xgb_reg_chatgpt_vis"))
            )
          )})
        
        output$xgb_reg_chatgpt_taxon <- renderUI({
          tagList(
            p(" ", style = "margin-bottom: +15px;"),
            selectInput("xgb_reg_chatgpt_taxon", "Select an important taxon (e.g., disease-predictor in the importance plot).", choices = xgb_reg_imp_var_list[[str_to_lower(input$xgb_reg_chatgpt_rank)]], selected = NULL, width = '55%')
          )
        })
        
        output$xgb_reg_taxon_rename <- renderUI({
          tagList(
            p(" ", style = "margin-bottom: +18px;"),
            h5(strong("Rename the taxon"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language, for instance, deleting possible codes, special symbols or numbers (e.g., from 'Saccharibacteria_(TM7)_[O-1]' to 'Saccharibacteria').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("xgb_reg_rename_taxa", label = NULL, value = input$xgb_reg_chatgpt_taxon, width = '55%'),
            p("", style = "margin-bottom: +10px;"),
          )
        })
        
        output$xgb_reg_response_var_rename <- renderUI({
          tagList(
            p(" ", style = "margin-bottom: +23px;"),
            h5(strong("Rename the output variable"), style = "color:black"),
            p("", style = "margin-bottom: -4px;"),
            p("You can rename it using a human language (e.g., from 'gingival_inflammation' to 'gingival inflammation').", style = "font-size:10pt;"),
            p("", style = "margin-bottom: -4px;"),
            textInput("xgb_reg_response_var_rename", label = NULL, value = input$xgb_reg_response, width = '55%')
          )
        })
      }
    )
    shinyjs::enable("xgb_reg_dataType")
    shinyjs::enable("xgb_reg_response")
    shinyjs::enable("xgb_reg_covariate_yn")
    shinyjs::enable("xgb_reg_covariate_options")
    shinyjs::enable("xgb_reg_nfold")
    shinyjs::enable("xgb_reg_loss")
    shinyjs::enable("xgb_reg_eta")
    shinyjs::enable("xgb_reg_penalty")
    shinyjs::enable("xgb_reg_nrounds")
    shinyjs::enable("xgb_reg_var_num")
    shinyjs::enable("xgb_reg_include_species")
    
    shinyjs::enable("dt_cla_runButton")
    shinyjs::enable("dt_reg_runButton")
    shinyjs::enable("rf_cla_runButton")
    shinyjs::enable("rf_reg_runButton")
    shinyjs::enable("xgb_cla_runButton")
    shinyjs::enable("xgb_reg_runButton")
    
    shinyjs::enable("dt_cla_chatgpt_run")
    shinyjs::enable("dt_reg_chatgpt_run")
    shinyjs::enable("rf_cla_chatgpt_run")
    shinyjs::enable("rf_reg_chatgpt_run")
    shinyjs::enable("xgb_cla_chatgpt_run")
    shinyjs::enable("xgb_reg_chatgpt_run")
  })
  
  observeEvent(input$xgb_reg_chatgpt_run, {
    withProgress(
      message = "Asking in progress",
      detail = "This may take a while...", value = 0, {
        incProgress(0.5, message = "Asking Chat GPT")
        
        taxa.name <- input$xgb_reg_rename_taxa
        var.name <- input$xgb_reg_response_var_rename
        api.key <- input$xgb_reg_chatgpt_key
        
        new.taxa.name <- gsub("[[:punct:]]", "+", taxa.name)
        new.taxa.name <- gsub(" ", "+", new.taxa.name)
        
        new.var.name <- gsub("[[:punct:]]", "+", var.name)
        new.var.name <- gsub(" ", "+", new.var.name)
        
        chat_result <- tryCatch(chat_gpt_MiTree(taxa.name, var.name, api.key), error = function(e){
          message("Invalid or incorrect API key. Please check it again.")
          showModal(modalDialog(div("Invalid or incorrect API key. Please check it again.")))
          return(NULL)
        })
        
        if(!is.null(chat_result)){
          output$xgb_reg_chatgpt_vis <-renderUI({
            tagList(br(), 
                    strong(paste("Tell me about the roles of a", taxa.name, "on a", var.name)), 
                    p(chat_result, style = "margin-top: 10px"), br(),
                    
                    strong("Reference Search Results"), br(), 
                    
                    p("Google Scholar", style = "margin-top: 10px"), 
                    p(tags$a(href = paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", new.taxa.name, "+", new.var.name,"&btnG="), target = "_blank")), 
                    
                    p("PubMed"), 
                    p(tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", new.taxa.name, "+", new.var.name), target = "_blank")))
          })
        }
      }
    )
  })
}

# RUN ------
shinyApp(ui = ui, server = server)
