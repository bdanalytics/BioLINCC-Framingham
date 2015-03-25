# Framingham Heart Study: TenYearCHD classification:: template2
bdanalytics  

**  **    
**Date: (Thu) Jun 25, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/framingham.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

Regression results:
First run:
    <glb_sel_mdl_id>: 
        OOB_RMSE=<0.4f>; new_RMSE=<0.4f>; <feat1>=<imp>; <feat2>=<imp>

Classification results:
First run:
    <glb_sel_mdl_id>: Leaderboard: <accuracy>
        newobs_tbl=[0=, 1=]; submit_filename=
        OOB_conf_mtrx=[YN=, NY=]=; max.Accuracy.OOB=; opt.prob.threshold.OOB=
            <feat1>=<imp>; <feat1>=<imp>; <feat1>=<imp>; 
            <txt.feat1>=<imp>; <txt.feat1>=<imp>; <txt.feat1>=<imp>; 

### Prediction Accuracy Enhancement Options:
- import.data chunk:
    - which obs should be in fit vs. OOB (currently dirty.0 vs .1 is split 50%)
    
- inspect.data chunk:
    - For date variables
        - Appropriate factors ?
        - Different / More last* features ?
        
- scrub.data chunk:        
- transform.data chunk:
    - derive features from multiple features
    
- manage.missing.data chunk:
    - Not fill missing vars
    - Fill missing numerics with a different algorithm
    - Fill missing chars with data based on clusters 
    
- extract.features chunk:
    - Text variables: move to date extraction chunk ???
        - Mine acronyms
        - Mine places

- Review set_global_options chunk after features are finalized

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")
#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/framingham.csv"
glb_newdt_url <- "<newdt_url>"
glb_out_pfx <- "template2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newobs_dataset <- FALSE    # or TRUE
    glb_split_entity_newobs_datasets <- TRUE   # or FALSE
    glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
    glb_split_newdata_condition <- NULL # or "is.na(<var>)"; "<var> <condition_operator> <value>"
    glb_split_newdata_size_ratio <- 0.35               # > 0 & < 1
    glb_split_sample.seed <- 1000               # or any integer

glb_max_fitobs <- NULL # or any integer                         
glb_is_regression <- FALSE; glb_is_classification <- !glb_is_regression; 
    glb_is_binomial <- TRUE # or TRUE or FALSE

glb_rsp_var_raw <- "TenYearCHD"

# for classification, the response variable has to be a factor
glb_rsp_var <- "TenYearCHD.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"), 
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) {
#     return(log(raw))
    ret_vals <- rep_len(NA, length(raw)); ret_vals[!is.na(raw)] <- ifelse(raw[!is.na(raw)] == 1, "Y", "N"); return(relevel(as.factor(ret_vals), ref="N"))
#     #as.factor(paste0("B", raw))
#     #as.factor(gsub(" ", "\\.", raw))    
}
glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA))
```

```
## [1] Y    Y    N    N    <NA>
## Levels: N Y
```

```r
glb_map_rsp_var_to_raw <- function(var) {
#     return(exp(var))
    as.numeric(var) - 1
#     #as.numeric(var)
#     #gsub("\\.", " ", levels(var)[as.numeric(var)])
#     c("<=50K", " >50K")[as.numeric(var)]
#     #c(FALSE, TRUE)[as.numeric(var)]
}
glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA)))
```

```
## [1]  1  1  0  0 NA
```

```r
if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- NULL # or c("<var1>")
glb_category_vars <- NULL # or c("<var1>", "<var2>")
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_map_vars <- NULL # or c("<var1>", "<var2>")
glb_map_urls <- list();
# glb_map_urls[["<var1>"]] <- "<var1.url>"

glb_assign_pairs_lst <- NULL; 
# glb_assign_pairs_lst[["<var1>"]] <- list(from=c(NA),
#                                            to=c("NA.my"))
glb_assign_vars <- names(glb_assign_pairs_lst)

# Derived features
glb_derive_lst <- NULL;
# glb_derive_lst[["Week.bgn"]] <- list(
#     mapfn=function(Week) { return(substr(Week, 1, 10)) }
#     , args=c("Week"))

# require(zoo)
# # If glb_allobs_df is not sorted in the desired manner
# glb_derive_lst[["ILI.2.lag"]] <- list(
#     mapfn=function(Week) { return(coredata(lag(zoo(orderBy(~Week, glb_allobs_df)$ILI), -2, na.pad=TRUE))) }
#     , args=c("Week"))
# glb_derive_lst[["ILI.2.lag"]] <- list(
#     mapfn=function(ILI) { return(coredata(lag(zoo(ILI), -2, na.pad=TRUE))) }
#     , args=c("ILI"))
# glb_derive_lst[["ILI.2.lag.log"]] <- list(
#     mapfn=function(ILI.2.lag) { return(log(ILI.2.lag)) }
#     , args=c("ILI.2.lag"))

#     mapfn=function(PTS, oppPTS) { return(PTS - oppPTS) }
#     , args=c("PTS", "oppPTS"))

# Add logs of numerics that are not distributed normally ->  do automatically ???

#     mapfn=function(raw) { tfr_raw <- as.character(cut(raw, 5)); 
#                           tfr_raw[is.na(tfr_raw)] <- "NA.my";
#                           return(as.factor(tfr_raw)) }

# glb_derive_lst[["<txt_var>.niso8859.log"]] <- list(
#     mapfn=function(<txt_var>) { match_lst <- gregexpr("&#[[:digit:]]{3};", <txt_var>)
#                         match_num_vctr <- unlist(lapply(match_lst, 
#                                                         function(elem) length(elem)))
#                         return(log(1 + match_num_vctr)) }
#     , args=c("<txt_var>"))

#     mapfn=function(raw) { mod_raw <- raw;
#         mod_raw <- gsub("&#[[:digit:]]{3};", " ", mod_raw);
#         # Modifications for this exercise only
#         mod_raw <- gsub("\\bgoodIn ", "good In", mod_raw);
#                           return(mod_raw)

#         # Create user-specified pattern vectors 
# #sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
#         if (txt_var %in% c("Snippet", "Abstract")) {
#             txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
#                 as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
#                                                    glb_allobs_df[, txt_var]))
#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])

# args_lst <- NULL; for (arg in glb_derive_lst[["Week.bgn"]]$args) args_lst[[arg]] <- glb_allobs_df[, arg]; do.call(mapfn, args_lst)

# glb_derive_lst[["<var1>"]] <- glb_derive_lst[["<var2>"]]
glb_derive_vars <- names(glb_derive_lst)

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# User-specified exclusions  
glb_exclude_vars_as_features <- NULL # or c("<var_name>") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](Fram_template2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 7.369  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
#glb_chunks_df <- myadd_chunk(NULL, "import.data")

glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/framingham.csv..."
## [1] "dimensions of data in ./data/framingham.csv: 4,240 rows x 16 cols"
##   male age education currentSmoker cigsPerDay BPMeds prevalentStroke
## 1    1  39         4             0          0      0               0
## 2    0  46         2             0          0      0               0
## 3    1  48         1             1         20      0               0
## 4    0  61         3             1         30      0               0
## 5    0  46         3             1         23      0               0
## 6    0  43         2             0          0      0               0
##   prevalentHyp diabetes totChol sysBP diaBP   BMI heartRate glucose
## 1            0        0     195 106.0    70 26.97        80      77
## 2            0        0     250 121.0    81 28.73        95      76
## 3            0        0     245 127.5    80 25.34        75      70
## 4            1        0     225 150.0    95 28.58        65     103
## 5            0        0     285 130.0    84 23.10        85      85
## 6            1        0     228 180.0   110 30.30        77      99
##   TenYearCHD
## 1          0
## 2          0
## 3          0
## 4          1
## 5          0
## 6          0
##      male age education currentSmoker cigsPerDay BPMeds prevalentStroke
## 147     0  59         1             1          1      0               0
## 646     0  39         1             1          3      0               0
## 2160    0  42         3             1         15      0               0
## 3085    0  40         1             1         15      0               0
## 3116    1  44         1             0          0      0               0
## 4195    1  65         1             1         20      1               0
##      prevalentHyp diabetes totChol sysBP diaBP   BMI heartRate glucose
## 147             1        0     259 141.0  86.0 25.97        70      86
## 646             0        0     273 116.0  86.0 28.73        75      NA
## 2160            0        0     212 115.0  72.0 23.72        73     100
## 3085            0        0     220 131.5  82.5 24.35        80      78
## 3116            0        0     238 132.0  86.0 27.22        75      85
## 4195            1        0     246 179.0  96.0 19.34        95      76
##      TenYearCHD
## 147           0
## 646           0
## 2160          0
## 3085          0
## 3116          0
## 4195          1
##      male age education currentSmoker cigsPerDay BPMeds prevalentStroke
## 4235    1  51         3             1         43      0               0
## 4236    0  48         2             1         20     NA               0
## 4237    0  44         1             1         15      0               0
## 4238    0  52         2             0          0      0               0
## 4239    1  40         3             0          0      0               0
## 4240    0  39         3             1         30      0               0
##      prevalentHyp diabetes totChol sysBP diaBP   BMI heartRate glucose
## 4235            0        0     207 126.5    80 19.71        65      68
## 4236            0        0     248 131.0    72 22.00        84      86
## 4237            0        0     210 126.5    87 19.16        86      NA
## 4238            0        0     269 133.5    83 21.47        80     107
## 4239            1        0     185 141.0    98 25.60        67      72
## 4240            0        0     196 133.0    86 20.91        85      80
##      TenYearCHD
## 4235          0
## 4236          0
## 4237          0
## 4238          0
## 4239          0
## 4240          0
## 'data.frame':	4240 obs. of  16 variables:
##  $ male           : int  1 0 1 0 0 0 0 0 1 1 ...
##  $ age            : int  39 46 48 61 46 43 63 45 52 43 ...
##  $ education      : int  4 2 1 3 3 2 1 2 1 1 ...
##  $ currentSmoker  : int  0 0 1 1 1 0 0 1 0 1 ...
##  $ cigsPerDay     : int  0 0 20 30 23 0 0 20 0 30 ...
##  $ BPMeds         : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ prevalentStroke: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ prevalentHyp   : int  0 0 0 1 0 1 0 0 1 1 ...
##  $ diabetes       : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ totChol        : int  195 250 245 225 285 228 205 313 260 225 ...
##  $ sysBP          : num  106 121 128 150 130 ...
##  $ diaBP          : num  70 81 80 95 84 110 71 71 89 107 ...
##  $ BMI            : num  27 28.7 25.3 28.6 23.1 ...
##  $ heartRate      : int  80 95 75 65 85 77 60 79 76 93 ...
##  $ glucose        : int  77 76 70 103 85 99 85 78 79 88 ...
##  $ TenYearCHD     : int  0 0 0 1 0 0 1 0 0 0 ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
## NULL
```

```r
# glb_trnobs_df <- read.delim("data/hygiene.txt", header=TRUE, fill=TRUE, sep="\t",
#                             fileEncoding='iso-8859-1')
# glb_trnobs_df <- read.table("data/hygiene.dat.labels", col.names=c("dirty"),
#                             na.strings="[none]")
# glb_trnobs_df$review <- readLines("data/hygiene.dat", n =-1)
# comment(glb_trnobs_df) <- "glb_trnobs_df"                                

# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
# glb_trnobs_df <- 
#     glb_trnobs_df %>% dplyr::filter(Year >= 1999)
                                
if (glb_is_separate_newobs_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newobs_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newobs_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
}         
```

```
## Loading required package: caTools
```

```
##    male age education currentSmoker cigsPerDay BPMeds prevalentStroke
## 2     0  46         2             0          0      0               0
## 5     0  46         3             1         23      0               0
## 9     1  52         1             0          0      0               0
## 14    0  41         3             0          0      1               0
## 17    1  48         3             1         10      0               0
## 19    0  38         2             1          5      0               0
##    prevalentHyp diabetes totChol sysBP diaBP   BMI heartRate glucose
## 2             0        0     250 121.0  81.0 28.73        95      76
## 5             0        0     285 130.0  84.0 23.10        85      85
## 9             1        0     260 141.5  89.0 26.36        76      79
## 14            1        0     332 124.0  88.0 31.31        65      84
## 17            1        0     232 138.0  90.0 22.37        64      72
## 19            0        0     195 122.0  84.5 23.24        75      78
##    TenYearCHD
## 2           0
## 5           0
## 9           0
## 14          0
## 17          0
## 19          0
##      male age education currentSmoker cigsPerDay BPMeds prevalentStroke
## 411     1  36         3             0          0      0               0
## 1562    1  67         1             0          0      0               0
## 1670    0  47         1             0          0      0               0
## 2219    1  43         2             1         20      0               0
## 3713    0  42         2             0          0      0               0
## 3796    0  48         1             0          0      0               0
##      prevalentHyp diabetes totChol sysBP diaBP   BMI heartRate glucose
## 411             0        0     174 131.0  86.0 26.23        75      64
## 1562            1        0     222 154.0 106.0 26.71        85      74
## 1670            1        0     305 128.0  92.5 27.64        75      62
## 2219            0        0     226 132.5  85.0 26.64        72      58
## 3713            0        0     234 124.0  80.5 20.06        75      NA
## 3796            1        0     224 152.5  90.0 29.80        67      85
##      TenYearCHD
## 411           0
## 1562          0
## 1670          0
## 2219          0
## 3713          0
## 3796          0
##      male age education currentSmoker cigsPerDay BPMeds prevalentStroke
## 4223    1  53         3             0          0      0               0
## 4225    1  47         2             1          3      0               0
## 4228    1  43         4             1         20      0               0
## 4229    0  50         1             0          0      0               0
## 4233    1  68         1             0          0      0               0
## 4236    0  48         2             1         20     NA               0
##      prevalentHyp diabetes totChol sysBP diaBP   BMI heartRate glucose
## 4223            1        0     289 188.0   110 26.70        70      63
## 4225            0        0     198 120.0    80 25.23        75      76
## 4228            0        0     187 129.5    88 25.62        80      75
## 4229            1        1     260 190.0   130 43.67        85     260
## 4233            1        0     176 168.0    97 23.14        60      79
## 4236            0        0     248 131.0    72 22.00        84      86
##      TenYearCHD
## 4223          0
## 4225          0
## 4228          0
## 4229          0
## 4233          1
## 4236          0
## 'data.frame':	1484 obs. of  16 variables:
##  $ male           : int  0 0 1 0 1 0 0 1 1 0 ...
##  $ age            : int  46 46 52 41 48 38 60 43 37 41 ...
##  $ education      : int  2 3 1 3 3 2 1 4 2 2 ...
##  $ currentSmoker  : int  0 1 0 0 1 1 0 1 0 1 ...
##  $ cigsPerDay     : int  0 23 0 0 10 5 0 43 0 1 ...
##  $ BPMeds         : int  0 0 0 1 0 0 0 0 0 0 ...
##  $ prevalentStroke: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ prevalentHyp   : int  0 0 1 1 1 0 0 0 1 0 ...
##  $ diabetes       : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ totChol        : int  250 285 260 332 232 195 260 226 225 237 ...
##  $ sysBP          : num  121 130 142 124 138 ...
##  $ diaBP          : num  81 84 89 88 90 84.5 72.5 85.5 92.5 78 ...
##  $ BMI            : num  28.7 23.1 26.4 31.3 22.4 ...
##  $ heartRate      : int  95 85 76 65 64 75 65 75 95 75 ...
##  $ glucose        : int  76 85 79 84 72 78 NA 75 83 74 ...
##  $ TenYearCHD     : int  0 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "comment")= chr "glb_newobs_df"
##   male age education currentSmoker cigsPerDay BPMeds prevalentStroke
## 1    1  39         4             0          0      0               0
## 3    1  48         1             1         20      0               0
## 4    0  61         3             1         30      0               0
## 6    0  43         2             0          0      0               0
## 7    0  63         1             0          0      0               0
## 8    0  45         2             1         20      0               0
##   prevalentHyp diabetes totChol sysBP diaBP   BMI heartRate glucose
## 1            0        0     195 106.0    70 26.97        80      77
## 3            0        0     245 127.5    80 25.34        75      70
## 4            1        0     225 150.0    95 28.58        65     103
## 6            1        0     228 180.0   110 30.30        77      99
## 7            0        0     205 138.0    71 33.11        60      85
## 8            0        0     313 100.0    71 21.68        79      78
##   TenYearCHD
## 1          0
## 3          0
## 4          1
## 6          0
## 7          1
## 8          0
##      male age education currentSmoker cigsPerDay BPMeds prevalentStroke
## 12      0  43         2             0          0      0               0
## 104     0  48         1             0          0      0               0
## 1010    0  51         2             1          1      0               0
## 2482    0  45         2             0          0      0               0
## 2674    1  36         1             1         20      0               0
## 3205    1  45         1             0          0      0               0
##      prevalentHyp diabetes totChol sysBP diaBP   BMI heartRate glucose
## 12              0        0     247   131  88.0 27.64        72      61
## 104             1        0     265   145  77.0 24.23        74      64
## 1010            0        0     220   142  82.5 21.02        56      78
## 2482            0        0     260    98  74.0 19.16        73      76
## 2674            0        0     242   115  75.0 25.64        83      83
## 3205            0        0     275   105  86.0 32.92        75      92
##      TenYearCHD
## 12            0
## 104           0
## 1010          0
## 2482          0
## 2674          0
## 3205          0
##      male age education currentSmoker cigsPerDay BPMeds prevalentStroke
## 4234    1  50         1             1          1      0               0
## 4235    1  51         3             1         43      0               0
## 4237    0  44         1             1         15      0               0
## 4238    0  52         2             0          0      0               0
## 4239    1  40         3             0          0      0               0
## 4240    0  39         3             1         30      0               0
##      prevalentHyp diabetes totChol sysBP diaBP   BMI heartRate glucose
## 4234            1        0     313 179.0    92 25.97        66      86
## 4235            0        0     207 126.5    80 19.71        65      68
## 4237            0        0     210 126.5    87 19.16        86      NA
## 4238            0        0     269 133.5    83 21.47        80     107
## 4239            1        0     185 141.0    98 25.60        67      72
## 4240            0        0     196 133.0    86 20.91        85      80
##      TenYearCHD
## 4234          1
## 4235          0
## 4237          0
## 4238          0
## 4239          0
## 4240          0
## 'data.frame':	2756 obs. of  16 variables:
##  $ male           : int  1 1 0 0 0 0 1 0 0 1 ...
##  $ age            : int  39 48 61 43 63 45 43 50 43 46 ...
##  $ education      : int  4 1 3 2 1 2 1 1 2 1 ...
##  $ currentSmoker  : int  0 1 1 0 0 1 1 0 0 1 ...
##  $ cigsPerDay     : int  0 20 30 0 0 20 30 0 0 15 ...
##  $ BPMeds         : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ prevalentStroke: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ prevalentHyp   : int  0 0 1 1 0 0 1 0 0 1 ...
##  $ diabetes       : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ totChol        : int  195 245 225 228 205 313 225 254 247 294 ...
##  $ sysBP          : num  106 128 150 180 138 ...
##  $ diaBP          : num  70 80 95 110 71 71 107 76 88 94 ...
##  $ BMI            : num  27 25.3 28.6 30.3 33.1 ...
##  $ heartRate      : int  80 75 65 77 60 79 93 75 72 98 ...
##  $ glucose        : int  77 70 103 99 85 78 88 76 61 64 ...
##  $ TenYearCHD     : int  0 0 1 0 1 0 0 0 0 0 ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
```

```r
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Combine trnent & newobs into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"

# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Train"))
    glb_newobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Test"))    
    glb_id_var <- ".rownames"
}
```

```
## Warning: using .rownames as identifiers for observations
```

```r
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 7.369 7.882   0.513
## 2 inspect.data          2          0 7.882    NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Loading required package: reshape2
```

![](Fram_template2_files/figure-html/inspect.data-1.png) 

```
##       TenYearCHD.0 TenYearCHD.1
## Test          1259          225
## Train         2337          419
##       TenYearCHD.0 TenYearCHD.1
## Test     0.8483827    0.1516173
## Train    0.8479681    0.1520319
## [1] "numeric data missing in glb_allobs_df: "
##  education cigsPerDay     BPMeds    totChol        BMI  heartRate 
##        105         29         53         50         19          1 
##    glucose 
##        388 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##            male   currentSmoker      cigsPerDay          BPMeds 
##            2420            2145            2145            4063 
## prevalentStroke    prevalentHyp        diabetes      TenYearCHD 
##            4215            2923            4131            3596 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## .rownames 
##         0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##   TenYearCHD TenYearCHD.fctr   .n
## 1          0               N 3596
## 2          1               Y  644
```

![](Fram_template2_files/figure-html/inspect.data-2.png) 

```
##       TenYearCHD.fctr.N TenYearCHD.fctr.Y
## Test               1259               225
## Train              2337               419
##       TenYearCHD.fctr.N TenYearCHD.fctr.Y
## Test          0.8483827         0.1516173
## Train         0.8479681         0.1520319
```

```r
# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: male"
```

![](Fram_template2_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: age"
```

![](Fram_template2_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: education"
```

```
## Warning: Removed 105 rows containing non-finite values (stat_boxplot).
```

```
## Warning: Removed 105 rows containing missing values (stat_summary).
```

![](Fram_template2_files/figure-html/inspect.data-5.png) 

```
## [1] "feat: currentSmoker"
```

![](Fram_template2_files/figure-html/inspect.data-6.png) 

```
## [1] "feat: cigsPerDay"
```

```
## Warning: Removed 29 rows containing non-finite values (stat_boxplot).
```

```
## Warning: Removed 29 rows containing missing values (stat_summary).
```

![](Fram_template2_files/figure-html/inspect.data-7.png) 

```
## [1] "feat: BPMeds"
```

```
## Warning: Removed 53 rows containing non-finite values (stat_boxplot).
```

```
## Warning: Removed 53 rows containing missing values (stat_summary).
```

![](Fram_template2_files/figure-html/inspect.data-8.png) 

```
## [1] "feat: prevalentStroke"
```

![](Fram_template2_files/figure-html/inspect.data-9.png) 

```
## [1] "feat: prevalentHyp"
```

![](Fram_template2_files/figure-html/inspect.data-10.png) 

```
## [1] "feat: diabetes"
```

![](Fram_template2_files/figure-html/inspect.data-11.png) 

```
## [1] "feat: totChol"
```

```
## Warning: Removed 50 rows containing non-finite values (stat_boxplot).
```

```
## Warning: Removed 50 rows containing missing values (stat_summary).
```

![](Fram_template2_files/figure-html/inspect.data-12.png) 

```
## [1] "feat: sysBP"
```

![](Fram_template2_files/figure-html/inspect.data-13.png) 

```
## [1] "feat: diaBP"
```

![](Fram_template2_files/figure-html/inspect.data-14.png) 

```
## [1] "feat: BMI"
```

```
## Warning: Removed 19 rows containing non-finite values (stat_boxplot).
```

```
## Warning: Removed 19 rows containing missing values (stat_summary).
```

![](Fram_template2_files/figure-html/inspect.data-15.png) 

```
## [1] "feat: heartRate"
```

```
## Warning: Removed 1 rows containing non-finite values (stat_boxplot).
```

```
## Warning: Removed 1 rows containing missing values (stat_summary).
```

![](Fram_template2_files/figure-html/inspect.data-16.png) 

```
## [1] "feat: glucose"
```

```
## Warning: Removed 388 rows containing non-finite values (stat_boxplot).
```

```
## Warning: Removed 388 rows containing missing values (stat_summary).
```

![](Fram_template2_files/figure-html/inspect.data-17.png) 

```
## [1] "feat: .rnorm"
```

![](Fram_template2_files/figure-html/inspect.data-18.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5) +
#         geom_vline(xintercept=84))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0  7.882 17.703   9.822
## 3   scrub.data          2          1 17.704     NA      NA
```

### Step `2.1: scrub data`

```r
mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##  education cigsPerDay     BPMeds    totChol        BMI  heartRate 
##        105         29         53         50         19          1 
##    glucose 
##        388 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##            male   currentSmoker      cigsPerDay          BPMeds 
##            2420            2145            2145            4063 
## prevalentStroke    prevalentHyp        diabetes      TenYearCHD 
##            4215            2923            4131            3596 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## .rownames 
##         0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

# sel_obs <- function(Popular=NULL, 
#                     NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
#         Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
#         Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
#         perl=FALSE) {
sel_obs <- function(vars_lst) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 17.704 19.657   1.953
## 4 transform.data          2          2 19.657     NA      NA
```

```r
### Mapping dictionary
#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}

### Forced Assignments
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}

### Derivations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (new_feat in glb_derive_vars) {
    print(sprintf("Creating new feature: %s...", new_feat))
    args_lst <- NULL 
    for (arg in glb_derive_lst[[new_feat]]$args) 
        args_lst[[arg]] <- glb_allobs_df[, arg]
    glb_allobs_df[, new_feat] <- do.call(glb_derive_lst[[new_feat]]$mapfn, args_lst)
}
```

## Step `2.2: transform data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 4   transform.data          2          2 19.657 19.717    0.06
## 5 extract.features          3          0 19.717     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 19.723  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

#stop(here"); sav_allobs_df <- glb_allobs_df #; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    for (sfx in c("", ".POSIX"))
        glb_exclude_vars_as_features <- 
            union(glb_exclude_vars_as_features, 
                    paste(glb_date_vars, sfx, sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last2 <- as.numeric(merge(z-lag(z, -2), b, all=TRUE)); last2[is.na(last2)] <- 0
        glb_allobs_df[, paste0(feat, ".last2.log")] <- log(1 + last2)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last2.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last2.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}
rm(last1, last10, last100)
```

```
## Warning in rm(last1, last10, last100): object 'last1' not found
```

```
## Warning in rm(last1, last10, last100): object 'last10' not found
```

```
## Warning in rm(last1, last10, last100): object 'last100' not found
```

```r
#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 19.723 19.737
## 2 extract.features_factorize.str.vars          2          0 19.738     NA
##   elapsed
## 1   0.014
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##        .src   .rownames 
##      ".src" ".rownames"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               c(glb_exclude_vars_as_features, glb_txt_vars))) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- 
            relevel(factor(glb_allobs_df[, var]),
                    names(which.max(table(glb_allobs_df[, var], useNA = "ifany"))))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}

if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(rex_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(rex_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }

#     match_lst <- gregexpr("\\bok(?!ay)", txt_vctr[746], ignore.case = FALSE, perl=TRUE); print(match_lst)
    dsp_pattern <- function(rex_str, ignore.case=TRUE, print.all=TRUE) {
        match_lst <- gregexpr(rex_str, txt_vctr, ignore.case = ignore.case, perl=TRUE)
        match_lst <- regmatches(txt_vctr, match_lst)
        match_df <- data.frame(matches=sapply(match_lst, 
                                              function (elems) paste(elems, collapse="#")))
        match_df <- subset(match_df, matches != "")
        if (print.all)
            print(match_df)
        return(match_df)
    }
    
    dsp_matches <- function(rex_str, ix) {
        print(match_pos <- gregexpr(rex_str, txt_vctr[ix], perl=TRUE))
        print(str_sub(txt_vctr[ix], (match_pos[[1]] / 100) *  99 +   0, 
                                    (match_pos[[1]] / 100) * 100 + 100))        
    }

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], 
                        glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#chk.equal( 1, 100)
#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[163, "rex_str"])
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining OK in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "(?<!(BO|HO|LO))OK(?!(E\\!|ED|IE|IN|S ))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "Ok(?!(a\\.|ay|in|ra|um))", ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "(?<!( b| B| c| C| g| G| j| M| p| P| w| W| r| Z|\\(b|ar|bo|Bo|co|Co|Ew|gk|go|ho|ig|jo|kb|ke|Ke|ki|lo|Lo|mo|mt|no|No|po|ra|ro|sm|Sm|Sp|to|To))ok(?!(ay|bo|e |e\\)|e,|e\\.|eb|ed|el|en|er|es|ey|i |ie|in|it|ka|ke|ki|ly|on|oy|ra|st|u |uc|uy|yl|yo))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))
    }    
    # txt_vctr <- glb_txt_lst[[glb_txt_vars[1]]]
    # print(chk_pattern_freq(rex_str <- "(?<!( b| c| C| p|\\(b|bo|co|lo|Lo|Sp|to|To))ok(?!(ay|e |e\\)|e,|e\\.|ed|el|en|es|ey|ie|in|on|ra))", ignore.case=FALSE))
    # print(chk_pattern_freq(rex_str <- "ok(?!(ay|el|on|ra))", ignore.case=FALSE))
    # dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
    # dsp_matches(rex_str, ix=8)
    # substr(txt_vctr[86], 5613, 5620)
    # substr(glb_allobs_df[301, "review"], 550, 650)

#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "([[:upper:]]\\.( *)){2,}", ignore.case=FALSE))
        
        # Check for names
        print(subset(chk_pattern_freq(rex_str <- "(([[:upper:]]+)\\.( *)){1}",
                                      ignore.case=FALSE),
                     .n > 1))
        # dsp_pattern(rex_str="(OK\\.( *)){1}", ignore.case=FALSE)
        # dsp_matches(rex_str="(OK\\.( *)){1}", ix=557)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)(\\B)", ix=461)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)", ix=461)        
        #print(str_sub(txt_vctr[676], 10100, 10200))
        #print(str_sub(txt_vctr[74], 1, -1))        
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 19.738 19.755
## 3                extract.features_end          3          0 19.756     NA
##   elapsed
## 2   0.017
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 19.738 19.755
## 1                extract.features_bgn          1          0 19.723 19.737
##   elapsed duration
## 2   0.017    0.017
## 1   0.014    0.014
## [1] "Total Elapsed Time: 19.755 secs"
```

![](Fram_template2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](Fram_template2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn   end elapsed
## 5 extract.features          3          0 19.717 21.03   1.313
## 6     cluster.data          4          0 21.031    NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 21.031 21.321    0.29
## 7 manage.missing.data          4          1 21.321     NA      NA
```

```r
# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##  education cigsPerDay     BPMeds    totChol        BMI  heartRate 
##        105         29         53         50         19          1 
##    glucose 
##        388 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##            male   currentSmoker      cigsPerDay          BPMeds 
##            2420            2145            2145            4063 
## prevalentStroke    prevalentHyp        diabetes      TenYearCHD 
##            4215            2923            4131            3596 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## .rownames 
##         0
```

```r
# glb_allobs_df <- na.omit(glb_allobs_df)

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    # complete(mice()) changes attributes of factors even though values don't change
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col], inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
```

```
##  education cigsPerDay     BPMeds    totChol        BMI  heartRate 
##        105         29         53         50         19          1 
##    glucose 
##        388
```

```
## Loading required package: mice
## Loading required package: Rcpp
## mice 2.22 2014-06-10
```

```
## [1] "Summary before imputation: "
##       male             age          education     currentSmoker   
##  Min.   :0.0000   Min.   :32.00   Min.   :1.000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:42.00   1st Qu.:1.000   1st Qu.:0.0000  
##  Median :0.0000   Median :49.00   Median :2.000   Median :0.0000  
##  Mean   :0.4292   Mean   :49.58   Mean   :1.979   Mean   :0.4941  
##  3rd Qu.:1.0000   3rd Qu.:56.00   3rd Qu.:3.000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :70.00   Max.   :4.000   Max.   :1.0000  
##                                   NA's   :105                     
##    cigsPerDay         BPMeds        prevalentStroke     prevalentHyp   
##  Min.   : 0.000   Min.   :0.00000   Min.   :0.000000   Min.   :0.0000  
##  1st Qu.: 0.000   1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.0000  
##  Median : 0.000   Median :0.00000   Median :0.000000   Median :0.0000  
##  Mean   : 9.006   Mean   :0.02962   Mean   :0.005896   Mean   :0.3106  
##  3rd Qu.:20.000   3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:1.0000  
##  Max.   :70.000   Max.   :1.00000   Max.   :1.000000   Max.   :1.0000  
##  NA's   :29       NA's   :53                                           
##     diabetes          totChol          sysBP           diaBP      
##  Min.   :0.00000   Min.   :107.0   Min.   : 83.5   Min.   : 48.0  
##  1st Qu.:0.00000   1st Qu.:206.0   1st Qu.:117.0   1st Qu.: 75.0  
##  Median :0.00000   Median :234.0   Median :128.0   Median : 82.0  
##  Mean   :0.02571   Mean   :236.7   Mean   :132.4   Mean   : 82.9  
##  3rd Qu.:0.00000   3rd Qu.:263.0   3rd Qu.:144.0   3rd Qu.: 90.0  
##  Max.   :1.00000   Max.   :696.0   Max.   :295.0   Max.   :142.5  
##                    NA's   :50                                     
##       BMI          heartRate         glucose           .rnorm         
##  Min.   :15.54   Min.   : 44.00   Min.   : 40.00   Min.   :-3.561788  
##  1st Qu.:23.07   1st Qu.: 68.00   1st Qu.: 71.00   1st Qu.:-0.688598  
##  Median :25.40   Median : 75.00   Median : 78.00   Median :-0.002425  
##  Mean   :25.80   Mean   : 75.88   Mean   : 81.96   Mean   :-0.000141  
##  3rd Qu.:28.04   3rd Qu.: 83.00   3rd Qu.: 87.00   3rd Qu.: 0.674799  
##  Max.   :56.80   Max.   :143.00   Max.   :394.00   Max.   : 3.739140  
##  NA's   :19      NA's   :1        NA's   :388                         
## 
##  iter imp variable
##   1   1  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   1   2  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   1   3  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   1   4  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   1   5  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   2   1  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   2   2  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   2   3  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   2   4  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   2   5  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   3   1  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   3   2  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   3   3  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   3   4  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   3   5  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   4   1  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   4   2  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   4   3  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   4   4  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   4   5  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   5   1  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   5   2  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   5   3  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   5   4  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##   5   5  education  cigsPerDay  BPMeds  totChol  BMI  heartRate  glucose
##       male             age          education     currentSmoker   
##  Min.   :0.0000   Min.   :32.00   Min.   :1.000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:42.00   1st Qu.:1.000   1st Qu.:0.0000  
##  Median :0.0000   Median :49.00   Median :2.000   Median :0.0000  
##  Mean   :0.4292   Mean   :49.58   Mean   :1.982   Mean   :0.4941  
##  3rd Qu.:1.0000   3rd Qu.:56.00   3rd Qu.:3.000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :70.00   Max.   :4.000   Max.   :1.0000  
##    cigsPerDay        BPMeds        prevalentStroke     prevalentHyp   
##  Min.   : 0.00   Min.   :0.00000   Min.   :0.000000   Min.   :0.0000  
##  1st Qu.: 0.00   1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.0000  
##  Median : 0.00   Median :0.00000   Median :0.000000   Median :0.0000  
##  Mean   : 9.05   Mean   :0.02995   Mean   :0.005896   Mean   :0.3106  
##  3rd Qu.:20.00   3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:1.0000  
##  Max.   :70.00   Max.   :1.00000   Max.   :1.000000   Max.   :1.0000  
##     diabetes          totChol          sysBP           diaBP      
##  Min.   :0.00000   Min.   :107.0   Min.   : 83.5   Min.   : 48.0  
##  1st Qu.:0.00000   1st Qu.:206.0   1st Qu.:117.0   1st Qu.: 75.0  
##  Median :0.00000   Median :234.0   Median :128.0   Median : 82.0  
##  Mean   :0.02571   Mean   :236.7   Mean   :132.4   Mean   : 82.9  
##  3rd Qu.:0.00000   3rd Qu.:263.0   3rd Qu.:144.0   3rd Qu.: 90.0  
##  Max.   :1.00000   Max.   :696.0   Max.   :295.0   Max.   :142.5  
##       BMI          heartRate         glucose           .rnorm         
##  Min.   :15.54   Min.   : 44.00   Min.   : 40.00   Min.   :-3.561788  
##  1st Qu.:23.06   1st Qu.: 68.00   1st Qu.: 71.00   1st Qu.:-0.688598  
##  Median :25.38   Median : 75.00   Median : 78.00   Median :-0.002425  
##  Mean   :25.79   Mean   : 75.88   Mean   : 81.71   Mean   :-0.000141  
##  3rd Qu.:28.04   3rd Qu.: 83.00   3rd Qu.: 87.00   3rd Qu.: 0.674799  
##  Max.   :56.80   Max.   :143.00   Max.   :394.00   Max.   : 3.739140
```

```r
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##  education cigsPerDay     BPMeds    totChol        BMI  heartRate 
##        105         29         53         50         19          1 
##    glucose 
##        388 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##             male    currentSmoker       cigsPerDay           BPMeds 
##             2420             2145             2145             4063 
##  prevalentStroke     prevalentHyp         diabetes       TenYearCHD 
##             4215             2923             4131             3596 
## cigsPerDay.nonNA     BPMeds.nonNA 
##             2145             4113 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## .rownames 
##         0
```

## Step `4.1: manage missing data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 7 manage.missing.data          4          1 21.321 30.357   9.036
## 8     select.features          5          0 30.357     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                id        cor.y exclude.as.feat   cor.y.abs
## TenYearCHD             TenYearCHD  1.000000000               1 1.000000000
## age                           age  0.226307320               0 0.226307320
## sysBP                       sysBP  0.208178303               0 0.208178303
## prevalentHyp         prevalentHyp  0.178624122               0 0.178624122
## diaBP                       diaBP  0.135980635               0 0.135980635
## glucose                   glucose  0.125905881               1 0.125905881
## glucose.nonNA       glucose.nonNA  0.117693791               0 0.117693791
## totChol.nonNA       totChol.nonNA  0.101569483               0 0.101569483
## totChol                   totChol  0.101114643               1 0.101114643
## BPMeds                     BPMeds  0.094314490               1 0.094314490
## BPMeds.nonNA         BPMeds.nonNA  0.092269768               0 0.092269768
## male                         male  0.085680208               0 0.085680208
## BMI                           BMI  0.082769510               1 0.082769510
## BMI.nonNA               BMI.nonNA  0.080208915               0 0.080208915
## prevalentStroke   prevalentStroke  0.078578145               0 0.078578145
## diabetes                 diabetes  0.077858385               0 0.077858385
## education               education -0.058939008               1 0.058939008
## education.nonNA   education.nonNA -0.057873618               0 0.057873618
## cigsPerDay             cigsPerDay  0.046633660               1 0.046633660
## cigsPerDay.nonNA cigsPerDay.nonNA  0.044731673               0 0.044731673
## .rnorm                     .rnorm -0.019000943               0 0.019000943
## currentSmoker       currentSmoker  0.012560421               0 0.012560421
## heartRate               heartRate  0.007677700               1 0.007677700
## heartRate.nonNA   heartRate.nonNA  0.006531896               0 0.006531896
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## [1] "cor(diaBP, sysBP)=0.7768"
## [1] "cor(TenYearCHD.fctr, diaBP)=0.1360"
## [1] "cor(TenYearCHD.fctr, sysBP)=0.2082"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified diaBP as highly correlated with sysBP
```

```
##                  id        cor.y exclude.as.feat   cor.y.abs cor.high.X
## 22       TenYearCHD  1.000000000               1 1.000000000       <NA>
## 2               age  0.226307320               0 0.226307320       <NA>
## 21            sysBP  0.208178303               0 0.208178303       <NA>
## 19     prevalentHyp  0.178624122               0 0.178624122       <NA>
## 11            diaBP  0.135980635               0 0.135980635      sysBP
## 14          glucose  0.125905881               1 0.125905881       <NA>
## 15    glucose.nonNA  0.117693791               0 0.117693791       <NA>
## 24    totChol.nonNA  0.101569483               0 0.101569483       <NA>
## 23          totChol  0.101114643               1 0.101114643       <NA>
## 5            BPMeds  0.094314490               1 0.094314490       <NA>
## 6      BPMeds.nonNA  0.092269768               0 0.092269768       <NA>
## 18             male  0.085680208               0 0.085680208       <NA>
## 3               BMI  0.082769510               1 0.082769510       <NA>
## 4         BMI.nonNA  0.080208915               0 0.080208915       <NA>
## 20  prevalentStroke  0.078578145               0 0.078578145       <NA>
## 10         diabetes  0.077858385               0 0.077858385       <NA>
## 7        cigsPerDay  0.046633660               1 0.046633660       <NA>
## 8  cigsPerDay.nonNA  0.044731673               0 0.044731673       <NA>
## 9     currentSmoker  0.012560421               0 0.012560421       <NA>
## 16        heartRate  0.007677700               1 0.007677700       <NA>
## 17  heartRate.nonNA  0.006531896               0 0.006531896       <NA>
## 1            .rnorm -0.019000943               0 0.019000943       <NA>
## 13  education.nonNA -0.057873618               0 0.057873618       <NA>
## 12        education -0.058939008               1 0.058939008       <NA>
##     freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 22   5.577566    0.07256894   FALSE FALSE    FALSE            FALSE
## 2    1.025424    1.41509434   FALSE FALSE    FALSE            FALSE
## 21   1.014493    7.94629898   FALSE FALSE    FALSE            FALSE
## 19   2.197216    0.07256894   FALSE FALSE    FALSE            FALSE
## 11   1.702128    4.89840348   FALSE FALSE    FALSE            FALSE
## 14   1.072072    4.24528302   FALSE FALSE    FALSE            FALSE
## 15   1.145299    4.28156749   FALSE FALSE    FALSE            FALSE
## 24   1.204545    8.38171263   FALSE FALSE    FALSE            FALSE
## 23   1.181818    8.38171263   FALSE FALSE    FALSE            FALSE
## 5   34.337662    0.07256894   FALSE  TRUE    FALSE            FALSE
## 6   34.333333    0.07256894   FALSE  TRUE    FALSE            FALSE
## 18   1.365665    0.07256894   FALSE FALSE    FALSE            FALSE
## 3    1.142857   40.89259797   FALSE FALSE    FALSE            FALSE
## 4    1.142857   40.89259797   FALSE FALSE    FALSE            FALSE
## 20 152.111111    0.07256894   FALSE  TRUE    FALSE            FALSE
## 10  37.816901    0.07256894   FALSE  TRUE    FALSE            FALSE
## 7    2.991507    1.16110305   FALSE FALSE    FALSE            FALSE
## 8    2.966316    1.16110305   FALSE FALSE    FALSE            FALSE
## 9    1.046028    0.07256894   FALSE FALSE    FALSE             TRUE
## 16   1.485944    2.46734398   FALSE FALSE    FALSE             TRUE
## 17   1.485944    2.46734398   FALSE FALSE    FALSE             TRUE
## 1    1.000000  100.00000000   FALSE FALSE    FALSE            FALSE
## 13   1.392298    0.14513788   FALSE FALSE    FALSE            FALSE
## 12   1.375765    0.14513788   FALSE FALSE    FALSE            FALSE
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

![](Fram_template2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
##  education cigsPerDay     BPMeds    totChol        BMI  heartRate 
##        105         29         53         50         19          1 
##    glucose 
##        388 
## [1] "numeric data w/ 0s in : "
##             male    currentSmoker       cigsPerDay           BPMeds 
##             2420             2145             2145             4063 
##  prevalentStroke     prevalentHyp         diabetes       TenYearCHD 
##             4215             2923             4131             3596 
## cigsPerDay.nonNA     BPMeds.nonNA 
##             2145             4113 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## .rownames 
##         0
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 30.357 31.135   0.778
## 9 partition.data.training          6          0 31.135     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## [1] "Newdata contains non-NA data for TenYearCHD.fctr; setting OOB to Newdata"
```

```r
if (!is.null(glb_max_fitobs) && (nrow(glb_fitobs_df) > glb_max_fitobs)) {
    warning("glb_fitobs_df restricted to glb_max_fitobs: ", 
            format(glb_max_fitobs, big.mark=","))
    org_fitobs_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitobs_df[split <- sample.split(org_fitobs_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitobs), ]
    org_fitobs_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newobs_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 24 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                              id exclude.as.feat rsp_var
## TenYearCHD.fctr TenYearCHD.fctr            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                              id cor.y exclude.as.feat cor.y.abs cor.high.X
## 22                   TenYearCHD     1            TRUE         1       <NA>
## TenYearCHD.fctr TenYearCHD.fctr    NA            TRUE        NA       <NA>
##                 freqRatio percentUnique zeroVar   nzv myNearZV
## 22               5.577566    0.07256894   FALSE FALSE    FALSE
## TenYearCHD.fctr        NA            NA      NA    NA       NA
##                 is.cor.y.abs.low interaction.feat rsp_var_raw rsp_var
## 22                         FALSE               NA        TRUE      NA
## TenYearCHD.fctr               NA               NA          NA    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 4240   28
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 2756   27
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 2756   27
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 1484   27
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 1484   27
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 31.135 31.473   0.338
## 10              fit.models          7          0 31.473     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##         N         Y 
## 0.8479681 0.1520319 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8479681 0.1520319
## 2 0.8479681 0.1520319
## 3 0.8479681 0.1520319
## 4 0.8479681 0.1520319
## 5 0.8479681 0.1520319
## 6 0.8479681 0.1520319
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.MFO.myMFO_classfr.N
## 1               N                                        2337
## 2               Y                                         419
##          Prediction
## Reference    N    Y
##         N 2337    0
##         Y  419    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.479681e-01   0.000000e+00   8.340130e-01   8.611796e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   5.130333e-01   1.096271e-92 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8479681 0.1520319
## 2 0.8479681 0.1520319
## 3 0.8479681 0.1520319
## 4 0.8479681 0.1520319
## 5 0.8479681 0.1520319
## 6 0.8479681 0.1520319
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.MFO.myMFO_classfr.N
## 1               N                                        1259
## 2               Y                                         225
##          Prediction
## Reference    N    Y
##         N 1259    0
##         Y  225    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.483827e-01   0.000000e+00   8.291112e-01   8.662638e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   5.177806e-01   2.000126e-50 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.267                 0.003         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8479681
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.834013             0.8611796             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8483827
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8291112             0.8662638             0
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Random.myrandom_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      table      numeric  
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in Random.Classifier$prob"
```

![](Fram_template2_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.2639370
## 2        0.1 0.2639370
## 3        0.2 0.1454994
## 4        0.3 0.1454994
## 5        0.4 0.1454994
## 6        0.5 0.1454994
## 7        0.6 0.1454994
## 8        0.7 0.1454994
## 9        0.8 0.1454994
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](Fram_template2_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Random.myrandom_classfr.Y
## 1               N                                              2337
## 2               Y                                               419
##          Prediction
## Reference    N    Y
##         N    0 2337
##         Y    0  419
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.1520319      0.0000000      0.1388204      0.1659870      0.8479681 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.0000000 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](Fram_template2_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.2633119
## 2        0.1 0.2633119
## 3        0.2 0.1755196
## 4        0.3 0.1755196
## 5        0.4 0.1755196
## 6        0.5 0.1755196
## 7        0.6 0.1755196
## 8        0.7 0.1755196
## 9        0.8 0.1755196
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](Fram_template2_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Random.myrandom_classfr.Y
## 1               N                                              1259
## 2               Y                                               225
##          Prediction
## Reference    N    Y
##         N    0 1259
##         Y    0  225
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.516173e-01   0.000000e+00   1.337362e-01   1.708888e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00  2.498464e-275 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.231                 0.002   0.4991605
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.1        0.263937        0.1520319
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.1388204              0.165987             0   0.5169305
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.2633119        0.1516173
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.1337362             0.1708888             0
```

```r
# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: age, sysBP"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.00398 on full training set
```

```
## Loading required package: rpart.plot
```

![](Fram_template2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 2756 
## 
##            CP nsplit rel error
## 1 0.003977725      0         1
## 
## Node number 1: 2756 observations
##   predicted class=N  expected loss=0.1520319  P(node) =1
##     class counts:  2337   419
##    probabilities: 0.848 0.152 
## 
## n= 2756 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 2756 419 N (0.8479681 0.1520319) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1               N                                           2337
## 2               Y                                            419
##          Prediction
## Reference    N    Y
##         N 2337    0
##         Y  419    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.479681e-01   0.000000e+00   8.340130e-01   8.611796e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   5.130333e-01   1.096271e-92 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1               N                                           1259
## 2               Y                                            225
##          Prediction
## Reference    N    Y
##         N 1259    0
##         Y  225    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.483827e-01   0.000000e+00   8.291112e-01   8.662638e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   5.177806e-01   2.000126e-50 
##               model_id model_method      feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart age, sysBP               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.586                 0.046         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8479681
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.834013             0.8611796             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8483827
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8291112             0.8662638             0
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: age, sysBP"
## Fitting cp = 0 on full training set
```

![](Fram_template2_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 2756 
## 
##             CP nsplit rel error
## 1 0.0039777247      0 1.0000000
## 2 0.0023866348      3 0.9880668
## 3 0.0017899761      7 0.9785203
## 4 0.0006818957     20 0.9451074
## 5 0.0000000000     27 0.9403341
## 
## Variable importance
## sysBP   age 
##    54    46 
## 
## Node number 1: 2756 observations,    complexity param=0.003977725
##   predicted class=N  expected loss=0.1520319  P(node) =1
##     class counts:  2337   419
##    probabilities: 0.848 0.152 
##   left son=2 (1511 obs) right son=3 (1245 obs)
##   Primary splits:
##       age   < 50.5   to the left,  improve=27.41007, (0 missing)
##       sysBP < 144.25 to the left,  improve=23.70079, (0 missing)
##   Surrogate splits:
##       sysBP < 137.75 to the left,  agree=0.667, adj=0.263, (0 split)
## 
## Node number 2: 1511 observations
##   predicted class=N  expected loss=0.08802118  P(node) =0.5482583
##     class counts:  1378   133
##    probabilities: 0.912 0.088 
## 
## Node number 3: 1245 observations,    complexity param=0.003977725
##   predicted class=N  expected loss=0.2297189  P(node) =0.4517417
##     class counts:   959   286
##    probabilities: 0.770 0.230 
##   left son=6 (767 obs) right son=7 (478 obs)
##   Primary splits:
##       sysBP < 144.25 to the left,  improve=9.907748, (0 missing)
##       age   < 64.5   to the left,  improve=5.245415, (0 missing)
##   Surrogate splits:
##       age < 62.5   to the left,  agree=0.635, adj=0.05, (0 split)
## 
## Node number 6: 767 observations,    complexity param=0.0006818957
##   predicted class=N  expected loss=0.1799218  P(node) =0.2783019
##     class counts:   629   138
##    probabilities: 0.820 0.180 
##   left son=12 (734 obs) right son=13 (33 obs)
##   Primary splits:
##       age   < 65.5   to the left,  improve=2.327719, (0 missing)
##       sysBP < 113.25 to the left,  improve=1.352177, (0 missing)
## 
## Node number 7: 478 observations,    complexity param=0.003977725
##   predicted class=N  expected loss=0.3096234  P(node) =0.1734398
##     class counts:   330   148
##    probabilities: 0.690 0.310 
##   left son=14 (461 obs) right son=15 (17 obs)
##   Primary splits:
##       sysBP < 209    to the left,  improve=4.014090, (0 missing)
##       age   < 54.5   to the left,  improve=3.039295, (0 missing)
## 
## Node number 12: 734 observations,    complexity param=0.0006818957
##   predicted class=N  expected loss=0.1716621  P(node) =0.266328
##     class counts:   608   126
##    probabilities: 0.828 0.172 
##   left son=24 (123 obs) right son=25 (611 obs)
##   Primary splits:
##       sysBP < 113.25 to the left,  improve=1.6227060, (0 missing)
##       age   < 59.5   to the left,  improve=0.5307999, (0 missing)
## 
## Node number 13: 33 observations
##   predicted class=N  expected loss=0.3636364  P(node) =0.01197388
##     class counts:    21    12
##    probabilities: 0.636 0.364 
## 
## Node number 14: 461 observations,    complexity param=0.002386635
##   predicted class=N  expected loss=0.29718  P(node) =0.1672714
##     class counts:   324   137
##    probabilities: 0.703 0.297 
##   left son=28 (98 obs) right son=29 (363 obs)
##   Primary splits:
##       age   < 54.5   to the left,  improve=2.1574210, (0 missing)
##       sysBP < 174.25 to the left,  improve=0.9362037, (0 missing)
## 
## Node number 15: 17 observations
##   predicted class=Y  expected loss=0.3529412  P(node) =0.00616836
##     class counts:     6    11
##    probabilities: 0.353 0.647 
## 
## Node number 24: 123 observations
##   predicted class=N  expected loss=0.09756098  P(node) =0.0446299
##     class counts:   111    12
##    probabilities: 0.902 0.098 
## 
## Node number 25: 611 observations,    complexity param=0.0006818957
##   predicted class=N  expected loss=0.1865794  P(node) =0.2216981
##     class counts:   497   114
##    probabilities: 0.813 0.187 
##   left son=50 (75 obs) right son=51 (536 obs)
##   Primary splits:
##       sysBP < 140.25 to the right, improve=0.7579615, (0 missing)
##       age   < 57.5   to the left,  improve=0.5568661, (0 missing)
## 
## Node number 28: 98 observations
##   predicted class=N  expected loss=0.2040816  P(node) =0.03555878
##     class counts:    78    20
##    probabilities: 0.796 0.204 
## 
## Node number 29: 363 observations,    complexity param=0.002386635
##   predicted class=N  expected loss=0.322314  P(node) =0.1317126
##     class counts:   246   117
##    probabilities: 0.678 0.322 
##   left son=58 (309 obs) right son=59 (54 obs)
##   Primary splits:
##       age   < 64.5   to the left,  improve=0.5623312, (0 missing)
##       sysBP < 149.75 to the left,  improve=0.5553818, (0 missing)
## 
## Node number 50: 75 observations
##   predicted class=N  expected loss=0.12  P(node) =0.02721335
##     class counts:    66     9
##    probabilities: 0.880 0.120 
## 
## Node number 51: 536 observations,    complexity param=0.0006818957
##   predicted class=N  expected loss=0.1958955  P(node) =0.1944848
##     class counts:   431   105
##    probabilities: 0.804 0.196 
##   left son=102 (483 obs) right son=103 (53 obs)
##   Primary splits:
##       sysBP < 137.75 to the left,  improve=1.3214890, (0 missing)
##       age   < 57.5   to the left,  improve=0.8333108, (0 missing)
## 
## Node number 58: 309 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3106796  P(node) =0.112119
##     class counts:   213    96
##    probabilities: 0.689 0.311 
##   left son=116 (8 obs) right son=117 (301 obs)
##   Primary splits:
##       sysBP < 198.5  to the right, improve=1.5853950, (0 missing)
##       age   < 56.5   to the right, improve=0.3752841, (0 missing)
## 
## Node number 59: 54 observations,    complexity param=0.002386635
##   predicted class=N  expected loss=0.3888889  P(node) =0.01959361
##     class counts:    33    21
##    probabilities: 0.611 0.389 
##   left son=118 (40 obs) right son=119 (14 obs)
##   Primary splits:
##       sysBP < 152.25 to the right, improve=1.2595240, (0 missing)
##       age   < 66.5   to the right, improve=0.1666667, (0 missing)
## 
## Node number 102: 483 observations
##   predicted class=N  expected loss=0.184265  P(node) =0.175254
##     class counts:   394    89
##    probabilities: 0.816 0.184 
## 
## Node number 103: 53 observations,    complexity param=0.0006818957
##   predicted class=N  expected loss=0.3018868  P(node) =0.01923077
##     class counts:    37    16
##    probabilities: 0.698 0.302 
##   left son=206 (16 obs) right son=207 (37 obs)
##   Primary splits:
##       sysBP < 139.5  to the right, improve=0.5997578, (0 missing)
##       age   < 54.5   to the left,  improve=0.1234064, (0 missing)
## 
## Node number 116: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.002902758
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 117: 301 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3189369  P(node) =0.1092163
##     class counts:   205    96
##    probabilities: 0.681 0.319 
##   left son=234 (288 obs) right son=235 (13 obs)
##   Primary splits:
##       sysBP < 191.75 to the left,  improve=1.3095260, (0 missing)
##       age   < 56.5   to the right, improve=0.3481652, (0 missing)
## 
## Node number 118: 40 observations,    complexity param=0.002386635
##   predicted class=N  expected loss=0.325  P(node) =0.01451379
##     class counts:    27    13
##    probabilities: 0.675 0.325 
##   left son=236 (30 obs) right son=237 (10 obs)
##   Primary splits:
##       sysBP < 182.25 to the left,  improve=2.01666700, (0 missing)
##       age   < 65.5   to the left,  improve=0.06648352, (0 missing)
## 
## Node number 119: 14 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.005079826
##     class counts:     6     8
##    probabilities: 0.429 0.571 
## 
## Node number 206: 16 observations
##   predicted class=N  expected loss=0.1875  P(node) =0.005805515
##     class counts:    13     3
##    probabilities: 0.812 0.188 
## 
## Node number 207: 37 observations,    complexity param=0.0006818957
##   predicted class=N  expected loss=0.3513514  P(node) =0.01342525
##     class counts:    24    13
##    probabilities: 0.649 0.351 
##   left son=414 (12 obs) right son=415 (25 obs)
##   Primary splits:
##       age   < 58.5   to the right, improve=1.21153200, (0 missing)
##       sysBP < 138.25 to the right, improve=0.04435204, (0 missing)
## 
## Node number 234: 288 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3090278  P(node) =0.1044993
##     class counts:   199    89
##    probabilities: 0.691 0.309 
##   left son=468 (26 obs) right son=469 (262 obs)
##   Primary splits:
##       sysBP < 182.25 to the right, improve=1.3764970, (0 missing)
##       age   < 56.5   to the right, improve=0.4030947, (0 missing)
## 
## Node number 235: 13 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.004716981
##     class counts:     6     7
##    probabilities: 0.462 0.538 
## 
## Node number 236: 30 observations
##   predicted class=N  expected loss=0.2333333  P(node) =0.01088534
##     class counts:    23     7
##    probabilities: 0.767 0.233 
## 
## Node number 237: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003628447
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 414: 12 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.004354136
##     class counts:    10     2
##    probabilities: 0.833 0.167 
## 
## Node number 415: 25 observations,    complexity param=0.0006818957
##   predicted class=N  expected loss=0.44  P(node) =0.009071118
##     class counts:    14    11
##    probabilities: 0.560 0.440 
##   left son=830 (13 obs) right son=831 (12 obs)
##   Primary splits:
##       age   < 54.5   to the left,  improve=0.94820510, (0 missing)
##       sysBP < 138.75 to the left,  improve=0.05333333, (0 missing)
##   Surrogate splits:
##       sysBP < 138.75 to the right, agree=0.6, adj=0.167, (0 split)
## 
## Node number 468: 26 observations
##   predicted class=N  expected loss=0.1538462  P(node) =0.009433962
##     class counts:    22     4
##    probabilities: 0.846 0.154 
## 
## Node number 469: 262 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3244275  P(node) =0.09506531
##     class counts:   177    85
##    probabilities: 0.676 0.324 
##   left son=938 (248 obs) right son=939 (14 obs)
##   Primary splits:
##       sysBP < 179.5  to the left,  improve=1.8047020, (0 missing)
##       age   < 63.5   to the right, improve=0.3939949, (0 missing)
## 
## Node number 830: 13 observations
##   predicted class=N  expected loss=0.3076923  P(node) =0.004716981
##     class counts:     9     4
##    probabilities: 0.692 0.308 
## 
## Node number 831: 12 observations
##   predicted class=Y  expected loss=0.4166667  P(node) =0.004354136
##     class counts:     5     7
##    probabilities: 0.417 0.583 
## 
## Node number 938: 248 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3104839  P(node) =0.08998549
##     class counts:   171    77
##    probabilities: 0.690 0.310 
##   left son=1876 (15 obs) right son=1877 (233 obs)
##   Primary splits:
##       sysBP < 145.25 to the left,  improve=1.0020790, (0 missing)
##       age   < 63.5   to the right, improve=0.2762462, (0 missing)
## 
## Node number 939: 14 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.005079826
##     class counts:     6     8
##    probabilities: 0.429 0.571 
## 
## Node number 1876: 15 observations
##   predicted class=N  expected loss=0.1333333  P(node) =0.005442671
##     class counts:    13     2
##    probabilities: 0.867 0.133 
## 
## Node number 1877: 233 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3218884  P(node) =0.08454282
##     class counts:   158    75
##    probabilities: 0.678 0.322 
##   left son=3754 (219 obs) right son=3755 (14 obs)
##   Primary splits:
##       sysBP < 146.25 to the right, improve=1.8550290, (0 missing)
##       age   < 63.5   to the right, improve=0.5573179, (0 missing)
## 
## Node number 3754: 219 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3059361  P(node) =0.07946299
##     class counts:   152    67
##    probabilities: 0.694 0.306 
##   left son=7508 (30 obs) right son=7509 (189 obs)
##   Primary splits:
##       sysBP < 149.75 to the left,  improve=2.0712330, (0 missing)
##       age   < 56.5   to the right, improve=0.7170902, (0 missing)
## 
## Node number 3755: 14 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.005079826
##     class counts:     6     8
##    probabilities: 0.429 0.571 
## 
## Node number 7508: 30 observations
##   predicted class=N  expected loss=0.1333333  P(node) =0.01088534
##     class counts:    26     4
##    probabilities: 0.867 0.133 
## 
## Node number 7509: 189 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3333333  P(node) =0.06857765
##     class counts:   126    63
##    probabilities: 0.667 0.333 
##   left son=15018 (45 obs) right son=15019 (144 obs)
##   Primary splits:
##       age   < 62.5   to the right, improve=0.525, (0 missing)
##       sysBP < 157.5  to the right, improve=0.336, (0 missing)
## 
## Node number 15018: 45 observations
##   predicted class=N  expected loss=0.2666667  P(node) =0.01632801
##     class counts:    33    12
##    probabilities: 0.733 0.267 
## 
## Node number 15019: 144 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3541667  P(node) =0.05224964
##     class counts:    93    51
##    probabilities: 0.646 0.354 
##   left son=30038 (94 obs) right son=30039 (50 obs)
##   Primary splits:
##       sysBP < 157.5  to the right, improve=0.66393620, (0 missing)
##       age   < 55.5   to the right, improve=0.09758065, (0 missing)
## 
## Node number 30038: 94 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3191489  P(node) =0.0341074
##     class counts:    64    30
##    probabilities: 0.681 0.319 
##   left son=60076 (69 obs) right son=60077 (25 obs)
##   Primary splits:
##       age   < 56.5   to the right, improve=0.9948319, (0 missing)
##       sysBP < 174.5  to the left,  improve=0.2333423, (0 missing)
## 
## Node number 30039: 50 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.42  P(node) =0.01814224
##     class counts:    29    21
##    probabilities: 0.580 0.420 
##   left son=60078 (41 obs) right son=60079 (9 obs)
##   Primary splits:
##       sysBP < 155.25 to the left,  improve=2.8098640, (0 missing)
##       age   < 56.5   to the left,  improve=0.9126316, (0 missing)
## 
## Node number 60076: 69 observations
##   predicted class=N  expected loss=0.2753623  P(node) =0.02503628
##     class counts:    50    19
##    probabilities: 0.725 0.275 
## 
## Node number 60077: 25 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.44  P(node) =0.009071118
##     class counts:    14    11
##    probabilities: 0.560 0.440 
##   left son=120154 (18 obs) right son=120155 (7 obs)
##   Primary splits:
##       sysBP < 161.25 to the right, improve=1.4628570, (0 missing)
##       age   < 55.5   to the left,  improve=0.4368831, (0 missing)
## 
## Node number 60078: 41 observations,    complexity param=0.001789976
##   predicted class=N  expected loss=0.3414634  P(node) =0.01487663
##     class counts:    27    14
##    probabilities: 0.659 0.341 
##   left son=120156 (30 obs) right son=120157 (11 obs)
##   Primary splits:
##       age   < 60.5   to the left,  improve=1.2511460, (0 missing)
##       sysBP < 150.25 to the right, improve=0.4996305, (0 missing)
## 
## Node number 60079: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.003265602
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 120154: 18 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.006531205
##     class counts:    12     6
##    probabilities: 0.667 0.333 
## 
## Node number 120155: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.002539913
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 120156: 30 observations
##   predicted class=N  expected loss=0.2666667  P(node) =0.01088534
##     class counts:    22     8
##    probabilities: 0.733 0.267 
## 
## Node number 120157: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.003991292
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## n= 2756 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##      1) root 2756 419 N (0.84796807 0.15203193)  
##        2) age< 50.5 1511 133 N (0.91197882 0.08802118) *
##        3) age>=50.5 1245 286 N (0.77028112 0.22971888)  
##          6) sysBP< 144.25 767 138 N (0.82007823 0.17992177)  
##           12) age< 65.5 734 126 N (0.82833787 0.17166213)  
##             24) sysBP< 113.25 123  12 N (0.90243902 0.09756098) *
##             25) sysBP>=113.25 611 114 N (0.81342062 0.18657938)  
##               50) sysBP>=140.25 75   9 N (0.88000000 0.12000000) *
##               51) sysBP< 140.25 536 105 N (0.80410448 0.19589552)  
##                102) sysBP< 137.75 483  89 N (0.81573499 0.18426501) *
##                103) sysBP>=137.75 53  16 N (0.69811321 0.30188679)  
##                  206) sysBP>=139.5 16   3 N (0.81250000 0.18750000) *
##                  207) sysBP< 139.5 37  13 N (0.64864865 0.35135135)  
##                    414) age>=58.5 12   2 N (0.83333333 0.16666667) *
##                    415) age< 58.5 25  11 N (0.56000000 0.44000000)  
##                      830) age< 54.5 13   4 N (0.69230769 0.30769231) *
##                      831) age>=54.5 12   5 Y (0.41666667 0.58333333) *
##           13) age>=65.5 33  12 N (0.63636364 0.36363636) *
##          7) sysBP>=144.25 478 148 N (0.69037657 0.30962343)  
##           14) sysBP< 209 461 137 N (0.70281996 0.29718004)  
##             28) age< 54.5 98  20 N (0.79591837 0.20408163) *
##             29) age>=54.5 363 117 N (0.67768595 0.32231405)  
##               58) age< 64.5 309  96 N (0.68932039 0.31067961)  
##                116) sysBP>=198.5 8   0 N (1.00000000 0.00000000) *
##                117) sysBP< 198.5 301  96 N (0.68106312 0.31893688)  
##                  234) sysBP< 191.75 288  89 N (0.69097222 0.30902778)  
##                    468) sysBP>=182.25 26   4 N (0.84615385 0.15384615) *
##                    469) sysBP< 182.25 262  85 N (0.67557252 0.32442748)  
##                      938) sysBP< 179.5 248  77 N (0.68951613 0.31048387)  
##                       1876) sysBP< 145.25 15   2 N (0.86666667 0.13333333) *
##                       1877) sysBP>=145.25 233  75 N (0.67811159 0.32188841)  
##                         3754) sysBP>=146.25 219  67 N (0.69406393 0.30593607)  
##                           7508) sysBP< 149.75 30   4 N (0.86666667 0.13333333) *
##                           7509) sysBP>=149.75 189  63 N (0.66666667 0.33333333)  
##                            15018) age>=62.5 45  12 N (0.73333333 0.26666667) *
##                            15019) age< 62.5 144  51 N (0.64583333 0.35416667)  
##                              30038) sysBP>=157.5 94  30 N (0.68085106 0.31914894)  
##                                60076) age>=56.5 69  19 N (0.72463768 0.27536232) *
##                                60077) age< 56.5 25  11 N (0.56000000 0.44000000)  
##                                 120154) sysBP>=161.25 18   6 N (0.66666667 0.33333333) *
##                                 120155) sysBP< 161.25 7   2 Y (0.28571429 0.71428571) *
##                              30039) sysBP< 157.5 50  21 N (0.58000000 0.42000000)  
##                                60078) sysBP< 155.25 41  14 N (0.65853659 0.34146341)  
##                                 120156) age< 60.5 30   8 N (0.73333333 0.26666667) *
##                                 120157) age>=60.5 11   5 Y (0.45454545 0.54545455) *
##                                60079) sysBP>=155.25 9   2 Y (0.22222222 0.77777778) *
##                         3755) sysBP< 146.25 14   6 Y (0.42857143 0.57142857) *
##                      939) sysBP>=179.5 14   6 Y (0.42857143 0.57142857) *
##                  235) sysBP>=191.75 13   6 Y (0.46153846 0.53846154) *
##               59) age>=64.5 54  21 N (0.61111111 0.38888889)  
##                118) sysBP>=152.25 40  13 N (0.67500000 0.32500000)  
##                  236) sysBP< 182.25 30   7 N (0.76666667 0.23333333) *
##                  237) sysBP>=182.25 10   4 Y (0.40000000 0.60000000) *
##                119) sysBP< 152.25 14   6 Y (0.42857143 0.57142857) *
##           15) sysBP>=209 17   6 Y (0.35294118 0.64705882) *
## [1] "    calling mypredict_mdl for fit:"
```

![](Fram_template2_files/figure-html/fit.models_0-7.png) 

```
##    threshold    f.score
## 1        0.0 0.26393701
## 2        0.1 0.35746902
## 3        0.2 0.36757991
## 4        0.3 0.31456954
## 5        0.4 0.27037037
## 6        0.5 0.27037037
## 7        0.6 0.10176991
## 8        0.7 0.05517241
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](Fram_template2_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1               N                                                2041
## 2               Y                                                 258
##   TenYearCHD.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                 296
## 2                                                 161
##          Prediction
## Reference    N    Y
##         N 2041  296
##         Y  258  161
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7989840      0.2483476      0.7835265      0.8138039      0.8479681 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.1159553 
## [1] "    calling mypredict_mdl for OOB:"
```

![](Fram_template2_files/figure-html/fit.models_0-9.png) 

```
##    threshold     f.score
## 1        0.0 0.263311878
## 2        0.1 0.332065906
## 3        0.2 0.285714286
## 4        0.3 0.153846154
## 5        0.4 0.097378277
## 6        0.5 0.097378277
## 7        0.6 0.025210084
## 8        0.7 0.008547009
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](Fram_template2_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1               N                                                 826
## 2               Y                                                  94
##   TenYearCHD.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                 433
## 2                                                 131
##          Prediction
## Reference   N   Y
##         N 826 433
##         Y  94 131
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   6.448787e-01   1.472157e-01   6.199261e-01   6.692564e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   4.553881e-49 
##                    model_id model_method      feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart age, sysBP               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.487                 0.043   0.6874657
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.3675799         0.798984
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7835265             0.8138039     0.2483476   0.6261266
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.3320659        0.6448787
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.6199261             0.6692564     0.1472157
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: age, sysBP"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00398 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](Fram_template2_files/figure-html/fit.models_0-11.png) ![](Fram_template2_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 2756 
## 
##            CP nsplit rel error
## 1 0.003977725      0         1
## 
## Node number 1: 2756 observations
##   predicted class=N  expected loss=0.1520319  P(node) =1
##     class counts:  2337   419
##    probabilities: 0.848 0.152 
## 
## n= 2756 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 2756 419 N (0.8479681 0.1520319) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Max.cor.Y.rpart.N
## 1               N                                      2337
## 2               Y                                       419
##          Prediction
## Reference    N    Y
##         N 2337    0
##         Y  419    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.479681e-01   0.000000e+00   8.340130e-01   8.611796e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   5.130333e-01   1.096271e-92 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Max.cor.Y.rpart.N
## 1               N                                      1259
## 2               Y                                       225
##          Prediction
## Reference    N    Y
##         N 1259    0
##         Y  225    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.483827e-01   0.000000e+00   8.291112e-01   8.662638e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   5.177806e-01   2.000126e-50 
##          model_id model_method      feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart age, sysBP               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.294                 0.061         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8436122
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.834013             0.8611796    0.01359321         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8483827
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8291112             0.8662638             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.00449187      0.01395845
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.glm"
## [1] "    indep_vars: age, sysBP"
## Aggregating results
## Fitting final model on full training set
```

![](Fram_template2_files/figure-html/fit.models_0-13.png) ![](Fram_template2_files/figure-html/fit.models_0-14.png) ![](Fram_template2_files/figure-html/fit.models_0-15.png) ![](Fram_template2_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.3273  -0.6051  -0.4543  -0.3519   2.6420  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -6.734453   0.404734 -16.639  < 2e-16 ***
## age          0.059106   0.007118   8.304  < 2e-16 ***
## sysBP        0.014450   0.002383   6.063 1.34e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2349.3  on 2755  degrees of freedom
## Residual deviance: 2170.9  on 2753  degrees of freedom
## AIC: 2176.9
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](Fram_template2_files/figure-html/fit.models_0-17.png) 

```
##    threshold     f.score
## 1        0.0 0.263937008
## 2        0.1 0.331125828
## 3        0.2 0.361467890
## 4        0.3 0.252252252
## 5        0.4 0.107438017
## 6        0.5 0.041666667
## 7        0.6 0.014218009
## 8        0.7 0.004761905
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](Fram_template2_files/figure-html/fit.models_0-18.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Max.cor.Y.glm.N
## 1               N                                    1863
## 2               Y                                     222
##   TenYearCHD.fctr.predict.Max.cor.Y.glm.Y
## 1                                     474
## 2                                     197
##          Prediction
## Reference    N    Y
##         N 1863  474
##         Y  222  197
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.474601e-01   2.144231e-01   7.308005e-01   7.635925e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.832389e-21 
## [1] "    calling mypredict_mdl for OOB:"
```

![](Fram_template2_files/figure-html/fit.models_0-19.png) 

```
##    threshold    f.score
## 1        0.0 0.26331188
## 2        0.1 0.33715799
## 3        0.2 0.36209814
## 4        0.3 0.26865672
## 5        0.4 0.08870968
## 6        0.5 0.02608696
## 7        0.6 0.00000000
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](Fram_template2_files/figure-html/fit.models_0-20.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Max.cor.Y.glm.N
## 1               N                                    1000
## 2               Y                                     118
##   TenYearCHD.fctr.predict.Max.cor.Y.glm.Y
## 1                                     259
## 2                                     107
##          Prediction
## Reference    N    Y
##         N 1000  259
##         Y  118  107
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.459569e-01   2.146098e-01   7.229984e-01   7.679377e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   5.580134e-13 
##        model_id model_method      feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm age, sysBP               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.971                 0.051   0.6995291
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.3614679         0.849783
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7308005             0.7635925    0.03285851   0.7108252
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3620981        0.7459569
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7229984             0.7679377     0.2146098    2176.886
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002818894      0.01435593
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.glm"
## [1] "    indep_vars: age, sysBP, age:sysBP"
## Aggregating results
## Fitting final model on full training set
```

![](Fram_template2_files/figure-html/fit.models_0-21.png) ![](Fram_template2_files/figure-html/fit.models_0-22.png) ![](Fram_template2_files/figure-html/fit.models_0-23.png) ![](Fram_template2_files/figure-html/fit.models_0-24.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.2120  -0.6163  -0.4536  -0.3378   2.7634  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -9.2140504  2.1572857  -4.271 1.94e-05 ***
## age          0.1039283  0.0389277   2.670  0.00759 ** 
## sysBP        0.0328995  0.0158792   2.072  0.03828 *  
## `age:sysBP` -0.0003307  0.0002815  -1.175  0.24013    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2349.3  on 2755  degrees of freedom
## Residual deviance: 2169.5  on 2752  degrees of freedom
## AIC: 2177.5
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](Fram_template2_files/figure-html/fit.models_0-25.png) 

```
##    threshold     f.score
## 1        0.0 0.263937008
## 2        0.1 0.331758034
## 3        0.2 0.359019264
## 4        0.3 0.259701493
## 5        0.4 0.101694915
## 6        0.5 0.023474178
## 7        0.6 0.004761905
## 8        0.7 0.004761905
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](Fram_template2_files/figure-html/fit.models_0-26.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Interact.High.cor.Y.glm.N
## 1               N                                              1819
## 2               Y                                               214
##   TenYearCHD.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                               518
## 2                                               205
##          Prediction
## Reference    N    Y
##         N 1819  518
##         Y  214  205
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.343977e-01   2.062131e-01   7.174842e-01   7.508120e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   4.114215e-29 
## [1] "    calling mypredict_mdl for OOB:"
```

![](Fram_template2_files/figure-html/fit.models_0-27.png) 

```
##    threshold     f.score
## 1        0.0 0.263311878
## 2        0.1 0.338893766
## 3        0.2 0.366242038
## 4        0.3 0.261127596
## 5        0.4 0.066666667
## 6        0.5 0.008810573
## 7        0.6 0.000000000
## 8        0.7 0.000000000
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](Fram_template2_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Interact.High.cor.Y.glm.N
## 1               N                                               971
## 2               Y                                               110
##   TenYearCHD.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                               288
## 2                                               115
##          Prediction
## Reference   N   Y
##         N 971 288
##         Y 110 115
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.318059e-01   2.131223e-01   7.084848e-01   7.542060e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   7.170948e-19 
##                  model_id model_method                 feats
## 1 Interact.High.cor.Y.glm          glm age, sysBP, age:sysBP
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.021                 0.058
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1     0.69957                    0.2       0.3590193        0.8486941
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7174842              0.750812    0.01782193   0.7114853
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2        0.366242        0.7318059
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7084848              0.754206     0.2131223    2177.511
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.001007458     0.006917152
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.glm"
## [1] "    indep_vars: age, sysBP, prevalentHyp, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA"
## Aggregating results
## Fitting final model on full training set
```

![](Fram_template2_files/figure-html/fit.models_0-29.png) ![](Fram_template2_files/figure-html/fit.models_0-30.png) ![](Fram_template2_files/figure-html/fit.models_0-31.png) ![](Fram_template2_files/figure-html/fit.models_0-32.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8416  -0.6040  -0.4335  -0.2909   2.7945  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -8.043981   0.793193 -10.141  < 2e-16 ***
## age               0.062270   0.007617   8.175 2.96e-16 ***
## sysBP             0.009767   0.003341   2.924  0.00346 ** 
## prevalentHyp      0.289330   0.157275   1.840  0.06582 .  
## glucose.nonNA     0.007811   0.002659   2.938  0.00330 ** 
## totChol.nonNA     0.003211   0.001276   2.517  0.01182 *  
## BPMeds.nonNA      0.342024   0.273340   1.251  0.21083    
## male              0.502425   0.124127   4.048 5.17e-05 ***
## BMI.nonNA         0.007127   0.014500   0.491  0.62309    
## prevalentStroke   1.360637   0.519648   2.618  0.00883 ** 
## diabetes         -0.164752   0.369521  -0.446  0.65570    
## cigsPerDay.nonNA  0.016615   0.007163   2.320  0.02037 *  
## currentSmoker     0.073394   0.178392   0.411  0.68077    
## heartRate.nonNA  -0.004818   0.004858  -0.992  0.32136    
## .rnorm           -0.066397   0.057142  -1.162  0.24525    
## education.nonNA  -0.017798   0.057579  -0.309  0.75724    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2349.3  on 2755  degrees of freedom
## Residual deviance: 2092.4  on 2740  degrees of freedom
## AIC: 2124.4
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](Fram_template2_files/figure-html/fit.models_0-33.png) 

```
##    threshold     f.score
## 1        0.0 0.263937008
## 2        0.1 0.346718903
## 3        0.2 0.387267905
## 4        0.3 0.321479374
## 5        0.4 0.183206107
## 6        0.5 0.126361656
## 7        0.6 0.059770115
## 8        0.7 0.032786885
## 9        0.8 0.018867925
## 10       0.9 0.009501188
## 11       1.0 0.000000000
```

![](Fram_template2_files/figure-html/fit.models_0-34.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Low.cor.X.glm.N
## 1               N                                    1844
## 2               Y                                     200
##   TenYearCHD.fctr.predict.Low.cor.X.glm.Y
## 1                                     493
## 2                                     219
##          Prediction
## Reference    N    Y
##         N 1844  493
##         Y  200  219
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.485486e-01   2.422143e-01   7.319111e-01   7.646567e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.369364e-28 
## [1] "    calling mypredict_mdl for OOB:"
```

![](Fram_template2_files/figure-html/fit.models_0-35.png) 

```
##    threshold    f.score
## 1        0.0 0.26331188
## 2        0.1 0.35078053
## 3        0.2 0.39065109
## 4        0.3 0.29551451
## 5        0.4 0.20494700
## 6        0.5 0.08230453
## 7        0.6 0.04273504
## 8        0.7 0.01754386
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](Fram_template2_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Low.cor.X.glm.N
## 1               N                                    1002
## 2               Y                                     108
##   TenYearCHD.fctr.predict.Low.cor.X.glm.Y
## 1                                     257
## 2                                     117
##          Prediction
## Reference    N    Y
##         N 1002  257
##         Y  108  117
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.540431e-01   2.483375e-01   7.313056e-01   7.757707e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   9.433068e-15 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                         feats
## 1 age, sysBP, prevalentHyp, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.451                 0.145
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7295872                    0.2       0.3872679        0.8519609
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7319111             0.7646567    0.09050139   0.7357374
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3906511        0.7540431
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7313056             0.7757707     0.2483375    2124.378
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004918549      0.04963018
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn   end elapsed
## 10 fit.models          7          0 31.473 59.43  27.958
## 11 fit.models          7          1 59.431    NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 63.707  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 63.707 63.726   0.019
## 2 fit.models_1_glm          2          0 63.726     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA"
## Aggregating results
## Fitting final model on full training set
```

![](Fram_template2_files/figure-html/fit.models_1-1.png) ![](Fram_template2_files/figure-html/fit.models_1-2.png) ![](Fram_template2_files/figure-html/fit.models_1-3.png) ![](Fram_template2_files/figure-html/fit.models_1-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8843  -0.6025  -0.4325  -0.2922   2.7774  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -7.858459   0.824711  -9.529  < 2e-16 ***
## age               0.061021   0.007762   7.862 3.79e-15 ***
## sysBP             0.011901   0.004256   2.796  0.00517 ** 
## prevalentHyp      0.309309   0.159398   1.940  0.05232 .  
## diaBP            -0.005972   0.007381  -0.809  0.41848    
## glucose.nonNA     0.007745   0.002663   2.908  0.00364 ** 
## totChol.nonNA     0.003218   0.001276   2.523  0.01164 *  
## BPMeds.nonNA      0.338660   0.273898   1.236  0.21629    
## male              0.514269   0.125041   4.113 3.91e-05 ***
## BMI.nonNA         0.009514   0.014815   0.642  0.52078    
## prevalentStroke   1.374075   0.520203   2.641  0.00826 ** 
## diabetes         -0.180211   0.370054  -0.487  0.62627    
## cigsPerDay.nonNA  0.016587   0.007159   2.317  0.02050 *  
## currentSmoker     0.072429   0.178428   0.406  0.68480    
## heartRate.nonNA  -0.004633   0.004861  -0.953  0.34060    
## .rnorm           -0.067111   0.057197  -1.173  0.24066    
## education.nonNA  -0.014297   0.057762  -0.248  0.80451    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2349.3  on 2755  degrees of freedom
## Residual deviance: 2091.7  on 2739  degrees of freedom
## AIC: 2125.7
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](Fram_template2_files/figure-html/fit.models_1-5.png) 

```
##    threshold     f.score
## 1        0.0 0.263937008
## 2        0.1 0.346116028
## 3        0.2 0.396074933
## 4        0.3 0.321839080
## 5        0.4 0.192816635
## 6        0.5 0.126637555
## 7        0.6 0.064220183
## 8        0.7 0.037383178
## 9        0.8 0.018867925
## 10       0.9 0.009501188
## 11       1.0 0.000000000
```

![](Fram_template2_files/figure-html/fit.models_1-6.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.All.X.glm.N
## 1               N                                1857
## 2               Y                                 197
##   TenYearCHD.fctr.predict.All.X.glm.Y
## 1                                 480
## 2                                 222
##          Prediction
## Reference    N    Y
##         N 1857  480
##         Y  197  222
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.543541e-01   2.540333e-01   7.378364e-01   7.703300e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.270390e-27 
## [1] "    calling mypredict_mdl for OOB:"
```

![](Fram_template2_files/figure-html/fit.models_1-7.png) 

```
##    threshold     f.score
## 1        0.0 0.263311878
## 2        0.1 0.349442379
## 3        0.2 0.387858347
## 4        0.3 0.304461942
## 5        0.4 0.209790210
## 6        0.5 0.089795918
## 7        0.6 0.042918455
## 8        0.7 0.008810573
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.All.X.glm.N
## 1               N                                1006
## 2               Y                                 110
##   TenYearCHD.fctr.predict.All.X.glm.Y
## 1                                 253
## 2                                 115
##          Prediction
## Reference    N    Y
##         N 1006  253
##         Y  110  115
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.553908e-01   2.459645e-01   7.326911e-01   7.770752e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   9.119486e-14 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                feats
## 1 age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.426                 0.157
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7300008                    0.2       0.3960749        0.8534118
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7378364               0.77033     0.1019044   0.7349254
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3878583        0.7553908
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7326911             0.7770752     0.2459645    2125.724
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004981347      0.05516261
##                   label step_major step_minor    bgn    end elapsed
## 2      fit.models_1_glm          2          0 63.726 70.072   6.347
## 3 fit.models_1_bayesglm          3          0 70.073     NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA"
```

```
## Loading required package: arm
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: Matrix
## Loading required package: lme4
## 
## arm (Version 1.8-5, built: 2015-05-13)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Lectures/LCTR3_BioLINCC_Framingham
```

![](Fram_template2_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8783  -0.6025  -0.4334  -0.2929   2.7746  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -7.833598   0.821378  -9.537  < 2e-16 ***
## age               0.060829   0.007732   7.867 3.62e-15 ***
## sysBP             0.011784   0.004218   2.794  0.00521 ** 
## prevalentHyp      0.310505   0.158504   1.959  0.05012 .  
## diaBP            -0.005751   0.007324  -0.785  0.43231    
## glucose.nonNA     0.007683   0.002633   2.918  0.00352 ** 
## totChol.nonNA     0.003199   0.001272   2.515  0.01191 *  
## BPMeds.nonNA      0.336633   0.270498   1.244  0.21332    
## male              0.511188   0.124579   4.103 4.07e-05 ***
## BMI.nonNA         0.009398   0.014765   0.637  0.52443    
## prevalentStroke   1.289595   0.503512   2.561  0.01043 *  
## diabetes         -0.168695   0.361345  -0.467  0.64061    
## cigsPerDay.nonNA  0.016450   0.007106   2.315  0.02062 *  
## currentSmoker     0.073509   0.177003   0.415  0.67792    
## heartRate.nonNA  -0.004633   0.004847  -0.956  0.33913    
## .rnorm           -0.066528   0.057044  -1.166  0.24351    
## education.nonNA  -0.015192   0.057693  -0.263  0.79230    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2349.3  on 2755  degrees of freedom
## Residual deviance: 2091.8  on 2739  degrees of freedom
## AIC: 2125.8
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](Fram_template2_files/figure-html/fit.models_1-9.png) 

```
##    threshold     f.score
## 1        0.0 0.263937008
## 2        0.1 0.345776031
## 3        0.2 0.394642857
## 4        0.3 0.320916905
## 5        0.4 0.189753321
## 6        0.5 0.126637555
## 7        0.6 0.064220183
## 8        0.7 0.037383178
## 9        0.8 0.018867925
## 10       0.9 0.009501188
## 11       1.0 0.000000000
```

![](Fram_template2_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.All.X.bayesglm.N
## 1               N                                     1857
## 2               Y                                      198
##   TenYearCHD.fctr.predict.All.X.bayesglm.Y
## 1                                      480
## 2                                      221
##          Prediction
## Reference    N    Y
##         N 1857  480
##         Y  198  221
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.539913e-01   2.523581e-01   7.374659e-01   7.699755e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   3.765910e-27 
## [1] "    calling mypredict_mdl for OOB:"
```

![](Fram_template2_files/figure-html/fit.models_1-11.png) 

```
##    threshold     f.score
## 1        0.0 0.263311878
## 2        0.1 0.349676226
## 3        0.2 0.386554622
## 4        0.3 0.300000000
## 5        0.4 0.209790210
## 6        0.5 0.089795918
## 7        0.6 0.042918455
## 8        0.7 0.008810573
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](Fram_template2_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.All.X.bayesglm.N
## 1               N                                     1004
## 2               Y                                      110
##   TenYearCHD.fctr.predict.All.X.bayesglm.Y
## 1                                      255
## 2                                      115
##          Prediction
## Reference    N    Y
##         N 1004  255
##         Y  110  115
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.540431e-01   2.439984e-01   7.313056e-01   7.757707e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   4.797852e-14 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                feats
## 1 age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.464                 0.204
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7299763                    0.2       0.3946429        0.8534118
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7374659             0.7699755     0.1019044   0.7350949
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3865546        0.7540431
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7313056             0.7757707     0.2439984    2125.755
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004981347      0.05516261
##                   label step_major step_minor    bgn   end elapsed
## 3 fit.models_1_bayesglm          3          0 70.073 76.68   6.607
## 4    fit.models_1_rpart          4          0 76.681    NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, education.nonNA"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00835 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](Fram_template2_files/figure-html/fit.models_1-13.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 2756 
## 
##            CP nsplit rel error
## 1 0.008353222      0         1
## 
## Node number 1: 2756 observations
##   predicted class=N  expected loss=0.1520319  P(node) =1
##     class counts:  2337   419
##    probabilities: 0.848 0.152 
## 
## n= 2756 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 2756 419 N (0.8479681 0.1520319) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.All.X.no.rnorm.rpart.N
## 1               N                                           2337
## 2               Y                                            419
##          Prediction
## Reference    N    Y
##         N 2337    0
##         Y  419    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.479681e-01   0.000000e+00   8.340130e-01   8.611796e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   5.130333e-01   1.096271e-92 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.All.X.no.rnorm.rpart.N
## 1               N                                           1259
## 2               Y                                            225
##          Prediction
## Reference    N    Y
##         N 1259    0
##         Y  225    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.483827e-01   0.000000e+00   8.291112e-01   8.662638e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   5.177806e-01   2.000126e-50 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                        feats
## 1 age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, education.nonNA
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      2.658                 0.149
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1         0.5                    0.5               0        0.8392632
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.834013             0.8611796    0.04571195         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8483827
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8291112             0.8662638             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.008360189      0.05858524
##                label step_major step_minor    bgn    end elapsed
## 4 fit.models_1_rpart          4          0 76.681 80.967   4.286
## 5    fit.models_1_rf          5          0 80.967     NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, education.nonNA"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](Fram_template2_files/figure-html/fit.models_1-14.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 2 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: mtry
```

![](Fram_template2_files/figure-html/fit.models_1-15.png) ![](Fram_template2_files/figure-html/fit.models_1-16.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted       2756   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           5512   matrix     numeric  
## oob.times       2756   -none-     numeric  
## classes            2   -none-     character
## importance        15   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               2756   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            15   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](Fram_template2_files/figure-html/fit.models_1-17.png) 

```
##    threshold     f.score
## 1        0.0 0.263937008
## 2        0.1 0.755635708
## 3        0.2 0.984723854
## 4        0.3 0.991576414
## 5        0.4 0.901703801
## 6        0.5 0.692667707
## 7        0.6 0.388461538
## 8        0.7 0.141906874
## 9        0.8 0.009501188
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](Fram_template2_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.All.X.no.rnorm.rf.N
## 1               N                                        2337
## 2               Y                                           7
##   TenYearCHD.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                          NA
## 2                                         412
##          Prediction
## Reference    N    Y
##         N 2337    0
##         Y    7  412
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.974601e-01   9.900811e-01   9.947739e-01   9.989782e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##  5.904678e-183   2.334220e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](Fram_template2_files/figure-html/fit.models_1-19.png) 

```
##    threshold    f.score
## 1        0.0 0.26331188
## 2        0.1 0.37486457
## 3        0.2 0.36862745
## 4        0.3 0.32352941
## 5        0.4 0.11718750
## 6        0.5 0.02586207
## 7        0.6 0.00000000
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](Fram_template2_files/figure-html/fit.models_1-20.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.All.X.no.rnorm.rf.N
## 1               N                                         734
## 2               Y                                          52
##   TenYearCHD.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                         525
## 2                                         173
##          Prediction
## Reference   N   Y
##         N 734 525
##         Y  52 173
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   6.111860e-01   1.888575e-01   5.858467e-01   6.360842e-01   8.483827e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   5.825060e-86 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                        feats
## 1 age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, education.nonNA
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     15.833                  3.72
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9999949                    0.3       0.9915764        0.8497826
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9947739             0.9989782     0.0325253    0.711503
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.3748646         0.611186
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5858467             0.6360842     0.1888575
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.003901884      0.03622717
```

```r
# User specified
#   Ensure at least 2 vars in each regression; else varImp crashes
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df

    # easier to exclude features
#model_id <- "";
# indep_vars_vctr <- head(subset(glb_models_df, grepl("All\\.X\\.", model_id), select=feats), 1)
# indep_vars_vctr <- setdiff(indep_vars_vctr, ".rnorm")

    # easier to include features
#model_id <- "Rank9.2"; indep_vars_vctr <- c(NULL
#    ,"<feat1>"
#    ,"<feat1>*<feat2>"
#    ,"<feat1>:<feat2>"
#                                            )
# for (method in c("bayesglm")) {
#     ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                     n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
#     csm_mdl_id <- paste0(model_id, ".", method)
#     csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
# }

# Ntv.1.lm <- lm(reformulate(indep_vars_vctr, glb_rsp_var), glb_trnobs_df); print(summary(Ntv.1.lm))

#print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                        feats
## MFO.myMFO_classfr                                                                                                                                                                                                     .rnorm
## Random.myrandom_classfr                                                                                                                                                                                               .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                              age, sysBP
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                         age, sysBP
## Max.cor.Y.rpart                                                                                                                                                                                                   age, sysBP
## Max.cor.Y.glm                                                                                                                                                                                                     age, sysBP
## Interact.High.cor.Y.glm                                                                                                                                                                                age, sysBP, age:sysBP
## Low.cor.X.glm                    age, sysBP, prevalentHyp, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA
## All.X.glm                 age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA
## All.X.bayesglm            age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA
## All.X.no.rnorm.rpart              age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, education.nonNA
## All.X.no.rnorm.rf                 age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, education.nonNA
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.267
## Random.myrandom_classfr                 0                      0.231
## Max.cor.Y.cv.0.rpart                    0                      0.586
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.487
## Max.cor.Y.rpart                         3                      1.294
## Max.cor.Y.glm                           1                      0.971
## Interact.High.cor.Y.glm                 1                      1.021
## Low.cor.X.glm                           1                      1.451
## All.X.glm                               1                      1.426
## All.X.bayesglm                          1                      2.464
## All.X.no.rnorm.rpart                    3                      2.658
## All.X.no.rnorm.rf                       3                     15.833
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.003   0.5000000
## Random.myrandom_classfr                   0.002   0.4991605
## Max.cor.Y.cv.0.rpart                      0.046   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.043   0.6874657
## Max.cor.Y.rpart                           0.061   0.5000000
## Max.cor.Y.glm                             0.051   0.6995291
## Interact.High.cor.Y.glm                   0.058   0.6995700
## Low.cor.X.glm                             0.145   0.7295872
## All.X.glm                                 0.157   0.7300008
## All.X.bayesglm                            0.204   0.7299763
## All.X.no.rnorm.rpart                      0.149   0.5000000
## All.X.no.rnorm.rf                         3.720   0.9999949
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2639370
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.2       0.3675799
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.2       0.3614679
## Interact.High.cor.Y.glm                      0.2       0.3590193
## Low.cor.X.glm                                0.2       0.3872679
## All.X.glm                                    0.2       0.3960749
## All.X.bayesglm                               0.2       0.3946429
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.3       0.9915764
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                0.8479681             0.8340130
## Random.myrandom_classfr          0.1520319             0.1388204
## Max.cor.Y.cv.0.rpart             0.8479681             0.8340130
## Max.cor.Y.cv.0.cp.0.rpart        0.7989840             0.7835265
## Max.cor.Y.rpart                  0.8436122             0.8340130
## Max.cor.Y.glm                    0.8497830             0.7308005
## Interact.High.cor.Y.glm          0.8486941             0.7174842
## Low.cor.X.glm                    0.8519609             0.7319111
## All.X.glm                        0.8534118             0.7378364
## All.X.bayesglm                   0.8534118             0.7374659
## All.X.no.rnorm.rpart             0.8392632             0.8340130
## All.X.no.rnorm.rf                0.8497826             0.9947739
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.8611796    0.00000000   0.5000000
## Random.myrandom_classfr               0.1659870    0.00000000   0.5169305
## Max.cor.Y.cv.0.rpart                  0.8611796    0.00000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8138039    0.24834760   0.6261266
## Max.cor.Y.rpart                       0.8611796    0.01359321   0.5000000
## Max.cor.Y.glm                         0.7635925    0.03285851   0.7108252
## Interact.High.cor.Y.glm               0.7508120    0.01782193   0.7114853
## Low.cor.X.glm                         0.7646567    0.09050139   0.7357374
## All.X.glm                             0.7703300    0.10190442   0.7349254
## All.X.bayesglm                        0.7699755    0.10190442   0.7350949
## All.X.no.rnorm.rpart                  0.8611796    0.04571195   0.5000000
## All.X.no.rnorm.rf                     0.9989782    0.03252530   0.7115030
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2633119
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.1       0.3320659
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.2       0.3620981
## Interact.High.cor.Y.glm                      0.2       0.3662420
## Low.cor.X.glm                                0.2       0.3906511
## All.X.glm                                    0.2       0.3878583
## All.X.bayesglm                               0.2       0.3865546
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.1       0.3748646
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                0.8483827             0.8291112
## Random.myrandom_classfr          0.1516173             0.1337362
## Max.cor.Y.cv.0.rpart             0.8483827             0.8291112
## Max.cor.Y.cv.0.cp.0.rpart        0.6448787             0.6199261
## Max.cor.Y.rpart                  0.8483827             0.8291112
## Max.cor.Y.glm                    0.7459569             0.7229984
## Interact.High.cor.Y.glm          0.7318059             0.7084848
## Low.cor.X.glm                    0.7540431             0.7313056
## All.X.glm                        0.7553908             0.7326911
## All.X.bayesglm                   0.7540431             0.7313056
## All.X.no.rnorm.rpart             0.8483827             0.8291112
## All.X.no.rnorm.rf                0.6111860             0.5858467
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.8662638     0.0000000
## Random.myrandom_classfr               0.1708888     0.0000000
## Max.cor.Y.cv.0.rpart                  0.8662638     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart             0.6692564     0.1472157
## Max.cor.Y.rpart                       0.8662638     0.0000000
## Max.cor.Y.glm                         0.7679377     0.2146098
## Interact.High.cor.Y.glm               0.7542060     0.2131223
## Low.cor.X.glm                         0.7757707     0.2483375
## All.X.glm                             0.7770752     0.2459645
## All.X.bayesglm                        0.7757707     0.2439984
## All.X.no.rnorm.rpart                  0.8662638     0.0000000
## All.X.no.rnorm.rf                     0.6360842     0.1888575
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                  0.004491870     0.013958451          NA
## Max.cor.Y.glm                    0.002818894     0.014355934    2176.886
## Interact.High.cor.Y.glm          0.001007458     0.006917152    2177.511
## Low.cor.X.glm                    0.004918549     0.049630177    2124.378
## All.X.glm                        0.004981347     0.055162607    2125.724
## All.X.bayesglm                   0.004981347     0.055162607    2125.755
## All.X.no.rnorm.rpart             0.008360189     0.058585237          NA
## All.X.no.rnorm.rf                0.003901884     0.036227169          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 5  fit.models_1_rf          5          0  80.967 100.559  19.592
## 6 fit.models_1_end          6          0 100.560      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1  59.431 100.566  41.135
## 12 fit.models          7          2 100.567      NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id",
                                    grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df),
                                    grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                        feats
## MFO.myMFO_classfr                                                                                                                                                                                                     .rnorm
## Random.myrandom_classfr                                                                                                                                                                                               .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                              age, sysBP
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                         age, sysBP
## Max.cor.Y.rpart                                                                                                                                                                                                   age, sysBP
## Max.cor.Y.glm                                                                                                                                                                                                     age, sysBP
## Interact.High.cor.Y.glm                                                                                                                                                                                age, sysBP, age:sysBP
## Low.cor.X.glm                    age, sysBP, prevalentHyp, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA
## All.X.glm                 age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA
## All.X.bayesglm            age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, .rnorm, education.nonNA
## All.X.no.rnorm.rpart              age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, education.nonNA
## All.X.no.rnorm.rf                 age, sysBP, prevalentHyp, diaBP, glucose.nonNA, totChol.nonNA, BPMeds.nonNA, male, BMI.nonNA, prevalentStroke, diabetes, cigsPerDay.nonNA, currentSmoker, heartRate.nonNA, education.nonNA
##                           max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                       0   0.5000000
## Random.myrandom_classfr                 0   0.4991605
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.6874657
## Max.cor.Y.rpart                         3   0.5000000
## Max.cor.Y.glm                           1   0.6995291
## Interact.High.cor.Y.glm                 1   0.6995700
## Low.cor.X.glm                           1   0.7295872
## All.X.glm                               1   0.7300008
## All.X.bayesglm                          1   0.7299763
## All.X.no.rnorm.rpart                    3   0.5000000
## All.X.no.rnorm.rf                       3   0.9999949
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2639370
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.2       0.3675799
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.2       0.3614679
## Interact.High.cor.Y.glm                      0.2       0.3590193
## Low.cor.X.glm                                0.2       0.3872679
## All.X.glm                                    0.2       0.3960749
## All.X.bayesglm                               0.2       0.3946429
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.3       0.9915764
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                0.8479681    0.00000000   0.5000000
## Random.myrandom_classfr          0.1520319    0.00000000   0.5169305
## Max.cor.Y.cv.0.rpart             0.8479681    0.00000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart        0.7989840    0.24834760   0.6261266
## Max.cor.Y.rpart                  0.8436122    0.01359321   0.5000000
## Max.cor.Y.glm                    0.8497830    0.03285851   0.7108252
## Interact.High.cor.Y.glm          0.8486941    0.01782193   0.7114853
## Low.cor.X.glm                    0.8519609    0.09050139   0.7357374
## All.X.glm                        0.8534118    0.10190442   0.7349254
## All.X.bayesglm                   0.8534118    0.10190442   0.7350949
## All.X.no.rnorm.rpart             0.8392632    0.04571195   0.5000000
## All.X.no.rnorm.rf                0.8497826    0.03252530   0.7115030
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2633119
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.1       0.3320659
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.2       0.3620981
## Interact.High.cor.Y.glm                      0.2       0.3662420
## Low.cor.X.glm                                0.2       0.3906511
## All.X.glm                                    0.2       0.3878583
## All.X.bayesglm                               0.2       0.3865546
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.1       0.3748646
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                0.8483827     0.0000000
## Random.myrandom_classfr          0.1516173     0.0000000
## Max.cor.Y.cv.0.rpart             0.8483827     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart        0.6448787     0.1472157
## Max.cor.Y.rpart                  0.8483827     0.0000000
## Max.cor.Y.glm                    0.7459569     0.2146098
## Interact.High.cor.Y.glm          0.7318059     0.2131223
## Low.cor.X.glm                    0.7540431     0.2483375
## All.X.glm                        0.7553908     0.2459645
## All.X.bayesglm                   0.7540431     0.2439984
## All.X.no.rnorm.rpart             0.8483827     0.0000000
## All.X.no.rnorm.rf                0.6111860     0.1888575
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                         3.74531835           333.3333333
## Random.myrandom_classfr                   4.32900433           500.0000000
## Max.cor.Y.cv.0.rpart                      1.70648464            21.7391304
## Max.cor.Y.cv.0.cp.0.rpart                 2.05338809            23.2558140
## Max.cor.Y.rpart                           0.77279753            16.3934426
## Max.cor.Y.glm                             1.02986612            19.6078431
## Interact.High.cor.Y.glm                   0.97943193            17.2413793
## Low.cor.X.glm                             0.68917988             6.8965517
## All.X.glm                                 0.70126227             6.3694268
## All.X.bayesglm                            0.40584416             4.9019608
## All.X.no.rnorm.rpart                      0.37622272             6.7114094
## All.X.no.rnorm.rf                         0.06315922             0.2688172
##                            inv.aic.fit
## MFO.myMFO_classfr                   NA
## Random.myrandom_classfr             NA
## Max.cor.Y.cv.0.rpart                NA
## Max.cor.Y.cv.0.cp.0.rpart           NA
## Max.cor.Y.rpart                     NA
## Max.cor.Y.glm             0.0004593717
## Interact.High.cor.Y.glm   0.0004592400
## Low.cor.X.glm             0.0004707261
## All.X.glm                 0.0004704280
## All.X.bayesglm            0.0004704212
## All.X.no.rnorm.rpart                NA
## All.X.no.rnorm.rf                   NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 4 rows containing missing values (geom_path).
```

```
## Warning: Removed 87 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

![](Fram_template2_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## Warning: max.AccuracyUpper.fit already exists in glb_models_df
```

```
## [1] "var:max.KappaSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

![](Fram_template2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 1          MFO.myMFO_classfr        0.8483827   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.8483827   0.5000000     0.0000000
## 5            Max.cor.Y.rpart        0.8483827   0.5000000     0.0000000
## 11      All.X.no.rnorm.rpart        0.8483827   0.5000000     0.0000000
## 9                  All.X.glm        0.7553908   0.7349254     0.2459645
## 8              Low.cor.X.glm        0.7540431   0.7357374     0.2483375
## 10            All.X.bayesglm        0.7540431   0.7350949     0.2439984
## 6              Max.cor.Y.glm        0.7459569   0.7108252     0.2146098
## 7    Interact.High.cor.Y.glm        0.7318059   0.7114853     0.2131223
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.6448787   0.6261266     0.1472157
## 12         All.X.no.rnorm.rf        0.6111860   0.7115030     0.1888575
## 2    Random.myrandom_classfr        0.1516173   0.5169305     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 11          NA                    0.5
## 9     2125.724                    0.2
## 8     2124.378                    0.2
## 10    2125.755                    0.2
## 6     2176.886                    0.2
## 7     2177.511                    0.2
## 4           NA                    0.1
## 12          NA                    0.1
## 2           NA                    0.1
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 38 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

![](Fram_template2_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.auc.OOB - max.Kappa.OOB + min.aic.fit - 
##     opt.prob.threshold.OOB
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: MFO.myMFO_classfr"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
#     if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
#         warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
#         glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
#     }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

```
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8479681 0.1520319
## 2 0.8479681 0.1520319
## 3 0.8479681 0.1520319
## 4 0.8479681 0.1520319
## 5 0.8479681 0.1520319
## 6 0.8479681 0.1520319
```

```r
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); #sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
```

```
## [1] "in MFO.Classifier$varImp"
##        Overall
## .rnorm       0
```

```r
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##        importance MFO.myMFO_classfr.importance
## .rnorm        NaN                          NaN
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
    
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))

#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

![](Fram_template2_files/figure-html/fit.models_2-4.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames TenYearCHD.fctr
## 3636      3636               Y
## 1891      1891               N
## 2            2               N
## 4236      4236               N
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr.prob
## 3636                                      0.1520319
## 1891                                      0.1520319
## 2                                         0.1520319
## 4236                                      0.1520319
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr
## 3636                                         N
## 1891                                         N
## 2                                            N
## 4236                                         N
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr.accurate
## 3636                                              FALSE
## 1891                                               TRUE
## 2                                                  TRUE
## 4236                                               TRUE
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr.error .label
## 3636                                      -0.3479681   3636
## 1891                                       0.0000000   1891
## 2                                          0.0000000      2
## 4236                                       0.0000000   4236
## [1] "Inaccurate: "
##      .rownames TenYearCHD.fctr
## 1029      1029               Y
## 1033      1033               Y
## 1045      1045               Y
## 1046      1046               Y
## 1052      1052               Y
## 1056      1056               Y
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr.prob
## 1029                                      0.1520319
## 1033                                      0.1520319
## 1045                                      0.1520319
## 1046                                      0.1520319
## 1052                                      0.1520319
## 1056                                      0.1520319
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr
## 1029                                         N
## 1033                                         N
## 1045                                         N
## 1046                                         N
## 1052                                         N
## 1056                                         N
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr.accurate
## 1029                                              FALSE
## 1033                                              FALSE
## 1045                                              FALSE
## 1046                                              FALSE
## 1052                                              FALSE
## 1056                                              FALSE
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr.error
## 1029                                      -0.3479681
## 1033                                      -0.3479681
## 1045                                      -0.3479681
## 1046                                      -0.3479681
## 1052                                      -0.3479681
## 1056                                      -0.3479681
##      .rownames TenYearCHD.fctr
## 149        149               Y
## 1522      1522               Y
## 2326      2326               Y
## 2391      2391               Y
## 248        248               Y
## 3339      3339               Y
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr.prob
## 149                                       0.1520319
## 1522                                      0.1520319
## 2326                                      0.1520319
## 2391                                      0.1520319
## 248                                       0.1520319
## 3339                                      0.1520319
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr
## 149                                          N
## 1522                                         N
## 2326                                         N
## 2391                                         N
## 248                                          N
## 3339                                         N
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr.accurate
## 149                                               FALSE
## 1522                                              FALSE
## 2326                                              FALSE
## 2391                                              FALSE
## 248                                               FALSE
## 3339                                              FALSE
##      TenYearCHD.fctr.predict.MFO.myMFO_classfr.error
## 149                                       -0.3479681
## 1522                                      -0.3479681
## 2326                                      -0.3479681
## 2391                                      -0.3479681
## 248                                       -0.3479681
## 3339                                      -0.3479681
##     .rownames TenYearCHD.fctr
## 895       895               Y
## 900       900               Y
## 934       934               Y
## 936       936               Y
## 966       966               Y
## 971       971               Y
##     TenYearCHD.fctr.predict.MFO.myMFO_classfr.prob
## 895                                      0.1520319
## 900                                      0.1520319
## 934                                      0.1520319
## 936                                      0.1520319
## 966                                      0.1520319
## 971                                      0.1520319
##     TenYearCHD.fctr.predict.MFO.myMFO_classfr
## 895                                         N
## 900                                         N
## 934                                         N
## 936                                         N
## 966                                         N
## 971                                         N
##     TenYearCHD.fctr.predict.MFO.myMFO_classfr.accurate
## 895                                              FALSE
## 900                                              FALSE
## 934                                              FALSE
## 936                                              FALSE
## 966                                              FALSE
## 971                                              FALSE
##     TenYearCHD.fctr.predict.MFO.myMFO_classfr.error
## 895                                      -0.3479681
## 900                                      -0.3479681
## 934                                      -0.3479681
## 936                                      -0.3479681
## 966                                      -0.3479681
## 971                                      -0.3479681
```

![](Fram_template2_files/figure-html/fit.models_2-5.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
# FN_OOB_ids <- c(4721, 4020, 693, 92)
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_feats_df$id[1:5]])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

# write.csv(glb_chunks_df, paste0(glb_out_pfx, tail(glb_chunks_df, 1)$label, "_",
#                                 tail(glb_chunks_df, 1)$step_minor,  "_chunks1.csv"),
#           row.names=FALSE)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 12 fit.models          7          2 100.567 117.668  17.101
## 13 fit.models          7          3 117.669      NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "TenYearCHD.fctr.predict.MFO.myMFO_classfr.prob"    
## [2] "TenYearCHD.fctr.predict.MFO.myMFO_classfr"         
## [3] "TenYearCHD.fctr.predict.MFO.myMFO_classfr.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](Fram_template2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 117.669 126.161   8.492
## 14 fit.data.training          8          0 126.162      NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## [1] "fitting model: Final.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Aggregating results
## Fitting final model on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##         N         Y 
## 0.8479681 0.1520319 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8479681 0.1520319
## 2 0.8479681 0.1520319
## 3 0.8479681 0.1520319
## 4 0.8479681 0.1520319
## 5 0.8479681 0.1520319
## 6 0.8479681 0.1520319
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   TenYearCHD.fctr TenYearCHD.fctr.predict.Final.myMFO_classfr.N
## 1               N                                          2337
## 2               Y                                           419
##          Prediction
## Reference    N    Y
##         N 2337    0
##         Y  419    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.479681e-01   0.000000e+00   8.340130e-01   8.611796e-01   8.479681e-01 
## AccuracyPValue  McnemarPValue 
##   5.130333e-01   1.096271e-92
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

```
##              model_id  model_method  feats max.nTuningRuns
## 1 Final.myMFO_classfr myMFO_classfr .rnorm               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.756                 0.004         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8479683
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1              0.834013             0.8611796             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0005331122               0
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 126.162 129.436   3.274
## 15 fit.data.training          8          1 129.437      NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id, : Using default probability threshold: 0.5
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8479681 0.1520319
## 2 0.8479681 0.1520319
## 3 0.8479681 0.1520319
## 4 0.8479681 0.1520319
## 5 0.8479681 0.1520319
## 6 0.8479681 0.1520319
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
```

```
## [1] "in MFO.Classifier$varImp"
##        Overall
## .rnorm       0
```

```r
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##        MFO.myMFO_classfr.importance importance
## .rnorm                          NaN        NaN
##        Final.myMFO_classfr.importance
## .rnorm                            NaN
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](Fram_template2_files/figure-html/fit.data.training_1-1.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames TenYearCHD.fctr
## 1            1               N
## 2485      2485               N
## 4240      4240               N
## 924        924               N
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.prob
## 1                                           0.1520319
## 2485                                        0.1520319
## 4240                                        0.1520319
## 924                                         0.1520319
##      TenYearCHD.fctr.predict.Final.myMFO_classfr
## 1                                              N
## 2485                                           N
## 4240                                           N
## 924                                            N
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.accurate
## 1                                                    TRUE
## 2485                                                 TRUE
## 4240                                                 TRUE
## 924                                                  TRUE
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.error .label
## 1                                                    0      1
## 2485                                                 0   2485
## 4240                                                 0   4240
## 924                                                  0    924
## [1] "Inaccurate: "
##      .rownames TenYearCHD.fctr
## 1028      1028               Y
## 1072      1072               Y
## 1077      1077               Y
## 108        108               Y
## 109        109               Y
## 1090      1090               Y
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.prob
## 1028                                        0.1520319
## 1072                                        0.1520319
## 1077                                        0.1520319
## 108                                         0.1520319
## 109                                         0.1520319
## 1090                                        0.1520319
##      TenYearCHD.fctr.predict.Final.myMFO_classfr
## 1028                                           N
## 1072                                           N
## 1077                                           N
## 108                                            N
## 109                                            N
## 1090                                           N
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.accurate
## 1028                                                FALSE
## 1072                                                FALSE
## 1077                                                FALSE
## 108                                                 FALSE
## 109                                                 FALSE
## 1090                                                FALSE
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.error
## 1028                                        -0.3479681
## 1072                                        -0.3479681
## 1077                                        -0.3479681
## 108                                         -0.3479681
## 109                                         -0.3479681
## 1090                                        -0.3479681
##      .rownames TenYearCHD.fctr
## 2646      2646               Y
## 3078      3078               Y
## 3144      3144               Y
## 377        377               Y
## 4120      4120               Y
## 95          95               Y
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.prob
## 2646                                        0.1520319
## 3078                                        0.1520319
## 3144                                        0.1520319
## 377                                         0.1520319
## 4120                                        0.1520319
## 95                                          0.1520319
##      TenYearCHD.fctr.predict.Final.myMFO_classfr
## 2646                                           N
## 3078                                           N
## 3144                                           N
## 377                                            N
## 4120                                           N
## 95                                             N
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.accurate
## 2646                                                FALSE
## 3078                                                FALSE
## 3144                                                FALSE
## 377                                                 FALSE
## 4120                                                FALSE
## 95                                                  FALSE
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.error
## 2646                                        -0.3479681
## 3078                                        -0.3479681
## 3144                                        -0.3479681
## 377                                         -0.3479681
## 4120                                        -0.3479681
## 95                                          -0.3479681
##     .rownames TenYearCHD.fctr
## 963       963               Y
## 975       975               Y
## 977       977               Y
## 979       979               Y
## 98         98               Y
## 986       986               Y
##     TenYearCHD.fctr.predict.Final.myMFO_classfr.prob
## 963                                        0.1520319
## 975                                        0.1520319
## 977                                        0.1520319
## 979                                        0.1520319
## 98                                         0.1520319
## 986                                        0.1520319
##     TenYearCHD.fctr.predict.Final.myMFO_classfr
## 963                                           N
## 975                                           N
## 977                                           N
## 979                                           N
## 98                                            N
## 986                                           N
##     TenYearCHD.fctr.predict.Final.myMFO_classfr.accurate
## 963                                                FALSE
## 975                                                FALSE
## 977                                                FALSE
## 979                                                FALSE
## 98                                                 FALSE
## 986                                                FALSE
##     TenYearCHD.fctr.predict.Final.myMFO_classfr.error
## 963                                        -0.3479681
## 975                                        -0.3479681
## 977                                        -0.3479681
## 979                                        -0.3479681
## 98                                         -0.3479681
## 986                                        -0.3479681
```

![](Fram_template2_files/figure-html/fit.data.training_1-2.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

# print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])

print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "TenYearCHD.fctr.predict.Final.myMFO_classfr.prob"
## [2] "TenYearCHD.fctr.predict.Final.myMFO_classfr"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](Fram_template2_files/figure-html/fit.data.training_1-3.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 129.437 144.132  14.695
## 16  predict.data.new          9          0 144.132      NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
# sav_newobs_df <- glb_newobs_df
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(glb_newobs_df, mdl_id = glb_fin_mdl_id,
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.5
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8479681 0.1520319
## 2 0.8479681 0.1520319
## 3 0.8479681 0.1520319
## 4 0.8479681 0.1520319
## 5 0.8479681 0.1520319
## 6 0.8479681 0.1520319
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](Fram_template2_files/figure-html/predict.data.new-1.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames TenYearCHD.fctr
## 3636      3636               Y
## 1891      1891               N
## 2            2               N
## 4236      4236               N
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.prob
## 3636                                        0.1520319
## 1891                                        0.1520319
## 2                                           0.1520319
## 4236                                        0.1520319
##      TenYearCHD.fctr.predict.Final.myMFO_classfr
## 3636                                           N
## 1891                                           N
## 2                                              N
## 4236                                           N
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.accurate
## 3636                                                FALSE
## 1891                                                 TRUE
## 2                                                    TRUE
## 4236                                                 TRUE
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.error .label
## 3636                                        -0.3479681   3636
## 1891                                         0.0000000   1891
## 2                                            0.0000000      2
## 4236                                         0.0000000   4236
## [1] "Inaccurate: "
##      .rownames TenYearCHD.fctr
## 1029      1029               Y
## 1033      1033               Y
## 1045      1045               Y
## 1046      1046               Y
## 1052      1052               Y
## 1056      1056               Y
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.prob
## 1029                                        0.1520319
## 1033                                        0.1520319
## 1045                                        0.1520319
## 1046                                        0.1520319
## 1052                                        0.1520319
## 1056                                        0.1520319
##      TenYearCHD.fctr.predict.Final.myMFO_classfr
## 1029                                           N
## 1033                                           N
## 1045                                           N
## 1046                                           N
## 1052                                           N
## 1056                                           N
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.accurate
## 1029                                                FALSE
## 1033                                                FALSE
## 1045                                                FALSE
## 1046                                                FALSE
## 1052                                                FALSE
## 1056                                                FALSE
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.error
## 1029                                        -0.3479681
## 1033                                        -0.3479681
## 1045                                        -0.3479681
## 1046                                        -0.3479681
## 1052                                        -0.3479681
## 1056                                        -0.3479681
##      .rownames TenYearCHD.fctr
## 1046      1046               Y
## 1879      1879               Y
## 2337      2337               Y
## 2931      2931               Y
## 895        895               Y
## 934        934               Y
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.prob
## 1046                                        0.1520319
## 1879                                        0.1520319
## 2337                                        0.1520319
## 2931                                        0.1520319
## 895                                         0.1520319
## 934                                         0.1520319
##      TenYearCHD.fctr.predict.Final.myMFO_classfr
## 1046                                           N
## 1879                                           N
## 2337                                           N
## 2931                                           N
## 895                                            N
## 934                                            N
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.accurate
## 1046                                                FALSE
## 1879                                                FALSE
## 2337                                                FALSE
## 2931                                                FALSE
## 895                                                 FALSE
## 934                                                 FALSE
##      TenYearCHD.fctr.predict.Final.myMFO_classfr.error
## 1046                                        -0.3479681
## 1879                                        -0.3479681
## 2337                                        -0.3479681
## 2931                                        -0.3479681
## 895                                         -0.3479681
## 934                                         -0.3479681
##     .rownames TenYearCHD.fctr
## 895       895               Y
## 900       900               Y
## 934       934               Y
## 936       936               Y
## 966       966               Y
## 971       971               Y
##     TenYearCHD.fctr.predict.Final.myMFO_classfr.prob
## 895                                        0.1520319
## 900                                        0.1520319
## 934                                        0.1520319
## 936                                        0.1520319
## 966                                        0.1520319
## 971                                        0.1520319
##     TenYearCHD.fctr.predict.Final.myMFO_classfr
## 895                                           N
## 900                                           N
## 934                                           N
## 936                                           N
## 966                                           N
## 971                                           N
##     TenYearCHD.fctr.predict.Final.myMFO_classfr.accurate
## 895                                                FALSE
## 900                                                FALSE
## 934                                                FALSE
## 936                                                FALSE
## 966                                                FALSE
## 971                                                FALSE
##     TenYearCHD.fctr.predict.Final.myMFO_classfr.error
## 895                                        -0.3479681
## 900                                        -0.3479681
## 934                                        -0.3479681
## 936                                        -0.3479681
## 966                                        -0.3479681
## 971                                        -0.3479681
```

![](Fram_template2_files/figure-html/predict.data.new-2.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
#     submit_df <- glb_newobs_df[, c(paste0(glb_rsp_var_out, glb_fin_mdl_id)), FALSE]
#     names(submit_df)[1] <- "BDscience"
#     submit_df$BDscience <- as.numeric(submit_df$BDscience) - 1
#     #submit_df <-rbind(submit_df, data.frame(bdanalytics=c(" ")))
#     print("Submission Stats:")
#     print(table(submit_df$BDscience, useNA = "ifany"))
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
submit_fname <- paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
                    "_submit.csv")
write.csv(submit_df, submit_fname, quote=FALSE, row.names=FALSE)
#cat(" ", "\n", file=submit_fn, append=TRUE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
```

```
## [1] 0.5
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: MFO.myMFO_classfr"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.myMFO_classfr"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 2756   27
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 1          MFO.myMFO_classfr        0.8483827   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.8483827   0.5000000     0.0000000
## 5            Max.cor.Y.rpart        0.8483827   0.5000000     0.0000000
## 11      All.X.no.rnorm.rpart        0.8483827   0.5000000     0.0000000
## 9                  All.X.glm        0.7553908   0.7349254     0.2459645
## 8              Low.cor.X.glm        0.7540431   0.7357374     0.2483375
## 10            All.X.bayesglm        0.7540431   0.7350949     0.2439984
## 6              Max.cor.Y.glm        0.7459569   0.7108252     0.2146098
## 7    Interact.High.cor.Y.glm        0.7318059   0.7114853     0.2131223
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.6448787   0.6261266     0.1472157
## 12         All.X.no.rnorm.rf        0.6111860   0.7115030     0.1888575
## 2    Random.myrandom_classfr        0.1516173   0.5169305     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 11          NA                    0.5
## 9     2125.724                    0.2
## 8     2124.378                    0.2
## 10    2125.755                    0.2
## 6     2176.886                    0.2
## 7     2177.511                    0.2
## 4           NA                    0.1
## 12          NA                    0.1
## 2           NA                    0.1
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_vars)) {
        stop("not implemented yet")
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
            pred_stats_df <- 
                mypredict_mdl(mdl=glb_models_lst[[glb_fin_mdl_id]], 
                              df=glb_newobs_df, 
                              rsp_var=glb_rsp_var, 
                              rsp_var_out=glb_rsp_var_out, 
                              model_id_method=glb_fin_mdl_id, 
                              label="new",
						      model_summaryFunction=glb_sel_mdl$control$summaryFunction, 
						      model_metric=glb_sel_mdl$metric,
						      model_metric_maximize=glb_sel_mdl$maximize,
						      ret_type="stats")        
            print(sprintf("%s prediction stats for glb_newobs_df:", glb_fin_mdl_id))
            print(pred_stats_df)
    }    
}    
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
        print(sprintf("%s new confusion matrix & accuracy: ", glb_fin_mdl_id))
        print(t(confusionMatrix(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)], 
                                glb_newobs_df[, glb_rsp_var])$table))
    }    

}    
```

```
## [1] "MFO.myMFO_classfr OOB confusion matrix & accuracy: "
##          Prediction
## Reference    N    Y
##         N 1259    0
##         Y  225    0
## [1] "Final.myMFO_classfr new confusion matrix & accuracy: "
##          Prediction
## Reference    N    Y
##         N 1259    0
##         Y  225    0
```

```r
dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

# if (glb_is_classification) {
#     print("FN_OOB_ids:")
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         glb_txt_vars])
#     print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
#                                 union(myfind_chr_cols_df(glb_OOBobs_df),
#                     grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
# }

dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##        MFO.myMFO_classfr.importance importance
## .rnorm                          NaN        NaN
##        Final.myMFO_classfr.importance
## .rnorm                            NaN
```

```r
# players_df <- data.frame(id=c("Chavez", "Giambi", "Menechino", "Myers", "Pena"),
#                          OBP=c(0.338, 0.391, 0.369, 0.313, 0.361),
#                          SLG=c(0.540, 0.450, 0.374, 0.447, 0.500),
#                         cost=c(1400000, 1065000, 295000, 800000, 300000))
# players_df$RS.predict <- predict(glb_models_lst[[csm_mdl_id]], players_df)
# print(orderBy(~ -RS.predict, players_df))

if (length(diff <- setdiff(names(glb_trnobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

if (length(diff <- setdiff(names(glb_fitobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
if (length(diff <- setdiff(names(glb_OOBobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
if (length(diff <- setdiff(names(glb_newobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor     bgn     end elapsed
## 16     predict.data.new          9          0 144.132 162.716  18.584
## 17 display.session.info         10          0 162.716      NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor     bgn     end elapsed
## 11              fit.models          7          1  59.431 100.566  41.135
## 10              fit.models          7          0  31.473  59.430  27.958
## 16        predict.data.new          9          0 144.132 162.716  18.584
## 12              fit.models          7          2 100.567 117.668  17.101
## 15       fit.data.training          8          1 129.437 144.132  14.695
## 2             inspect.data          2          0   7.882  17.703   9.822
## 7      manage.missing.data          4          1  21.321  30.357   9.036
## 13              fit.models          7          3 117.669 126.161   8.492
## 14       fit.data.training          8          0 126.162 129.436   3.274
## 3               scrub.data          2          1  17.704  19.657   1.953
## 5         extract.features          3          0  19.717  21.030   1.313
## 8          select.features          5          0  30.357  31.135   0.778
## 1              import.data          1          0   7.369   7.882   0.513
## 9  partition.data.training          6          0  31.135  31.473   0.338
## 6             cluster.data          4          0  21.031  21.321   0.290
## 4           transform.data          2          2  19.657  19.717   0.060
##    duration
## 11   41.135
## 10   27.957
## 16   18.584
## 12   17.101
## 15   14.695
## 2     9.821
## 7     9.036
## 13    8.492
## 14    3.274
## 3     1.953
## 5     1.313
## 8     0.778
## 1     0.513
## 9     0.338
## 6     0.290
## 4     0.060
## [1] "Total Elapsed Time: 162.716 secs"
```

![](Fram_template2_files/figure-html/display.session.info-1.png) 

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] tcltk     grid      parallel  stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1        randomForest_4.6-10 arm_1.8-5          
##  [4] lme4_1.1-8          Matrix_1.2-1        MASS_7.3-41        
##  [7] rpart.plot_1.5.2    rpart_4.1-9         ROCR_1.0-7         
## [10] gplots_2.17.0       mice_2.22           Rcpp_0.11.6        
## [13] dplyr_0.4.2         plyr_1.8.3          sqldf_0.4-10       
## [16] RSQLite_1.0.0       DBI_0.3.1           gsubfn_0.6-6       
## [19] proto_0.3-10        reshape2_1.4.1      caTools_1.17.1     
## [22] doMC_1.3.3          iterators_1.0.7     foreach_1.4.2      
## [25] doBy_4.5-13         survival_2.38-2     caret_6.0-47       
## [28] ggplot2_1.0.1       lattice_0.20-31    
## 
## loaded via a namespace (and not attached):
##  [1] class_7.3-12        gtools_3.5.0        assertthat_0.1     
##  [4] digest_0.6.8        R6_2.0.1            BradleyTerry2_1.0-6
##  [7] chron_2.3-47        coda_0.17-1         evaluate_0.7       
## [10] e1071_1.6-4         lazyeval_0.1.10     minqa_1.2.4        
## [13] SparseM_1.6         car_2.0-25          nloptr_1.0.4       
## [16] rmarkdown_0.7       labeling_0.3        splines_3.2.0      
## [19] stringr_1.0.0       munsell_0.4.2       compiler_3.2.0     
## [22] mgcv_1.8-6          htmltools_0.2.6     nnet_7.3-9         
## [25] codetools_0.2-11    brglm_0.5-9         bitops_1.0-6       
## [28] nlme_3.1-120        gtable_0.1.2        magrittr_1.5       
## [31] formatR_1.2         scales_0.2.5        KernSmooth_2.23-14 
## [34] stringi_0.5-2       RColorBrewer_1.1-2  tools_3.2.0        
## [37] abind_1.4-3         pbkrtest_0.4-2      yaml_2.1.13        
## [40] colorspace_1.2-6    knitr_1.10.5        quantreg_5.11
```
