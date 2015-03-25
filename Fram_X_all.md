# Framingham Heart Study: TenYearCHD classification
bdanalytics  

**  **    
**Date: (Wed) Mar 25, 2015**    

# Introduction:  

Data: 
Source: BioLINCC
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/framingham.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
#suppressPackageStartupMessages(require())

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # or "condition"
glb_split_newdata_size <- 1 - 0.65               # > 0 & < 1
glb_split_sample.seed <- 1000               # or any integer 

glb_predct_var <- "TenYearCHD"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- NULL

glb_exclude_vars_as_features <- union(glb_id_vars, ".rnorm")     # or NULL                      
# List chrs converted into factors; num/int transformed  
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("<col_name>")     # or NULL
#                                       )
# List feats that shd be excluded due to known causation by prediction variable
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("<col_name>")     # or NULL
#                                       )

glb_is_regression <- FALSE; glb_is_classification <- TRUE

glb_mdl <- glb_sel_mdl <- NULL
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/framingham.csv", 
    comment="glb_entity_df", force_header=TRUE,
    print_diagn=(glb_is_separate_newent_dataset | 
                !glb_split_entity_newent_datasets))
```

```
## [1] "Reading file ./data/framingham.csv..."
## [1] "dimensions of data in ./data/framingham.csv: 4,240 rows x 16 cols"
```

```r
if (glb_is_separate_newent_dataset) {
    glb_newent_df <- myimport_data(
        url="<newdt_url>", 
        comment="glb_newent_df", force_header=TRUE, print_diagn=TRUE)
} else {
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newent_df <- glb_entity_df[sample(1:nrow(glb_entity_df),
                                          max(2, nrow(glb_entity_df) / 1000)),]                    
    } else {
        if (glb_split_newdata_method == "condition") {
            stop("Not implemented yet") 
#     glb_newent_df <- subset(glb_entity_df, <condition>)        
        } else { 
            if (glb_split_newdata_method == "sample") {
                require(caTools)
                set.seed(glb_split_sample.seed)
                lcl_split <- sample.split(glb_entity_df[, glb_predct_var], 
                                      SplitRatio=(1-glb_split_newdata_size))
                glb_newent_df <- glb_entity_df[!lcl_split, ] 
                glb_entity_df <- glb_entity_df[lcl_split ,]
            }
        }    
    }
    comment(glb_newent_df) <- "glb_newent_df"
    myprint_df(glb_newent_df)
    str(glb_newent_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_entity_df)
        str(glb_entity_df)        
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
##  - attr(*, "comment")= chr "glb_newent_df"
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
##  - attr(*, "comment")= chr "glb_entity_df"
```

```r
script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>)

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))) 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<max_n_val>") 

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>)
        .rnorm=rnorm(1)
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_newent_df)
```

```
## Loading required package: plyr
```

```
##       male             age          education     currentSmoker   
##  Min.   :0.0000   Min.   :32.00   Min.   :1.000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:42.00   1st Qu.:1.000   1st Qu.:0.0000  
##  Median :0.0000   Median :49.00   Median :2.000   Median :0.0000  
##  Mean   :0.4227   Mean   :49.67   Mean   :1.969   Mean   :0.4888  
##  3rd Qu.:1.0000   3rd Qu.:56.00   3rd Qu.:3.000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :70.00   Max.   :4.000   Max.   :1.0000  
##                                   NA's   :70                      
##    cigsPerDay         BPMeds       prevalentStroke     prevalentHyp   
##  Min.   : 0.000   Min.   :0.0000   Min.   :0.000000   Min.   :0.0000  
##  1st Qu.: 0.000   1st Qu.:0.0000   1st Qu.:0.000000   1st Qu.:0.0000  
##  Median : 0.000   Median :0.0000   Median :0.000000   Median :0.0000  
##  Mean   : 8.802   Mean   :0.0283   Mean   :0.006531   Mean   :0.3128  
##  3rd Qu.:20.000   3rd Qu.:0.0000   3rd Qu.:0.000000   3rd Qu.:1.0000  
##  Max.   :70.000   Max.   :1.0000   Max.   :1.000000   Max.   :1.0000  
##  NA's   :18       NA's   :35                                          
##     diabetes          totChol          sysBP           diaBP      
##  Min.   :0.00000   Min.   :107.0   Min.   : 83.5   Min.   : 48.0  
##  1st Qu.:0.00000   1st Qu.:206.0   1st Qu.:116.0   1st Qu.: 74.0  
##  Median :0.00000   Median :234.0   Median :128.0   Median : 82.0  
##  Mean   :0.02576   Mean   :237.3   Mean   :132.3   Mean   : 82.7  
##  3rd Qu.:0.00000   3rd Qu.:264.0   3rd Qu.:144.0   3rd Qu.: 89.0  
##  Max.   :1.00000   Max.   :600.0   Max.   :295.0   Max.   :142.5  
##                    NA's   :30                                     
##       BMI          heartRate         glucose         TenYearCHD   
##  Min.   :15.54   Min.   : 44.00   Min.   : 44.00   Min.   :0.000  
##  1st Qu.:23.07   1st Qu.: 68.00   1st Qu.: 72.00   1st Qu.:0.000  
##  Median :25.36   Median : 75.00   Median : 78.00   Median :0.000  
##  Mean   :25.80   Mean   : 75.94   Mean   : 82.06   Mean   :0.152  
##  3rd Qu.:28.04   3rd Qu.: 83.00   3rd Qu.: 87.00   3rd Qu.:0.000  
##  Max.   :56.80   Max.   :143.00   Max.   :394.00   Max.   :1.000  
##  NA's   :14      NA's   :1        NA's   :240                     
##      .rnorm       
##  Min.   :0.08328  
##  1st Qu.:0.08328  
##  Median :0.08328  
##  Mean   :0.08328  
##  3rd Qu.:0.08328  
##  Max.   :0.08328  
##                   
##            male             age       education   currentSmoker 
##               0               0              70               0 
##      cigsPerDay          BPMeds prevalentStroke    prevalentHyp 
##              18              35               0               0 
##        diabetes         totChol           sysBP           diaBP 
##               0              30               0               0 
##             BMI       heartRate         glucose      TenYearCHD 
##              14               1             240               0 
##          .rnorm 
##               0
```

```r
glb_newent_df <- add_new_diag_feats(glb_newent_df, glb_entity_df)
```

```
##       male             age          education     currentSmoker  
##  Min.   :0.0000   Min.   :33.00   Min.   :1.000   Min.   :0.000  
##  1st Qu.:0.0000   1st Qu.:42.00   1st Qu.:1.000   1st Qu.:0.000  
##  Median :0.0000   Median :48.50   Median :2.000   Median :1.000  
##  Mean   :0.4414   Mean   :49.42   Mean   :1.999   Mean   :0.504  
##  3rd Qu.:1.0000   3rd Qu.:56.00   3rd Qu.:3.000   3rd Qu.:1.000  
##  Max.   :1.0000   Max.   :69.00   Max.   :4.000   Max.   :1.000  
##                                   NA's   :35                     
##    cigsPerDay         BPMeds        prevalentStroke     prevalentHyp   
##  Min.   : 0.000   Min.   :0.00000   Min.   :0.000000   Min.   :0.0000  
##  1st Qu.: 0.000   1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.0000  
##  Median : 1.000   Median :0.00000   Median :0.000000   Median :0.0000  
##  Mean   : 9.384   Mean   :0.03206   Mean   :0.004717   Mean   :0.3066  
##  3rd Qu.:20.000   3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:1.0000  
##  Max.   :60.000   Max.   :1.00000   Max.   :1.000000   Max.   :1.0000  
##  NA's   :11       NA's   :18                                           
##     diabetes          totChol          sysBP           diaBP       
##  Min.   :0.00000   Min.   :113.0   Min.   : 83.5   Min.   : 51.00  
##  1st Qu.:0.00000   1st Qu.:205.0   1st Qu.:118.0   1st Qu.: 75.00  
##  Median :0.00000   Median :233.5   Median :128.5   Median : 82.00  
##  Mean   :0.02561   Mean   :235.7   Mean   :132.5   Mean   : 83.26  
##  3rd Qu.:0.00000   3rd Qu.:262.0   3rd Qu.:143.0   3rd Qu.: 90.00  
##  Max.   :1.00000   Max.   :696.0   Max.   :244.0   Max.   :133.00  
##                    NA's   :20                                      
##       BMI          heartRate         glucose         TenYearCHD    
##  Min.   :16.48   Min.   : 45.00   Min.   : 40.00   Min.   :0.0000  
##  1st Qu.:23.05   1st Qu.: 67.75   1st Qu.: 71.00   1st Qu.:0.0000  
##  Median :25.46   Median : 75.00   Median : 78.00   Median :0.0000  
##  Mean   :25.80   Mean   : 75.77   Mean   : 81.78   Mean   :0.1516  
##  3rd Qu.:28.06   3rd Qu.: 83.00   3rd Qu.: 86.00   3rd Qu.:0.0000  
##  Max.   :51.28   Max.   :130.00   Max.   :386.00   Max.   :1.0000  
##  NA's   :5                        NA's   :148                      
##      .rnorm      
##  Min.   :-1.079  
##  1st Qu.:-1.079  
##  Median :-1.079  
##  Mean   :-1.079  
##  3rd Qu.:-1.079  
##  Max.   :-1.079  
##                  
##            male             age       education   currentSmoker 
##               0               0              35               0 
##      cigsPerDay          BPMeds prevalentStroke    prevalentHyp 
##              11              18               0               0 
##        diabetes         totChol           sysBP           diaBP 
##               0              20               0               0 
##             BMI       heartRate         glucose      TenYearCHD 
##               5               0             148               0 
##          .rnorm 
##               0
```

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_newent_df
# Check for glb_newent_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(glb_entity_df[which.max(glb_entity_df$<col_name>),])

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(mycreate_xtab(glb_entity_df, <col1_name>))
# print(mycreate_xtab(glb_entity_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](Fram_X_all_files/figure-html/inspect_explore_data_1-1.png) 

```r
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>"))

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_entity_df <- na.omit(glb_entity_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newent_df <- mymap_codes(glb_newent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))
# glb_newent_df$<col_name>.fctr <- factor(glb_newent_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
```

## Step `3`: extract features

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_entity_df$<col_name>), -2, na.pad=TRUE)
# glb_entity_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newent_df$<col_name>), -2, na.pad=TRUE)
# glb_newent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newent_df[1, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df) - 1, 
#                                                    "<col_name>"]
# glb_newent_df[2, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_newent_df <- mutate(glb_newent_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_newent_df))

# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- 
    myselect_features(glb_entity_df, glb_exclude_vars_as_features, glb_predct_var))
```

```
##                              id       cor.y  cor.y.abs
## age                         age  0.22630732 0.22630732
## sysBP                     sysBP  0.20817830 0.20817830
## prevalentHyp       prevalentHyp  0.17862412 0.17862412
## diaBP                     diaBP  0.13598063 0.13598063
## glucose                 glucose  0.12590588 0.12590588
## totChol                 totChol  0.10111464 0.10111464
## BPMeds                   BPMeds  0.09431449 0.09431449
## male                       male  0.08568021 0.08568021
## BMI                         BMI  0.08276951 0.08276951
## prevalentStroke prevalentStroke  0.07857814 0.07857814
## diabetes               diabetes  0.07785838 0.07785838
## education             education -0.05893901 0.05893901
## cigsPerDay           cigsPerDay  0.04663366 0.04663366
## currentSmoker     currentSmoker  0.01256042 0.01256042
## heartRate             heartRate  0.00767770 0.00767770
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, merge(glb_feats_df, 
          mydelete_cor_features(glb_feats_df, glb_entity_df, glb_predct_var, 
                                glb_exclude_vars_as_features), 
          all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##                          age       sysBP  prevalentHyp       diaBP
## age              1.000000000  0.41326218  0.3153090700  0.22486803
## sysBP            0.413262184  1.00000000  0.6974155926  0.77682763
## prevalentHyp     0.315309070  0.69741559  1.0000000000  0.61295623
## diaBP            0.224868033  0.77682763  0.6129562281  1.00000000
## glucose          0.111908047  0.16898015  0.0943194276  0.08349467
## totChol          0.267882471  0.22615506  0.1830656243  0.19186816
## BPMeds           0.113269329  0.21643871  0.2534041449  0.16274013
## male            -0.025448252 -0.04140623  0.0009830021  0.04866241
## BMI              0.150262707  0.32106171  0.2895816469  0.38249134
## prevalentStroke  0.058796990  0.04640512  0.0716071012  0.04298333
## diabetes         0.101872820  0.13751543  0.0928398677  0.06355844
## education       -0.188229904 -0.15179787 -0.0871193789 -0.08524587
## cigsPerDay      -0.189628184 -0.09398776 -0.0604195561 -0.04849414
## currentSmoker   -0.216933400 -0.13662817 -0.1038093439 -0.10395587
## heartRate       -0.009532991  0.18624326  0.1452279449  0.18542515
##                     glucose      totChol      BPMeds          male
## age              0.11190805  0.267882471  0.11326933 -0.0254482516
## sysBP            0.16898015  0.226155056  0.21643871 -0.0414062307
## prevalentHyp     0.09431943  0.183065624  0.25340414  0.0009830021
## diaBP            0.08349467  0.191868162  0.16274013  0.0486624068
## glucose          1.00000000  0.061163443  0.06339551  0.0246939798
## totChol          0.06116344  1.000000000  0.08037482 -0.0671180696
## BPMeds           0.06339551  0.080374815  1.00000000 -0.0297270885
## male             0.02469398 -0.067118070 -0.02972709  1.0000000000
## BMI              0.10244310  0.150911677  0.09591403  0.0917258412
## prevalentStroke  0.01423816 -0.004973045  0.12277227 -0.0146704914
## diabetes         0.59132353  0.070177330  0.07115569  0.0184866748
## education       -0.02666946 -0.038246242 -0.01314912  0.0121527600
## cigsPerDay      -0.06605912 -0.038567726 -0.04258116  0.3297723388
## currentSmoker   -0.05675304 -0.064549548 -0.04753463  0.2095434014
## heartRate        0.09763358  0.069139622  0.01351274 -0.1127422732
##                         BMI prevalentStroke    diabetes   education
## age              0.15026271     0.058796990  0.10187282 -0.18822990
## sysBP            0.32106171     0.046405125  0.13751543 -0.15179787
## prevalentHyp     0.28958165     0.071607101  0.09283987 -0.08711938
## diaBP            0.38249134     0.042983327  0.06355844 -0.08524587
## glucose          0.10244310     0.014238159  0.59132353 -0.02666946
## totChol          0.15091168    -0.004973045  0.07017733 -0.03824624
## BPMeds           0.09591403     0.122772274  0.07115569 -0.01314912
## male             0.09172584    -0.014670491  0.01848667  0.01215276
## BMI              1.00000000     0.018628712  0.08924740 -0.15650645
## prevalentStroke  0.01862871     1.000000000  0.01524825 -0.04705743
## diabetes         0.08924740     0.015248255  1.00000000 -0.03014622
## education       -0.15650645    -0.047057425 -0.03014622  1.00000000
## cigsPerDay      -0.08452663    -0.036191779 -0.04598088  0.03004891
## currentSmoker   -0.17639037    -0.034220666 -0.05819551  0.05010793
## heartRate        0.07934246    -0.017935394  0.03761657 -0.06707243
##                  cigsPerDay currentSmoker    heartRate
## age             -0.18962818   -0.21693340 -0.009532991
## sysBP           -0.09398776   -0.13662817  0.186243259
## prevalentHyp    -0.06041956   -0.10380934  0.145227945
## diaBP           -0.04849414   -0.10395587  0.185425146
## glucose         -0.06605912   -0.05675304  0.097633579
## totChol         -0.03856773   -0.06454955  0.069139622
## BPMeds          -0.04258116   -0.04753463  0.013512744
## male             0.32977234    0.20954340 -0.112742273
## BMI             -0.08452663   -0.17639037  0.079342460
## prevalentStroke -0.03619178   -0.03422067 -0.017935394
## diabetes        -0.04598088   -0.05819551  0.037616573
## education        0.03004891    0.05010793 -0.067072431
## cigsPerDay       1.00000000    0.76851738  0.063283452
## currentSmoker    0.76851738    1.00000000  0.033969207
## heartRate        0.06328345    0.03396921  1.000000000
##                         age      sysBP prevalentHyp      diaBP    glucose
## age             0.000000000 0.41326218 0.3153090700 0.22486803 0.11190805
## sysBP           0.413262184 0.00000000 0.6974155926 0.77682763 0.16898015
## prevalentHyp    0.315309070 0.69741559 0.0000000000 0.61295623 0.09431943
## diaBP           0.224868033 0.77682763 0.6129562281 0.00000000 0.08349467
## glucose         0.111908047 0.16898015 0.0943194276 0.08349467 0.00000000
## totChol         0.267882471 0.22615506 0.1830656243 0.19186816 0.06116344
## BPMeds          0.113269329 0.21643871 0.2534041449 0.16274013 0.06339551
## male            0.025448252 0.04140623 0.0009830021 0.04866241 0.02469398
## BMI             0.150262707 0.32106171 0.2895816469 0.38249134 0.10244310
## prevalentStroke 0.058796990 0.04640512 0.0716071012 0.04298333 0.01423816
## diabetes        0.101872820 0.13751543 0.0928398677 0.06355844 0.59132353
## education       0.188229904 0.15179787 0.0871193789 0.08524587 0.02666946
## cigsPerDay      0.189628184 0.09398776 0.0604195561 0.04849414 0.06605912
## currentSmoker   0.216933400 0.13662817 0.1038093439 0.10395587 0.05675304
## heartRate       0.009532991 0.18624326 0.1452279449 0.18542515 0.09763358
##                     totChol     BPMeds         male        BMI
## age             0.267882471 0.11326933 0.0254482516 0.15026271
## sysBP           0.226155056 0.21643871 0.0414062307 0.32106171
## prevalentHyp    0.183065624 0.25340414 0.0009830021 0.28958165
## diaBP           0.191868162 0.16274013 0.0486624068 0.38249134
## glucose         0.061163443 0.06339551 0.0246939798 0.10244310
## totChol         0.000000000 0.08037482 0.0671180696 0.15091168
## BPMeds          0.080374815 0.00000000 0.0297270885 0.09591403
## male            0.067118070 0.02972709 0.0000000000 0.09172584
## BMI             0.150911677 0.09591403 0.0917258412 0.00000000
## prevalentStroke 0.004973045 0.12277227 0.0146704914 0.01862871
## diabetes        0.070177330 0.07115569 0.0184866748 0.08924740
## education       0.038246242 0.01314912 0.0121527600 0.15650645
## cigsPerDay      0.038567726 0.04258116 0.3297723388 0.08452663
## currentSmoker   0.064549548 0.04753463 0.2095434014 0.17639037
## heartRate       0.069139622 0.01351274 0.1127422732 0.07934246
##                 prevalentStroke   diabetes  education cigsPerDay
## age                 0.058796990 0.10187282 0.18822990 0.18962818
## sysBP               0.046405125 0.13751543 0.15179787 0.09398776
## prevalentHyp        0.071607101 0.09283987 0.08711938 0.06041956
## diaBP               0.042983327 0.06355844 0.08524587 0.04849414
## glucose             0.014238159 0.59132353 0.02666946 0.06605912
## totChol             0.004973045 0.07017733 0.03824624 0.03856773
## BPMeds              0.122772274 0.07115569 0.01314912 0.04258116
## male                0.014670491 0.01848667 0.01215276 0.32977234
## BMI                 0.018628712 0.08924740 0.15650645 0.08452663
## prevalentStroke     0.000000000 0.01524825 0.04705743 0.03619178
## diabetes            0.015248255 0.00000000 0.03014622 0.04598088
## education           0.047057425 0.03014622 0.00000000 0.03004891
## cigsPerDay          0.036191779 0.04598088 0.03004891 0.00000000
## currentSmoker       0.034220666 0.05819551 0.05010793 0.76851738
## heartRate           0.017935394 0.03761657 0.06707243 0.06328345
##                 currentSmoker   heartRate
## age                0.21693340 0.009532991
## sysBP              0.13662817 0.186243259
## prevalentHyp       0.10380934 0.145227945
## diaBP              0.10395587 0.185425146
## glucose            0.05675304 0.097633579
## totChol            0.06454955 0.069139622
## BPMeds             0.04753463 0.013512744
## male               0.20954340 0.112742273
## BMI                0.17639037 0.079342460
## prevalentStroke    0.03422067 0.017935394
## diabetes           0.05819551 0.037616573
## education          0.05010793 0.067072431
## cigsPerDay         0.76851738 0.063283452
## currentSmoker      0.00000000 0.033969207
## heartRate          0.03396921 0.000000000
## [1] "cor(sysBP, diaBP)=0.7768"
```

![](Fram_X_all_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(TenYearCHD, sysBP)=0.2082"
## [1] "cor(TenYearCHD, diaBP)=0.1360"
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping diaBP as a feature
```

![](Fram_X_all_files/figure-html/remove_correlated_features-2.png) 

```
##                              id       cor.y  cor.y.abs
## age                         age  0.22630732 0.22630732
## sysBP                     sysBP  0.20817830 0.20817830
## prevalentHyp       prevalentHyp  0.17862412 0.17862412
## glucose                 glucose  0.12590588 0.12590588
## totChol                 totChol  0.10111464 0.10111464
## BPMeds                   BPMeds  0.09431449 0.09431449
## male                       male  0.08568021 0.08568021
## BMI                         BMI  0.08276951 0.08276951
## prevalentStroke prevalentStroke  0.07857814 0.07857814
## diabetes               diabetes  0.07785838 0.07785838
## education             education -0.05893901 0.05893901
## cigsPerDay           cigsPerDay  0.04663366 0.04663366
## currentSmoker     currentSmoker  0.01256042 0.01256042
## heartRate             heartRate  0.00767770 0.00767770
##                          age       sysBP  prevalentHyp     glucose
## age              1.000000000  0.41326218  0.3153090700  0.11190805
## sysBP            0.413262184  1.00000000  0.6974155926  0.16898015
## prevalentHyp     0.315309070  0.69741559  1.0000000000  0.09431943
## glucose          0.111908047  0.16898015  0.0943194276  1.00000000
## totChol          0.267882471  0.22615506  0.1830656243  0.06116344
## BPMeds           0.113269329  0.21643871  0.2534041449  0.06339551
## male            -0.025448252 -0.04140623  0.0009830021  0.02469398
## BMI              0.150262707  0.32106171  0.2895816469  0.10244310
## prevalentStroke  0.058796990  0.04640512  0.0716071012  0.01423816
## diabetes         0.101872820  0.13751543  0.0928398677  0.59132353
## education       -0.188229904 -0.15179787 -0.0871193789 -0.02666946
## cigsPerDay      -0.189628184 -0.09398776 -0.0604195561 -0.06605912
## currentSmoker   -0.216933400 -0.13662817 -0.1038093439 -0.05675304
## heartRate       -0.009532991  0.18624326  0.1452279449  0.09763358
##                      totChol      BPMeds          male         BMI
## age              0.267882471  0.11326933 -0.0254482516  0.15026271
## sysBP            0.226155056  0.21643871 -0.0414062307  0.32106171
## prevalentHyp     0.183065624  0.25340414  0.0009830021  0.28958165
## glucose          0.061163443  0.06339551  0.0246939798  0.10244310
## totChol          1.000000000  0.08037482 -0.0671180696  0.15091168
## BPMeds           0.080374815  1.00000000 -0.0297270885  0.09591403
## male            -0.067118070 -0.02972709  1.0000000000  0.09172584
## BMI              0.150911677  0.09591403  0.0917258412  1.00000000
## prevalentStroke -0.004973045  0.12277227 -0.0146704914  0.01862871
## diabetes         0.070177330  0.07115569  0.0184866748  0.08924740
## education       -0.038246242 -0.01314912  0.0121527600 -0.15650645
## cigsPerDay      -0.038567726 -0.04258116  0.3297723388 -0.08452663
## currentSmoker   -0.064549548 -0.04753463  0.2095434014 -0.17639037
## heartRate        0.069139622  0.01351274 -0.1127422732  0.07934246
##                 prevalentStroke    diabetes   education  cigsPerDay
## age                 0.058796990  0.10187282 -0.18822990 -0.18962818
## sysBP               0.046405125  0.13751543 -0.15179787 -0.09398776
## prevalentHyp        0.071607101  0.09283987 -0.08711938 -0.06041956
## glucose             0.014238159  0.59132353 -0.02666946 -0.06605912
## totChol            -0.004973045  0.07017733 -0.03824624 -0.03856773
## BPMeds              0.122772274  0.07115569 -0.01314912 -0.04258116
## male               -0.014670491  0.01848667  0.01215276  0.32977234
## BMI                 0.018628712  0.08924740 -0.15650645 -0.08452663
## prevalentStroke     1.000000000  0.01524825 -0.04705743 -0.03619178
## diabetes            0.015248255  1.00000000 -0.03014622 -0.04598088
## education          -0.047057425 -0.03014622  1.00000000  0.03004891
## cigsPerDay         -0.036191779 -0.04598088  0.03004891  1.00000000
## currentSmoker      -0.034220666 -0.05819551  0.05010793  0.76851738
## heartRate          -0.017935394  0.03761657 -0.06707243  0.06328345
##                 currentSmoker    heartRate
## age               -0.21693340 -0.009532991
## sysBP             -0.13662817  0.186243259
## prevalentHyp      -0.10380934  0.145227945
## glucose           -0.05675304  0.097633579
## totChol           -0.06454955  0.069139622
## BPMeds            -0.04753463  0.013512744
## male               0.20954340 -0.112742273
## BMI               -0.17639037  0.079342460
## prevalentStroke   -0.03422067 -0.017935394
## diabetes          -0.05819551  0.037616573
## education          0.05010793 -0.067072431
## cigsPerDay         0.76851738  0.063283452
## currentSmoker      1.00000000  0.033969207
## heartRate          0.03396921  1.000000000
##                         age      sysBP prevalentHyp    glucose     totChol
## age             0.000000000 0.41326218 0.3153090700 0.11190805 0.267882471
## sysBP           0.413262184 0.00000000 0.6974155926 0.16898015 0.226155056
## prevalentHyp    0.315309070 0.69741559 0.0000000000 0.09431943 0.183065624
## glucose         0.111908047 0.16898015 0.0943194276 0.00000000 0.061163443
## totChol         0.267882471 0.22615506 0.1830656243 0.06116344 0.000000000
## BPMeds          0.113269329 0.21643871 0.2534041449 0.06339551 0.080374815
## male            0.025448252 0.04140623 0.0009830021 0.02469398 0.067118070
## BMI             0.150262707 0.32106171 0.2895816469 0.10244310 0.150911677
## prevalentStroke 0.058796990 0.04640512 0.0716071012 0.01423816 0.004973045
## diabetes        0.101872820 0.13751543 0.0928398677 0.59132353 0.070177330
## education       0.188229904 0.15179787 0.0871193789 0.02666946 0.038246242
## cigsPerDay      0.189628184 0.09398776 0.0604195561 0.06605912 0.038567726
## currentSmoker   0.216933400 0.13662817 0.1038093439 0.05675304 0.064549548
## heartRate       0.009532991 0.18624326 0.1452279449 0.09763358 0.069139622
##                     BPMeds         male        BMI prevalentStroke
## age             0.11326933 0.0254482516 0.15026271     0.058796990
## sysBP           0.21643871 0.0414062307 0.32106171     0.046405125
## prevalentHyp    0.25340414 0.0009830021 0.28958165     0.071607101
## glucose         0.06339551 0.0246939798 0.10244310     0.014238159
## totChol         0.08037482 0.0671180696 0.15091168     0.004973045
## BPMeds          0.00000000 0.0297270885 0.09591403     0.122772274
## male            0.02972709 0.0000000000 0.09172584     0.014670491
## BMI             0.09591403 0.0917258412 0.00000000     0.018628712
## prevalentStroke 0.12277227 0.0146704914 0.01862871     0.000000000
## diabetes        0.07115569 0.0184866748 0.08924740     0.015248255
## education       0.01314912 0.0121527600 0.15650645     0.047057425
## cigsPerDay      0.04258116 0.3297723388 0.08452663     0.036191779
## currentSmoker   0.04753463 0.2095434014 0.17639037     0.034220666
## heartRate       0.01351274 0.1127422732 0.07934246     0.017935394
##                   diabetes  education cigsPerDay currentSmoker   heartRate
## age             0.10187282 0.18822990 0.18962818    0.21693340 0.009532991
## sysBP           0.13751543 0.15179787 0.09398776    0.13662817 0.186243259
## prevalentHyp    0.09283987 0.08711938 0.06041956    0.10380934 0.145227945
## glucose         0.59132353 0.02666946 0.06605912    0.05675304 0.097633579
## totChol         0.07017733 0.03824624 0.03856773    0.06454955 0.069139622
## BPMeds          0.07115569 0.01314912 0.04258116    0.04753463 0.013512744
## male            0.01848667 0.01215276 0.32977234    0.20954340 0.112742273
## BMI             0.08924740 0.15650645 0.08452663    0.17639037 0.079342460
## prevalentStroke 0.01524825 0.04705743 0.03619178    0.03422067 0.017935394
## diabetes        0.00000000 0.03014622 0.04598088    0.05819551 0.037616573
## education       0.03014622 0.00000000 0.03004891    0.05010793 0.067072431
## cigsPerDay      0.04598088 0.03004891 0.00000000    0.76851738 0.063283452
## currentSmoker   0.05819551 0.05010793 0.76851738    0.00000000 0.033969207
## heartRate       0.03761657 0.06707243 0.06328345    0.03396921 0.000000000
## [1] "cor(cigsPerDay, currentSmoker)=0.7685"
```

```
## Warning: Removed 18 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/remove_correlated_features-3.png) 

```
## [1] "cor(TenYearCHD, cigsPerDay)=0.0466"
## [1] "cor(TenYearCHD, currentSmoker)=0.0126"
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 18 rows containing missing values (stat_smooth).
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 18 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 18 rows containing missing values (geom_point).
```

```
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping currentSmoker as a feature
```

![](Fram_X_all_files/figure-html/remove_correlated_features-4.png) 

```
##                              id       cor.y  cor.y.abs
## age                         age  0.22630732 0.22630732
## sysBP                     sysBP  0.20817830 0.20817830
## prevalentHyp       prevalentHyp  0.17862412 0.17862412
## glucose                 glucose  0.12590588 0.12590588
## totChol                 totChol  0.10111464 0.10111464
## BPMeds                   BPMeds  0.09431449 0.09431449
## male                       male  0.08568021 0.08568021
## BMI                         BMI  0.08276951 0.08276951
## prevalentStroke prevalentStroke  0.07857814 0.07857814
## diabetes               diabetes  0.07785838 0.07785838
## education             education -0.05893901 0.05893901
## cigsPerDay           cigsPerDay  0.04663366 0.04663366
## heartRate             heartRate  0.00767770 0.00767770
##                          age       sysBP  prevalentHyp     glucose
## age              1.000000000  0.41326218  0.3153090700  0.11190805
## sysBP            0.413262184  1.00000000  0.6974155926  0.16898015
## prevalentHyp     0.315309070  0.69741559  1.0000000000  0.09431943
## glucose          0.111908047  0.16898015  0.0943194276  1.00000000
## totChol          0.267882471  0.22615506  0.1830656243  0.06116344
## BPMeds           0.113269329  0.21643871  0.2534041449  0.06339551
## male            -0.025448252 -0.04140623  0.0009830021  0.02469398
## BMI              0.150262707  0.32106171  0.2895816469  0.10244310
## prevalentStroke  0.058796990  0.04640512  0.0716071012  0.01423816
## diabetes         0.101872820  0.13751543  0.0928398677  0.59132353
## education       -0.188229904 -0.15179787 -0.0871193789 -0.02666946
## cigsPerDay      -0.189628184 -0.09398776 -0.0604195561 -0.06605912
## heartRate       -0.009532991  0.18624326  0.1452279449  0.09763358
##                      totChol      BPMeds          male         BMI
## age              0.267882471  0.11326933 -0.0254482516  0.15026271
## sysBP            0.226155056  0.21643871 -0.0414062307  0.32106171
## prevalentHyp     0.183065624  0.25340414  0.0009830021  0.28958165
## glucose          0.061163443  0.06339551  0.0246939798  0.10244310
## totChol          1.000000000  0.08037482 -0.0671180696  0.15091168
## BPMeds           0.080374815  1.00000000 -0.0297270885  0.09591403
## male            -0.067118070 -0.02972709  1.0000000000  0.09172584
## BMI              0.150911677  0.09591403  0.0917258412  1.00000000
## prevalentStroke -0.004973045  0.12277227 -0.0146704914  0.01862871
## diabetes         0.070177330  0.07115569  0.0184866748  0.08924740
## education       -0.038246242 -0.01314912  0.0121527600 -0.15650645
## cigsPerDay      -0.038567726 -0.04258116  0.3297723388 -0.08452663
## heartRate        0.069139622  0.01351274 -0.1127422732  0.07934246
##                 prevalentStroke    diabetes   education  cigsPerDay
## age                 0.058796990  0.10187282 -0.18822990 -0.18962818
## sysBP               0.046405125  0.13751543 -0.15179787 -0.09398776
## prevalentHyp        0.071607101  0.09283987 -0.08711938 -0.06041956
## glucose             0.014238159  0.59132353 -0.02666946 -0.06605912
## totChol            -0.004973045  0.07017733 -0.03824624 -0.03856773
## BPMeds              0.122772274  0.07115569 -0.01314912 -0.04258116
## male               -0.014670491  0.01848667  0.01215276  0.32977234
## BMI                 0.018628712  0.08924740 -0.15650645 -0.08452663
## prevalentStroke     1.000000000  0.01524825 -0.04705743 -0.03619178
## diabetes            0.015248255  1.00000000 -0.03014622 -0.04598088
## education          -0.047057425 -0.03014622  1.00000000  0.03004891
## cigsPerDay         -0.036191779 -0.04598088  0.03004891  1.00000000
## heartRate          -0.017935394  0.03761657 -0.06707243  0.06328345
##                    heartRate
## age             -0.009532991
## sysBP            0.186243259
## prevalentHyp     0.145227945
## glucose          0.097633579
## totChol          0.069139622
## BPMeds           0.013512744
## male            -0.112742273
## BMI              0.079342460
## prevalentStroke -0.017935394
## diabetes         0.037616573
## education       -0.067072431
## cigsPerDay       0.063283452
## heartRate        1.000000000
##                         age      sysBP prevalentHyp    glucose     totChol
## age             0.000000000 0.41326218 0.3153090700 0.11190805 0.267882471
## sysBP           0.413262184 0.00000000 0.6974155926 0.16898015 0.226155056
## prevalentHyp    0.315309070 0.69741559 0.0000000000 0.09431943 0.183065624
## glucose         0.111908047 0.16898015 0.0943194276 0.00000000 0.061163443
## totChol         0.267882471 0.22615506 0.1830656243 0.06116344 0.000000000
## BPMeds          0.113269329 0.21643871 0.2534041449 0.06339551 0.080374815
## male            0.025448252 0.04140623 0.0009830021 0.02469398 0.067118070
## BMI             0.150262707 0.32106171 0.2895816469 0.10244310 0.150911677
## prevalentStroke 0.058796990 0.04640512 0.0716071012 0.01423816 0.004973045
## diabetes        0.101872820 0.13751543 0.0928398677 0.59132353 0.070177330
## education       0.188229904 0.15179787 0.0871193789 0.02666946 0.038246242
## cigsPerDay      0.189628184 0.09398776 0.0604195561 0.06605912 0.038567726
## heartRate       0.009532991 0.18624326 0.1452279449 0.09763358 0.069139622
##                     BPMeds         male        BMI prevalentStroke
## age             0.11326933 0.0254482516 0.15026271     0.058796990
## sysBP           0.21643871 0.0414062307 0.32106171     0.046405125
## prevalentHyp    0.25340414 0.0009830021 0.28958165     0.071607101
## glucose         0.06339551 0.0246939798 0.10244310     0.014238159
## totChol         0.08037482 0.0671180696 0.15091168     0.004973045
## BPMeds          0.00000000 0.0297270885 0.09591403     0.122772274
## male            0.02972709 0.0000000000 0.09172584     0.014670491
## BMI             0.09591403 0.0917258412 0.00000000     0.018628712
## prevalentStroke 0.12277227 0.0146704914 0.01862871     0.000000000
## diabetes        0.07115569 0.0184866748 0.08924740     0.015248255
## education       0.01314912 0.0121527600 0.15650645     0.047057425
## cigsPerDay      0.04258116 0.3297723388 0.08452663     0.036191779
## heartRate       0.01351274 0.1127422732 0.07934246     0.017935394
##                   diabetes  education cigsPerDay   heartRate
## age             0.10187282 0.18822990 0.18962818 0.009532991
## sysBP           0.13751543 0.15179787 0.09398776 0.186243259
## prevalentHyp    0.09283987 0.08711938 0.06041956 0.145227945
## glucose         0.59132353 0.02666946 0.06605912 0.097633579
## totChol         0.07017733 0.03824624 0.03856773 0.069139622
## BPMeds          0.07115569 0.01314912 0.04258116 0.013512744
## male            0.01848667 0.01215276 0.32977234 0.112742273
## BMI             0.08924740 0.15650645 0.08452663 0.079342460
## prevalentStroke 0.01524825 0.04705743 0.03619178 0.017935394
## diabetes        0.00000000 0.03014622 0.04598088 0.037616573
## education       0.03014622 0.00000000 0.03004891 0.067072431
## cigsPerDay      0.04598088 0.03004891 0.00000000 0.063283452
## heartRate       0.03761657 0.06707243 0.06328345 0.000000000
##                 id       cor.y  cor.y.abs cor.low
## 1              age  0.22630732 0.22630732       1
## 14           sysBP  0.20817830 0.20817830       1
## 12    prevalentHyp  0.17862412 0.17862412       1
## 7            diaBP  0.13598063 0.13598063      NA
## 9          glucose  0.12590588 0.12590588       1
## 15         totChol  0.10111464 0.10111464       1
## 3           BPMeds  0.09431449 0.09431449       1
## 11            male  0.08568021 0.08568021       1
## 2              BMI  0.08276951 0.08276951       1
## 13 prevalentStroke  0.07857814 0.07857814       1
## 6         diabetes  0.07785838 0.07785838       1
## 4       cigsPerDay  0.04663366 0.04663366       1
## 5    currentSmoker  0.01256042 0.01256042      NA
## 10       heartRate  0.00767770 0.00767770       1
## 8        education -0.05893901 0.05893901       1
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Add dummy model - random variable ?
ret_lst <- myrun_mdl_fn(indep_vars_vctr=".rnorm",
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
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
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5743  -0.5743  -0.5743  -0.5743   1.9410  
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.71875    0.05305   -32.4   <2e-16 ***
## .rnorm            NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2349.3  on 2755  degrees of freedom
## Residual deviance: 2349.3  on 2755  degrees of freedom
## AIC: 2351.3
## 
## Number of Fisher Scoring iterations: 3
## 
##    feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit
## 1 .rnorm  2756       NA       NA           NA 21377.89      NA 2351.311
##   auc.fit auc.OOB
## 1     0.5     0.5
```

```r
glb_dmy_mdl <- glb_mdl

# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0410  -0.6247  -0.4722  -0.3669   2.4561  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -5.602447   0.354515  -15.80   <2e-16 ***
## age          0.075325   0.006566   11.47   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2349.3  on 2755  degrees of freedom
## Residual deviance: 2207.5  on 2754  degrees of freedom
## AIC: 2211.5
## 
## Number of Fisher Scoring iterations: 5
## 
##    feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit
## 2    age  2756       NA       NA           NA 26545.63      NA 2211.487
## 1 .rnorm  2756       NA       NA           NA 21377.89      NA 2351.311
##     auc.fit   auc.OOB
## 2 0.6792565 0.6796646
## 1 0.5000000 0.5000000
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, 
              subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                        glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df)    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4913  -0.6108  -0.4612  -0.3450   2.5547  
## 
## Coefficients:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -5.640e+00  3.715e-01 -15.180  < 2e-16 ***
## age                4.093e-02  1.032e-02   3.966 7.31e-05 ***
## age:diaBP          3.732e-04  8.083e-05   4.617 3.89e-06 ***
## age:currentSmoker  7.558e-03  2.103e-03   3.594 0.000326 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2349.3  on 2755  degrees of freedom
## Residual deviance: 2175.1  on 2752  degrees of freedom
## AIC: 2183.1
## 
## Number of Fisher Scoring iterations: 5
## 
##                               feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit
## 3 age, age:diaBP, age:currentSmoker  2756       NA       NA           NA
## 2                               age  2756       NA       NA           NA
## 1                            .rnorm  2756       NA       NA           NA
##    SSE.fit SSE.OOB  AIC.fit   auc.fit   auc.OOB
## 3 27279.62      NA 2183.120 0.6978369 0.7127191
## 2 26545.63      NA 2211.487 0.6792565 0.6796646
## 1 21377.89      NA 2351.311 0.5000000 0.5000000
```

```r
# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, 
                                               cor.low == 1)[, "id"],
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8230  -0.6028  -0.4243  -0.2814   2.8515  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -7.972108   0.849155  -9.388  < 2e-16 ***
## age              0.062723   0.008178   7.669 1.73e-14 ***
## sysBP            0.009704   0.003572   2.716 0.006601 ** 
## prevalentHyp     0.297319   0.169248   1.757 0.078967 .  
## glucose          0.009007   0.002834   3.179 0.001480 ** 
## totChol          0.003823   0.001377   2.777 0.005489 ** 
## BPMeds           0.316942   0.286782   1.105 0.269087    
## male             0.519824   0.134655   3.860 0.000113 ***
## BMI              0.007543   0.015598   0.484 0.628678    
## prevalentStroke  1.150633   0.570227   2.018 0.043607 *  
## diabetes        -0.416985   0.407669  -1.023 0.306378    
## cigsPerDay       0.017731   0.005355   3.311 0.000929 ***
## heartRate       -0.008218   0.005308  -1.548 0.121532    
## education       -0.061393   0.062211  -0.987 0.323716    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2020.7  on 2384  degrees of freedom
## Residual deviance: 1792.8  on 2371  degrees of freedom
##   (371 observations deleted due to missingness)
## AIC: 1820.8
## 
## Number of Fisher Scoring iterations: 5
## 
##                                                                                                                        feats
## 4 age, sysBP, prevalentHyp, glucose, totChol, BPMeds, male, BMI, prevalentStroke, diabetes, cigsPerDay, heartRate, education
## 3                                                                                          age, age:diaBP, age:currentSmoker
## 2                                                                                                                        age
## 1                                                                                                                     .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 4  2756       NA       NA           NA 33850.62      NA 1820.835 0.7347683
## 3  2756       NA       NA           NA 27279.62      NA 2183.120 0.6978369
## 2  2756       NA       NA           NA 26545.63      NA 2211.487 0.6792565
## 1  2756       NA       NA           NA 21377.89      NA 2351.311 0.5000000
##     auc.OOB
## 4 0.7416772
## 3 0.7127191
## 2 0.6796646
## 1 0.5000000
```

```r
# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(setdiff(names(glb_entity_df),
                                                        glb_predct_var),
                                                glb_exclude_vars_as_features),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8487  -0.6007  -0.4257  -0.2842   2.8369  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -7.886574   0.890729  -8.854  < 2e-16 ***
## male             0.528457   0.135443   3.902 9.55e-05 ***
## age              0.062055   0.008343   7.438 1.02e-13 ***
## education       -0.058923   0.062430  -0.944  0.34525    
## currentSmoker    0.093240   0.194008   0.481  0.63080    
## cigsPerDay       0.015008   0.007826   1.918  0.05514 .  
## BPMeds           0.311221   0.287408   1.083  0.27887    
## prevalentStroke  1.165794   0.571215   2.041  0.04126 *  
## prevalentHyp     0.315818   0.171765   1.839  0.06596 .  
## diabetes        -0.421494   0.407990  -1.033  0.30156    
## totChol          0.003835   0.001377   2.786  0.00533 ** 
## sysBP            0.011344   0.004566   2.485  0.01297 *  
## diaBP           -0.004740   0.008001  -0.592  0.55353    
## BMI              0.010723   0.016157   0.664  0.50689    
## heartRate       -0.008099   0.005313  -1.524  0.12739    
## glucose          0.008935   0.002836   3.150  0.00163 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2020.7  on 2384  degrees of freedom
## Residual deviance: 1792.3  on 2369  degrees of freedom
##   (371 observations deleted due to missingness)
## AIC: 1824.3
## 
## Number of Fisher Scoring iterations: 5
## 
##                                                                                                                                              feats
## 5 male, age, education, currentSmoker, cigsPerDay, BPMeds, prevalentStroke, prevalentHyp, diabetes, totChol, sysBP, diaBP, BMI, heartRate, glucose
## 4                       age, sysBP, prevalentHyp, glucose, totChol, BPMeds, male, BMI, prevalentStroke, diabetes, cigsPerDay, heartRate, education
## 3                                                                                                                age, age:diaBP, age:currentSmoker
## 2                                                                                                                                              age
## 1                                                                                                                                           .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 5  2756       NA       NA           NA 33049.46      NA 1824.251 0.7350117
## 4  2756       NA       NA           NA 33850.62      NA 1820.835 0.7347683
## 3  2756       NA       NA           NA 27279.62      NA 2183.120 0.6978369
## 2  2756       NA       NA           NA 26545.63      NA 2211.487 0.6792565
## 1  2756       NA       NA           NA 21377.89      NA 2351.311 0.5000000
##     auc.OOB
## 5 0.7421095
## 4 0.7416772
## 3 0.7127191
## 2 0.6796646
## 1 0.5000000
```

```r
glb_sel_mdl <- glb_mdl

# User specified
# ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("<feat1_name>", "<feat2_name>"),
#                         glb_predct_var, glb_predct_var_name,
#                         fit_df=glb_entity_df, OOB_df=glb_newent_df)

# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
if (glb_is_regression)
    print(myplot_scatter(plot_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))

if (glb_is_classification) {
    # Lower AIC is better
    plot_models_df[, "inv.AIC.fit"] <- 1.0 / plot_models_df[, "AIC.fit"] 
    print(myplot_scatter(plot_models_df, "inv.AIC.fit", "auc.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}
```

![](Fram_X_all_files/figure-html/run_models-1.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
print(mdl_feats_df <- myextract_mdl_feats(glb_sel_mdl, glb_entity_df))
```

```
##                     Estimate  Std. Error    z value         Pr.z
## age              0.062055171 0.008342644  7.4383100 1.019815e-13
## male             0.528456611 0.135442809  3.9016956 9.552123e-05
## glucose          0.008934502 0.002836199  3.1501677 1.631767e-03
## totChol          0.003835404 0.001376544  2.7862558 5.332077e-03
## sysBP            0.011344170 0.004565868  2.4845591 1.297120e-02
## prevalentStroke  1.165794355 0.571214588  2.0409044 4.126033e-02
## cigsPerDay       0.015008176 0.007825929  1.9177501 5.514271e-02
## prevalentHyp     0.315817835 0.171764681  1.8386657 6.596437e-02
## heartRate       -0.008099344 0.005312864 -1.5244778 1.273894e-01
## BPMeds           0.311220755 0.287408088  1.0828532 2.788736e-01
## diabetes        -0.421493888 0.407990411 -1.0330975 3.015583e-01
## education       -0.058923451 0.062429747 -0.9438361 3.452534e-01
## BMI              0.010723329 0.016157214  0.6636868 5.068908e-01
## diaBP           -0.004740114 0.008000514 -0.5924762 5.535317e-01
## currentSmoker    0.093239650 0.194008031  0.4805969 6.308030e-01
##                              id fit.feat
## age                         age     TRUE
## male                       male     TRUE
## glucose                 glucose     TRUE
## totChol                 totChol     TRUE
## sysBP                     sysBP     TRUE
## prevalentStroke prevalentStroke     TRUE
## cigsPerDay           cigsPerDay     TRUE
## prevalentHyp       prevalentHyp     TRUE
## heartRate             heartRate     TRUE
## BPMeds                   BPMeds     TRUE
## diabetes               diabetes     TRUE
## education             education     TRUE
## BMI                         BMI     TRUE
## diaBP                     diaBP     TRUE
## currentSmoker     currentSmoker     TRUE
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_entity_df[, glb_predct_var_name] - glb_entity_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_entity_df)))                             
}    

if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl        
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_entity_df, type="response")

    require(ROCR)
    ROCRpred <- prediction(glb_entity_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_entity_df[, glb_predct_var])
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))
    
    # 0 & 1 does not generate outcomes for certain categories
    thresholds_df <- data.frame(threshold=seq(0.1, 0.9, 0.1))
    thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix) 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     thresholds_df[row_ix, "threshold"], 
                                     glb_predct_var, glb_predct_var_name))
    print(thresholds_df)
    print(myplot_line(thresholds_df, "threshold", "f.score"))
    
    glb_clf_proba_threshold <- thresholds_df[which.max(thresholds_df$f.score), 
                                             "threshold"]
    # This should change to maximize f.score.OOB ???
    print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.fit",
                  glb_clf_proba_threshold))

    glb_entity_df[, glb_predct_var_name] <- 
        (glb_entity_df[, paste0(glb_predct_var_name, ".proba")] >= 
             glb_clf_proba_threshold) * 1.0
    print(mycreate_xtab(glb_entity_df, c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     glb_clf_proba_threshold, 
                                     glb_predct_var, glb_predct_var_name)))    
}    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8487  -0.6007  -0.4257  -0.2842   2.8369  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -7.886574   0.890729  -8.854  < 2e-16 ***
## age              0.062055   0.008343   7.438 1.02e-13 ***
## male             0.528457   0.135443   3.902 9.55e-05 ***
## glucose          0.008935   0.002836   3.150  0.00163 ** 
## totChol          0.003835   0.001377   2.786  0.00533 ** 
## sysBP            0.011344   0.004566   2.485  0.01297 *  
## prevalentStroke  1.165794   0.571215   2.041  0.04126 *  
## cigsPerDay       0.015008   0.007826   1.918  0.05514 .  
## prevalentHyp     0.315818   0.171765   1.839  0.06596 .  
## heartRate       -0.008099   0.005313  -1.524  0.12739    
## BPMeds           0.311221   0.287408   1.083  0.27887    
## diabetes        -0.421494   0.407990  -1.033  0.30156    
## education       -0.058923   0.062430  -0.944  0.34525    
## BMI              0.010723   0.016157   0.664  0.50689    
## diaBP           -0.004740   0.008001  -0.592  0.55353    
## currentSmoker    0.093240   0.194008   0.481  0.63080    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2020.7  on 2384  degrees of freedom
## Residual deviance: 1792.3  on 2369  degrees of freedom
##   (371 observations deleted due to missingness)
## AIC: 1824.3
## 
## Number of Fisher Scoring iterations: 5
## 
##                                                                                                                                              feats
## 5 male, age, education, currentSmoker, cigsPerDay, BPMeds, prevalentStroke, prevalentHyp, diabetes, totChol, sysBP, diaBP, BMI, heartRate, glucose
## 4                       age, sysBP, prevalentHyp, glucose, totChol, BPMeds, male, BMI, prevalentStroke, diabetes, cigsPerDay, heartRate, education
## 3                                                                                                                age, age:diaBP, age:currentSmoker
## 2                                                                                                                                              age
## 1                                                                                                                                           .rnorm
## 6 age, male, glucose, totChol, sysBP, prevalentStroke, cigsPerDay, prevalentHyp, heartRate, BPMeds, diabetes, education, BMI, diaBP, currentSmoker
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 5  2756       NA       NA           NA 33049.46      NA 1824.251 0.7350117
## 4  2756       NA       NA           NA 33850.62      NA 1820.835 0.7347683
## 3  2756       NA       NA           NA 27279.62      NA 2183.120 0.6978369
## 2  2756       NA       NA           NA 26545.63      NA 2211.487 0.6792565
## 1  2756       NA       NA           NA 21377.89      NA 2351.311 0.5000000
## 6  2756       NA       NA           NA 33049.46      NA 1824.251 0.7350117
##     auc.OOB
## 5 0.7421095
## 4 0.7416772
## 3 0.7127191
## 2 0.6796646
## 1 0.5000000
## 6        NA
```

![](Fram_X_all_files/figure-html/fit_training.all-1.png) 

```
##   threshold    f.score
## 1       0.1 0.35660704
## 2       0.2 0.38429752
## 3       0.3 0.31177446
## 4       0.4 0.19870410
## 5       0.5 0.12787724
## 6       0.6 0.06951872
## 7       0.7 0.02739726
## 8       0.8 0.01652893
## 9       0.9 0.01108033
```

![](Fram_X_all_files/figure-html/fit_training.all-2.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   TenYearCHD TenYearCHD.predict.0 TenYearCHD.predict.1
## 1          0                 1603                  423
## 2          1                  173                  186
##   TenYearCHD.predict.NA
## 1                   311
## 2                    60
## [1] "f.score=0.3843"
```

```r
print(glb_feats_df <- mymerge_feats_Pr.z(glb_feats_df, glb_sel_mdl, glb_entity_df))
```

```
##                 id       cor.y  cor.y.abs cor.low         Pr.z
## 1              age  0.22630732 0.22630732       1 1.019815e-13
## 11            male  0.08568021 0.08568021       1 9.552123e-05
## 9          glucose  0.12590588 0.12590588       1 1.631767e-03
## 15         totChol  0.10111464 0.10111464       1 5.332077e-03
## 14           sysBP  0.20817830 0.20817830       1 1.297120e-02
## 13 prevalentStroke  0.07857814 0.07857814       1 4.126033e-02
## 4       cigsPerDay  0.04663366 0.04663366       1 5.514271e-02
## 12    prevalentHyp  0.17862412 0.17862412       1 6.596437e-02
## 10       heartRate  0.00767770 0.00767770       1 1.273894e-01
## 3           BPMeds  0.09431449 0.09431449       1 2.788736e-01
## 6         diabetes  0.07785838 0.07785838       1 3.015583e-01
## 8        education -0.05893901 0.05893901       1 3.452534e-01
## 2              BMI  0.08276951 0.08276951       1 5.068908e-01
## 7            diaBP  0.13598063 0.13598063      NA 5.535317e-01
## 5    currentSmoker  0.01256042 0.01256042      NA 6.308030e-01
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1],
                    glb_predct_var, glb_predct_var_name)
#               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1,2] is a factor                                                         
#               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
              )
    }    
    
    if (glb_is_classification) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_classification(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"),
                                               plot_vars_df$id[1],
                    glb_predct_var, glb_predct_var_name)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(glb_entity_df)
```

```
## Warning: Removed 371 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/fit_training.all-3.png) 

```
## Warning: Removed 371 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/fit_training.all-4.png) 

```
## Warning: Removed 240 rows containing missing values (geom_point).
```

```
## Warning: Removed 371 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/fit_training.all-5.png) 

```
## Warning: Removed 30 rows containing missing values (geom_point).
```

```
## Warning: Removed 371 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/fit_training.all-6.png) 

```
## Warning: Removed 371 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/fit_training.all-7.png) 

```
## Warning: Removed 371 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/fit_training.all-8.png) 

```
## Warning: Removed 18 rows containing missing values (geom_point).
```

```
## Warning: Removed 371 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/fit_training.all-9.png) 

```
## Warning: Removed 371 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/fit_training.all-10.png) 

```
## Warning: Removed 371 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/fit_training.all-11.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression)
    glb_newent_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_newent_df, type="response")

if (glb_is_classification) {
    glb_newent_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, glb_predct_var_name] <- 
        (predict(glb_sel_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0

    # Compute dmy model predictions
    glb_newent_df[, paste0(glb_predct_var, ".preddmy.proba")] <- 
        predict(glb_dmy_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, paste0(glb_predct_var, ".preddmy")] <- 
        (predict(glb_dmy_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0

    # Sync w/ lecture
    glb_newent_df[, paste0(glb_predct_var_name, ".threshold.5")] <- 
        (predict(glb_sel_mdl, newdata=glb_newent_df, type="response") > 
            0.5) * 1.0
    glb_newent_df[, paste0(glb_predct_var, ".preddmy.threshold.5")] <- 
        (predict(glb_dmy_mdl, newdata=glb_newent_df, type="response") > 
            0.5) * 1.0
}
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```r
myprint_df(glb_newent_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##    TenYearCHD TenYearCHD.predict
## 2           0                  0
## 5           0                  0
## 9           0                  1
## 14          0                  0
## 17          0                  0
## 19          0                  0
##      TenYearCHD TenYearCHD.predict
## 971           1                  0
## 1204          1                  1
## 2886          0                 NA
## 3161          0                 NA
## 3266          0                  0
## 3511          0                  1
##      TenYearCHD TenYearCHD.predict
## 4223          0                  1
## 4225          0                  0
## 4228          0                  0
## 4229          0                  1
## 4233          1                  1
## 4236          0                 NA
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2) / nrow(glb_newent_df)) ^ 0.5))                        
    print(myplot_scatter(glb_newent_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
                         
    glb_newent_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_newent_df[, glb_predct_var_name] - glb_newent_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_newent_df)))                                                      

#     glb_newent_df[, "<Output Pred variable>"] <- func(glb_newent_df[, glb_pred_var_name])                         
}                         

if (glb_is_classification) {
    ROCRpred <- prediction(glb_newent_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_newent_df[, glb_predct_var])
    print(sprintf("auc=%0.4f", auc <- as.numeric(performance(ROCRpred, "auc")@y.values)))
    
    print(sprintf("probability threshold=%0.4f", glb_clf_proba_threshold))
    print(mycreate_xtab(glb_newent_df, c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score.sel=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_sel_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=glb_predct_var_name)))
    
    print(mycreate_xtab(glb_newent_df, c(glb_predct_var, paste0(glb_predct_var, ".preddmy"))))
    print(sprintf("f.score.dmy=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_dmy_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=paste0(glb_predct_var, ".preddmy"))))
    
    print("sync with lecture")
    print(sprintf("probability threshold=%0.4f", 0.5))
    print(mycreate_xtab(glb_newent_df, 
                        c(glb_predct_var, "TenYearCHD.predict.threshold.5")))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_newent_df, 0.5, 
                                     glb_predct_var, glb_predct_var_name)))    
}    
```

```
## [1] "auc=0.7421"
## [1] "probability threshold=0.2000"
##   TenYearCHD TenYearCHD.predict.0 TenYearCHD.predict.1
## 1          0                  865                  210
## 2          1                   88                  110
##   TenYearCHD.predict.NA
## 1                   184
## 2                    27
## [1] "f.score.sel=0.4247"
##   TenYearCHD TenYearCHD.preddmy.0
## 1          0                 1259
## 2          1                  225
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## [1] "f.score.dmy=0.0000"
## [1] "sync with lecture"
## [1] "probability threshold=0.5000"
##   TenYearCHD TenYearCHD.predict.threshold.5.0
## 1          0                             1069
## 2          1                              187
##   TenYearCHD.predict.threshold.5.1 TenYearCHD.predict.threshold.5.NA
## 1                                6                               184
## 2                               11                                27
## [1] "f.score=0.1023"
```

```r
glb_analytics_diag_plots(glb_newent_df)
```

```
## Warning: Removed 211 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/predict_newdata-1.png) 

```
## Warning: Removed 211 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/predict_newdata-2.png) 

```
## Warning: Removed 148 rows containing missing values (geom_point).
```

```
## Warning: Removed 211 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/predict_newdata-3.png) 

```
## Warning: Removed 20 rows containing missing values (geom_point).
```

```
## Warning: Removed 211 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/predict_newdata-4.png) 

```
## Warning: Removed 211 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/predict_newdata-5.png) 

```
## Warning: Removed 211 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/predict_newdata-6.png) 

```
## Warning: Removed 11 rows containing missing values (geom_point).
```

```
## Warning: Removed 211 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/predict_newdata-7.png) 

```
## Warning: Removed 211 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/predict_newdata-8.png) 

```
## Warning: Removed 211 rows containing missing values (geom_point).
```

![](Fram_X_all_files/figure-html/predict_newdata-9.png) 

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.2 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] ROCR_1.0-6      gplots_2.16.0   mgcv_1.8-4      nlme_3.1-120   
##  [5] reshape2_1.4.1  plyr_1.8.1      caTools_1.17.1  doBy_4.5-13    
##  [9] survival_2.38-1 ggplot2_1.0.1  
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6       colorspace_1.2-6   digest_0.6.8      
##  [4] evaluate_0.5.5     formatR_1.0        gdata_2.13.3      
##  [7] grid_3.1.3         gtable_0.1.2       gtools_3.4.1      
## [10] htmltools_0.2.6    KernSmooth_2.23-14 knitr_1.9         
## [13] labeling_0.3       lattice_0.20-30    MASS_7.3-39       
## [16] Matrix_1.1-5       munsell_0.4.2      proto_0.3-10      
## [19] Rcpp_0.11.5        rmarkdown_0.5.1    scales_0.2.4      
## [22] splines_3.1.3      stringr_0.6.2      tools_3.1.3       
## [25] yaml_2.1.13
```
