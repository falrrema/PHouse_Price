
# EDA ---------------------------------------------------------------------
setwd("~/Dropbox/ProyectosDS/PHouse_Price")
source("helper.R")
library(mlr)
library(skimr)
library(tidyverse)
library(magrittr)
library(forcats)

train <- readr::read_csv("data/train.csv", col_types = cols())
test <- readr::read_csv("data/train.csv", col_types = cols())
names(train) <- make.names(names(train)) # making valid names for R (avoid names that start with numbers)
names(test) <- make.names(names(train)) # making valid names for R (avoid names that start with numbers)

glimpse(train)
(stats <- skim(train)) # Fecthing important stats

# Analyzing target variable: SalePrice ------------------------------------
# looking at SalePrice
train %>% gghist(SalePrice) # its right skewed
train %>% gghist(SalePrice, tlog = TRUE) # normalize by log transform
train$logSalePrice <- log1p(train$SalePrice)

# Missing Value Evaluation ------------------------------------------------
visualize_NA(train) # There are 5 variables that have over 20% of missing values

missing_cols <- table_NA(train) %>%  # table_na is a function that analyzes a dataframe and shows missingness statistics
    filter(percent > 0) %$% column # lets extract all columns that have missing values

stats %>% filter(var %in% missing_cols, level == ".all") %>% 
    spread(stat, value) %>% 
    arrange(desc(missing))

# Looking in detailed variables that have most of missing values
# PoolQc, MiscFeature, Fence, FireplaceQu are features that its NA means it doesn't have that characteristic
# Therefore, it means that most of our data set doesn't have houses that have one or more of these features
# since there are special I am going to collapse this variables to one, specialFeat as TRUE or FALSE
# and number of special features nSpecialFeat

train <- train %>% mutate(specialFeat = !is.na(PoolQC) | !is.na(MiscFeature) | !is.na(Fence) | !is.na(FireplaceQu),
                          nSpecialFeat = as.numeric(!is.na(PoolQC)) +
                                         as.numeric(!is.na(MiscFeature)) + 
                                         as.numeric(!is.na(Fence)) +
                                         as.numeric(!is.na(FireplaceQu)))
train %>% select(PoolQC, MiscFeature, Fence, FireplaceQu, specialFeat, nSpecialFeat) 

train %>% select(logSalePrice, specialFeat, nSpecialFeat) %>% 
    mutate(nSpecialFeat = factor(nSpecialFeat, levels = unique(nSpecialFeat), ordered = TRUE)) %>% 
    gather(variables, values, specialFeat, nSpecialFeat) %>% 
    ggplot(aes(x = values, y = logSalePrice, fill = variables)) +
    geom_boxplot() + theme(legend.position = "none") + 
    facet_wrap(~variables, scales = "free") 

# Alley NA value means it doesn't have an alley access, lets turn it into a logical and see it vs Sales
train %>% mutate(has_alley = !is.na(Alley)) %>% 
    ggplot(aes(x = has_alley, y = logSalePrice, fill = has_alley)) +
    geom_boxplot() + theme(legend.position = "none") # There seems to be a difference

# Old variables will be dropped, these include related ones like MiscVal, and PoolArea and keep new variables
train <- train %>% mutate(has_alley = !is.na(Alley)) %>% 
    select(-PoolQC, -MiscFeature, -Fence, -FireplaceQu, -Alley, -MiscVal, -PoolArea)

# Garage and Basement missing values just imply that they don't have this feature, then it will be recoded as None
colcat <- stats %>% filter(type == "character") %$% unique(var) # get character columns

recode_NA <- function(t) { # function to recode NA as None
    fct_explicit_na(t, na_level = "None")
}

train <- train %>% mutate_at(vars(colcat[grepl("^Bs|Ga", colcat)]), # select only columns starting with Bs or Ga
                    funs(recode_NA))

# lets look at GarageYrBlt and MasVnrType
mean(train$YearBuilt == train$GarageYrBlt, na.rm = TRUE) # 79% percent of the time the garage was built with the house

train %>% mutate(has_remodeled = ifelse(YearRemodAdd == YearBuilt, "FALSE", "TRUE")) %>% 
    filter(has_remodeled == TRUE) %>% 
    summarise(perGarage = mean(GarageYrBlt == YearRemodAdd, na.rm = TRUE)) # 8% percent of the time a house was remodeled it built a Garage

train <- train %>% 
    mutate(has_remodeled = ifelse(YearRemodAdd == YearBuilt, "FALSE", "TRUE"), # Will keep these new feature
           GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt), # Assuming that the garage was built with the house on missing values
           MasVnrType = ifelse(is.na(MasVnrType), "None", MasVnrType)) # Assuming Na is None

# The rest values will be imputed using a rpart learner
# eliminating Id because is not predictive and transforming all categorical and logical variables to factors
colcat <- skim(train) %>% filter(type %in% c("character", "logical")) %$% unique(var) # get character columns

imp <- train %>% select(-Id,) %>% 
    mutate_at(vars(colcat), funs(factor)) %>% data.frame %>% 
    impute(target = "logSalePrice", 
           cols = list(LotFrontage = imputeLearner("classif.rpart"),
                       MasVnrArea = imputeLearner("classif.rpart"),
                       Electrical = imputeLearner("classif.rpart")))

train$LotFrontage <- imp$data$LotFrontage
train$MasVnrArea <- imp$data$MasVnrArea
train$Electrical <- imp$data$Electrical

# Feature Engineering and transformation ----------------------------------






# Looking at categorical variables
train %>% select(colcat) %>% # Counts of levels by each categorical column
    gather(col, levels) %>% 
    count(col, levels) %>% 
    ggplot(aes(x = levels, y = n)) + geom_bar(stat = "identity", fill = "dodgerblue") + 
    facet_wrap(~col, scales = "free")

train %>% # lets analyze the contribution of levels of categories in sales price
    select(SalePrice, colcat) %>% 
    mutate(logSalesPrice = log10(SalePrice + 1)) %>% # log transformation of sales price
    gather(col, levels, MSZoning:SaleCondition) %>% 
    ggplot(aes(x = levels, y = logSalesPrice, fill = col)) + 
    geom_boxplot() + theme(legend.position = "none") + 
    facet_wrap(~col, scales = "free") 

# we will transforme both numeric and categorical variables
# Numerical
names(train)[grepl("^[0-9]", names(train))] <- c("FlrSF1st", "FlrSF2nd", "SsnPorch")
traintask <- makeRegrTask(id = "id", data = train, target = "logSalesPrice")
traintask

# Merging smallFactors variables
traintask <- mergeSmallFactorLevels(traintask, min.perc = 0.01)

getTaskData(traintask) %>% select(colcat) %>% # Counts of levels by each categorical column
    gather(col, levels) %>% 
    count(col, levels) %>% 
    ggplot(aes(x = levels, y = n)) + geom_bar(stat = "identity", fill = "dodgerblue") + 
    facet_wrap(~col, scales = "free")


  

