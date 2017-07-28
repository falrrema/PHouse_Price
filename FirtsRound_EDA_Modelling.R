#################
# Preprocessing #
#################
setwd("~/Dropbox/ProyectosDS/PHouse_Price")
Sys.setlocale(locale = "es_ES.UTF-8") # Para visualizar caracteres especiales
source("helper.R")
library(plotly)
library(mlr)
library(skimr)
library(forcats)
loadThesePackages()

train <- fread("data/train.csv")
test <- fread("data/test.csv")

# EDA ---------------------------------------------------------------------
glimpse(train)

# Missing Value evaluation
visualize_NA(train) # PoolQC, MiscFeature, Alley, Fence
train <- removeColumns_NA(data = train, upperBound = 80) # Removing high percent NA columns (> 80%)
# train[is.na(train)] <- "NO VAL" # Creating a category for NA values

# Transformations
stats <- skim(train)
colcat <- stats %>% filter(type == "character") %$% unique(var) # get character columns

train %>% select(colcat) %>% # Counts of levels by each categorical column
    gather(col, levels) %>% 
    count(col, levels) %>% 
    ggplot(aes(x = levels, y = n)) + geom_bar(stat = "identity", fill = "dodgerblue") + 
    facet_wrap(~col, scales = "free")

train$Neighborhood %>% unique() 
train$Exterior1st %>% unique() 
train$Exterior2nd %>% unique() 

sales_by_factor <- train %>% # lets analyze the contribution of levels of categories in sales price
    select(SalePrice, colcat) %>% 
    mutate(logSalesPrice = log10(SalePrice + 1)) %>% # log transformation of sales price
    gather(col, levels, MSZoning:SaleCondition)

sales_by_factor %>% 
    ggplot(aes(x = levels, y = logSalesPrice, fill = col)) + 
    geom_boxplot() + theme(legend.position = "none") + 
    facet_wrap(~col, scales = "free") 

# Apply Anova test to determine if there is a significant difference in Sales Price within each levels of categorical variables
anSales <- sales_by_factor %>% 
    split(.$col) %>% # divide de DF by categorical variables
    map(~ aov(logSalesPrice ~ levels, data = .x)) %>% # apply to each split an anova test
    map(~broom::tidy(.x)) # pretty print with broom

colEliminate <- names(anSales[sapply(anSales, function(t) t$p.value[1] >= 0.05)]) # Yes there are!

train %>% select(c(colEliminate)) %>% # These variables show extremely unbalanced levels
    gather(col, levels) %>% 
    count(col, levels) %>% 
    ggplot(aes(x = levels, y = n)) + geom_bar(stat = "identity", fill = "dodgerblue") + 
    facet_wrap(~col, scales = "free")

sales_by_factor %>% # There is no big difference
    filter(col %in% colEliminate) %>% 
    ggplot(aes(x = levels, y = logSalesPrice, fill = col)) + 
    geom_boxplot() + theme(legend.position = "none") + 
    facet_wrap(~col, scales = "free") 

tukSales <- sales_by_factor %>% 
    filter(!col %in% colEliminate) %>% 
    split(.$col) %>% # divide de DF by categorical variables
    map(~ aov(logSalesPrice ~ levels, data = .x)) %>%
    map(~TukeyHSD(.x)) %>% 
    map(~broom::tidy(.x)) # pretty print with broom



fuse <- tukSales$BldgType %>% mutate(significant = adj.p.value < 0.05) %>%
    filter(significant == FALSE) %>% 
    separate(comparison, c("V1", "V2")) %>% 
    mutate(term = paste0(term, 1:length(term))) %>% 
    select(term, V1, V2) %>% 
    mutate_at(vars(V1:V2), funs(factor)) %>% 
    createDummyFeatures()
    


fuse <- tukSales$BldgType %>% mutate(significant = adj.p.value < 0.05) %>%
    filter(significant == FALSE) %>% 
    separate(comparison, c("V1", "V2")) %>% 
    mutate(term = paste0(term, 1:length(term))) %>% 
    select(term, V1, V2) %>% 
    gather(col, cat, V1:V2) %>% 
    mutate(cat = factor(cat)) %>% 
    createDummyFeatures()

str_char_split <- function(x, pat = "-") {
    stringr::str_split(x, pattern = pat, simplify = TRUE)
}

fuse <- tukSales$Condition1 %>% mutate(significant = adj.p.value < 0.05) %>%
    filter(significant == FALSE) %>% 
    select(term, comparison) %>% 
    mutate(V1 = sapply(comparison, function(t) str_char_split(t)[[1]]),
           V2 = sapply(comparison, function(t) str_char_split(t)[[2]]))

v1 <- unique(fuse$V1) %>% paste0(., "-")
v2 <- unique(fuse$V2) %>% paste0("-", .)

v1 <- lapply(v1, function(t) {
    p1 <- fuse$comparison[grepl(t, fuse$comparison)]
})

v2 <- lapply(v2, function(t) {
    p1 <- fuse$comparison[grepl(t, fuse$comparison)]
})

var <- mapply(function(x, y) sort(unique(c(str_char_split(x), str_char_split(y)))), v1, v2) 
groupVar <- var[!duplicated(var)]




m <- melt(train)
glimpse(sales_by_factor)
