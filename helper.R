#########################
# Key functions for PNLP
########################

installThesePackages <- function() {
    list.of.packages <- c("tm", "SnowballC", "parallelMap", "pbapply", "dplyr", "data.table", "text2vec", 
                          "magrittr", "tidyr", "ggplot2", "dtplyr", "topicmodels", "tokenizers", "mlrMBO",
                          "cleanNLP", "tidytext", "pbmcapply", "stringdist", "textreuse", "tidyverse",
                          "DMR")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    if(!require(ParamHelpers)) devtools::install_github("berndbischl/ParamHelpers") 
    if(!require(mlrHyperopt)) devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)
}

mlrDependencies <- function() {
  cat("Go for a coffee this could take a while...")
  if(!require(mlr)) devtools::install_github("mlr-org/mlr", dependencies = c("Depends", "Imports", "Suggests"))
  if(!require(ParamHelpers)) devtools::install_github("berndbischl/ParamHelpers") 
  if(!require(mlrHyperopt)) devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)
}

loadThesePackages <- function() {
    lib <- list("tm", "SnowballC", "parallelMap", "pbapply", "text2vec", "data.table", "tidyverse",
             "magrittr", "dtplyr", "topicmodels", "tokenizers", "mlr", "mlrMBO", 
             "parallelMap", "mlrHyperopt", "cleanNLP", "tidytext", "pbmcapply")
    loading <- lapply(lib, require, character.only = TRUE)
}

# CleanText ---------------------------------------------------------------
# "removeMostPunctuation" is the function that allows flexible punctuation removal. Those puntuation marks you want to preserve
# are needed to be pass as a vector, for example, c("@", "#"). By default this value is NULL, which the functions then 
# goes back to base "removePunctuation()". 

removeMostPunctuation <- function (text, preserveWhich = NULL) { # The functionality is base in tagging, removing and detagging. 
    if (!is.null(preserveWhich)) {
        for (i in 1:length(preserveWhich)) {
            replacement <- paste("000", i, sep = "")
            text <- gsub(preserveWhich[i], replacement, text)
        }
        text <- removePunctuation(text)
        for (i in 1:length(preserveWhich)) {
            replacement <- paste("000", i, sep = "")
            text <- gsub(replacement, preserveWhich[i], text)
        }
    } else {
        text <- removePunctuation(text)
    }
    return(text)
}  

# "cleanText" is a function that:
# - eliminates accentuation and strange characters (emoticons).
# - converts every strings to lower case strings
# - it eliminates all puntuations if the parameter 'preservePunct' is NULL
# - Preserves puntuations if pass a vector of punctuations to the parameter 'preservePunct'
# - strips whitespace from the string
# - if 'columnNames' is TRUE, then formatting for column names is applied. This means strings more than one word are 
# capitalize, excepto the first one. Strings that have words of 3 or less characters are eliminated, but some are kept
# for better understanding of the variable (id, url, app, etc). 
# Sys.setlocale(locale="es_ES.UTF-8") # spanish reading format

cleanText <- function(whateverText, columnNames = F, removeNum = T, encode = T, lowercase = T, preservePunct = NULL, stemming = F, removeExtraWords = NULL) {
    # iconv() eliminates accentuation and strange characters. Accentuation are turned to apostrophes.
    if (encode == T) {
        # whateverText <- iconv(whateverText, to = "UTF-8")
        whateverText <- iconv(whateverText, "UTF-8", "ASCII//TRANSLIT", sub="") 
    }
    if (!is.null(preservePunct)) { 
        if (!"all" %in% preservePunct) { #  Calling 'removeMostPunctuation' function if not "all"
            whateverText <- removeMostPunctuation(whateverText, preserveWhich = preservePunct) 
        }
    } else {
        whateverText <- tm::removePunctuation(whateverText) 
    }
    if (stemming == T) {
        whateverText <- stemDocument(whateverText, language = "english")
    }
    if (lowercase == T) {
        whateverText <- tolower(whateverText) # lower case function
    }
    if (removeNum == T) {
        whateverText <- removeNumbers(whateverText) # remove numbers
    }
    if (!is.null(removeExtraWords)) {
        whateverText <- 
            tm::removeWords(whateverText, removeExtraWords)
    }
    whateverText <- stripWhitespace(whateverText) # Trim extra whitespace between.
    whateverText <- trimws(whateverText) # Trim extra whitespace at the beginning and the end.
    if (columnNames == T) { 
        whateverText <- sapply(whateverText, function(t) {
            str = unlist(strsplit(t, split = " "))
            if (length(str) >= 3) {
                isKeyword = str %in% c("app", "url", "id", "me")
                isSmall = !(nchar(str) <= 3)
                isNumber = grepl("[[:digit:]]",str)
                keywordOrSmallorNumber = (isKeyword | isSmall | isNumber)
                str = str[keywordOrSmallorNumber]
            }
            str = stringr::str_to_title(str)
            str[1] = tolower(str[1])
            str = paste0(str, collapse = "")
        })
    }
    return(whateverText)
}

# Empaquetamiento de CleanText
prep_fun <- function(text, stop_words = NULL) {
    text <- cleanText(text, removeNum = F, encode = F, stemming = T, removeExtraWords = stop_words)
    return(text)
}

# Missingness --------------------------------------------------------------
removeColumns_NA <- function(data, upperBound = 0) {
    missing_data <- table_NA(data)
    columns_for_elimination <- missing_data$column[which(missing_data$percent >= upperBound)] # column names
    columns_for_elimination <- which(names(data) %in% columns_for_elimination) # column numbers in the dataset
    data[, c(columns_for_elimination)] <- list(NULL)
    return(data)
}

# Barplot of % missingness for each column of the data set
visualize_NA <- function(data, percent.show = 0) {
    missing_data <- table_NA(data)
    missing_data$column <- factor(missing_data$column, levels = missing_data$column)
    
    p1 <-  plotly::plot_ly(missing_data, x = ~column, y = ~percent, type = "bar", marker = list(color = ~percent)) %>%
        plotly::layout(margin = list(b = 120))
    print(p1)
    return(p1)
} 

table_NA <- function(data) {
    data <- data.frame(data)
    names_column <- names(data)
    list_na <- unlist(lapply(names_column, function(t) sum(is.na(data[t]))))
    missing_data <- data.frame(column = names(data), number.na = list_na, row.names = NULL, stringsAsFactors = F)
    data_rows <- nrow(data)
    missing_data$percent <- missing_data$number.na/data_rows*100
    
    missingData_order <- missing_data[order(missing_data$number.na, decreasing = T),]
    return(missingData_order)
} 

# Visualization -----------------------------------------------------------
ggbar <- function(data, category, top = NULL, highcol = 'black', print = TRUE) {
    .category <- col_name(substitute(category))
    
    table <- data %>% data.frame %>% count_(.category) %>% 
        mutate(perc = round(n/sum(n)*100, 2))
    
    if(!is.null(top) & is.numeric(top)) {
        table <- head(table, top)
    } else if (!is.null(top) & !is.numeric(top)) {
        stop("ERROR en 'top': Ingrese un valor númerico")
    }
    
    p <- ggplot(table, aes(x = reorder(table[[.category]], n), y = n, fill = n)) + geom_bar(stat = 'identity') +
        coord_flip() + 
        scale_fill_gradient(low = "lightgrey", high = highcol) +
        labs(y = "FRECUENCIA", x = "") +
        theme(text = element_text(size = 16), legend.position = "none", 
              plot.margin = unit(c(1,2,1,1), "cm")) +
        geom_text(aes(label = scales::percent(perc/100)), hjust = -0.3, size = 3.5, fontface = "bold") +
        scale_x_discrete(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) 
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid::grid.draw(gt)
    
    return(table)
} 

ggpie <- function(data, category, color = "tableau20", sentido = 1, print = F) {
    .category <- col_name(substitute(category))
    if (length(unique(data[[.category]])) > 10) {
        message("Sobre 10 variables categóricas detectadas, para vizualizar mejor usa 'ggbar'")
    }
    
    table <- data %>% data.frame %>% count_(.category) %>% 
        mutate(perc = round(n/sum(n)*100, 2)) %>% 
        arrange(perc) 
    
    y <- table$perc
    
    breaks <- if (length(y) > 5) {
        (cumsum(y) - y/2)[length(y):(length(y) - 8)]
    } else {
        (cumsum(y) - y/2)[length(y):1]
    }
    
    ylabels <- if (length(y) > 5) {
        table$n[length(y):(length(y) - 8)]
    } else {
        table$n[length(y):1]
    }
    
    alpha <- if (length(y) > 5) {
        rev(c(rep(1, 5), rep(0, length(y) - 5)))
    } else {
        1
    }
    
    if (sentido == -1) {
        fill <- reorder(table[[.category]], y)
        cy <- (cumsum(rev(y)) - rev(y)/2)
        label <- rev(paste0(y, "%"))
        breaks <- if (length(y) > 5) {
            (cumsum(rev(y)) - rev(y)/2)[length(y):(length(y) - 8)]
        } else {
            (cumsum(rev(y)) - rev(y)/2)[length(y):1]
        }
        ylabels <- rev(ylabels)
    } else {
        fill <- reorder(table[[.category]], rev(y))
        cy <- (cumsum(y) - y/2)
        label <- paste0(y, "%")
    }
    
    p <- ggplot(table, aes(x=1, y = y, fill = fill)) + 
        geom_bar(stat = "identity", color = "black", lwd = 0.5) +
        coord_polar(theta = "y", direction = sentido) +
        ggthemes::scale_fill_tableau(color) +
        theme(axis.ticks=element_blank(),
              axis.title=element_blank(),
              axis.text.y=element_blank(),
              axis.text.x=element_text(color='black', face = "bold"),
              panel.grid.major.y=element_line(color = "black")) +
        scale_y_continuous(
            breaks = breaks,
            labels = ylabels) + 
        ggrepel::geom_text_repel(aes(y = cy, label = label), 
                                 alpha = alpha, size=4, color = "white", fontface = "bold")  +
        labs(fill = .category) +
        theme(legend.title = element_text(face = "bold"))
    
    if (print == T) print(p)
    
    return(p)
} 

gghist <- function(data, x, tlog = F, type = "count", xlab = NULL, bin = 50) {
    .x <- col_name(substitute(x))
    if (tlog == F) {
        xcol <- data[[.x]]
        xlab <- toupper(ifelse(is.null(xlab), .x, xlab))
        p <- ggplot(data, aes(x = xcol))
    } else {
        xcol <- log1p(data[[.x]])
        xlab <- toupper(ifelse(is.null(xlab), paste(.x, "(escala logaritmica)"), 
                               paste(xlab, "(escala logaritmica)")))
        p <-  ggplot(data, aes(x = log1p(data[[.x]])))
    }
    
    if (type == "count") {
        p <- p + geom_histogram(aes(y =..count..), bins = bin, colour = "black", fill = "dodgerblue", alpha = 0.7) +
            geom_vline(aes(xintercept  = mean(xcol, na.rm = T)), linetype = "dashed", size = 1.1, col = "red3") +
            geom_vline(aes(xintercept  = median(xcol, na.rm = T)), linetype ="dashed", size = 1.1, col = "black") +
            xlab(xlab) + ylab("FRECUENCIA")  
    } else if (type == "density") {
        p <- p + geom_histogram(aes(y =..density..), bins = bin, colour = "black", fill = "dodgerblue", alpha = 0.7) +
            geom_density(alpha = .4, colour = "black", size = 1, fill = "salmon") +
            geom_vline(aes(xintercept  = mean(xcol, na.rm = T)), linetype = "dashed", size = 1.1, col = "red3") +
            geom_vline(aes(xintercept  = median(xcol, na.rm = T)), linetype ="dashed", size = 1.1, col = "black") +
            xlab(xlab) + ylab("DENSIDAD")  
    }
    
    return(p)
}


# utils -------------------------------------------------------------------
# Para la entrega de columnas como objeto y no string
col_name <- function (x, default = stop("Please supply column name", call. = FALSE)) {
    if (is.character(x))
        return(x)
    if (identical(x, quote(expr = )))
        return(default)
    if (is.name(x))
        return(as.character(x))
    if (is.null(x))
        return(x)
    stop("Invalid column specification", call. = FALSE)
}

uncount <- function(data, countCol) {
    .countCol <- col_name(substitute(countCol))
    data$ids <- sapply(data[[.countCol]], seq_len)
    data <- tidyr::unnest(data)
    data[[.countCol]] <- data$ids <- NULL
    return(data)
}
