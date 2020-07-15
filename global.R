# Useful functions for data reading and table processing, used in 
# cluster_sizing.Rmd and in the shiny app

x <- c("tibble", "magrittr", "dplyr", "ggplot2", "reshape2","plotly", "tidyverse", 
       "DT", "ggpubr", "rcompanion", "fitdistrplus", "logspline", "gamlss",
       "goftest", "kableExtra", "EnvStats")
lapply(x, require, character.only = TRUE)

separateText <- function(txt, sp, i) {
  strsplit(txt, split = sp) %>% unlist() %>% extract2(i) %>% return()
}

# given the defining filename in:
# C:\Users\vasen\Documents\Pre-MSc courses\Honours Research Project\SummerProject-2020\, 
# read in the data
readData <- function(filename) {
  filename %>% 
    read.csv(file = ., numerals = "no.loss", stringsAsFactors = FALSE) %>% 
    as_tibble() %>% return()
}

# merge tables of 
# 1) isolates and cluster assignments at all heights, and
# 2) clusters and cluster sizes at all height
# output: a melted tibble
isolateClusterSize <- function(clusters, sizes) {
  
  df_clusters <- reshape2::melt(clusters, id.vars = "isolate") %>% 
    as_tibble() %>% set_colnames(c("isolate", "height", "cluster"))
  
  df_sizes <- reshape2::melt(sizes, id.vars = "cluster") %>% 
    as_tibble() %>% set_colnames(c("cluster", "height", "size"))
  
  df_both <- left_join(df_clusters, df_sizes, by = c("height", "cluster"))
  df_both$height <- as.character(df_both$height)
  
  return(df_both)
}

# returns tibble of clusters and their associated sizes at each height
clusterSizes <- function(clusters) {
  # note, first column of "clusters" is the column of isolate names
  sizes <- lapply(2:ncol(clusters), function(j) {
    if (j %% 200 == 0) {
      j %>% paste0(., "/", ncol(clusters)-1) %>% print()
    }
    clusters %>% pull(j) %>% table() %>% 
      as.data.frame() %>% set_colnames(c("cluster", colnames(clusters)[j])) %>% 
      as_tibble()
  }) %>% reduce(full_join, by = "cluster")
  
  sizes$cluster <- as.numeric(sizes$cluster)
  sizes <- sizes[order(sizes$cluster),]
  print("Done generating matrix of cluster sizes")
  return(sizes)
}

sparseCheck <- function(hist_x) {
  df <- hist_x[['counts']] %>% table() %>% as_tibble() %>% set_colnames(c("value","freq"))
  if (0 %in% df$value) {
    denom <- hist_x[['breaks']] %>% length()
    numerator <- df$freq[df$value == 0]
    sparse_val <- (numerator/denom) %>% scales::percent()
  }else {
    sparse_val <- "none"
  }
  return(sparse_val)
}



