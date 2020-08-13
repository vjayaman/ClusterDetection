# Useful functions for data reading and table processing, used in 
# cluster_sizing.Rmd and in the shiny app

x <- c("tibble", "magrittr", "dplyr", "ggplot2", "reshape2","plotly", "tidyverse", 
       "DT", "ggpubr", "rcompanion", "fitdistrplus", "logspline", "gamlss",
       "goftest", "kableExtra", "EnvStats", "pracma", "e1071", "kernlab", "caret", 
       "LiblineaR", "gridExtra", "transformr", "gifski", "gganimate", "fitdistrplus", 
       "flexdashboard")
lapply(x, require, character.only = TRUE)

get <- .Primitive("[[")

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
  # note, first column of "clusters" is the column of genome labels
  sizes <- lapply(2:ncol(clusters), function(h) {
    if (h %% 200 == 0) {
      h %>% paste0(., "/", ncol(clusters)-1) %>% print()
    }
    clusters %>% pull(h) %>% table() %>% 
      as.data.frame() %>% set_colnames(c("cluster", colnames(clusters)[h])) %>% 
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

boxplotSubsetPoints <- function(df) {
  p <- ggplot(df, aes(x = heights, y = size_difference)) + geom_boxplot()
  p_data <- ggplot_build(p) %>% extract2("data")
  p_points <- tibble(heights = df$heights %>% unique(), 
                     median = p_data[[1]]$middle, q3 = p_data[[1]]$upper)
  return(p_points)
}

boxplotSubset <- function(df, title_sec) {
  p_points <- boxplotSubsetPoints(df)
  
  ggplot(df, aes(x = heights, y = size_difference)) + 
    geom_boxplot() + 
    geom_point(data = p_points, aes(x = heights, y = median, color = "red")) + 
    geom_point(data = p_points, aes(x = heights, y = q3, color = "blue")) +
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
    scale_color_manual(name = "Point color", values = c("red"="red","blue"="blue"), 
                       labels = c("Quantile 3", "Median")) +
    xlab("Heights") + ylab("Change in cluster size") + 
    ggtitle(paste0("Change in cluster across thresholds", title_sec)) %>% return()
}

# clusterMatching <- function(df1, df2, dfx, cluster_x) {
#   cluster_x_isolates <- df1 %>% 
#     filter(TP1 == cluster_x) %>% pull(Isolates)
#   a <- dfx[dfx$Clusters==cluster_x,] %>% 
#     set_colnames(c("TP1","TP1-size"))
#   dfy <- df2 %>% filter(Isolates %in% cluster_x_isolates) %>% 
#     pull(TP2) %>% table() %>% as.data.frame() %>% 
#     as_tibble() %>% set_colnames(c("TP2","TP2-size"))
#   
#   x <- bind_cols(a, dfy) %>% as_tibble()
#   x %<>% bind_cols(., Change_in_size = x$`TP2-size` - x$`TP1-size`) %>% 
#     return()
# }
# 
# all_heights <- intersect(colnames(timepoint1), colnames(timepoint2))[-1]
# # note, some heights that don't match, when comparing
# # time points 1 and 2
# for (h in all_heights) {
#   print(h)
#   df1 <- timepoint1 %>% dplyr::select(isolate, all_of(h)) %>% 
#     set_colnames(c("Isolates", "TP1"))  
#   df2 <- timepoint2 %>% dplyr::select(isolate, all_of(h)) %>% 
#     set_colnames(c("Isolates", "TP2"))
#   
#   dfx <- df1$TP1 %>% table() %>% as.data.frame() %>% 
#     set_colnames(c("Clusters","Freq")) %>% as_tibble()
#   dfx$Clusters %<>% as.character() %>% as.numeric()
#   
#   b <- lapply(1:nrow(dfx), function(i) {
#     clusterMatching(df1, df2, dfx, dfx$Clusters[i])
#   }) %>% bind_rows() %>% 
#     bind_cols(Height = h, .)
#   
#   if (h == "h_0") {
#     full_dataset <- b
#   }else {
#     full_dataset <- bind_rows(full_dataset, b)  
#   }
# }
# 
# saveRDS(full_dataset, "cluster_matching.Rds")









