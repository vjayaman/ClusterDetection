install.packages("tibble")
install.packages("magrittr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")
install.packages("plotly")
install.packages("tidyverse")
install.packages("DT")
install.packages("ggpubr")
install.packages("rcompanion")
install.packages("fitdistrplus")
install.packages("logspline")
install.packages("gamlss")
install.packages("goftest")
install.packages("kableExtra")
install.packages("EnvStats")

library(magrittr); library(tidyverse); library(dplyr)

# Replace the following `initial_files` if necessary.
initial_files <- list.files(pattern = "-dm")

# Given the folders with files containing cluster and tree data, this section rearranges the data to output two files:
#   (1) a file of thresholds with heights of form "h_x" (x a number) 
#   (2) a lookup table of new height names paired with the original 

for (x in initial_files) {
  
  cl_file <- list.files(x, pattern = "clusters")
  file_no <- strsplit(x, split = "_tree") %>% unlist() %>% extract2(1)
  
  TPx <- paste0(x, "/", cl_file) %>%
    read.csv(file = ., numerals = "no.loss", stringsAsFactors = FALSE) %>% as_tibble()
  
  heights_x <- colnames(TPx)[2:ncol(TPx)]
  heights_x <- lapply(heights_x, function(h) gsub("X", "", h)) %>% unlist()
  
  lookup_table <- lapply(2:ncol(TPx), function(j) TPx[,j] %>% unlist() %>% unique() %>% length()) %>%
    unlist() %>% rev() %>% tibble(., heights_x) %>% set_colnames(c("New_name", "Old_name"))
  
  colnames(TPx)[2:ncol(TPx)] <- paste0("h_", lookup_table$New_name)
  
  paste0(x, "/", file_no, "_thresholds.csv") %>% write.csv(TPx, file = ., row.names = FALSE)
  paste0(x, "/", file_no, "_lookup_table.csv") %>% write.csv(lookup_table, file = ., row.names = FALSE)
}

print("Done.")