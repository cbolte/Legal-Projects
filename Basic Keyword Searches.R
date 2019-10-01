
library(tidyverse)
library(pdftools)
library(dplyr)
library(pdfsearch)



sort_pdfs_keyword <- function(keyword_str, starting_folder) {
  setwd(paste("~/Desktop/R/Legal_Projects/Keyword PDF Sorting Project/", starting_folder, sep= ""))
  folder_start <- paste("~/Desktop/R/Legal_Projects/Keyword PDF Sorting Project/", starting_folder, sep= "")
  #dir.create(folder_end)
  keywords <- keyword_str
  #large function that searches all pdfs in WD, then copies those files into the target folder.
  #each iteration will use a different keyword, (may encounter problems by copying the same folders?)
  for (k in keywords) {
    print(k)
    folder_end <- paste("~/Desktop/R/Legal_Projects/Keyword PDF Sorting Project/", k, sep= "")
    dir.create(folder_end)
    df_pdfs <- keyword_directory(folder_start, k, ignore_case = TRUE)
    if (nrow(df_pdfs > 0 )) {
      all_pdfs <- unique(df_pdfs$pdf_name)
      for (p in all_pdfs) {
        file.copy(p, folder_end)
      }
    }
  }
}



########################################################################################




sort_pdfs_keyword_pairs <- function(keyword_str, starting_folder) {
  setwd(paste("~/Desktop/R/Legal_Projects/Keyword PDF Sorting Project/", starting_folder, sep= ""))
  folder_start <- paste("~/Desktop/R/Legal_Projects/Keyword PDF Sorting Project/", starting_folder, sep= "")
  keywords <- keyword_str
  folder_name <- paste(keywords[1], keywords[2])
  print(folder_name)
  dir.create(paste("~/Desktop/R/Legal_Projects/Keyword PDF Sorting Project/", folder_name))
  #large function that searches all pdfs in WD, then copies those files into the target folder.
  all_pdf_data <- NULL
  for (k in keywords) {
    #identify all pdfs that have both keywords in it. Might need to use data.frame pairs?
    print(k)
    folder_end <- paste("~/Desktop/R/Legal_Projects/Keyword PDF Sorting Project/", k, sep= "")
    #dir.create(folder_end)
    df_pdfs <- as.data.frame(keyword_directory(folder_start, k, ignore_case = TRUE))
    all_pdf_data  <- rbind(all_pdf_data, df_pdfs)
  }
  
  all_pdf_data <- all_pdf_data[ ,1:3]
  if (nrow(all_pdf_data) > 0) { 
    all_pdfs <- unique(all_pdf_data$pdf_name)
    for (p in all_pdfs) { #go through all pdfs and if they have both words, copy the file.
      print(p)
      ind_pdf <- subset(all_pdf_data, pdf_name == p)
      #print(head(ind_pdf))
      len_kw <- length(c(unique(ind_pdf$keyword)))
      print(len_kw)
      if (len_kw == 2) {
        file.copy(p, paste("~/Desktop/R/Legal_Projects/Keyword PDF Sorting Project/", folder_name)) #need to change this
      }
    }
  }
  else (print("No Keywords Found in Folder's Contents"))
}

sort_pdfs_keyword_pairs(c("bull", "radiotelemetry") , "PDFs")

#########################################################################################################


run_keyword_search <- function(df_keywords, fun_starting_folder) {
  df <- df_keywords
  all_rows <- c(1:nrow(df))
  for (r in all_rows) {
    sort_pdfs_keyword_pairs(df[r, ], fun_starting_folder)
  }
}
