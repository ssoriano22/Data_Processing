#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(readr)
library(readxl)
library(openxlsx)
library(stringr)
library(EnvStats)
library(knitr)
library(naniar)
library(stats)
library(toolbox)

# Overview: Script to create an updated plate map file with MS file names
#           matched to samples in plate map order

# User Input Option 1 - command line w/ prompt
#For cmd line interface:
cat("Enter path to file with MS file names: ")
arg_fn = readLines(con = "stdin", n = 1)
#For in-R testing:
#arg_fn = "~/Code/XT_Comparison/test_XT_MSfiles/" #For creating fn list in R
#arg_fn = "~/Code/XT_Comparison/filenames.txt" #For reading in fn list txt file
#For cmd line interface:
cat("Enter path to plate map file: ")
arg_pmf = readLines(con = "stdin", n = 1)
#For in-R testing:
#arg_pmf = "~/Code/XT_Comparison/PlateMap_01Feb2024_MurineLiverExplPlasma_20240201_0745_1L0221427_a.xlsx"

# User Input Option 2 - command line w/ arguments
# args = commandArgs(trailingOnly = TRUE)

#Create MS filename list based on user-input MS parent directory
#   Using absolute path avoids occasional issue parsing relative path
#   Path example: "~/Code/XT_Comparison/test_MSfiles/"
# MSfiles = data.frame(list.files(path = arg_fn))
#Read in txt file with filenames
MSfiles = data.frame(read.delim(arg_fn))
colnames(MSfiles) = c("File_Name")

#Load plate map file based on user-input path
blank_pmf = read_xlsx(arg_pmf)
#Alter pmf to prepare for MS file merge
temp_pmf = blank_pmf %>% filter(!is.na(`Sample name`))
#Get sample names from pmf
pmf_samples = temp_pmf$`Sample name` %>% unique()

#Alter MS file to create NP column for sorting
# **This requires a specific naming pattern w/ "_" for MS files - ex. 20240207_Murine_neatA_NPB_BH10_1_6817.d
MSfiles_df = MSfiles %>% filter(endsWith(File_Name,".d") & grepl("NP",File_Name)) %>%
                          separate_wider_regex(File_Name,
                                               patterns = c("^[0-9]{8}_", Sample_Name = ".*", "_NP",NP = ".","_.*"),
                                               cols_remove = FALSE) %>%
                          mutate(Sample_Name = sub("([A-Z])$","_\\1",Sample_Name),
                                 Volume = sub(".*_(.{2,})uL_.*","\\1",Sample_Name),
                                 Volume = ifelse(grepl("neat",Sample_Name),250,Volume),
                                 Volume = suppressWarnings(as.numeric(Volume))) %>%
                          filter(Sample_Name %in% pmf_samples) %>%
                          arrange(NP, Volume, Sample_Name)

#Save final version of plate map file (w/ added MS file names)
#final_pmf = merge(temp_pmf,MSfiles_df,by.x = `Sample name`,by.y = "Sample_Name")
temp_pmf$`MS file name` = MSfiles_df$File_Name
final_pmf = temp_pmf

#Output to excel file
output_path = sub("(.*).xlsx","\\1_wMSfiles.xlsx",arg_pmf)
write.xlsx(final_pmf, output_path)

output_pmf = sub(".*/(.*)$","\\1",output_path)
output_statement = paste("Plate map file successfully generated with MS file names matched to samples:\n",output_pmf,"\n",sep="")
cat(output_statement)


