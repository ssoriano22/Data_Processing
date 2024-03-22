#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(readxl)
library(openxlsx)
library(stringr)
library(toolbox)

#OVERVIEW: Script to create an updated plate map file with MS file names
#           matched to samples in plate map order. For control files, will
#           only fill in PC1 and PC2. Option (currently commented out) is 
#           included to create filenames.txt list (MSfiles) with this script.

#FOR RUNNING IN RSTUDIO: Hard-coded inputs for arg_fn and arg_pmf

#arg_fn = "~/Code/XT_Comparison/test_XT_MSfiles/" # When using MS DIRECTORY path option described below
#arg_fn = "~/Code/XT_Comparison/20240207_Murine_Liver_Expl_Plasma/filenames.txt"
#arg_pmf = "~/Code/XT_Comparison/20240207_Murine_Liver_Expl_Plasma/PlateMap_01Feb2024_MurineLiverExplPlasma_20240201_0745_1L0221427_a_blank.xlsx"

#FOR USER INPUT - Command Line/Terminal:

# Print prompt to command line/terminal requesting path - MS file names (i.e. filenames.txt)
#     NOTE: See OPTIONAL section below to create this MS file names list using this script
cat("Enter path to file with MS file names: ")
# Save one user input (stdin) entry as the filename.txt path (arg_fn)
arg_fn = readLines(con = "stdin", n = 1)

# Print prompt to command line/terminal requesting path - "blank" plate map file (excel .xlsx file)
cat("Enter path to plate map file: ")
# Save one user input (stdin) entry as the starting plate map file path (arg_pmf)
arg_pmf = readLines(con = "stdin", n = 1)

#OPTIONAL: Create MS filename list based on user-input MS DIRECTORY path
#           Using absolute path avoids occasional issue parsing relative path
#           Path example: "~/Code/XT_Comparison/test_MSfiles/"
#           If using this option:
#               1) Enter MS FILE DIRECTORY path in above arg_fn prompt instead of MS FILE NAME FILE path
#               2) Uncomment line 45 and comment-out line 48 (where MSfiles is initialized)

# MSfiles = data.frame(list.files(path = arg_fn))

#Read in txt file with MS file names as data frame
MSfiles = data.frame(read.delim(arg_fn))
#Change MSfiles column name to match plate map file File_Name column
colnames(MSfiles) = c("File_Name")

#Load plate map file as data frame
blank_pmf = read_xlsx(arg_pmf)
#Create temp_pmf that only contains rows from original pmf with non-NA values in "Sample name" column
temp_pmf = blank_pmf %>% filter(!is.na(`Sample name`))
#Save vector of sample names from pmf
pmf_samples = temp_pmf$`Sample name` %>% unique()

#Alter MSfiles to MSfiles_df to create columns for sorting into same row order as pmf
# **This requires a specific naming pattern for MS files w/ "_"s - ex. 20240207_Murine_neatA_NPB_BH10_1_6817.d
# **Volume column needed for 20240207_Murine_Liver_Expl_Plasma
#   When this script is used for other projects:
#       1) Volume-related mutate lines shouldn't effect script function, but can be commented out/removed
#       2) Volume SHOULD be removed from arrange() on line XX

                        #Only keep sample file names - ".d" file names containing a "NP"
MSfiles_df = MSfiles %>% filter(endsWith(File_Name,".d") & grepl("NP",File_Name)) %>%
                          #Add two new columns - Sample_Name and NP - from File_Name column using regex
                          separate_wider_regex(File_Name,
                                               patterns = c("^[0-9]{8}_", Sample_Name = ".*", "_NP",NP = ".","_.*"),
                                               cols_remove = FALSE) %>%
                          #Add "_" between sample name and replicate letter to match pmf "Sample name" column format
                          #Add volume column from Sample_Name using regex, replace "neat" w/ 250, change Volume column to numeric
                          mutate(Sample_Name = sub("([A-Z])$","_\\1",Sample_Name),
                                 Volume = sub(".*_(.{2,})uL_.*","\\1",Sample_Name),
                                 Volume = ifelse(grepl("neat",Sample_Name),250,Volume),
                                 Volume = suppressWarnings(as.numeric(Volume))) %>%
                          #Filter to only keep MS file names for samples in the pmf
                          filter(Sample_Name %in% pmf_samples) %>%
                          #Arrange rows in pmf order (first by NP (A,B), then by Volume (10-250), then by Sample_Name (Murine, PC3))
                          arrange(NP, Volume, Sample_Name)

#Fill in empty temp_pmf file name column w/ arranged MS file names for experimental samples from MSfiles_df
temp_pmf$`MS file name` = MSfiles_df$File_Name
#Create empty_pmf that contains rows from starting blank_pmf with NA in "Sample name" column
empty_pmf = blank_pmf %>% filter(is.na(`Sample name`))
#Combine temp_pmf with empty_pmf to create temp2_pmf
#   This now has all rows from the starting blank_pmf, w/ filled-in MS file names for experimental samples, but all NA "Sample name" rows at the end
temp2_pmf = rbind(temp_pmf,empty_pmf)

#Create temp3_pmf to sort temp2_pmf into "Well location" order (like starting pmf) 
                          #Add two columns - Well_Letter and Well_Num - from "Well location" using regex 
temp3_pmf = temp2_pmf %>% separate_wider_regex(`Well location`,
                                    patterns = c(Well_Letter = "[A-Z]","",Well_Num = "[0-9]{1,}"),
                                    cols_remove = FALSE) %>%
                          #Make Well_Num numeric
                          mutate(Well_Num = as.numeric(Well_Num)) %>%
                          #Arrange rows in "Well location" order (first by Well_Num, then by Well_Letter)
                          arrange(Well_Num, Well_Letter) %>%
                          #Remove "Well_Num" and "Well_Letter" columns (only needed for above sort)
                          dplyr::select(-c("Well_Num","Well_Letter"))

#Add process ctrl file names - only keep one file per control w/ highest final numeric in file name
                              #Filter to keep MS file names w/ "ctrl" and "PC" from MSfiles
MSfiles_ctrl_df = MSfiles %>% filter(grepl("ctrl",File_Name)) %>%
                              filter(grepl("PC",File_Name)) %>%
                              #Create CTRL_Type column from File_Name using regex - either "PC1" or "PC2"
                              #Create Sort_Numeric column from File_Name using regex - numeric btwn last "_" and ".d" ending
                              mutate(CTRL_Type = sub(".*_(PC[0-9]).*$","\\1",File_Name),
                                     Sort_Numeric = sub(".*_(.*).d$","\\1",File_Name)) %>%
                              #Group by CTRL_Type (PC1 and PC2)
                              group_by(CTRL_Type) %>%
                              #Keep one row per group that has the highest Sort_Numeric value in that group
                              top_n(1, Sort_Numeric) %>%
                              #Arrange remaining two rows - one PC1, one PC2 - in that order
                              arrange(CTRL_Type)

#Create ctrl_pmf that only has the two "Process Control" rows from the temp3_pmf file, and two columns "MS file name" and "Control"
ctrl_pmf = temp3_pmf %>% filter(grepl("Process Control",Control)) %>% dplyr::select(c("MS file name","Control"))
#Fill in MS file names in ctrl_pmf with arranged MSfile_ctrl_df MS file names
ctrl_pmf$`MS file name` = MSfiles_ctrl_df$File_Name

#Assemble final pmf combining experimental samples and blank pmf df with ctrl_pmf df
                          #Join two columns of ctrl_pmf to temp3_pmf by "Control"
final_pmf = temp3_pmf %>% left_join(ctrl_pmf, by = "Control") %>%
                          #Join renames "MS file name" columns w/ ".x" (temp3_pmf), and ctrl_pmf column w/ ".y" (ctrl_pmf)
                          #Fill in "MS file name.x" with "MS file name.y" value for the two "Process Control" rows
                          mutate(`MS file name.x` = ifelse(!is.na(`MS file name.x`),
                                                           `MS file name.x`,
                                                           ifelse(grepl("Process Control",Control),
                                                                  `MS file name.y`,
                                                                  NA))) %>%
                          #Remove extra "MS file name.y" column
                          dplyr::select(-c("MS file name.y"))
#Remove ".x" suffix from final_pmf "MS file name" column
colnames(final_pmf)[colnames(final_pmf) == "MS file name.x"] = "MS file name"

#Output to excel file
#Create output path from arg_pmf path using regex
output_path = sub("(.*).xlsx","\\1_wMSfiles.xlsx",arg_pmf)
#Write final_pmf to output_path
write.xlsx(final_pmf, output_path)

#Write conclusion statement to command line/terminal
#Create output_pmf file name (for output_statement) from output_path used above
output_pmf = sub(".*/(.*)$","\\1",output_path)
#Print output_statement to command line/terminal
output_statement = paste("Plate map file successfully generated with MS file names matched to samples:\n",output_pmf,"\n",sep="")
cat(output_statement)


