#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(rlang)
library(tidyverse)
library(Orthology.eg.db)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(KEGGREST)

#OVERVIEW: Script to perform nomenclature pivot for mouse -> human and human -> mouse UniProt IDs.
#           More details can be found in "LabNotebook_SomaBenchmarkcon_SS.txt" either in the SomaLogic_Benchmark project folder OR https://github.com/ssoriano22/SomaLogic_Benchmark/tree/main/2024_Updates
#   NOTES: This pivot relies on:
#               1) the NCBI EntrezID database included in the Bioconductor R packages
#                   * Orthology.eg.db - orthology database
#                   * org.Hs.eg.db - homo sapien ortholog database
#                   * org.Mm.eg.db - mus musculus
#               2) the KEGG Orthology (REST API) database
#                   * KEGGREST 
#                   * Internet connection required. May experience some "KEGG timeout" errors, especially when using OHSU internet.
#                     ** Issue should resolve with repeated 1-2 runs of the line causing the error.
#                     ** If timeout error is not resolving, or another KEGG error is observed:
#                           1) Try clearing R environment and run script again
#                           2) Try restarting computer and run script again
#                           3) Running the KEGG REST queries repeatedly can max out the number of search requests the KEGG REST API
#                               allows in one period of time. Wait at least 4 hours and try again, 24 hours is best if possible.

#Example gene sets for testing this script:
# NOTE: IN CURRENT FUNCTION SETUP, UNIPROT IDS ARE USED AS INPUT - NOT GENE NAMES.
#       SEARCHING BY GENE NAMES IS POSSIBLE, BUT REQUIRES SWAPPING OUT "UNIPROT" FOR "GENENAME" IN FUNCTION SEARCH QUERY LINES.
#       AS THESE FUNCTIONS ARE INTENDED FOR PROTEIN NOMENCLATURE PIVOTS, "UNIPROT" IS BELIEVED TO BE THE BEST QUERY PARAMETER.
# Simple MUS -> HOM Test:
# Known HOM ortholog exists in either NCBI or KEGG databases
# musGenes = c("Hmmr", "Tlx3", "Cpeb4")
# Complex MUS -> HOM Tests:
# HOM ortholog may not exist OR may result in multiple HOM ortholog results (and 1:1 MUS:HOM ortholog match is not known)
# musGenes2 = c("Alb","Amy2a5","Lyz1","Reg1","Reg3g")
# musGenes3 = c("Alb","Amy1","Lyz1","Reg2","Reg3b")
# Including a string that is not a gene name
# musGenes4 = c("Hmmr", "Tlx3", "ONYXAMBER")
# Simple HOM -> MUS Test:
# Known MUS ortholog exists in either NCBI or KEGG databases
# humGenes = c("HMMR", "TLX3", "CPEB4")
# Complex HOM -> MUS Test:
# MUS ortholog may not exist OR may result in multiple MUS ortholog results (and 1:1 HOM:MUS ortholog match is not known)
# humGenes2 = c("ALB","AMY2A","LYZ","REG1A","REG3G")

#Example uniprot sets for testing this script:
# Simple:
# Known MUS/HOM ortholog exists in either NCBI or KEGG databases
musUniProt = c("Q00547","O55144","Q7TN98")
humUniProt = c("O75330","O43711","Q17RY0")

#FUNCTION: Human --> Mouse Nomenclature Pivot
#   NOTE: Several print statements are included to track progress of function/mapping steps, and to assist with error location/debugging.
nomConvert_Human_Mouse <- function(uniprots){
  print("Starting mapping...")
  #Get EntrezIDs of submitted human uniprots (key for orthologyDB search)
  #   NOTE: This is a vector of EntrezIDs (i.e. c("3161", "30012","80315")).
  #         While in theory you SHOULD be able to do the below mapIds() steps with UNIPROT inputs, I found it unreliable compared to ENTREZID inputs.
  egs = unlist(unname(mapIds(org.Hs.eg.db, uniprots, "ENTREZID","UNIPROT")))
  #Check to make sure valid uniprots are submitted (valid uniprots == mapIDs() found an EntrezID)
  if(anyNA(egs)){
    #Uncomment the below line to force the script to stop w/ error message if egs contains non-valid EntrezIds
    #stop(simpleError("Pivot failed: All uniprots submitted must be valid in either NCBI (EntrezID) or KEGG DB."))
    #Otherwise removes NAs and continues w/ only valid EntrezIds:
    print("Not all UniProts submitted are valid in NCBI (EntrezID) DB. Proceeding with pivot for valid UniProts only.")
    #Removes any non-valid strings from vector of EntrezIds
    egs = na.omit(egs)
  }
  print("Human uniprots mapped to EntrezID...")
  #Perform orthology selection with human EntrezIDs - creates mapped_uniprots df with mapped HOM and MUS EntrezIds
  mapped_uniprots = select(Orthology.eg.db, egs, "Mus.musculus","Homo.sapiens")
  print("Human --> Mouse Entrez ID Orthology Match performed...")
  #Add columns for each annotation of interest (HOM and MUS) - uses Bioconductor NCBI database
  #   NOTE: One of the added columns is HOM_UniProt, which should result in the same UniProts used in the initial query
  mapped_uniprots$HOM_Gene = mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "SYMBOL", "ENTREZID")
  mapped_uniprots$HOM_EnsemblID = mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "ENSEMBL", "ENTREZID")
  mapped_uniprots$HOM_EnsemblProtID = mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "ENSEMBLPROT", "ENTREZID")
  mapped_uniprots$HOM_GeneDescription = mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "GENENAME", "ENTREZID")
  mapped_uniprots$HOM_PATH = mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "PATH", "ENTREZID")
  mapped_uniprots$HOM_GO = mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "GO", "ENTREZID")
  mapped_uniprots$HOM_Ontology = mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "ONTOLOGY", "ENTREZID")
  mapped_uniprots$HOM_UniProt = mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "UNIPROT", "ENTREZID")
  mapped_uniprots$MUS_Gene = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "SYMBOL", "ENTREZID"))
  mapped_uniprots$MUS_EnsemblID = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "ENSEMBL", "ENTREZID"))
  mapped_uniprots$MUS_EnsemblProtID = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "ENSEMBLPROT", "ENTREZID"))
  mapped_uniprots$MUS_MGI = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "MGI", "ENTREZID"))
  mapped_uniprots$MUS_GeneDescription = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "GENENAME", "ENTREZID"))
  mapped_uniprots$MUS_GO = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "GO", "ENTREZID"))
  mapped_uniprots$MUS_Ontology = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "ONTOLOGY", "ENTREZID"))
  mapped_uniprots$MUS_UniProt = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "UNIPROT", "ENTREZID"))
  print("All annotation columns successfully added to NCBI-Mapped DF...")
  #Reorganize/rename columns for clarity
  mapped_uniprots = mapped_uniprots %>% dplyr::select(Homo.sapiens,Mus.musculus,HOM_Gene,everything())
  colnames(mapped_uniprots)[1] = "HOM_EntrezID"
  colnames(mapped_uniprots)[2] = "MUS_EntrezID"
  
  #Separate out uniprots WITH mouse orthology found using Entrez ID/orthology.eg.db and change MUS_EntrezID to character
  #   NOTE: The as.character() mutate makes a KEGG-related merge later on in this script much easier.
  EnzID_mapped_uniprots = mapped_uniprots %>%
                          filter(!is.na(MUS_EntrezID)) %>%
                          mutate(MUS_EntrezID = as.character(MUS_EntrezID)) %>%
                          mutate(Mapping_Method = "Bioc_NCBIEntrezID")
  #Separate out uniprots WITH NO mouse orthology found using Entrez ID/orthology.eg.db and create HOM_KEGGID column
  KEGG_mapped_uniprots = mapped_uniprots %>%
                          filter(is.na(MUS_EntrezID)) %>%
                          mutate(HOM_KEGGID = paste("hsa",HOM_EntrezID,sep = ":")) %>%
                          mutate(Mapping_Method = "KEGG_KOID")
  
  #If HOM -> MUS pivots were performed for all inputs using NCBI Entrez database (i.e. KEGG_mapped_uniprots is empty)
  if(length(KEGG_mapped_uniprots$MUS_EntrezID) == 0){
    print("Uniprots Nomenclature Conversion: Human --> Mouse Complete. NCBI DB used for all uniprots pivots.")
    #End function here and return complete df of HOM -> MUS UniProt pivot/mapping results
    return(EnzID_mapped_uniprots)
  }
  #Else HOM -> MUS pivot could not be completed for all inputs using NCBI Entrez database (i.e. KEGG_mapped_uniprots contains row data)
  else{
    #Broadly, check for orthologs not detected by Entrez search using KEGG data
    
    #Create keggLink urls to get KO id - split data into subsets of 100 for KEGG map request
    #Set data subset size.
    #   NOTE: 100 was chosen after trial and error runs experimenting with the KEGG database,
    #         balancing number of queries submitted with the size of each query list. Submitting
    #         all IDs in one query, or individual queries for each ID both do not work for the KEGG database
    #         and will likely result in HTTP 403 errors.
    sub_size = 100
    #Get total length of IDs to be searched w/ KEGG
    total_length = nrow(KEGG_mapped_uniprots)
    #This sets row numbers to split df by in the next line (to create X dfs of 100 rows each(or less for the final split)
    r  = rep(1:ceiling(total_length/sub_size),each=sub_size)[1:total_length]
    #Split df by r - results in data_frac, a list of X dfs of 100 rows each
    data_frac = split(KEGG_mapped_uniprots,r)
    #Create a new list to store KEGG query URLs in - same length as number of dfs in data_frac
    link_urls = vector("list", length = length(data_frac))
    #Set each link_urls list value to the KEGG REST url beginning
    link_urls = lapply(link_urls,function(x) paste(x,"https://rest.kegg.jp/link/ko/",sep = ""))
    #The following stacked for loop effectively "assembles" the url for each query/df in the list
    j = 1 #Initialize data fraction counter
    #For each df of 100 IDs/rows in the data_frac list
    for (sub_df in data_frac){
      i = 1 #Initialize KEGG ID counter - restart per sub_df/url
      #Set the current_link to be editing - index in data_frac list matches index in link_urls list
      current_link = link_urls[j]
      #For each id in the HOM_KEGGID column of this subset df
      for (ezid in sub_df$HOM_KEGGID){
          #Add first KEGGID to url
          if (i == 1){current_link = paste(current_link,ezid,sep = "")}
          #Add each other KEGGIDs to url (w/ "+" separator)
          else{current_link = paste(current_link,ezid,sep = "+")}
          i = i + 1 #Increment KEGGID counter
      }
      #Save current_link url to link_urls at index position for this sub_df in data_frac
      link_urls[j] = current_link
      j = j + 1 #Increment sub_df counter
    }
    
    #Submit urls to retrieve KO# results in new df - lapply() performs a function on each element in the list (x) in link_urls, and returns a list of each result in the same order
    HOM_KO_reslist = lapply(link_urls,function(x) read_tsv(x,col_names = c("HOM_KEGG_ID","KO_ID")))
    
    #Create keggLink urls to get mmu KEGGIDs (mmu = MUS)
    #Create a new list to store mmu KEGG query URLs in - same length as number of dfs in data_frac
    mmu_link_urls = vector("list", length = length(data_frac))
    #Set each mmu_link_urls list value to the KEGG REST url beginning
    mmu_link_urls = lapply(mmu_link_urls,function(x) paste(x,"https://rest.kegg.jp/link/mmu/",sep = ""))
    #The following stacked for loop effectively "assembles" the mmu url for each query/df in the mmu list
    j = 1 #Initialize data frac counter
    #For each sub_df of KO results in HOM_KO_reslist
    for (ko_sub_df in HOM_KO_reslist){
      i = 1 #Initialize KEGG ID counter - restart per ko_sub_df/url
      #Set the current_mmu_link to be editing - index in HOM_KO_reslist matches index in mmu_link_urls list
      current_mmu_link = mmu_link_urls[j]
      #For each id in the KO_ID column of this subset df
      for (koid in ko_sub_df$KO_ID){
        #Add first KO ID to url
        if (i == 1){current_mmu_link = paste(current_mmu_link,koid,sep = "")}
        #Add each other KEGGIDs to url (w/ "+" separator)
        else{current_mmu_link = paste(current_mmu_link,koid,sep = "+")}
        i = i + 1 #Increment KO_ID counter
      }
      #Save current_mmu_link url to mmu_link_urls at index position for this ko_sub_df in HOM_KO_reslist
      mmu_link_urls[j] = current_mmu_link
      j = j + 1 #Increment ko_sub_df counter
    }
    
    #Submit mmu urls to retrieve MUS_KEGGID results in new df - lapply() performs a function on each element in the list (x) in link_urls, and returns a list of each result in the same order
    #   NOTE: This function currently keeps all MUS_KEGGID results for a single KO ID in cases with 1:many results.
    MUS_KEGGID_res = lapply(mmu_link_urls,function(x) read_tsv(x,col_names = c("KO_ID","MUS_KEGG_ID")))
    
    #Turn all mmu KEGGID# to MUS EntrezIDs in new column (this is equivalent to a mutate() column creation, but without dplyr/tidyr and applied to multiple dfs in a list)
    new_MUS_KEGGID_res = lapply(MUS_KEGGID_res, function(x) cbind(x, MUS_EntrezID = sub("mmu:","",x$MUS_KEGG_ID)))
    
    #Combine w/ human EntrezID df
    #Combines all sub_df for MUS data into MUS_res - do.call() tells R to do the specified function "rbind" to all list elements.
    MUS_res = do.call("rbind",new_MUS_KEGGID_res)
    #Combines all sub_df for HOM data into HOM_res
    HOM_res = do.call("rbind",HOM_KO_reslist)
    #Adds HOM_EntrezID column
    HOM_res = HOM_res %>% mutate(HOM_EntrezID = sub("hsa:","",HOM_KEGG_ID))
    #Combine HOM_res and MUS_res df by KO_ID.
    #   NOTE: Using inner_join() should only keep the most likely HOM pivots from the MUS KO_ID -> KEGG_ID search (MUS_res rows without a KO to match a HOM_res result are eliminated).
    #         It is still possible for more than one MUS_KEGGID result per HOM_KEGGID, with common KO_ID.
    HOM_MUS_res = inner_join(HOM_res,MUS_res, by = "KO_ID")
    
    #Keep both EntrezID columns
    HOM_MUS_EzID_res = HOM_MUS_res %>% dplyr::select(c("HOM_EntrezID","MUS_EntrezID"))
    #Combine KEGG-mapped results with main KEGG_mapped_uniprots df
    KEGG_mapped_uniprots = inner_join(KEGG_mapped_uniprots,HOM_MUS_EzID_res,by = "HOM_EntrezID")
    #Fix ".x/.y" column naming issue in KEGG_mapped_uniprots from inner_join() to only have one MUS_EntrezID column
    KEGG_mapped_uniprots = KEGG_mapped_uniprots %>% mutate(MUS_EntrezID.x = ifelse((is.na(MUS_EntrezID.x) & !is.na(MUS_EntrezID.y)),
                                                                                   MUS_EntrezID.y,
                                                                                   MUS_EntrezID.x)) %>%
                                                    dplyr::select(-c("MUS_EntrezID.y"))
    colnames(KEGG_mapped_uniprots)[colnames(KEGG_mapped_uniprots) == "MUS_EntrezID.x"] =  "MUS_EntrezID"
    #Fill in Orthology.eg.db (NCBI database) MUS columns in KEGG_mapped_uniprots for now-filled-in MUS EntrezIDs
    KEGG_mapped_uniprots = KEGG_mapped_uniprots %>% mutate(MUS_Gene = mapIds(org.Mm.eg.db, MUS_EntrezID, "SYMBOL", "ENTREZID"),
                                                            MUS_EnsemblID = mapIds(org.Mm.eg.db, MUS_EntrezID, "ENSEMBL", "ENTREZID"),
                                                            MUS_EnsemblProtID = mapIds(org.Mm.eg.db, MUS_EntrezID, "ENSEMBLPROT", "ENTREZID"),
                                                            MUS_MGI = mapIds(org.Mm.eg.db, MUS_EntrezID, "MGI", "ENTREZID"),
                                                            MUS_GeneDescription = mapIds(org.Mm.eg.db, MUS_EntrezID, "GENENAME", "ENTREZID"),
                                                            MUS_GO = mapIds(org.Mm.eg.db, MUS_EntrezID, "GO", "ENTREZID"),
                                                            MUS_Ontology = mapIds(org.Mm.eg.db, MUS_EntrezID, "ONTOLOGY", "ENTREZID"),
                                                            MUS_UniProt = mapIds(org.Mm.eg.db, MUS_EntrezID, "UNIPROT", "ENTREZID")
                                                          )
    
  #Combine NCBI-mapped and KEGG-mapped uniprots, keep unique rows
  combo_mapped_uniprots = bind_rows(EnzID_mapped_uniprots,KEGG_mapped_uniprots)
  combo_mapped_uniprots = combo_mapped_uniprots %>% unique()
  #Print final statement confirming completed execution of the above and return NCBI and KEGG-mapped data for HOM -> MUS pivot
  #   NOTE: Currently, the entire df of pivot results is returned (not just the MUS UniProts)
  print("UniProt Nomenclature Conversion: Human --> Mouse Complete. KEGG Orthology DB used to address gaps in NCBI DB.")
  return(combo_mapped_uniprots)
  }
}

#For testing the above function for HOM -> MUS nomenclature pivot
# orthDB_HtM_mapped_res_df = nomConvert_Human_Mouse(humUniProt)

#FUNCTION: Mouse --> Human Nomenclature Pivot
#   NOTE: Several print statements are included to track progress of function/mapping steps, and to assist with error location/debugging.
nomConvert_Mouse_Human <- function(uniprots){
  print("Starting mapping...")
  #Get EntrezIDs of submitted human uniprots (key for orthologyDB search)
  #   NOTE: This is a vector of EntrezIDs (i.e. c("3161", "30012","80315")).
  #         While in theory you SHOULD be able to do the below mapIds() steps with UNIPROT inputs, I found it unreliable compared to ENTREZID inputs.
  egs = mapIds(org.Mm.eg.db, uniprots, "ENTREZID","UNIPROT")
  #Check to make sure valid uniprots are submitted (valid uniprots == mapIDs() found an EntrezID)
  if(any(is.na(egs))){
    #Uncomment the below line to force the script to stop w/ error message if egs contains non-valid EntrezIds
    #stop(simpleError("Pivot failed: All uniprots submitted must be valid in either NCBI (EntrezID) or KEGG DB."))
    #Otherwise removes NAs and continues w/ only valid EntrezIds:
    print("Not all uniprots submitted are valid in NCBI (EntrezID) DB. Proceeding with pivot for valid uniprots only.")
    #Removes any non-valid strings from vector of EntrezIds
    egs = na.omit(egs)
  }
  print("Mouse uniprots mapped to EntrezID...")
  #Perform orthology selection with mouse EntrezIDs - creates mapped_uniprots df with mapped MUS and HOM EntrezIds
  mapped_uniprots = select(Orthology.eg.db, egs, "Homo.sapiens", "Mus.musculus")
  print("Mouse --> Human Entrez ID Orthology Match performed...")
  #Add columns for each annotation of interest (MUS and HOM) - uses Bioconductor NCBI database
  #   NOTE: One of the added columns is MUS_UniProt, which should result in the same UniProts used in the initial query
  mapped_uniprots$MUS_Gene = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "SYMBOL", "ENTREZID"))
  mapped_uniprots$MUS_EnsemblID = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "ENSEMBL", "ENTREZID"))
  mapped_uniprots$MUS_EnsemblProtID = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "ENSEMBLPROT", "ENTREZID"))
  mapped_uniprots$MUS_MGI = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "MGI", "ENTREZID"))
  mapped_uniprots$MUS_GeneDescription = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "GENENAME", "ENTREZID"))
  mapped_uniprots$MUS_GO = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "GO", "ENTREZID"))
  mapped_uniprots$MUS_Ontology = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "ONTOLOGY", "ENTREZID"))
  mapped_uniprots$MUS_UniProt = as.character(mapIds(org.Mm.eg.db, as.character(mapped_uniprots$Mus.musculus), "UNIPROT", "ENTREZID"))
  mapped_uniprots$HOM_Gene = as.character(mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "SYMBOL", "ENTREZID"))
  mapped_uniprots$HOM_EnsemblID = as.character(mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "ENSEMBL", "ENTREZID"))
  mapped_uniprots$HOM_EnsemblProtID = as.character(mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "ENSEMBLPROT", "ENTREZID"))
  mapped_uniprots$HOM_GeneDescription = as.character(mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "GENENAME", "ENTREZID"))
  mapped_uniprots$HOM_PATH = as.character(mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "PATH", "ENTREZID"))
  mapped_uniprots$HOM_GO = as.character(mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "GO", "ENTREZID"))
  mapped_uniprots$HOM_Ontology = as.character(mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "ONTOLOGY", "ENTREZID"))
  mapped_uniprots$HOM_UniProt = as.character(mapIds(org.Hs.eg.db, as.character(mapped_uniprots$Homo.sapiens), "UNIPROT", "ENTREZID"))
  print("All annotation columns successfully added to NCBI-Mapped DF...")
  #Reorganize/rename columns for clarity
  mapped_uniprots = mapped_uniprots %>% dplyr::select(Mus.musculus,Homo.sapiens,everything())
  colnames(mapped_uniprots)[1] = "MUS_EntrezID"
  colnames(mapped_uniprots)[2] = "HOM_EntrezID"
  
  #Separate out uniprots WITH human orthology found using Entrez ID/orthology.eg.db and change HOM_EntrezID to character
  #   NOTE: The as.character() mutate makes a KEGG-related merge later on in this script much easier.
  EnzID_mapped_uniprots = mapped_uniprots %>%
                          filter(!is.na(HOM_EntrezID)) %>%
                          mutate(HOM_EntrezID = as.character(HOM_EntrezID)) %>%
                          mutate(Mapping_Method = "Bioc_NCBIEntrezID")
  #Separate out uniprots WITH NO human orthology found using Entrez ID/orthology.eg.db and create MUS_KEGGID column
  KEGG_mapped_uniprots = mapped_uniprots %>%
                          filter(is.na(HOM_EntrezID)) %>%
                          mutate(MUS_KEGGID = paste("mmu",MUS_EntrezID,sep = ":")) %>%
                          mutate(Mapping_Method = "KEGG_KOID")
  
  #If MUS -> HOM pivots were performed for all inputs using NCBI Entrez database (i.e. KEGG_mapped_uniprots is empty)
  if(length(KEGG_mapped_uniprots$MUS_EntrezID) == 0){
      print("UniProt Nomenclature Conversion: Mouse --> Human Complete. NCBI DB used for all uniprots pivots.")
      #End function here and return complete df of MUS -> HOM UniProt pivot/mapping results
      return(EnzID_mapped_uniprots)
  }
  #Else MUS -> HOM pivot could not be completed for all inputs using NCBI Entrez database (i.e. KEGG_mapped_uniprots contains row data)
  else{
      #Broadly, check for orthologs not detected by Entrez search using KEGG data
      
      #Create keggLink urls to get KO id - split data into subsets of 100 for KEGG map request
      #Set data subset size.
      #   NOTE: 100 was chosen after trial and error runs experimenting with the KEGG database,
      #         balancing number of queries submitted with the size of each query list. Submitting
      #         all IDs in one query, or individual queries for each ID both do not work for the KEGG database
      #         and will likely result in HTTP 403 errors.
      sub_size = 100
      #Get total length of IDs to be searched w/ KEGG
      total_length = nrow(KEGG_mapped_uniprots)
      #This sets row numbers to split df by in the next line (to create X dfs of 100 rows each(or less for the final split)
      r  = rep(1:ceiling(total_length/sub_size),each=sub_size)[1:total_length]
      #Split df by r - results in data_frac, a list of X dfs of 100 rows each
      data_frac = split(KEGG_mapped_uniprots,r)
      #Create a new list to store KEGG query URLs in - same length as number of dfs in data_frac
      link_urls = vector("list", length = length(data_frac))
      #Set each link_urls list value to the KEGG REST url beginning
      link_urls = lapply(link_urls,function(x) paste(x,"https://rest.kegg.jp/link/ko/",sep = ""))
      #The following stacked for loop effectively "assembles" the url for each query/df in the list
      j = 1 #Initialize data fraction counter
      #For each df of 100 IDs/rows in the data_frac list
      for (sub_df in data_frac){
        i = 1 #Initialize KEGG ID counter - restart per sub_df/url
        #Set the current_link to be editing - index in data_frac list matches index in link_urls list
        current_link = link_urls[j]
        #For each id in the MUS_KEGGID column of this subset df
        for (ezid in sub_df$MUS_KEGGID){
          #Add first KEGGID to url
          if (i == 1){current_link = paste(current_link,ezid,sep = "")}
          #Add each other KEGGIDs to url (w/ "+" separator)
          else{current_link = paste(current_link,ezid,sep = "+")}
          i = i + 1 #Increment KEGGID counter
        }
        #Save current_link url to link_urls at index position for this sub_df in data_frac
        link_urls[j] = current_link
        j = j + 1 #Increment sub_df counter
      }
      
      #Submit urls to retrieve KO# results in new df - lapply() performs a function on each element in the list (x) in link_urls, and returns a list of each result in the same order
      MUS_KO_reslist = lapply(link_urls,function(x) read_tsv(x,col_names = c("MUS_KEGG_ID","KO_ID")))
      
      #Create keggLink urls to get hsa KEGGIDs (hsa = HOM)
      #Create a new list to store hsa KEGG query URLs in - same length as number of dfs in data_frac
      hsa_link_urls = vector("list", length = length(data_frac))
      #Set each hsa_link_urls list value to the KEGG REST url beginning
      hsa_link_urls = lapply(hsa_link_urls,function(x) paste(x,"https://rest.kegg.jp/link/hsa/",sep = ""))
      #The following stacked for loop effectively "assembles" the mmu url for each query/df in the hsa list
      j = 1 #Initialize data fraction counter
      #For each sub_df of KO results in MUS_KO_reslist
      for (ko_sub_df in MUS_KO_reslist){
        i = 1 #Initialize KEGG ID counter - restart per sub_df/url
        #Set the current_hsa_link to be editing - index in MUS_KO_reslist matches index in hsa_link_urls list
        current_hsa_link = hsa_link_urls[j]
        #For each id in the KO_ID column of this subset df
        for (koid in ko_sub_df$KO_ID){
          #Add first KO ID to url
          if (i == 1){current_hsa_link = paste(current_hsa_link,koid,sep = "")}
          #Add each other KEGGIDs to url (w/ "+" separator)
          else{current_hsa_link = paste(current_hsa_link,koid,sep = "+")}
          i = i + 1 #Increment KO_ID counter
        }
        #Save current_hsa_link url to hsa_link_urls at index position for this ko_sub_df in MUS_KO_reslist
        hsa_link_urls[j] = current_hsa_link
        j = j + 1 #Increment ko_sub_df counter
      }
      
      #Submit hsa urls to retrieve HOM_KEGGID results in new df - lapply() performs a function on each element in the list (x) in link_urls, and returns a list of each result in the same order
      #   NOTE: This function currently keeps all HOM_KEGGID results for a single KO ID in cases with 1:many results.
      HOM_KEGGID_res = lapply(hsa_link_urls,function(x) read_tsv(x,col_names = c("KO_ID","HOM_KEGG_ID")))
      #Turn all hsa KEGGID# to HOM EntrezIDs in new column (this is equivalent to a mutate() column creation, but without dplyr/tidyr and applied to multiple dfs in a list)
      new_HOM_KEGGID_res = lapply(HOM_KEGGID_res, function(x) cbind(x, HOM_EntrezID = sub("hsa:","",x$HOM_KEGG_ID)))
      
      #Combine w/ mouse EntrezID df
      #Combines all sub_df for HOM data into HOM_res - do.call() tells R to do the specified function "rbind" to all list elements.
      HOM_res = do.call("rbind",new_HOM_KEGGID_res)
      #Combines all sub_df for MUS data into MUS_res
      MUS_res = do.call("rbind",MUS_KO_reslist)
      #Adds MUS_EntrezID column
      MUS_res = MUS_res %>% mutate(MUS_EntrezID = sub("mmu:","",MUS_KEGG_ID))
      #Combine MUS_res and HOM_res df by KO_ID.
      #   NOTE: Using inner_join() should only keep the most likely HOM pivots from the HOM KO_ID -> KEGG_ID search (HOM_res rows without a KO to match a MUS_res result are eliminated).
      #         It is still possible for more than one HOM_KEGGID result per MUS_KEGGID, with common KO_ID.
      MUS_HOM_res = inner_join(MUS_res,HOM_res, by = "KO_ID")
      
      #Keep both EntrezID columns
      MUS_HOM_EzID_res = MUS_HOM_res %>% dplyr::select(c("MUS_EntrezID","HOM_EntrezID"))
      #Combine KEGG-mapped results with main KEGG_mapped_uniprots df
      KEGG_mapped_uniprots = inner_join(KEGG_mapped_uniprots,MUS_HOM_EzID_res,by = "MUS_EntrezID")
      #Fix ".x/.y" column naming issue in KEGG_mapped_uniprots from inner_join() to only have one HOM_EntrezID column
      KEGG_mapped_uniprots = KEGG_mapped_uniprots %>% mutate(HOM_EntrezID.x = ifelse((is.na(HOM_EntrezID.x) & !is.na(HOM_EntrezID.y)),
                                                                                     HOM_EntrezID.y,
                                                                                     HOM_EntrezID.x)) %>%
                                                      dplyr::select(-c("HOM_EntrezID.y"))
      colnames(KEGG_mapped_uniprots)[colnames(KEGG_mapped_uniprots) == "HOM_EntrezID.x"] =  "HOM_EntrezID"
      #Fill in Orthology.eg.db (NCBI database) HOM columns in KEGG_mapped_uniprots for now-filled-in HOM EntrezIDs
      KEGG_mapped_uniprots = KEGG_mapped_uniprots %>% mutate(HOM_Gene = mapIds(org.Hs.eg.db, HOM_EntrezID, "SYMBOL", "ENTREZID"),
                                                             HOM_EnsemblID = mapIds(org.Hs.eg.db, HOM_EntrezID, "ENSEMBL", "ENTREZID"),
                                                             HOM_EnsemblProtID = mapIds(org.Hs.eg.db, HOM_EntrezID, "ENSEMBLPROT", "ENTREZID"),
                                                             HOM_GeneDescription = mapIds(org.Hs.eg.db, HOM_EntrezID, "GENENAME", "ENTREZID"),
                                                             HOM_GO = mapIds(org.Hs.eg.db, HOM_EntrezID, "GO", "ENTREZID"),
                                                             HOM_Ontology = mapIds(org.Hs.eg.db, HOM_EntrezID, "ONTOLOGY", "ENTREZID"),
                                                             HOM_UniProt = mapIds(org.Hs.eg.db, HOM_EntrezID, "UNIPROT", "ENTREZID")
      )
      #Combine NCBI-mapped and KEGG-mapped uniprots, keep unique rows
      combo_mapped_uniprots = bind_rows(EnzID_mapped_uniprots,KEGG_mapped_uniprots)
      combo_mapped_uniprots = combo_mapped_uniprots %>% unique()
      #Print final statement confirming completed execution of the above and return NCBI and KEGG-mapped data for MUS -> HOM pivot
      #   NOTE: Currently, the entire df of pivot results is returned (not just the HOM UniProts)
      print("UniProt Nomenclature Conversion: Mouse --> Human Complete. KEGG Orthology DB used to address gaps in NCBI DB.")
      return(combo_mapped_uniprots)
  }
}

#For testing this function for MUS -> HOM nomenclature pivot
orthDB_MtH_mapped_res_df = nomConvert_Mouse_Human(musUniProt)
