
# library(TCGAbiolinks)
# library(SummarizedExperiment)
library(stringr)
library(magrittr)
library(dplyr)
# library(tinyarray)
# library(AnnoProbe)
# 
# 
# packageVersion("TCGAbiolinks")# 2.24.3
# 
# library(TCGAcm)
# library(XML)
library(xml2)

# 临床文件xml
all_files <- list.files("./TCGA-GBM/Clinical/", recursive = T, full.names = T)

files <- all_files[str_detect(all_files, pattern = "nationwidechildrens.org_clinical")]

file1 <- files[595] # 595
xfile <- xml2::read_xml(file1)


# 尝试读取单个文件为list
test1 <- tcga_xml_parse(xfile = files[which(str_detect(files, pattern = "TCGA-12-3644"))] ) # 595 # 子节点含有多个重复
test2 <- tcga_xml_parse(xfile = files[1]) # 无nte 
test3 <- tcga_xml_parse(xfile = files[which(str_detect(files, pattern = "TCGA-76-6664"))] ) # 525 # 无 nte 和 radiations
test4 <- tcga_xml_parse(xfile = files[5] ) # 无 drugs 和 radiations
test5 <- tcga_xml_parse(xfile = files[7] ) # 无 follow_ups 

#######################
# admin      = list_admin,
# patient    = list_patient,
# drugs      = list_drugs,
# radiations = list_radiations,
# follow_ups = list_followups,
# nte        = list_nte

all_patient <- lapply(seq_along(files), function(x){
  message(x," ",files[x])
  return(tcga_xml_parse(xfile = files[x])[["patient"]])
})
all_patient_df <- do.call(dplyr::bind_rows, all_patient)

all_drugs <- lapply(seq_along(files), function(x){
  message(x," ",files[x])
  return(tcga_xml_parse(xfile = files[x])[["drugs"]])
})
all_drugs_df <- do.call(dplyr::bind_rows, all_drugs[!is.na(all_drugs)])

all_drugs_df$bcr_patient_barcode %>% unique() %>% length()



#################
# 读取多个xml文件为list
all_list <- tcga_xml2list(xfiles = xfiles)

# 将所有文件的list中的某个元素合并为df
df_admin <- tcga_list2df(lists = all_list, element = "admin")
# 读取单个xml文件，将其中某个元素合并为df
df_admin2 <- tcga_xml2df(xfiles = xfiles, element = "admin")

df_patient <- tcga_list2df(lists = all_list, element = "patient")
df_patient2 <- tcga_xml2df(xfiles = xfiles, element = "patient")

df_drugs <- tcga_list2df(lists = all_list, element = "drugs")
df_drugs2 <- tcga_xml2df(xfiles = xfiles, element = "drugs")

df_radiations <- tcga_list2df(lists = all_list, element = "radiations")
df_radiations2 <- tcga_xml2df(xfiles = xfiles, element = "radiations")

df_follow_ups <- tcga_list2df(lists = all_list, element = "follow_ups")
df_follow_ups2 <- tcga_xml2df(xfiles = xfiles, element = "follow_ups")

df_nte <- tcga_list2df(lists = all_list, element = "nte")
df_nte2 <- tcga_xml2df(xfiles = xfiles, element = "nte")

save.image("01.output/read_TCGA_clinical_infor.Rdata")
