## -----------------------------------------------------------------------------
#| label: setup
library(TCGAeasy)
library(stringr)
xml_gzfile <- system.file("extdata/TCGA-GBM.zip",package = "TCGAeasy")
# file.copy(from = xml_gzfile, to = "./",overwrite = T)
# unzip(zipfile = "./TCGA-GBM.zip", exdir = "./")
unzip(zipfile = xml_gzfile, exdir = "./")

xml_files <- list.files("./TCGA-GBM/", pattern = ".xml$", recursive = T, full.names = T)


## ----cache=TRUE---------------------------------------------------------------

test1 <- tcga_xml_parse(xfile = xml_files[which(str_detect(xml_files, pattern = "TCGA-12-3644"))] ) # 595 # 子节点含有多个重复
print(test1)
test2 <- tcga_xml_parse(xfile = xml_files[which(str_detect(xml_files, pattern = "TCGA-14-1829"))] ) # 无nte 
test3 <- tcga_xml_parse(xfile = xml_files[which(str_detect(xml_files, pattern = "TCGA-76-6664"))] ) # 525 # 无 nte 和 radiations
test4 <- tcga_xml_parse(xfile = xml_files[which(str_detect(xml_files, pattern = "TCGA-02-0039"))] ) # 无 drugs 和 radiations
test5 <- tcga_xml_parse(xfile = xml_files[which(str_detect(xml_files, pattern = "TCGA-06-0240"))] ) # 无follow_ups 和 nte



## ----cache=TRUE---------------------------------------------------------------
all_list <- tcga_xml2list(xfiles = xml_files)
length(all_list)


## ----cache=TRUE---------------------------------------------------------------
# 将所有文件的list中的某个元素合并为df
df_admin <- tcga_list2df(lists = all_list, element = "admin")
# 读取单个xml文件，将其中某个元素合并为df
df_admin2 <- tcga_xml2df(xfiles = xml_files, element = "admin")
dim(df_admin)
colnames(df_admin)


## ----cache=TRUE---------------------------------------------------------------
df_patient <- tcga_list2df(lists = all_list, element = "patient")
df_patient2 <- tcga_xml2df(xfiles = xml_files, element = "patient")
dim(df_patient)
colnames(df_patient)


## -----------------------------------------------------------------------------
df_drugs <- tcga_list2df(lists = all_list, element = "drugs")
df_drugs2 <- tcga_xml2df(xfiles = xml_files, element = "drugs")
dim(df_drugs)
colnames(df_drugs)


## -----------------------------------------------------------------------------
df_radiations <- tcga_list2df(lists = all_list, element = "radiations")
df_radiations2 <- tcga_xml2df(xfiles = xml_files, element = "radiations")
dim(df_radiations)
colnames(df_radiations)


## -----------------------------------------------------------------------------
df_follow_ups <- tcga_list2df(lists = all_list, element = "follow_ups")
df_follow_ups2 <- tcga_xml2df(xfiles = xml_files, element = "follow_ups")
dim(df_follow_ups)
colnames(df_follow_ups)


## -----------------------------------------------------------------------------
df_nte <- tcga_list2df(lists = all_list, element = "nte")
df_nte2 <- tcga_xml2df(xfiles = xml_files, element = "nte")
dim(df_nte)
colnames(df_nte)

