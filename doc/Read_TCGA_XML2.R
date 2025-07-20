## -----------------------------------------------------------------------------
#| label: setup
library(TCGAeasy)
library(xml2)
library(tibble)
xml_gzfile <- system.file("extdata/TCGA-GBM.zip",package = "TCGAeasy")
# file.copy(from = xml_gzfile, to = "./",overwrite = T)
# unzip(zipfile = "./TCGA-GBM.zip", exdir = "./")
unzip(zipfile = xml_gzfile, exdir = "./")

xml_files <- list.files("./TCGA-GBM/", pattern = ".xml$", 
                        recursive = T, full.names = T)


## -----------------------------------------------------------------------------
doc <- read_xml(x = xml_files[1])

test_df <- tibble(
   tumor_tissue_site = get_text(
     doc, ".//clin_shared:tumor_tissue_site", return_all = F),
   histological_type = get_text(
     doc, ".//shared:histological_type", return_all = F),
   prior_glioma = get_text(doc, ".//gbm:prior_glioma", return_all = F),
   gender = get_text(doc, ".//shared:gender", return_all = F),
   vital_status = get_text(
     doc, ".//clin_shared:vital_status", return_all = F),
   days_to_birth = get_numeric(
     doc, ".//clin_shared:days_to_birth", return_all = F),
   days_to_death = get_integer(
     doc, ".//clin_shared:days_to_death", return_all = F),
   days_to_last_followup = get_integer(
     doc, ".//clin_shared:days_to_last_followup", return_all = F),
   race = get_text(doc, ".//clin_shared:race", return_all = F),
   bcr_patient_barcode = get_text(
     doc, ".//shared:bcr_patient_barcode", return_all = F),
   tissue_source_site = get_text(
     doc, ".//shared:tissue_source_site", return_all = F),
   patient_id = get_text(doc, ".//shared:patient_id", return_all = F),
   bcr_patient_uuid = get_text(
     doc, ".//shared:bcr_patient_uuid", return_all = F),
   days_to_initial_pathologic_diagnosis = get_integer(
     doc, ".//clin_shared:days_to_initial_pathologic_diagnosis", return_all = F),
   age_at_initial_pathologic_diagnosis = get_integer(
     doc, ".//clin_shared:age_at_initial_pathologic_diagnosis", return_all = F),
   year_of_initial_pathologic_diagnosis = get_integer(
     doc, ".//clin_shared:year_of_initial_pathologic_diagnosis", return_all = F)
)
print(test_df)

