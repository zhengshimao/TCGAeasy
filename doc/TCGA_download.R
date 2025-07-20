## -----------------------------------------------------------------------------
#| label: setup
library(TCGAeasy)
tcga_download_clinic("TCGA-LGG")
query <- tcga_download_clinic("TCGA-LGG")
xfiles <- list.files("GDCdata/TCGA-LGG/Clinical/", pattern = "xml", recursive = TRUE, full.names = T)
clin_patient <- tcga_xml2df(xfiles = xfiles, element = "patient", verbose = FALSE)
print(clin_patient)
collectGarbage()


## -----------------------------------------------------------------------------
tcga_download_rna_seq_exp(project = "TCGA-LGG")
query <- tcga_download_rna_seq_exp(project = "TCGA-LGG")
print(query)
collectGarbage()


## -----------------------------------------------------------------------------
count <- get_tcga_count("TCGA-LGG", clean = F)
clean_count <- clean_exp(df = count)

clean_count <- get_tcga_count("TCGA-LGG", clean = TRUE)
print(clean_count)
collectGarbage()


## -----------------------------------------------------------------------------
tpm <- get_tcga_tpm("TCGA-LGG", clean = F)
clean_tpm <- clean_exp(df = tpm)

clean_tpm <- get_tcga_count("TCGA-LGG", clean = TRUE)
print(clean_tpm)
collectGarbage()

