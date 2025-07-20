#' Extract and optionally write manifest from a GDC query result
#'
#' This function extracts the manifest (including file ID, name, md5, size, and state)
#' from the `TCGAbiolinks::GDCquery()` result. If the `manifest` argument is not provided,
#' the manifest will be saved as a tab-delimited file in the working directory.
#'
#' @param query A query object returned by `TCGAbiolinks::GDCquery()`.
#' @param manifest (Optional) A path to save a manifest file. If missing,
#'   the manifest will be generated and written to disk.
#' @param download_dir Directory to store downloaded files. Default is `"GDCdata"`.
#'
#' @return If `manifest` is missing, returns the file path to the saved manifest file.
#'   Otherwise, returns the original manifest path.
#' @export

get_manifest <- function (query, manifest, download_dir = "GDCdata"){

  manifest_df <- query$results[[1]][, c("file_id", "file_name",
                                     "md5sum", "file_size", "state")]
  colnames(manifest_df) <- c("id", "filename", "md5", "size",
                          "state")
  if (missing(manifest)) {
    if(is.na(query$workflow.type)){
      manifest <- paste0(query$project[[1]],"_",gsub(" ","_",query$data.category),"_",gsub(" ","_",query$data.type),"_",gsub(" ","_",query$results[[1]][["data_format"]][1]) )
    }else{
      manifest <- paste0(query$project[[1]],"_",gsub(" ","_",query$data.category),"_",gsub(" ","_",query$data.type),"_",gsub(" ","_",query$results[[1]][["data_format"]][1]),"_",gsub(" ","_",query$workflow.type) )
    }

    if(!is.na(query$platform[[1]])){
      manifest <- paste0(manifest, "_",gsub(" ","_",query$platform[[1]]))
    }
    manifest <- paste0(manifest, "_gdc_manifest.txt")

    manifest <- file.path(download_dir, manifest)
    readr::write_delim(manifest_df, manifest, delim = "\t", col_names = TRUE)
    message("Manifest saved as: ", manifest)
  }
  return(manifest)
}

#' Download TCGA data using TCGAbiolinks
#'
#' This function queries and optionally downloads TCGA data from GDC using the TCGAbiolinks package.
#' It supports retrieving only the manifest file or both the manifest and data files.
#'
#' @param project A character string specifying the TCGA project, e.g., `"TCGA-LGG"`.
#' @param data.category The data category, e.g., `"Clinical"`.
#' @param data.type The data type, e.g., `"Clinical Supplement"`.
#' @param data.format The file format, e.g., `"BCR XML"`.
#' @param workflow.type GDC workflow type.
#' @param platform same with \link[TCGAbiolinks]{GDCquery}
#' @param query_rds save query to RDS.
#' @param manifest (Optional) A path to save a manifest file. If missing, it will be generated.
#' @param manifest_only Logical; if `TRUE`, only the manifest file will be generated without downloading data.
#' @param download_dir Directory to store downloaded files. Default is `"GDCdata"`.
#' @param ... \link[TCGAbiolinks]{GDCquery}
#'
#' @return Returns the internal query object invisibly. Nothing is printed unless assigned to a variable.
#' @export
tcga_download <- function(project,
                          data.category,
                          data.type,
                          data.format,
                          workflow.type,
                          platform,
                          query_rds,
                          manifest,
                          manifest_only = FALSE,
                          download_dir = "GDCdata",
                          ...
                          ){
  if(!dir.exists(download_dir)) dir.create(path = download_dir, showWarnings = TRUE, recursive = TRUE)
  if(missing(query_rds)){
    if(missing(workflow.type)){
      query_rds <- paste0(project,"_",gsub(" ","_",data.category),"_",gsub(" ","_",data.type),"_",gsub(" ","_",data.format))
    }else{
      query_rds <- paste0(project,"_",gsub(" ","_",data.category),"_",gsub(" ","_",data.type),"_",gsub(" ","_",data.format),"_",gsub(" ","_",workflow.type))
    }

    if(!missing(platform)){
      query_rds <- paste0(query_rds, "_",gsub(" ","_",platform))
    }

    query_rds <- paste0(query_rds,"_query.rds")
    query_rds <- file.path(download_dir, query_rds)
  }
  # query_value <- paste0(project,"_",data.category,"_",gsub(" ","_",data.format),"_query")

  if(file.exists(query_rds)){
    query <- readRDS(file = query_rds)
  }else{
    query <- TCGAbiolinks::GDCquery(
      project = project,
      data.category = data.category,
      data.type = data.type,
      data.format = data.format,
      workflow.type = workflow.type,
      platform = platform,
      ...
    )

    # assign(query_value,
    #        query)
    # saveRDS(get(query_value), file = query_rds)
    saveRDS(query, file = query_rds)
  }

  get_manifest(query = query, manifest = manifest, download_dir = download_dir)

  if(!manifest_only){
    TCGAbiolinks::GDCdownload(query = query,
                              method = "api",
                              directory = download_dir,
                              files.per.chunk = 50
    )
  }
  invisible(query)
}

#' Download TCGA Clinical Supplement Data (BCR XML format)
#'
#' This function provides a convenient wrapper to download **TCGA Clinical Supplement**
#' data in **BCR XML** format using the `TCGAbiolinks` package. It supports downloading
#' either just the manifest file or the full dataset.
#'
#' @param project A `character` string specifying the TCGA project ID (e.g., `"TCGA-LGG"`).
#' @param manifest An optional `character` file path to save a manifest file.
#'   If not provided, the manifest will be generated from the `GDCquery` result.
#' @param download_dir A `character` string specifying the directory where data
#'   should be downloaded. Default is `"GDCdata"`.
#' @param manifest_only A `logical` value indicating whether to only create the
#'   manifest file (`TRUE`) or also download the data (`FALSE`). Default is `FALSE`.
#'
#' @return Returns the internal query object invisibly. Nothing is printed unless assigned to a variable.
#'  The function performs side effects: writing a manifest file and/or downloading data into the specified directory.
#'
#' @examples
#' \dontrun{
#'   tcga_download_clinic(
#'     project = "TCGA-LGG",
#'     manifest_only = TRUE
#'   )
#' }
#'
#' @export
tcga_download_clinic <- function(project,
                                 manifest,
                                 download_dir = "GDCdata",
                                 manifest_only = FALSE
                                 ){
  query <- tcga_download(project,
                         data.category = "Clinical",
                         data.type = "Clinical Supplement",
                         data.format = "BCR XML",
                         manifest = manifest,
                         manifest_only = manifest_only,
                         download_dir = download_dir)
  invisible(query)
}

#' Download TCGA mRNA expression quantification data
#'
#' This function downloads gene expression quantification data
#' (STAR-counts format) from the GDC database for a specified TCGA project.
#'
#' @param project Character. TCGA project ID (e.g., `"TCGA-LGG"`).
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Character. Directory to store downloaded files. Default is `"GDCdata"`.
#' @param manifest_only Logical. If `TRUE`, only generate the manifest without downloading. Default is `FALSE`.
#'
#' @return A query object returned invisibly (for optional re-use).
#' @export
tcga_download_mrna_seq_exp <- function(project,
                                      manifest,
                                      download_dir = "GDCdata",
                                      manifest_only = FALSE){
  query <- tcga_download(project = project,
                         data.category = "Transcriptome Profiling",
                         data.type = "Gene Expression Quantification",
                         data.format = "STAR - Counts",
                         manifest = manifest,
                         manifest_only = manifest_only,
                         download_dir = download_dir)
  invisible(query)
}

#' Clean expression matrix from raw gene expression data
#'
#' This function removes unwanted rows and columns from a gene expression dataframe,
#' and optionally removes Ensembl version suffixes and sets row names.
#'
#' @param df A data.frame or tibble containing gene expression data.
#' @param exclude_cols A character vector of column names to remove from `df`. Defaults to \code{c("gene_name", "gene_type")}.
#' @param row_name The column name to be used as row names. Default is \code{"gene_id"}.
#' @param remove_ensembl_id_version Logical. Whether to remove Ensembl version numbers (e.g., ".1") from gene IDs. Default is \code{TRUE}.
#' @param ensembl_id The column name containing Ensembl IDs. Defaults to the value of `row_name`.
#'
#' @return A cleaned data.frame with gene IDs as row names, filtered rows and optional columns removed.
#' @export

clean_exp <- function(df,
                      exclude_cols = c("gene_name","gene_type"),
                      row_name = "gene_id",
                      remove_ensembl_id_version = TRUE,
                      ensembl_id = row_name
                      ){
  df <- df[!stringr::str_detect(df[["gene_id"]], "_PAR_Y$"),]
  df <- df %>% dplyr::filter(! df[["gene_id"]] %in% c("N_ambiguous",
                                                     "N_multimapping",
                                                     "N_noFeature",
                                                     "N_unmapped"))
  if(!is.null(exclude_cols)){
    exclude_cols <- exclude_cols[exclude_cols %in% colnames(df)]
    if(length(exclude_cols) > 0){
      df <- df %>% dplyr::select(-dplyr::all_of(exclude_cols))
    }
  }

  if(remove_ensembl_id_version){
    df[[ensembl_id]] <- df[[ensembl_id]] %>% stringr::str_remove("\\.\\d+")
  }

  if(!is.null(row_name)){
    df <- df %>% tibble::column_to_rownames(var = row_name)
  }

  return(df)
}

#' Extract raw expression counts from TCGA mRNA
#'
#' This function downloads and parses raw gene expression count data from TCGA.
#'
#' @param project Character. TCGA project ID.
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Character. Directory for downloaded files. Default is `"GDCdata"`.
#' @param clean Logical. If `TRUE`, remove unwanted gene entries. Default is `TRUE`.
#'
#' @return A data frame of raw expression counts with gene annotations.
#' @export
get_tcga_mrna_count <- function(project,
                           manifest,
                           download_dir = "GDCdata",
                           clean = TRUE
                           ){
  query <- tcga_download_mrna_seq_exp(project = project,
                                     manifest = manifest,
                                     download_dir = download_dir,
                                     manifest_only = FALSE)
  exp <- TCGAbiolinks::GDCprepare(query, directory = download_dir, summarizedExperiment = FALSE)

  # if(any(stringr::str_detect(colnames(exp), "^unstranded_"))){
  #   prefix <- "unstranded_" # mRNA
  # }else if(any(stringr::str_detect(colnames(exp), "^read_count"))){
  #   prefix <- "read_count_" # miRNA
  # }
  prefix <- "unstranded_"
  count <- exp %>%
    dplyr::select("gene_id","gene_name","gene_type", tidyselect::starts_with(match = prefix))

  colnames(count) %<>% stringr::str_remove(prefix)
  if(clean){
    count <- clean_exp(count)
  }
  return(count)
}

#' Extract FPKM expression matrix from TCGA mRNA
#'
#' This function downloads and parses FPKM-normalized expression data from TCGA.
#'
#' @param project Character. TCGA project ID.
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Character. Directory for downloaded files. Default is `"GDCdata"`.
#' @param clean Logical. If `TRUE`, remove unwanted gene entries. Default is `TRUE`.
#'
#' @return A data frame of TPM values with gene annotations.
#' @export
get_tcga_mrna_fpkm <- function(project,
                           manifest,
                           download_dir = "GDCdata",
                           clean = TRUE
){
  query <- tcga_download_mrna_seq_exp(project = project,
                                     manifest = manifest,
                                     download_dir = download_dir,
                                     manifest_only = FALSE)
  exp <- TCGAbiolinks::GDCprepare(query, directory = download_dir, summarizedExperiment = FALSE)
  fpkm <- exp %>% dplyr::select("gene_id","gene_name","gene_type", tidyselect::starts_with(match = "fpkm_unstranded"))
  colnames(fpkm) %<>% stringr::str_remove("fpkm_unstranded")
  if(clean){
    fpkm <- clean_exp(fpkm)
  }
  return(fpkm)
}

#' Extract TPM expression matrix from TCGA mRNA
#'
#' This function downloads and parses TPM-normalized expression data from TCGA.
#'
#' @param project Character. TCGA project ID.
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Character. Directory for downloaded files. Default is `"GDCdata"`.
#' @param clean Logical. If `TRUE`, remove unwanted gene entries. Default is `TRUE`.
#'
#' @return A data frame of TPM values with gene annotations.
#' @export
get_tcga_mrna_tpm <- function(project,
                         manifest,
                         download_dir = "GDCdata",
                         clean = TRUE

){
  query <- tcga_download_mrna_seq_exp(project = project,
                                     manifest = manifest,
                                     download_dir = download_dir,
                                     manifest_only = FALSE)
  exp <- TCGAbiolinks::GDCprepare(query, directory = download_dir, summarizedExperiment = FALSE)
  tpm <- exp %>% dplyr::select("gene_id","gene_name","gene_type", tidyselect::starts_with(match = "tpm_unstranded_"))
  colnames(tpm) %<>% stringr::str_remove("tpm_unstranded_")
  if(clean){
    tpm <- clean_exp(tpm)
  }
  return(tpm)
}

#' Download TCGA miRNA-seq expression data
#'
#' This function downloads miRNA expression quantification data from TCGA using a manifest file.
#'
#' @param project TCGA project ID (e.g., "TCGA-BRCA").
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Directory where the downloaded data will be saved. Default is "GDCdata".
#' @param manifest_only Logical. If TRUE, only the manifest file is generated without actual download. Default is FALSE.
#'
#' @return Returns invisibly the query object generated by `tcga_download()`.
#' @export
tcga_download_mirna_seq_exp <- function(project,
                                       manifest,
                                       download_dir = "GDCdata",
                                       manifest_only = FALSE){
  query <- tcga_download(project = project,
                         data.category = "Transcriptome Profiling",
                         data.type = "miRNA Expression Quantification",
                         data.format = "txt",
                         workflow.type = "BCGSC miRNA Profiling",
                         manifest = manifest,
                         manifest_only = manifest_only,
                         download_dir = download_dir)
  invisible(query)
}

#' Extract raw expression counts from TCGA miRNA
#'
#' This function downloads and parses raw gene expression count data from TCGA miRNA.
#'
#' @param project Character. TCGA project ID.
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Character. Directory for downloaded files. Default is `"GDCdata"`.
#'
#' @return A data frame of raw expression counts with gene annotations.
#' @export
get_tcga_mirna_count <- function(project,
                                manifest,
                                download_dir = "GDCdata"
){
  query <- tcga_download_mirna_seq_exp(project = project,
                                      manifest = manifest,
                                      download_dir = download_dir,
                                      manifest_only = FALSE)
  exp <- TCGAbiolinks::GDCprepare(query, directory = download_dir, summarizedExperiment = FALSE)

  # if(any(stringr::str_detect(colnames(exp), "^unstranded_"))){
  #   prefix <- "unstranded_" # mRNA
  # }else if(any(stringr::str_detect(colnames(exp), "^read_count"))){
  #   prefix <- "read_count_" # miRNA
  # }
  prefix <- "read_count_"
  count <- exp %>%
    dplyr::select("miRNA_ID", tidyselect::starts_with(match = prefix))

  colnames(count) %<>% stringr::str_remove(prefix)

  return(count)
}

#' Extract RPM expression matrix from TCGA miRNA
#'
#' This function downloads and parses FPKM-normalized expression data from TCGA.
#'
#' @param project Character. TCGA project ID.
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Character. Directory for downloaded files. Default is `"GDCdata"`.
#'
#' @return A data frame of RPM values with gene annotations.
#' @export
get_tcga_mirna_rpm <- function(project,
                              manifest,
                              download_dir = "GDCdata"

){
  query <- tcga_download_mirna_seq_exp(project = project,
                                      manifest = manifest,
                                      download_dir = download_dir,
                                      manifest_only = FALSE)
  exp <- TCGAbiolinks::GDCprepare(query, directory = download_dir, summarizedExperiment = FALSE)
  prefix <- "reads_per_million_miRNA_mapped_"
  rpm <- exp %>% dplyr::select("miRNA_ID", tidyselect::starts_with(match = prefix))
  colnames(rpm) %<>% stringr::str_remove(prefix)
  return(rpm)
}

#' Download TCGA DNA methylation beta values
#'
#' This function downloads DNA methylation beta value data from TCGA using a manifest file.
#'
#' @param project TCGA project ID (e.g., "TCGA-CHOL").
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Directory where the downloaded data will be saved. Default is "GDCdata".
#' @param platform Platform name for DNA methylation data (e.g., "Illumina Human Methylation 450" or "Illumina Human Methylation 27").
#' @param manifest_only Logical. If TRUE, only the manifest file is generated without actual download. Default is FALSE.
#'
#' @return Returns invisibly the query object generated by `tcga_download()`.
#' @export
#' @examples
#' \dontrun{
#'
#' query <- tcga_download_methy_beta(project = "TCGA-CHOL", platform = "Illumina Human Methylation 450")
#' beta <- TCGAbiolinks::GDCprepare(query, directory = "GDCdata", summarizedExperiment = FALSE)
#'
#' }
#'
tcga_download_methy_beta <- function(project,
                                     manifest,
                                     download_dir = "GDCdata",
                                     platform = "Illumina Human Methylation 450",
                                     manifest_only = FALSE){
  query <- tcga_download(project = project,
                         data.category = "DNA Methylation",
                         data.type = "Methylation Beta Value",
                         data.format = "txt",
                         workflow.type = "SeSAMe Methylation Beta Estimation",
                         platform = platform,
                         manifest = manifest,
                         manifest_only = manifest_only,
                         download_dir = download_dir)
  invisible(query)
}

#' Download TCGA copy number variation (CNV) data
#'
#' This function downloads CNV data from TCGA using a manifest file.
#'
#' @param project TCGA project ID (e.g., "TCGA-LUAD").
#' @param data.type The type of CNV data to download ("Copy Number Segment" or "Masked Copy Number Segment"). Default is "Copy Number Segment".
#' @param workflow.type Workflow type used to generate the CNV data ("DNAcopy" or "GATK4 CNV"). Default is "DNAcopy".
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Directory where the downloaded files will be saved. Default is "GDCdata".
#' @param manifest_only Logical. If TRUE, only the manifest file is generated without downloading the data. Default is FALSE.
#'
#' @return Returns invisibly the query object created by `tcga_download()`.
#' @export
tcga_download_cnv <- function(project,
                              data.type = "Copy Number Segment",
                              workflow.type = "DNAcopy",
                              manifest,
                              download_dir = "GDCdata",
                              manifest_only = FALSE){
  query <- tcga_download(project = project,
                         data.category = "Copy Number Variation",
                         data.type = data.type,
                         data.format = "txt",
                         workflow.type = workflow.type,
                         manifest = manifest,
                         manifest_only = manifest_only,
                         download_dir = download_dir
                         )
  invisible(query)
}

#' Download TCGA protein expression data
#'
#' This function downloads protein expression quantification data from TCGA using a manifest file.
#'
#' @param project TCGA project ID (e.g., "TCGA-CHOL").
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Directory where the downloaded files will be saved. Default is "GDCdata".
#' @param manifest_only Logical. If TRUE, only the manifest file is generated without downloading the data. Default is FALSE.
#'
#' @return Returns invisibly the query object created by `tcga_download()`.
#' @export
tcga_download_cnv <- function(project,
                              manifest,
                              download_dir = "GDCdata",
                              manifest_only = FALSE){
  query <- tcga_download(project = project,
                         data.category = "Proteome Profiling",
                         data.type = "Protein Expression Quantification",
                         data.format = "tsv",
                         manifest = manifest,
                         manifest_only = manifest_only,
                         download_dir = download_dir
  )
  invisible(query)
}

#' Download TCGA somatic mutation (SNP) data
#'
#' This function downloads masked somatic mutation data (SNP) from TCGA using a manifest file.
#' Only open-access MAF files are supported.
#'
#' @param project TCGA project ID (e.g., "TCGA-CHOL").
#' @param manifest A file path of the manifest file used for download.
#' @param download_dir Directory where the downloaded files will be stored. Default is "GDCdata".
#' @param manifest_only Logical. If TRUE, only generates the manifest file without downloading data. Default is FALSE.
#'
#' @return Returns invisibly the query object created by `tcga_download()`.
#' @export
tcga_download_snp <- function(project,
                              manifest,
                              download_dir = "GDCdata",
                              manifest_only = FALSE){
  query <- tcga_download(project = project,
                         data.category = "Simple Nucleotide Variation",
                         data.type = "Masked Somatic Mutation", # 只有该类型的开放,
                         # "Masked Somatic Mutation", "Aggregated Somatic Mutation", "Annotated Somatic Mutation",
                         # "Raw Simple Somatic Mutation","Simple Germline Variation"
                         data.format = "maf", # maf, vcf, tsv # 都是一个结果
                         manifest = manifest,
                         manifest_only = manifest_only,
                         download_dir = download_dir,
                         access = "open"
  )
  invisible(query)
}


# query1 <- tcga_download(project = "TCGA-CHOL",
#                        data.category = "Simple Nucleotide Variation",
#                        data.type = "Masked Somatic Mutation", # 只有该类型的开放
#                        # "Masked Somatic Mutation", "Aggregated Somatic Mutation", "Annotated Somatic Mutation",
#                        # "Raw Simple Somatic Mutation","Simple Germline Variation"
#                        data.format = "maf", # maf, vcf, tsv # 都是一个结果
#                        access = "open"
# )

# df1 <- TCGAbiolinks::GDCprepare(query1, directory = "GDCdata", summarizedExperiment = FALSE)

# query2 <- tcga_download(project = "TCGA-CHOL",
#                         data.category = "Simple Nucleotide Variation", # 不让下
#                         data.type = "Annotated Somatic Mutation",
#                         # "Masked Somatic Mutation", "Aggregated Somatic Mutation", "Annotated Somatic Mutation",
#                         # "Raw Simple Somatic Mutation","Simple Germline Variation"
#                         data.format = "vcf", # maf, vcf, tsv
#                         access = "open"
# )
#
# df2 <- TCGAbiolinks::GDCprepare(query2, directory = "GDCdata", summarizedExperiment = FALSE)


