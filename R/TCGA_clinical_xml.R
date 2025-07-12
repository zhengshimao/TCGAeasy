#' Parse TCGA Clinical XML File
#'
#' Parses a single TCGA XML clinical file and extracts six structured data frames:
#' admin, patient, drugs, radiations, follow_ups, and new tumor events (nte).
#'
#' @param xfile Path to a TCGA clinical XML file.
#'
#' @return A named list of tibbles with six elements:
#' \describe{
#'   \item{admin}{Basic metadata such as disease code and upload date.}
#'   \item{patient}{Patient-level clinical data.}
#'   \item{drugs}{Chemotherapy-related treatment information.}
#'   \item{radiations}{Radiation therapy information.}
#'   \item{follow_ups}{Follow-up visits data.}
#'   \item{nte}{New tumor event information.}
#' }
#' If a section (e.g., drugs, radiations) is not present, the corresponding element is `NA`.
#'
#' @importFrom xml2 read_xml xml_find_all xml_text xml_name xml_children
#' @importFrom dplyr as_tibble mutate relocate select bind_rows
#' @export
tcga_xml_parse <- function(xfile) {

  doc <- xml2::read_xml(x = xfile)
  
  node_admin <- xml2::xml_find_all(doc,xpath = ".//admin:admin")

    # 1.整理admin：方法二
  list_admin <- data.frame( 
    matrix(data = xml2::xml_text(xml2::xml_children(node_admin)), 
           nrow = 1, 
           ncol = length(xml2::xml_name(xml2::xml_children(node_admin)))
           )
    )
  colnames(list_admin) <- xml2::xml_name(xml2::xml_children(node_admin))
  list_admin <- dplyr::as_tibble(list_admin)
  list_admin[list_admin == ""] <- NA
  ## 转为整数
  for (x in c("day_of_dcc_upload","month_of_dcc_upload","year_of_dcc_upload")) {
    list_admin[[x]] <- as.integer(list_admin[[x]]) 
  }

  ## 2.整理病人基本信息：方法二 # 去掉最后的"new_tumor_events" "drugs" "radiations" "follow_ups"
  node2_xpath <- paste0(".//",tolower(list_admin$disease_code),":patient") # ".//gbm:patient"
  
  text_patient <- xml2::xml_find_all(doc, xpath = node2_xpath) %>% xml2::xml_children() %>% xml2::xml_text()
  name_patient <- xml2::xml_find_all(doc, xpath = node2_xpath) %>% xml2::xml_children() %>% xml2::xml_name()


  list_patient <- data.frame(
    matrix(data = text_patient,
           nrow = 1,
           ncol = length(name_patient)
    )
  )
  colnames(list_patient) <- name_patient
  list_patient <- dplyr::as_tibble(list_patient)
  list_patient[list_patient == ""] <- NA
  ## 转为数值
  to_num <- c("days_to_birth","days_to_death","days_to_last_followup",
              "age_at_initial_pathologic_diagnosis","year_of_initial_pathologic_diagnosis",
              "year_of_initial_pathologic_diagnosis",
              "karnofsky_performance_score"
              )
  for (x in to_num) {
    if(x %in% name_patient){
      list_patient[[x]] <- as.numeric(list_patient[[x]])
    }
  }
  # 去掉最后的几行；将 bcr_patient_barcode 放到第1列
  exclude_col <- c("new_tumor_events","drugs","radiations","follow_ups")
  list_patient <- list_patient %>% dplyr::select(-dplyr::all_of(exclude_col)) %>% dplyr::relocate("bcr_patient_barcode", .before = 1)
  
  ## 1. 整理admin: 增加 bcr_patient_barcode
  list_admin <- list_admin %>% dplyr::mutate("bcr_patient_barcode" = list_patient[["bcr_patient_barcode"]], .before =1 )

  ## 3.整理病人药物基本信息
  ## rx:drugs
  ## 含有多个节点therapy_types节点。therapy_types 节点虽然有子节点therapy_type，但是有text的子节点也只有一个，因此可以直接获取其text。
  node_drugs <- xml2::xml_find_all(doc, xpath = ".//rx:drugs") %>% xml2::xml_children()
  text_drugs <- node_drugs %>% xml2::xml_children() %>% xml2::xml_text()
  name_drugs <- node_drugs %>% xml2::xml_children() %>% xml2::xml_name()
  name_drugs_uniq <- name_drugs[!duplicated(name_drugs)]
  
  if(length(name_drugs) != 0){ # 有化疗信息时
    list_drugs <- data.frame(
      matrix(ncol = length(name_drugs_uniq), 
             nrow = max(table(name_drugs)))
    )
    colnames(list_drugs) <- name_drugs_uniq
    list_drugs <- list_drugs %>% dplyr::as_tibble()
    
    for (x in name_drugs_uniq) {
      if(x %in% name_drugs_uniq){
        list_drugs[[x]] <- text_drugs[name_drugs %in% x]
      }
    }
    # 转为数值
    to_num <- c("total_dose","prescribed_dose","number_cycles","days_to_drug_therapy_start","days_to_drug_therapy_end","day_of_form_completion","month_of_form_completion","year_of_form_completion")
    for (x in to_num) {
      if(x %in% name_drugs_uniq){ # 有该列再转换
        list_drugs[[x]] <- as.numeric(list_drugs[[x]])
      }
    }
    
    # 处理缺失值
    list_drugs[list_drugs == ""] <- NA
    # 新增 bcr_patient_barcode 列
    list_drugs <- list_drugs %>% dplyr::mutate("bcr_patient_barcode" = list_patient[["bcr_patient_barcode"]], .before =1 )
  }else{
    list_drugs <- NA # 无化疗信息时
  }
  
  ## 4. 整理病人放疗信息
  ## rad:radiation
  node_radiations <- xml2::xml_find_all(doc, xpath = ".//rad:radiations") %>% xml2::xml_children()
  text_radiations <- node_radiations %>% xml2::xml_children() %>% xml2::xml_text()
  name_radiations <- node_radiations %>% xml2::xml_children() %>% xml2::xml_name()
  name_radiations_uniq <- name_radiations[!duplicated(name_radiations)]
  
  if(length(name_radiations) != 0){ # 有放疗信息时
    list_radiations <- data.frame(
      matrix(ncol = length(name_radiations_uniq), 
             nrow = max(table(name_radiations)))
    )
    colnames(list_radiations) <- name_radiations_uniq
    list_radiations <- list_radiations %>% dplyr::as_tibble()
    
    for (x in name_radiations_uniq) {
      list_radiations[[x]] <- text_radiations[name_radiations %in% x]
    }
    # 转为数值
    to_num <- c("days_to_radiation_therapy_end","days_to_radiation_therapy_end",
                "radiation_dosage","numfractions","day_of_form_completion","month_of_form_completion","year_of_form_completion")
    
    for (x in to_num) {
      if(x %in% name_radiations_uniq){ # 有该列再转换
        list_radiations[[x]] <- as.numeric(list_radiations[[x]])
      }
    }
    # 处理缺失值
    list_radiations[list_radiations == ""] <- NA
    # 新增bcr_patient_barcode列
    list_radiations <- list_radiations %>% dplyr::mutate("bcr_patient_barcode" = list_patient[["bcr_patient_barcode"]], .before = 1)
  }else{
    list_radiations <- NA # 无放疗信息时
  }
  
  ## 5.整理随访信息
  node5_xpath <- paste0(".//",tolower(list_admin$disease_code),":follow_ups") # ".//gbm:follow_ups"
  node_followups <- xml2::xml_find_all(doc, xpath = node5_xpath) # %>% xml_children()
  text_followups <- node_followups %>% xml2::xml_children() %>% xml2::xml_children() %>% xml2::xml_text()
  name_followups <- node_followups %>% xml2::xml_children() %>% xml2::xml_children() %>% xml2::xml_name()
  name_followups_uniq <- name_followups[!duplicated(name_followups)]
  
  if(length(name_followups) != 0){  # 有随访信息时
    list_followups <- data.frame(
      matrix(ncol = length(name_followups_uniq), 
             nrow = max(table(name_followups)))
    )
    colnames(list_followups) <- name_followups_uniq
    list_followups <- list_followups %>% dplyr::as_tibble()
    
    for (x in name_followups_uniq) {
      list_followups[[x]] <- text_followups[name_followups %in% x]
    }
    # 转为数值
    to_num <- c("days_to_last_followup","days_to_death",
                "karnofsky_performance_score","eastern_cancer_oncology_group","day_of_form_completion","month_of_form_completion","year_of_form_completion")
    
    for (x in to_num) {
      if(x %in% name_followups_uniq){ # 有该列再转换
        list_followups[[x]] <- as.numeric(list_followups[[x]])
      }
    }
    # 处理缺失值
    list_followups[list_followups == ""] <- NA
    # 去掉最后的几行；将 bcr_patient_barcode 放到第1列
    exclude_col <- c("new_tumor_events")
    # 新增bcr_patient_barcode列
    list_followups <- list_followups %>% dplyr::select(-dplyr::all_of(exclude_col)) %>% dplyr::mutate("bcr_patient_barcode" = list_patient[["bcr_patient_barcode"]], .before = 1)
  }else{ # 无随访信息时
    list_followups <- NA 
  }
  
  # 6. 整理新发肿瘤事件信息
  node6_xpath <- paste0(".//",tolower(list_admin$disease_code),"_nte:new_tumor_event") # ".//gbm_nte:new_tumor_event"
  node_nte <- xml2::xml_find_all(doc, xpath = node6_xpath) # %>% xml_children()
  text_nte <- node_nte %>% xml2::xml_children() %>% xml2::xml_text()
  name_nte <- node_nte %>% xml2::xml_children() %>% xml2::xml_name()
  name_nte_uniq <- name_nte[!duplicated(name_nte)]
  
  if(length(name_nte) != 0){
    list_nte <- data.frame(
      matrix(ncol = length(name_nte_uniq), 
             nrow = max(table(name_nte)))
    )
    colnames(list_nte) <- name_nte_uniq
    list_nte <- list_nte %>% dplyr::as_tibble()
    
    for (x in name_nte_uniq) {
      list_nte[[x]] <- text_nte[name_nte %in% x]
    }
    # 转为数值
    to_num <- c("days_to_new_tumor_event_after_initial_treatment","days_to_new_tumor_event_additional_surgery_procedure" )
    
    for (x in to_num) {
      if(x %in% name_nte_uniq){ # 有该列再转换
        list_nte[[x]] <- as.numeric(list_nte[[x]])
      }
    }
    # 处理缺失值
    list_nte[list_nte == ""] <- NA
    
    # 新增bcr_patient_barcode列
    list_nte <- list_nte%>% dplyr::mutate("bcr_patient_barcode" = list_patient[["bcr_patient_barcode"]], .before = 1)
  }else{
    list_nte <- NA
  }
  
  
  return(list(admin      = list_admin,
              patient    = list_patient,
              drugs      = list_drugs,
              radiations = list_radiations,
              follow_ups = list_followups,
              nte        = list_nte
              ))
}  

#' Parse Multiple TCGA XML Files into List
#'
#' Applies \code{tcga_xml_parse()} to a vector of TCGA XML clinical files.
#'
#' @param xfiles A character vector of file paths to TCGA XML clinical files.
#'
#' @return A list where each element is the output of \code{tcga_xml_parse()}.
#'
#' @seealso \code{\link{tcga_xml_parse}}, \code{\link{tcga_xml2df}}, \code{\link{tcga_list2df}}
#' @export
# 读取多个xml文件为list
tcga_xml2list <- function(xfiles){
  lists <- lapply(seq_along(xfiles), function(x){
    message(x," ",xfiles[x])
    return(tcga_xml_parse(xfile = xfiles[x]))
  })
  return(lists)
}

#' Extract Specific Element from Multiple TCGA XML Files as Data Frame
#'
#' Parses multiple TCGA XML clinical files and merges a specific component
#' (e.g., patient, drugs) into a single data frame.
#'
#' @param xfiles A character vector of file paths to TCGA XML files.
#' @param element The name of the element to extract. One of:
#' \code{"admin"}, \code{"patient"}, \code{"drugs"},
#' \code{"radiations"}, \code{"follow_ups"}, or \code{"nte"}.
#'
#' @return A tibble combining the specified element from all XML files.
#' Skips any entries where the element is missing (i.e., \code{NA}).
#'
#' @seealso \code{\link{tcga_xml_parse}}, \code{\link{tcga_xml2list}}, \code{\link{tcga_list2df}}
#' @export
#' 
# 读取单个xml文件，将其中某个元素合并为df
tcga_xml2df <- function(xfiles, element){
  all_element <- names(tcga_xml_parse(xfiles[1])) # c("admin", "patient", "drugs", "radiations", "follow_ups", "nte" )

  # 检查element是否在对应list中
  if(!element %in% all_element){
    stop(paste0("'",element, "' not in elements '",paste0(all_element, collapse = ", "),"'"))
  }
  
  all_element_list <- lapply(seq_along(xfiles), function(x){
    message(x," ",xfiles[x])
    return(tcga_xml_parse(xfile = xfiles[x])[[element]])
  })
  
  all_element_df <- do.call(dplyr::bind_rows, all_element_list[!is.na(all_element_list)])
  return(all_element_df)
}

#' Merge Specific Element from Parsed XML Lists into Data Frame
#'
#' Given a list (as returned by \code{tcga_xml2list()}), extracts and combines
#' a specific component from all list elements into a single tibble.
#'
#' @param lists A list of parsed XML outputs (each from \code{tcga_xml_parse()}).
#' @param element The name of the element to extract. One of:
#' \code{"admin"}, \code{"patient"}, \code{"drugs"},
#' \code{"radiations"}, \code{"follow_ups"}, or \code{"nte"}.
#'
#' @return A tibble combining the specified element from all entries in the list.
#' Skips any entries where the element is missing (i.e., \code{NA}).
#'
#' @seealso \code{\link{tcga_xml_parse}}, \code{\link{tcga_xml2df}}
#' @export
#' 
# 将所有文件的list中的某个元素合并为df
tcga_list2df <- function(lists, element){
  all_element <- names(lists[[1]]) # c("admin", "patient", "drugs", "radiations", "follow_ups", "nte" )
  
  # 检查element是否在对应list中
  if(!element %in% all_element){
    stop(paste0("'",element, "' not in elements '",paste0(all_element, collapse = ", "),"'"))
  }
  
  all_element_list <- lapply(seq_along(lists), function(x){
    return(lists[[x]][[element]])
  })
  all_element_df <- do.call(dplyr::bind_rows, all_element_list[!is.na(all_element_list)])
}
