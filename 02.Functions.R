############################################################################################
get_specific_text <- function(x, pt){
  x <- unlist(x)
  y = "Not found"
  regex_pattern <- paste0("(?<=", pt, ").*")
  try(y <- stringr::str_trim(stringr::str_extract(stringr::str_subset(x, pt)[1], regex_pattern)))
  return(y)
}

############################################################################################
get_target_values <- function(pdf_text_content_Ori)
{
  # -- List of titles from document ---------------------------------------------
  title_list <- c(
  "1. CLÁUSULA I - OBJETO",
  "2. CLÁUSULA II - DA EXECUÇÃO CONTRATUAL",
  "3. CLÁUSULA III - DO VALOR CONTRATUAL E CONDIÇÕES DE PAGAMENTO",
  "4. CLÁUSULA IV - DOS REAJUSTES",
  "5. CLÁUSULA V - DA VIGÊNCIA",
  "6. CLÁUSULA VI - DAS OBRIGAÇÕES DA CONTRATADA (ITMS)",
  "7. CLÁUSULA VII - DAS OBRIGAÇÕES DO CONTRATANTE",
  "8. CLÁUSULA VIII - DA RESCISÃO CONTRATUAL",
  "9. CLÁUSULA IX - DA MULTA",
  "10. CLÁUSULA X – TRATAMENTO DE DADOS",
  "11. CLÁUSULA XI - DAS DISPOSIÇÕES GERAIS",
  "12. CLÁUSULA XII - DO FORO"
  )
  df_title <- data.frame(title_id = 1:length(title_list), title_txt = title_list)


  # -- Standarizartion ----------------------------------------------------------
  pdf_text_content_Ori <- unlist(lapply(pdf_text_content_Ori, str_trim))
  pdf_text_content     <- tolower(pdf_text_content_Ori)

  # -- Matrix of title identification -------------------------------------------
  df_aux <- matrix(NA, nrow = length(pdf_text_content_Ori), ncol = length(title_list))
  for (i in seq_along(pdf_text_content_Ori)) {
    for (j in seq_along(title_list)) {
      if(str_detect(pdf_text_content_Ori[i], fixed(title_list[j]) )){
        df_aux[i, j] <- 1
      }
      else {
        df_aux[i, j] <- 0
      }
    }
  }
  first_true_column <- apply(df_aux, 1, function(x) {
    non_zero_indices <- which(x != 0) 
    if (length(non_zero_indices) > 0) {
      return(non_zero_indices[1])
    } else {
      return(0) 
    }
  })
  last_non_zero <- 0
  title_id <- sapply(first_true_column, function(element) {
    if (element != 0) {
      last_non_zero <<- element
    }
    return(last_non_zero)
  })
  df_content <- data.frame(       line_id          = 1:length(pdf_text_content_Ori), 
                                  original_text    = pdf_text_content_Ori, 
                                  lower_text       = pdf_text_content,
                                  title_id
                                  )
  
  # -- Target search ------------------------------------------------------------  
  raz_soc    <- get_specific_text(pdf_text_content, "razão social:")
  cnpj       <- get_specific_text(str_remove_all(pdf_text_content," "), "cnpj/cpf:")
  cnpj       <- ifelse(is.na(cnpj), get_specific_text(str_remove_all(pdf_text_content," "), "cnpj:"), cnpj)
  
  insc_est   <- get_specific_text(pdf_text_content, "inscrição estadual:")
  endereco   <- get_specific_text(pdf_text_content, "endereço:")
  cid_uf     <- get_specific_text(pdf_text_content, "cidade / estado:")
  cep        <- get_specific_text(pdf_text_content, "cep:")
  tel        <- get_specific_text(pdf_text_content, "telefone:")
  email      <- get_specific_text(str_remove_all(pdf_text_content," "), "e-mail:")
  resp_legal <- get_specific_text(pdf_text_content, "representante legal:")
  
  cargo_cpf  <- get_specific_text(str_remove_all(pdf_text_content," "), "cargo/cpf:")
  
  cargo      <- ifelse(!is.na(cargo_cpf), str_split("cargo/cpf:", "/")[[1]][1], 
                       get_specific_text(pdf_text_content, "cargo:"))
  cpf        <- ifelse(!is.na(cargo_cpf), str_split("cargo/cpf:", "/")[[1]][2], 
                       get_specific_text(pdf_text_content, "cpf:"))

  flag_aditivo <- ifelse(str_detect(pdf_text_content[1:10], "aditivo", "contrato"))
  

  y = "Not found"
  pt <- "uberlândia,(\\d+)de"
  x <- stringr::str_subset(str_remove_all(pdf_text_content," "), pt)
  x <- stringr::str_split(x,"\n")
  x <- stringr::str_subset(unlist(x), pt)
  try(y <- stringr::str_trim(stringr::str_extract(x, "(?<=uberlândia,).*")))
  dt_sign <- str_replace_all(y, 
                                  c( 
                                    "janeiro" = "01", 
                                    "fevereiro" = "02", 
                                    "março" = "03", 
                                    "abril" = "04", 
                                    "maio" = "05", 
                                    "junho" = "06", 
                                    "julho" = "07", 
                                    "agosto" = "08", 
                                    "setembro" = "09", 
                                    "outubro" = "10", 
                                    "novembro" = "11", 
                                    "dezembro" = "12"))
  dt_sign <- str_replace_all(dt_sign,"de", "/")

  dt_sign <- str_remove_all(dt_sign, "[.]")

  ind_reajuste <- str_split(get_specific_text(pdf_text_content, "positiva do"), " ")[[1]][1]

  autoriza_uso_marca <- ifelse(str_detect(paste(pdf_text_content[which(title_id==6)], collapse=" "), "ressalvado o direito da contratada"),1,0)

  possui_vencimento <- ifelse(str_detect(paste(pdf_text_content[which(title_id<=12)], collapse=" "), "prazo indeterminado"),0,1)

  volume_minimo <- str_split(get_specific_text(paste(pdf_text_content[which(title_id==3)], collapse=" "), "transmitir até"), " ")[[1]][1]

  valor_contrato <- str_split(get_specific_text(paste(pdf_text_content[which(title_id==3)], collapse=" "), "o valor do presente contrato é de r[$]"), " ")[[1]][1]
  if(flag_aditivo)
  {
    valor_contrato <- str_split(get_specific_text(paste(pdf_text_content, collapse=" "), "r[$]"), " ")[[1]][1]
  }
  
  

  
  df <- data.frame(
                    flag_aditivo,
                    raz_soc, 
                    cnpj, 
                    insc_est, 
                    endereco, 
                    cid_uf, 
                    cep, 
                    tel, 
                    email, 
                    resp_legal, 
                    cargo, 
                    cpf, 
                    dt_sign, 
                    ind_reajuste, 
                    autoriza_uso_marca, 
                    possui_vencimento, 
                    volume_minimo, 
                    valor_contrato
                  )

  return(df)
}


################################################################################
# Check if pdf is a readble file
is_pdf_readable <- function(pdf_path) {
  tryCatch({
    # Extract text from the PDF
    text <- pdf_text(pdf_path)
    return(any(nchar(text) > 0))  # Check if any text was extracted
  }, error = function(e) {
    return(FALSE)  # Return FALSE if an error occurs (e.g., non-readable PDF)
  })
}


################################################################################
# Check the file creation date
get_file_times <- function(file_path) {
  file_info <- file.info(file_path)
  atime <- file_info$atime
  mtime <- file_info$mtime
  ctime <- file_info$ctime
  return(list(atime, mtime, ctime))
}


################################################################################
# Check if the file is a pdf
is_pdf <- function(file_path) {
  extension <- tools::file_ext(file_path)
  tolower(extension) == "pdf"
}

################################################################################
# Create a dataframe with every element in a file list
create_df_list_of_files <- function(file_list){
  file_data <- data.frame(
    folder_path = dirname(file_list),
    file_name = basename(file_list)
  )
  file_data <- file_data %>%  
    mutate(full_file_path = paste(folder_path, file_name, sep = "/")) %>%
    mutate(is_pdf = is_pdf(full_file_path)) %>%
    mutate(readable_pdf = sapply(full_file_path, is_pdf_readable)) %>%
    mutate(client = basename(folder_path))

  aux_times <- get_file_times(file_data$full_file_path)
  names(aux_times) <- c("atime","mtime","ctime")

  df_times <- data.frame(aux_times$atime, aux_times$mtime, aux_times$ctime)
  names(df_times) <- c("atime","mtime","ctime")
  file_data <- cbind(file_data, df_times) %>%
    mutate(modification_file = as.POSIXct(mtime, origin = "1970-01-01", tz = "UTC")) %>%
    mutate(last_status_change_file = as.POSIXct(ctime, origin = "1970-01-01", tz = "UTC")) %>%
    mutate(last_access_file = as.POSIXct(atime, origin = "1970-01-01", tz = "UTC")) %>%
    select(-c(atime,mtime,ctime))
  rownames(file_data) <- NULL
  file_data
}

