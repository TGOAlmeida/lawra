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




################################################################################
# Create dataframe with due information

# df_loop <- total_file_data[2,]
get_basic_info <- function(df_loop){
  arq_pdf_2_read <- paste(df_loop$folder_path, df_loop$file_name, sep="/")
  client <- df_loop$client
  
  pdf_text_content <- pdf_text(arq_pdf_2_read)
  pdf_text_combined <- tolower(paste(pdf_text_content, collapse = "\n"))
  
  all_dates_fmt_1 <- str_extract_all(pdf_text_combined, "\\b\\d{2}/\\d{2}/\\d{4}\\b")
  #all_dates_fmt_2 <- str_extract_all(pdf_text_combined, "\\b\\d{2}/\\d{2}/\\d{2}\\b")
  
  all_dates <- unlist(c(all_dates_fmt_1))
  all_dates_conc <- paste(all_dates, collapse = " | ")
  all_dates_d <- as.Date(all_dates, format = "%d/%m/%Y")
  
  min_date <- min(all_dates_d)
  max_date <- max(all_dates_d)
  
  cnpj <- unlist(str_extract_all(pdf_text_combined, "(?<=cnpj: )\\d+[\\d\\.\\/]"))

  cnpj <- unlist(str_extract_all(pdf_text_combined, "(?<=cnpj: d{2}.d{3}.d{3}/d{4}-d{2}$"))

  cpf_cnpj_credor <- cpf_cnpj[1]
  cpf_cnpj_devedor <- cpf_cnpj[2]
  
  nome_do_credor <- str_extract(pdf_text_combined, "(?<=nome do credor:).*")
  nome_do_devedor <- str_extract(pdf_text_combined, "(?<=nome do devedor:).*")
  valor_protesto <- str_extract(pdf_text_combined, "(?<=valor protesto:).*")
  
  nome_do_credor <- str_trim(nome_do_credor)
  nome_do_devedor <- str_trim(nome_do_devedor)
  
  valor_protesto <- str_trim(valor_protesto)
  valor_protesto <- gsub("[^0-9,]+", "", valor_protesto)
  valor_protesto <- gsub(",", ".", valor_protesto)
  valor_protesto <- as.numeric(valor_protesto)
  
  date_e_t <- str_extract(pdf_text_combined, "(?<=data da emissão: )\\d{2}/\\d{2}/\\d{4}")
  date_v_t <- str_extract(pdf_text_combined, "(?<=data da vencimento: )\\d{2}/\\d{2}/\\d{4}")
  
  dt_emis <- as.Date(date_e_t, format = "%d/%m/%Y")
  dt_vcmto <- as.Date(date_v_t, format = "%d/%m/%Y")
  
  final_df <- data.frame(
    arquivo = arq_pdf_2_read,
    file = df_loop$file_name,
    cliente = client,
    cpf_cnpj_credor = cpf_cnpj_credor,
    nome_do_credor = nome_do_credor,
    cpf_cnpj_devedor = cpf_cnpj_devedor,
    nome_do_devedor = nome_do_devedor,
    dt_min = min_date,
    dt_max = max_date,
    dt_emissao = dt_emis,
    dt_vencimento = dt_vcmto,
    valor_protesto = valor_protesto
  )
  final_df
}

################################################################################
# Get all information from pdf
get_info_pdf <- function(arq_pdf_2_read){

  pdf_text_content <- pdf_text(arq_pdf_2_read)
  first_page <- pdf_text_content[1]
  list_fp <- str_split(tolower(paste(first_page, collapse = "\n")), "\n")
  list_fp <- lapply(list_fp, str_trim)
  
  raz_soc   <- get_specific_text(list_fp, "razão social:")
  cnpj      <- get_specific_text(list_fp, "cnpj:")
  insc_est  <- get_specific_text(list_fp, "inscrição estadual:")
  end       <- get_specific_text(list_fp, "endereço:")
  cid_uf    <- get_specific_text(list_fp, "cidade / estado:")
  cep       <- get_specific_text(list_fp, "cep:")
  tel       <- get_specific_text(list_fp, "telefone:")
  email     <- get_specific_text(list_fp, "e-mail:")
  rep_legal <- get_specific_text(list_fp, "representante legal:")
  cargo     <- get_specific_text(list_fp, "cargo:")
  cpf       <- get_specific_text(list_fp, "cpf:")
  dt_sign   <- get_specific_text(list_fp, "uberlândia,")
  dt_sign <- str_replace_all(dt_sign, 
                                 c(" de " = "/", 
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
  dt_sign <- str_remove_all(dt_sign, "[.]")
  

  
  
  df <- data.frame(raz_soc, cnpj, insc_est, end, cid_uf, cep, tel, email, rep_legal, cargo, cpf, dt_sign)
  df
  
}

get_specific_text <- function(x, pt){
  x <- unlist(x)
  y = "Not found"
  regex_pattern <- paste0("(?<=", pt, ").*")
  try(y <- stringr::str_trim(stringr::str_extract(stringr::str_subset(x, pt)[1], regex_pattern)))
  return(y)
}




################################################################################
# Function to get file details
get_file_details <- function(dir_path) {
  files <- list.files(path = dir_path, full.names = TRUE, recursive = TRUE)
  files_info <- file.info(files)
  files_df <- data.frame(
    File = files,
    Creation_Date = as.character(files_info$ctime),
    Size_MB = round(files_info$size / (1024 * 1024), 2)
  )
  return(files_df)
}
