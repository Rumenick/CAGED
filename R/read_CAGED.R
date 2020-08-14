#' @title Verifica se os dados e/ou endereço estão disponíveis
#' 
#' @name avaliable_CAGED
#' 
#' @param m Mês do ano qual deseja verificar os dados, deve ser especificado como string com dois dígitos,
#'  ex.: "01" (mês de janeiro).
#'
#' @param y Ano qual deseja verificar os dados, deve ser especificado como string, ex.: "2019".
#' 
#' @noRd
#'  
#' @importFrom RCurl url.exists
available_CAGED <- function(m = "12", y = "2019") {
  url_path <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED"
  url_file <- file.path(url_path, y, paste0("CAGEDEST_", m, y, ".7z"))
  check_file <- RCurl::url.exists(url_file)
  if(check_file) {
    message("\n Dados e endereço DISPONÍVEIS! Mês/Ano: ", paste(m, y, sep = "/"), "\n")
  } else {
    message("\n Dados OU endereço INDISPONÍVEL! Mês/Ano: ", paste(m, y, sep = "/"), "\n")
  }
  invisible(check_file)
}

#' @title Dado uma data inicial verifica-se a disponibilidade dos dados e/ou endereço até a data atual
#' 
#' @description Permite avaliar a disponibilidade dos dados e/ou endereço para todos os meses e anos no
#' período compreendido entre uma data incial até a data atual.
#' 
#' @name available_update_CAGED
#' 
#' @param last.m,last.y  mês e ano para formar a data inicial, ex.: "12/2019".
#' 
#' @return Um conjunto de dados contendo as seguintes colunas:
#' 
#' \item{month }{
#' mês de referência dos dados consultados;
#' }
#' \item{year }{
#' ano de referência dos dados consultados;
#' }
#' \item{available }{
#' Se TRUE, os dados e endereços estão disponíveis e FALSO, caso contrário;
#' }
#' \item{date_check }{
#' data da verificação.
#' }
#' 
#' @examples
#' 
#' (file_available <- available_update_CAGED(last. = "01", last.y = "2020"))
#' 
#' @references
#' 
#' \samp{ftp://ftp.mtps.gov.br/pdet/microdados/CAGED}
#' 
#' @export
available_update_CAGED <- function(last.m = "01", last.y = "2007") {
  data_current <- Sys.Date() 
  current_y <-  format(data_current, "%Y")
  current_m <-  format(data_current, "%m")
  last_aux <- as.numeric(paste(last.y, last.m, sep = "."))
  current_aux <- as.numeric(paste(current_y, current_m, sep = "."))
  if(last_aux <= current_aux) {
    available_CAGED <- 
      base::expand.grid("month" = c(paste0("0", 1:9), 10:12), 
                        "year" = as.integer(last.y):as.numeric(current_y),
                        stringsAsFactors = FALSE) %>%
      dplyr::as_tibble() %>% 
      dplyr::filter(dplyr::between(x = as.numeric(paste(year, month, sep = ".")),
                                   left = last_aux, 
                                   right = current_aux)) %>% 
      dplyr::group_by(year, month) %>% 
      dplyr::mutate(available = available_CAGED(m = month, y = year), 
                    description = dplyr::if_else(available, 
                                                 "Dados e endereço disponível!", 
                                                 "Dados OU endereço INDISPONÍVEl!"),
                    date_check = Sys.Date()) %>% 
      dplyr::ungroup()
  } else {
    stop("Last date (month/year) greater current date (today)")
  }
  return(available_CAGED)
} 


#' @title Verifica alterações nas arquivos de dados do repositório do CAGED
#' 
#' @description Acessa a ftp onde está disponível os dados do CAGED e retorna metadados dos arquivos de dados 
#' do repositório.
#' 
#' @param check_files Saída da função \code{available_update_CAGED}, se NULL esta função será executada internamente
#' por \code{repository_update_CAGED} considerando \code{last.m = "01"} e \code{last.y = "2007"}.
#' 
#' @return Um conjunto de dados contendo as seguintes colunas:
#' 
#' \item{file_url }{
#' link do arquivo que estou verificando alterações no repositório do CAGED;
#' }
#' \item{file_name }{
#' nome do arquivo de dados;
#' }
#' \item{date_update }{
#' data da última alteração no arquivo realizada pelo o gorveno;
#' }
#' \item{date_check }{
#' data da verificação.
#' }
#' 
#' @references
#' 
#' \samp{ftp://ftp.mtps.gov.br/pdet/microdados/CAGED}
#' 
#' @examples 
#' 
#' # Execute:
#' check_files <- available_update_CAGED()
#' repository_update_CAGED(check_files)
#' 
#' # Ou
#' 
#' repository_update_CAGED()
#' 
#' @import dplyr
#' @importFrom RCurl getURL
#' @export
repository_update_CAGED <- function(check_files = NULL) {
  if(is.null(check_files)) {
    check_files <- available_update_CAGED()
  }
  
  path_url <- paste0("ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/", unique(check_files$year[check_files$available]), "/")
  check_sucess <-
    tryCatch(metadata <- 
               RCurl::getURL(path_url) %>% 
               lapply(., function(x)  {strsplit(x, "\n")}) %>% 
               {
                 df_aux <- dplyr::tibble()
                 for(i in seq_along(.)) {
                   df_aux <- 
                     bind_rows(df_aux,
                               .[[i]][[1]] %>% 
                                 strsplit(x = ., split = " ") %>% 
                                 lapply(X = ., 
                                        FUN = function(x) {dplyr::tibble("file_name" = x[17], 
                                                                         "date_update" = as.character(as.Date(x[1], tryFormats = c("%m-%d-%y"))))}) %>% 
                                 dplyr::bind_rows() %>% 
                                 dplyr::mutate("file_url" = gsub(pattern = "\r", replacement = "", x = paste0(path_url[i], `file_name`)),
                                               `file_name` = gsub(pattern = "\r", replacement = "", x = `file_name`),
                                               "date_check" = as.character(Sys.Date())) %>% 
                                 dplyr::select(`file_url`, `file_name`, `date_update`, `date_check`))
                   
                 }
                 df_aux
               }, 
             error = function(e) {stop("'ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/' INDISPONÍVEl")}, 
             finally = TRUE)
  
  metadata <-
    metadata %>% 
    dplyr::mutate(check = gsub(pattern = "CAGEDEST_|.7z", replacement = "", x = file_name)) %>% 
    dplyr::filter(check %in% paste0(check_files$month[check_files$available], check_files$year[check_files$available])) %>% 
    dplyr::select(-check)
  
  return(metadata)
}

#' @importFrom utils download.file
#'
download_CAGED <- function(m = "12", y = "2019", dir.output = ".") {
  m <- if (is.null(m)) { format(Sys.Date(), "%m") } else { m }
  y <- if (is.null(y)) { format(Sys.Date(), "%Y") } else { y }
  
  url_path <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED"
  url_file <- file.path(url_path, y, paste0("CAGEDEST_", m, y, ".7z"))
  dir_file <- file.path(dir.output, paste("CAGEDEST_", m, y, ".7z", sep = ""))
  
  check_file <- available_CAGED(m, y)
  if (check_file) {
    utils::download.file(url = url_file, destfile = dir_file, mode = "wb")
  } else {
    stop("\n Erro ao realizar download do arquivo! \n")
  }
  
  invisible(list(check_file = check_file,
                 dir_file   = dir_file))
}


#' @title Importar dados do CAGED
#'
#' @name read_CAGED
#'
#' @description Função para leitura dos dados do Cadastro Geral de Empregados e Desempregados
#' (CAGED) do governo federal do Brasil.
#'
#'
#' @param month Mês do ano qual deseja os dados, deve ser especificado como caracter com dois dígitos,
#'  ex.: "01" (mês de janeiro).
#'
#' @param year Ano qual deseja os dados, deve ser especificado como caracter com quatro dígitos,
#' ex.: "2019".
#' 
#' @param ... argumentos passado para função \code{un7z}, ex.: \code{path7zip}.
#'
#' @details Instituído pela Lei n° 4.923/65, que obrigou as empresas abrangidas pelo Sistema de
#' Consolidação das Leis do Trabalho a informarem ao Ministério do Trabalho, em relação nominal, a
#' movimentação de seus empregados, o CAGED tem periodicidade mensal e suas informações destinam-se
#' a acompanhar e fiscalizar o processo de admissão e dispensa dos trabalhadores, bem como subsidiar
#' a adoção de medidas contra o desemprego e o estabelecimento de mecanismos de assistência aos
#' desempregados.
#'
#' @references 
#' 
#' \samp{http://portalfat.mte.gov.br/programas-e-acoes-2/caged-3/}
#' 
#' @author Rumenick Pereira da Silva
#'
#' @keywords baixar importar download CAGED Brasil
#'
#' @examples
#'
#' ## Baixa dados do mês de fevereiro de 2018
#' CAGEDEST_022018 <- read_CAGED(month = "02", year = "2018")
#' head(CAGEDEST_022018)
#'
#' ## Dois ou mais meses ou anos:
#' # install.packages("dplyr")
#'
#' # configs <- expand.grid(meses = c("01", "02"), anos = c("2018", "2019"))
#' 
#' @importFrom data.table fread
#' @export
read_CAGED <- function(month = "12", year = "2019", ...) {
  # create temporary directory
  td <- tempdir()
  
  # download 7zip file:
  info_download <- download_CAGED(m = month, y = year, dir.output = td)
  
  # Extract file form 7zip archive:
  info_un7z <- un7z(zipfile = info_download$dir_file, dir.output = td, ...)
  
  # Read data CAGED for month/year:
  file_txt <- file.path(td, dir(td)[grepl(pattern = paste0(month, year, ".txt"), dir(td))])
  data_CAGED <-
    data.table::fread(file = file_txt,
                      sep = ";",
                      encoding = "Latin-1" ,
                      dec = ",",
                      na.strings = "-1",
                      showProgress = FALSE ,
                      data.table = FALSE ,
                      stringsAsFactors = FALSE ,
                      fill = TRUE ,
                      strip.white = TRUE)
  file.remove(file_txt)
  file.remove(info_download$dir_file)
  cat("\n Dados importados com sucesso! \n")
  return(data_CAGED)
}

