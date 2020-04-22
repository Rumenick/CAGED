#' @importFrom utils download.file
#'
download_CAGED <- function(m = "12", y = "2019", dir.output = ".") {
  m <- if (is.null(m)) { format(Sys.Date(), "%m") } else { m }
  y <- if (is.null(y)) { format(Sys.Date(), "%Y") } else { y }
  
  url_path <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED"
  url_file <- file.path(url_path, y, paste0("CAGEDEST_", m, y, ".7z"))
  dir_file <- file.path(dir.output, paste("CAGEDEST_", m, y, ".7z", sep = ""))
  check_erro <- tryCatch(utils::download.file(url = url_file, destfile = dir_file, mode = "wb"),
                         error = function(e) { TRUE },
                         finally = FALSE)
  return(list(check_erro = check_erro,
              dir_file   = dir_file))
}



#' @title Importar dados do CAGED
#'
#' @name read_CAGED
#'
#' @description Função para leitura dos dados do Cadastro Geral de Empregados e Desempregados
#' (CAGED) do governo federal do Brasil.
#'
#' @importFrom data.table fread
#'
#' @param month Mês do ano qual deseja os dados, deve ser especificado como caracter com dois dígitos,
#'  ex.: "01" (mês de janeiro).
#'
#' @param year Ano qual deseja os dados, deve ser especificado como caracter com quatro dígitos,
#' ex.: "2019".
#'
#' @details Instituído pela Lei n° 4.923/65, que obrigou as empresas abrangidas pelo Sistema de
#' Consolidação das Leis do Trabalho a informarem ao Ministério do Trabalho, em relação nominal, a
#' movimentação de seus empregados, o CAGED tem periodicidade mensal e suas informações destinam-se
#' a acompanhar e fiscalizar o processo de admissão e dispensa dos trabalhadores, bem como subsidiar
#' a adoção de medidas contra o desemprego e o estabelecimento de mecanismos de assistência aos
#' desempregados.
#'
#' @references http://portalfat.mte.gov.br/programas-e-acoes-2/caged-3/
#' 
#' @author Rumenick Pereira da Silva
#'
#' @keywords baixar importar download CAGED Brasil
#'
#' @examples
#'
#' ## Baixa dados do mês de fevereiro de 2018
#' # CAGEDEST_022018 <- read_CAGED(month = "02", year = "2018")
#' # CAGEDEST_022018
#'
#' ## Dois ou mais meses ou anos:
#' # install.packages("dplyr")
#'
#' # configs <- expand.grid(meses = c("01", "02"), anos = c("2018", "2019"))
#'
#' # CAGED_list <- mapply(FUN = function(m, y) {read_CAGED(month = m, year = y)},
#' #                     m = configs$meses, y = configs$anos)
#' # dplyr::bind_rows(CAGED_list) # Juntar todos os dados
#'
#' @export
read_CAGED <- function(month = "12", year = "2019") {
  # create temporary items
  # tf <- tempfile()
  # td <- path.expand(file.path(".", "temp", paste0(sample(c(letters, LETTERS), size = 10), collapse = "")))
  # while(dir.exists(td)) {
  #   td <- path.expand(file.path(".", "temp", paste0(sample(c(letters, LETTERS), size = 10), collapse = "")))
  # }
  # dir.create(td, recursive = TRUE)
  td <- tempdir()
  check_erro1 <- TRUE
  check_erro2 <- TRUE
  while (check_erro1 | check_erro2) {
    info_download <- download_CAGED(m = month, y = year, dir.output = td)
    check_erro1 <- info_download$check_erro
    check_erro2 <- tryCatch(info_un7z <- suppressMessages(un7z(zipfile = info_download$dir_file, dir.output = td)),
                            error = function(e) TRUE, finally = FALSE)
  }
  
  file_txt <- gsub(pattern = ".7z", replacement = ".txt", x = info_download$dir_file)
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

