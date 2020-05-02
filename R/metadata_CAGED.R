#' @title Metadados de verificação de atualização dos dados do CAGED
#'
#' @describeIn Os dados representam o status de atualização dos dados do CAGED em 01/05/2020. Indicando a 
#' disponibilidade dos dados por mês e ano até a data supracitada.
#'
#' @details \itemize{
#'   \item month Mês do ano dos dados verificados atualização;
#'   \item year Ano dos dados verificados atualização;
#'   \item date data da verificação da disponibilidade dos dados ou endereço;
#'   \item available Se TRUE, os Dados e endereços estão disponíveis e FALSO, caso contrário;
#'   \item description Status dos dados e/ou endereço.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name metadata
#' @usage data(metadata)
#' @format A data frame with 161 rows and 5 variables
#' @references
#' ftp://ftp.mtps.gov.br/pdet/microdados/CAGED
NULL