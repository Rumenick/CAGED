#' @title Criar período de competências declaradas para filtrar na tabela CAGED
#' @noRd
select_period_CAGED <- function(data.ref, n.ref) {
  format.Date(seq(from = as.Date(paste("01", data.ref, sep = "/"), format = "%d/%m/%Y"), 
                  by = "-1 month", 
                  length.out = n.ref), 
              format = "%Y%m")
}

#' @title Query de filtro na tabela CAGED
#' @noRd
query_CAGED <- function(par.CAGED = list()) {
  query <- "SELECT * FROM CAGED WHERE "
  for (i in 1:(length(par.CAGED))) {
    query <- paste0(query, names(par.CAGED)[i], " IN (", paste(par.CAGED[[i]], collapse = ', '), ifelse(i < length(par.CAGED), ") AND ", ")"), collapse = "")
  }
  return(query)
}

#' @title Query de filtro na tabela DeProjParaCBO
#' @noRd
#' 
query_DeProjParaCBO <- function(par.DeProjParaCBO = list()) {
  query <- "SELECT * FROM DeProjParaCBO WHERE "
  for (i in 1:(length(par.DeProjParaCBO))) {
    query <- paste0(query, names(par.DeProjParaCBO)[i], " IN (", paste("'",par.DeProjParaCBO[[i]], "'", collapse = ", ", sep = ""), ifelse(i < length(par.DeProjParaCBO), ") AND ", ")"), collapse = "")
  }
  return(query)
}

#' @title Filtrar os dados do CAGED de acordo com o projeto
#' 
#' @description Acessa o banco de dados do CAGED e aplica filtros nos dados de acordo com os parâmetros 
#' CAGEG (par_CAGED) estabelecidos pela SIMG para um determinado projeto. 
#' 
#' @name filter_CAGED
#' 
#' @param proj Nome do projeto (Padronizar nomes no futuro).
#' 
#' @param data.ref Data de referência do projeto.
#' 
#' @param n.ref Números de meses do CAGED a serem filtrados em relação a \code{data_ref}. Usada para filtrar
#' os dados do CAGED, por padrão o filtro é realizado para um período de 12 meses.
#' 
#' @param par.CAGED Configurações usadas no projeto específico que implicam em filtros nos dados do CAGED.
#' 
#' @param conn Objeto DBIConnection produzido por DBI::dbConnect().
#' 
#' @return um \code{tbl_sql} com os dados de acordo com os parâmetros selecionados de um dado projeto. 
#' Aplicar função \code{dplyr::collect(object)} para carregar os dados na memória.
#' 
#' @examples  
#' 
# Ex.: SICRO seção f
# par_CAGED <- list(admitidos = c("1", "2"),
#                   cor = c("1", "2", "4", "6", "8", "9"),
#                   qtd_horas_contrat = 44,
#                   ind_trab_parcial = c("1", "0"),
#                   ind_trab_intermitente = c("1", "0"),
#                   cod_uf = c("35", "33", "32", "31", "41",
#                              "43", "42", "13", "14", "16", "15", "17", "11", "12", "21", "22",
#                              "23", "24", "26", "25", "28", "27", "29", "51", "50", "52", "53"),
#                   porte_empresa = c("5", "6", "7", "8", "9"),
#                   escolaridade = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
#                   sexo = c("1", "2"))
# 
# con <- DBI::dbConnect(RSQLite::SQLite(), "/home/rumenick/Documentos/data/MDO.sqlite")
# SICRO_sec_f <- filter_CAGED(data.ref = "10/2019", n.ref = 12, par.CAGED = par_CAGED, conn = con)
# dplyr::collect(SICRO_sec_f) # Carregar dados na memória.
#'
#' @author Rumenick Pereira da Silva
#'
#' @import dplyr
#' @importFrom stats na.omit
#' @export
filter_CAGED <- function(proj = "SICRO_secao_f", data.ref = "10/2019", n.ref = 12, par.CAGED = list(), conn) {
  
  # Consultando CBO's do respectivo projeto (proj) na tabela DeProjParaCBO:
  par.CAGED$cod_cbo <-  
    conn %>% 
    dplyr::tbl(dplyr::sql(query_DeProjParaCBO(list(projeto = proj, data_ref = data.ref)))) %>% 
    dplyr::select(cod_cbo) %>% 
    dplyr::collect() %>% 
    unlist() %>% 
    unique() %>% 
    na.omit()
  
  # Selecionado o período de tempo:
  par.CAGED$comp_declarada <- select_period_CAGED(data.ref, n.ref)
  
  # Consultando dados das CBO's na tabela CAGED:
  dt_filter_proj <-
    conn %>% 
    dplyr::tbl(dplyr::sql(query_CAGED(par.CAGED)))
  # return (use collect(dt_filter_proj))
  return(dt_filter_proj)
}
