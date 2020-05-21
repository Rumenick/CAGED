#' @title Função para filtrar os dados do CAGED de acordo com o projeto
#' 
#' @description Acessa o banco de dados do CAGED e aplica filtros nos dados de acordo com os parâmetros 
#' CAGEG (par_CAGED) estabelecidos pela SIMG para um determinado projeto. 
#' 
#' @name filter_proj
#' 
#' @param proj nome do projeto (Padronizar nomes no futuro).
#' 
#' @param data_ref data de referência do projeto.
#' 
#' @param n.ref números de meses do CAGED a serem filtrados em relação a \code{data_ref}. Usada para filtrar
#' os dados do CAGED, por padrão o filtro é realizado para um período de 12 meses.
#' 
#' @param par.CAGED Configurações usadas no projeto específico que implicam em filtros nos dados do CAGED.
#' 
#' @param BD.SQLite caminho para o Banco de Dados (BD), ex.: \code{"./data/MDO.sqlite"}.
#' 
#' @return um \code{tibble} com os dados de acordo com os parâmetros selecionados de um dado projeto. 
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
# SICRO_sec_f <- filter_proj(data.ref = "10/2019", n.ref = 12, par.CAGED = par_CAGED, BD.SQLite = "/home/rumenick/Documentos/data/MDO.sqlite")

#' @noRd
select_period <- function(data.ref, n.ref) {
  format.Date(seq(from = as.Date(paste("01", data.ref, sep = "/"), format = "%d/%m/%Y"), 
                  by = "-1 month", 
                  length.out = n.ref), 
              format = "%Y%m")
}

# select_ DeProjParaCBO <- function(proj, data.ref, drv) {
#   return("DeProjParaCBO")
# }


#' @import dplyr
#' @import DBI
#' @import RSQLite
filter_proj <- function(proj = "SICRO_secao_f", data.ref, n.ref = 12, par.CAGED = list(), BD.SQLite = "./data/MDO.sqlite") {
  # Criando conexão com o Banco de dados (DB):
  db <- DBI::dbConnect(RSQLite::SQLite(), BD.SQLite)
  
  # Consultando CBO's do respectivo projeto (proj) na tabela DeProjParaCBO:
  par.CAGED$cod_cbo <- na.omit(unique(DBI::dbGetQuery(db, sprintf("SELECT cod_cbo FROM DeProjParaCBO WHERE proj IN ('%s') AND data_ref IN ('%s')", 
                                                                  paste(proj, collapse = ", "), 
                                                                  data.ref)))$cod_cbo)
  
  # Consultando dados das CBO's na tabela CAGED:
  dt_filter_proj <- DBI::dbGetQuery(db, sprintf("SELECT * FROM CAGED WHERE comp_declarada IN (%s) AND qtd_horas_contrat IN (%s)  AND cod_cbo IN (%s) AND porte_empresa IN (%s) AND cor IN (%s) AND ind_trab_parcial IN (%s) AND ind_trab_intermitente IN (%s) AND cod_uf IN (%s) AND escolaridade IN (%s) AND admitidos IN (%s)", 
                                                paste0(select_period(data.ref, n.ref), collapse = ", "), 
                                                paste0(par.CAGED$qtd_horas_contrat, collapse = ", "), 
                                                paste0(par.CAGED$cod_cbo, collapse = ", "),
                                                paste0(par.CAGED$porte_empresa, collapse = ", "),
                                                paste0(par.CAGED$cor, collapse = ", "),
                                                paste0(par.CAGED$ind_trab_parcial, collapse = ", "),
                                                paste0(par.CAGED$ind_trab_intermitente, collapse = ", "),
                                                paste0(par.CAGED$cod_uf, collapse = ", "),
                                                paste0(par.CAGED$escolaridade, collapse = ", "),
                                                paste0(par.CAGED$admitidos, collapse = ", ")))
  DBI::dbDisconnect(db)
  return(dplyr::as_tibble(dt_filter_proj))
}
