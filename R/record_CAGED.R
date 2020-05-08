#' @title Configurar e atualizar bancos de dados com os dados do CAGED
#' 
#' @name config_RSQLite_CAGEG
#' 
#' @param last.m,last.y  mês e ano para formar a data inicial, ex.: "12/2019".
#' 
#' @param dir.SQLite diretório onde deve ser criado o Banco de Dados (BD), no caso
#' de existir o banco, diretório onde está o banco de dados SQLite.
#' 
#' @param name.SQLite nome do BD, por padrão "BD.sqlite".
#' 
#' 
#' @return Se o BD ainda não foi criado essa função criará o banco de dados e 
#' adicionará todos os dados do CAGED disponível até a data atual. Por padrão,
#' a tabela CAGED guardará apenas as colunas:
#' 
#' \item{admitidos }{
#' Admitidos/Desligados;
#' }
#' \item{comp_declarada }{
#' Competência Declarada;
#' }
#' \item{cnae_2 }{
#' CNAE 2.0 Classe;
#' }
#' \item{porte_emprasa }{
#' Faixa Empr Início Jan;
#' }
#' \item{escolaridade }{
#' Grau Instrução;
#' }
#' \item{idade }{
#' Idade;
#' }
#' \item{cor }{
#' Raça Cor;
#' }
#' \item{salario_mensal }{
#' Salário Mensal;
#' }
#' \item{saldo_mov }{
#' Saldo Mov;
#' }
#' \item{sexo }{
#' Sexo;
#' }
#' \item{tempo_emprego }{
#' Tempo Emprego;
#' }
#' \item{tipo_mov_desagregado }{
#' Tipo Mov Desagregado;
#' }
#' \item{uf_cod }{
#' UF;
#' }
#' \item{ind_trab_parcial }{
#' Ind Trab Parcial;
#' }
#' \item{ind_trab_intermitente }{
#' Ind Trab Intermitente.
#' } 
#' 
#' Se deseja acessar todas as variáveis use a função \code{read_CAGED}. Ademais
#' a função criará uma tabela repository_metadata_CAGED com metadados gerado pela
#' função \code{repository_metadata_CAGED} de mesmo nome.
#' 
#' @examples
#' 
#' db <- config_RSQLite_CAGEG("12", "2019", dir.SQLite = ":memory:")
#' 
#' # Tabelas:
#' DBI::dbListTables(db)
#' 
#' # Acessando o tabela CADED:
#' DBI::dbReadTable(db, "CAGED")
#' 
#' # Acessando a tabela repository_metadata_CAGED:
#' DBI::dbReadTable(db, "repository_metadata_CAGED")
#' 
#' # Desconectando:
#' DBI::dbDisconnect(db)
#' 
#' @import DBI
#' @import RSQLite
#' @import dplyr
#' 
#' @export
config_RSQLite_CAGEG <- function(last.m = "01", last.y = "2007", dir.SQLite = "./", name.SQLite = "DB.sqlite") {
  
  if (dir.SQLite == ":memory:") {
    name_DB <- dir.SQLite
  } else {
    name_DB <- file.path(dir.SQLite, name.SQLite)
  }
  
  db <- DBI::dbConnect(RSQLite::SQLite(), name_DB)
  
  metadata_files <- available_update_CAGED(last.m, last.y)
  
  metadata_files %>% 
    dplyr::filter(available) %>%
    {
      for (y in unique(.[["year"]])) {
        for(m in unique(.[["month"]])) {
         read_CAGED(month = m, year = y) %>% 
            cleaning_CAGED() %>% 
            DBI::dbWriteTable(db, "CAGED", ., append = TRUE)
        }
      }
    }
  
  DBI::dbWriteTable(db, "repository_metadata_CAGED", repository_update_CAGED(), append = T)
  invisible(db)
}


