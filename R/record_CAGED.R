#' @title Selecionar variáveis de interesse nos dados do CAGED
#' 
#' @name select_vars_CAGED
#' 
#' @param .data output da função \code{read_CAGED}.
#' 
#' @noRd
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
select_vars_CAGED <- function(.data) {
  vars <- 
    c(admitidos = "Admitidos/Desligados",  
      comp_declarada = "Competência Declarada", 
      "Município",             
      "Ano Declarado",         
      cod_cbo = "CBO 2002 Ocupação",     
      "CNAE 1.0 Classe",       
      cod_cnae = "CNAE 2.0 Classe",       
      "CNAE 2.0 Subclas",     
      porte_empresa = "Faixa Empr Início Jan", 
      escolaridade = "Grau Instrução",        
      qtd_horas_contrat = "Qtd Hora Contrat",      
      "IBGE Subsetor",         
      idade = "Idade",                 
      "Ind Aprendiz",          
      "Ind Portador Defic",   
      cor = "Raça Cor",             
      salario_mensal = "Salário Mensal",        
      saldo_mov = "Saldo Mov",            
      sexo = "Sexo",                 
      tempo_emprego = "Tempo Emprego",         
      "Tipo Estab",            
      "Tipo Defic",            
      tipo_mov_desagregado = "Tipo Mov Desagregado",  
      cod_uf = "UF",                   
      "Bairros SP",            
      "Bairros Fortaleza",     
      "Bairros RJ",            
      "Distritos SP",          
      "Regiões Adm DF",        
      "Mesorregião",          
      "Microrregião",         
      "Região Adm RJ",        
      "Região Adm SP",         
      "Região Corede",
      "Região Corede 04",
      "Região Gov SP",
      "Região Senac PR",
      "Região Senai PR",
      "Região Senai SP",    
      "Sub-Região Senai PR",
      ind_trab_parcial = "Ind Trab Parcial",
      ind_trab_intermitente = "Ind Trab Intermitente")
  
  vars_aux1 <- vars[vars %in% names(.data) & names(vars) != ""]
  vars_aux2 <- vars[!(vars %in% names(.data)) & names(vars) != ""]
  
  .data %>% 
    dplyr::select(all_of(vars_aux1)) %>% 
    {
      vector(mode = "list", length = length(vars_aux2)) %>% 
        stats::setNames(vars_aux2) %>% 
        lapply(., FUN = function(x) {rep(NA, nrow(.data))}) %>% 
        dplyr::bind_cols(.data, .)
    } %>% 
    dplyr::select(tidyselect::all_of(vars[names(vars) != ""]))
}

#' @title Gravar e atualizar bancos de dados com os dados do CAGED
#' 
#' @name record_CAGEG
#' 
#' @param last.m,last.y  mês e ano para formar a data inicial, ex.: "12/2019".
#' 
#' @param dir.SQLite diretório onde deve ser criado o Banco de Dados (BD), no caso
#' de existir o banco, diretório onde está o banco de dados SQLite.
#' 
#' @param name.SQLite nome do BD, por padrão "BD.sqlite".
#' 
#' @param ... argumentos passado para função \code{un7z}, ex.: \code{path7zip}.
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
#' \item{cod_cbo }{
#' CBO 2002 Ocupação
#' }
#' \item{cod_cnae }{
#' CNAE 2.0 Classe;
#' }
#' \item{porte_emprasa }{
#' Faixa Empr Início Jan;
#' }
#' \item{escolaridade }{
#' Grau Instrução;
#' }
#' \item{qtd_horas_contrat }{
#' Qtd Hora Contrat;
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
#' \item{cod_uf }{
#' UF;
#' }
#' \item{ind_trab_parcial }{
#' Ind Trab Parcial;
#' }
#' \item{ind_trab_intermitente }{
#' Ind Trab Intermitente.
#' } 
#' 
#' Se deseja acessar todas as variáveis use a função \code{read_CAGED}. Ademais,
#' a função criará a tabela MetadadosCAGED com os metadados gerado pela
#' função \code{repository_metadata_CAGED} de mesmo nome.
#' 
#' @examples
#' 
#' db <- record_CAGEG("12", "2019", dir.SQLite = ":memory:")
#' 
#' # Tabelas:
#' DBI::dbListTables(db)
#' 
#' # Acessando o tabela CADED:
#' DBI::dbReadTable(db, "CAGED")
#' 
#' # Acessando a tabela repository_metadata_CAGED:
#' DBI::dbReadTable(db, "MetadadosCAGED")
#' 
#' # Desconectando:
#' DBI::dbDisconnect(db)
#' 
#' @import DBI
#' @import RSQLite
#' @import dplyr
#' 
#' @export
record_CAGEG <- function(last.m = "01", last.y = "2007", dir.SQLite = "./", name.SQLite = "DB.sqlite", ...) {
  
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
         read_CAGED(month = m, year = y, ...) %>% 
            select_vars_CAGED() %>% 
            DBI::dbWriteTable(db, "CAGED", ., append = TRUE)
        }
      }
    }
  
  DBI::dbWriteTable(db, "MetadadosCAGED", repository_update_CAGED(), append = TRUE)
  invisible(db)
}


