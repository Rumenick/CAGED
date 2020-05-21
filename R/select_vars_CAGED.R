#' @title Função para selecionar variáveis de interesse nos dados do CAGED
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
