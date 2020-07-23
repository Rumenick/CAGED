#' @title Converte o DePara antigo para o novo formato, de modo que seja um código de cliente por linha
#' 
#' @noRd
collapse_DeProjParaCBO <- function(DePara) {
  depara <-
    de_para %>% 
    dplyr::transmute(projeto = as.character(projeto),
              cod_cliente = as.character(codigo_cliente),
              descri_cliente = as.character(descricao_cliente),
              unidade = as.character(unidade),
              cod_cbo = ifelse(codigo_cbo == "-", NA, as.numeric(codigo_cbo)),
              descri_cbo = ifelse(descricao_cbo == "-", NA, as.character(descricao_cbo)),
              cod_cliente_equivalente = ifelse(equivalente == "-", NA, as.character(equivalente)),
              sec_cnae = ifelse(cnae == "-", NA, as.character(cnae)),
              crit_corte_percentil = ifelse(criterio_2 %in% c("mean", "-"), NA, criterio_2),
              crit_espacial = sapply(strsplit(x = criterio, split = "media_da_"), function(x) {x[2]}),
              crit_calc = "media",
              calc_fator = as.numeric(fator), 
              calc_tipo = as.numeric(tipo_calculo),
              data_ref_geral = gsub(pattern = "_", replacement = "/", x = referencia)) %>% 
    {
      for(i in seq_along(.$crit_espacial)) {
        if(!is.na(.$cod_cliente_equivalente[i])) {
          .$crit_espacial[i] <- unique(.$crit_espacial[.$cod_cliente == .$cod_cliente_equivalente[i]])
        }
      }
      .
    } %>% 
    dplyr::group_by(cod_cliente) %>%
    dplyr::summarise(projeto = paste(unique(projeto), collapse = ", "),
              descri_cliente = paste(unique(descri_cliente), collapse = ", "),
              unidade = paste(unique(unidade)),
              cod_cbo = paste(unique(cod_cbo), collapse = ", "), 
              descri_cbo = paste(unique(descri_cbo), collapse = ", "), 
              cod_cliente_equivalente = paste(unique(cod_cliente_equivalente, collapse = ", ")),
              sec_cnae = paste(unique(sec_cnae), collapse = ", "),
              crit_corte_percentil = paste(unique(crit_corte_percentil), collapse = ", "),
              crit_corte_percentil = paste(unique(crit_corte_percentil), collapse = ", "),
              crit_espacial = paste(unique(crit_espacial), collapse = ", "),
              crit_calc = paste(unique(crit_calc), collapse = ", "),
              calc_fator = paste(unique(calc_fator), collapse = ", "),
              calc_tipo = paste(unique(calc_tipo), collapse = ", "),
              data_ref_geral = paste(unique(data_ref_geral), collapse = ", ")) %>%
    dplyr::mutate_all(.funs = function(x) {ifelse(x == "NA", NA, x)}) %>% 
    dplyr::mutate(cliente_equivalente = !is.na(cod_cliente_equivalente)) %>% 
    {
      for (i in 1:nrow(.)) {
        if (.[["cliente_equivalente"]][i]) {
          .[["cod_cbo"]][i] <-  .[["cod_cbo"]][(.[["cod_cliente_equivalente"]][i] == .[["cod_cliente"]])]
          .[["descri_cbo"]][i] <-  .[["descri_cbo"]][(.[["cod_cliente_equivalente"]][i] == .[["cod_cliente"]])]
          .[["sec_cnae"]][i] <-  .[["sec_cnae"]][(.[["cod_cliente_equivalente"]][i] == .[["cod_cliente"]])]
          
        }
      }
      .
    } %>% 
    dplyr::mutate(crit_espacial = case_when(is.na(crit_espacial) ~ "Nacional", 
                                            crit_espacial == "regiao" ~ "Regional",
                                            crit_espacial == "uf" ~ "UF")) %>% 
    dplyr::select(projeto, everything(), -cliente_equivalente)
  
  depara
}

#' @title Função com todas as opções de cálculo para média salarial por código do cliente
#' 
#' @description Facilmente pode-se adicionar outros critérios com a evolução dos projetos.
#' O nome da função deve ser em conformidade com o DeProjParaCBO. 
#' 
#' @noRd
#' 
calc_means <- function(.data, crits = list(cut = NULL, spatial = NULL, calculus = NULL)) {
  
  ind_reg <- grepl("reg", tolower(crits$spatial)) # Verificar se média por região
  ind_uf <-  grepl("uf", tolower(crits$spatial)) # Verificar se média por UF
  percentile <- as.numeric(gsub("p", "", crits$cut)) / 100 # Obtendo o percentil
  
  .data <- 
    if (is.na(percentile)) {
      .data
    } else {
      if(percentile <= .5) {
        .data %>% 
          filter(salario_mensal <= quantile(salario_mensal, probs = percentile, type = 2))
      } else {
        .data %>% 
          filter(salario_mensal >= quantile(salario_mensal, probs = percentile, type = 2))
      }
    }
  
  .data %>% 
    {
      if(ind_uf) { # Estatística por UF
        group_by(.data = ., cod_uf) %>% 
          summarise(mean = mean(salario_mensal))
      } else {
        if(ind_reg) { # Estatística por região:
          dplyr::mutate(.data = ., reg_uf = dplyr::case_when(cod_uf >= 11 & cod_uf <= 17 ~ "Norte",
                                                             cod_uf >= 21 & cod_uf <= 29 ~ "Nordeste",
                                                             cod_uf >= 31 & cod_uf <= 35 ~ "Sudeste",
                                                             cod_uf >= 41 & cod_uf <= 43 ~ "Sul",
                                                             cod_uf >= 50 & cod_uf <= 53 ~ "Centro-oeste")) %>% 
            dplyr::group_by(reg_uf) %>%
            dplyr::summarise(mean = mean(salario_mensal)) %>% 
            dplyr::left_join(x = info_ufs[, c("cod_uf", "sigla_uf", "reg_uf")], y = ., by = "reg_uf") %>% 
            dplyr::transmute(mean = mean, cod_uf = cod_uf)
        } else { # Estatística Nacional:
          dplyr::summarise(.data = ., mean = mean(salario_mensal), cod_uf = info_ufs$cod_uf)
        }
      }
    }
} 

#' @title Resume os dados por UF de acordo com o DeProjParaCBO
#' 
#' @description Permite obter as estatísticas da variável salário de acordo com o DeProjParaCBO do respectivo
#' projeto.
#' 
#' @name summary_CAGED
#' 
#' @param conn Objeto DBIConnection produzido por DBI::dbConnect().
#' 
#' @param proj Nome do projeto (Padronizar nomes no futuro).
#' 
#' @param data.ref.geral Data de referência do projeto.
#' 
#' @param data.ref.coleta Data de coleta dos dados do CAGED, referencia o mês/ano até onde pegar os dados.
#' 
#' @param n.ref.coleta Números de meses do CAGED a serem filtrados em relação a \code{data.coleta}. Usada para filtrar
#' os dados do CAGED, por padrão o filtro é realizado para um período de 12 meses.
#' 
#' @param par.CAGED Configurações usadas no projeto específico que implicam em filtros nos dados do CAGED.
#' 
#' @importFrom utils data
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @export
summary_CAGED <- function(conn, proj, data.ref.geral, data.ref.coleta, n.ref.coleta = 12, par.CAGED) {
  # Lendo o DeProjParaCBO em formado de trabalho:
  df_DeProjParaCBO <- 
    conn %>% 
    dplyr::tbl("DeProjParaCBO") %>%
    dplyr::filter(projeto == proj, data_ref_geral == data.ref.geral) %>% 
    dplyr::collect()
  
  # Consultando CBO's do respectivo projeto (proj) na tabela DeProjParaCBO:
  par.CAGED$cod_cbo <- unique(strsplit(paste(df_DeProjParaCBO$cod_cbo, collapse = ", "), ", ")[[1]])
  
  # Filtrando os dados do respectivo projeto:
  df_proj <- 
    conn %>% 
    filter_CAGED(data.ref.coleta, n.ref.coleta, par.CAGED) %>% 
    dplyr::select(cod_cbo, salario_mensal, cod_uf) %>% 
    dplyr::collect()
  
  # Obtendo salários médios por UF e critérios:
  df_DeProjParaCBO %>% 
    dplyr::group_by(cod_cliente) %>%
    tidyr::nest() %>% 
    dplyr::mutate(summary = lapply(data, 
                                   FUN = function(df_cliente, CAGED) {CAGED %>% dplyr::filter(cod_cbo %in% strsplit(df_cliente$cod_cbo, ", ")[[1]]) %>% calc_means(crits = list(cut = df_cliente$crit_corte_percentil, spatial = df_cliente$crit_espacial, calculus = df_cliente$crit_calc))}, 
                                   CAGED = df_proj)) %>% 
    tidyr::unnest(cols = c(data, summary)) %>% 
    dplyr::mutate(mean = ifelse(unidade == "h", as.numeric(calc_fator) * mean / 220, as.numeric(calc_fator)  * mean))
}

# Faltar aplicar fator (obs.: fator 220)
# Verificar cálculos




