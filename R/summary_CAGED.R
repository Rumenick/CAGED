#' @title Compacta os dados de modo que seja um código de cliente por linha
#' 
#' @noRd
collapse_DeProjParaCBO <- function(conn, proj = "SICRO_secao_f", data.ref = "10/2019") {
  conn %>% 
    dplyr::tbl("DeProjParaCBO") %>% 
    dplyr::filter(projeto %in% proj, data_ref == data.ref) %>%
    dplyr::group_by(cod_cliente) %>% 
    dplyr::collect() %>% 
    summarise(projeto = paste(unique(projeto), collapse = ", "),
              descri_cliente = paste(unique(descri_cliente), collapse = ", "),
              unidade = paste(unique(unidade)),
              cod_cbo = paste(unique(cod_cbo), collapse = ", "), 
              descri_cbo = paste(unique(descri_cbo), collapse = ", "), 
              cod_cliente_equivalente = paste(unique(cod_cliente_equivalente, collapse = ", ")),
              sec_cnae = paste(unique(sec_cnae), collapse = ", "),
              calc_crit = paste(unique(calc_crit), collapse = ", "),
              calc_fator = paste(unique(calc_fator), collapse = ", "),
              calc_tipo = paste(unique(calc_tipo), collapse = ", "),
              data_ref = paste(unique(data_ref), collapse = ", ")) %>%
    mutate_all(.funs = function(x) {ifelse(x == "NA", NA, x)}) %>% 
    mutate(cliente_equivalente = !is.na(cod_cliente_equivalente)) %>% 
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
    select(projeto, everything(), -cod_cliente_equivalente)
}

#' @title Lista com todas as opções de cálculo para média salarial por código do cliente
#' 
#' @description Facilmente pode-se adicionar outros critérios com a evolução dos projetos.
#' O nome da função deve ser em conformidade com o DeProjParaCBO. 
#' 
#' @noRd
#' 
#' @importFrom tidyr uncount
calcs_mean_by <- 
  list(
    media_da_uf = function(.data) {
      .data %>% 
        dplyr::group_by(cod_uf) %>% 
        dplyr::summarise(mean = mean(salario_mensal))
    },
    media_da_regiao = function(.data) {
      .data %>% 
        dplyr::mutate(reg_uf = dplyr::case_when(cod_uf >= 11 & cod_uf <= 17 ~ "Norte",
                                                cod_uf >= 21 & cod_uf <= 29 ~ "Nordeste",
                                                cod_uf >= 31 & cod_uf <= 35 ~ "Sudeste",
                                                cod_uf >= 41 & cod_uf <= 43 ~ "Sul",
                                                cod_uf >= 50 & cod_uf <= 53 ~ "Centro-oeste")) %>% 
        dplyr::group_by(reg_uf) %>% 
        dplyr::summarise(mean = mean(salario_mensal)) %>% 
        tidyr::uncount(c(4, 9, 7, 4, 3)) %>% 
        dplyr::transmute(mean = mean, cod_uf = info_ufs$cod_uf)
    },
    media_nacional = function(.data) {
      .data %>% 
        dplyr::summarise(mean = mean(salario_mensal)) %>% 
        tidyr::uncount(length(info_ufs$cod_uf)) %>% 
        dplyr::mutate(cod_uf = info_ufs$cod_uf)
    },
    mean = function(.data) {
      .data %>% 
        dplyr::summarise(mean = mean(salario_mensal)) %>% 
        dplyr::mutate(n = length(info_ufs$cod_uf)) %>% 
        tidyr::uncount(n) %>% 
        dplyr::mutate(cod_uf = info_ufs$cod_uf)
    },
    p25 = function(.data) {
      .data %>% 
        dplyr::filter(salario_mensal <= quantile(salario_mensal, probs = 0.25)) %>% 
        dplyr::summarise(mean = mean(salario_mensal)) %>% 
        dplyr::mutate(n = length(info_ufs$cod_uf)) %>% 
        tidyr::uncount(n) %>% 
        dplyr::mutate(cod_uf = info_ufs$cod_uf)
    },
    p75 = function(.data) {
      .data %>% 
        dplyr::filter(salario_mensal >= quantile(salario_mensal, probs = 0.75)) %>% 
        dplyr::summarise(mean = mean(salario_mensal)) %>% 
        dplyr::mutate(n = length(info_ufs$cod_uf)) %>% 
        tidyr::uncount(n) %>% 
        dplyr::mutate(cod_uf = info_ufs$cod_uf)
    }
) 

#' @title Resume os dados por UF de acordo com o DeProjParaCBO
#' 
#' @description Permite obter as estatísticas da variável salário de acordo com o DeProjParaCBO do respectivo
#' projeto.
#' 
#' @name summarise_CAGED
#' 
#' @param conn Objeto DBIConnection produzido por DBI::dbConnect().
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
#' @importFrom utils data
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @export
summarise_CAGED <- function(conn, proj, data.ref, n.ref, par.CAGED) {
  # Lendo e convertendo o DeProjParaCBO em formado de trabalho:
  df_DeProjParaCBO <-
    conn %>% 
    collapse_DeProjParaCBO(proj, data.ref)
  
  # Consultando CBO's do respectivo projeto (proj) na tabela DeProjParaCBO:
  par.CAGED$cod_cbo <- unique(strsplit(paste(df_DeProjParaCBO$cod_cbo, collapse = ", "), ", ")[[1]])
  
  # Filtrando os dados do respectivo projeto:
  df_proj <- 
    conn %>% 
    filter_CAGED(data.ref, n.ref, par.CAGED) %>% 
    select(cod_cbo, salario_mensal, cod_uf) %>% 
    dplyr::collect()
  
  # Obtendo salários médios por UF e critérios:
  df_DeProjParaCBO %>% 
    dplyr::group_by(cod_cliente) %>%
    tidyr::nest() %>% 
    dplyr::mutate(summary = lapply(data, 
                                   FUN = function(df, CAGED) {CAGED %>% filter(cod_cbo %in% strsplit(df$cod_cbo, ", ")[[1]]) %>% calcs_mean_by[[df$calc_crit]]()}, 
                                   CAGED = df_proj)) %>% 
    tidyr::unnest(cols = c(data, summary)) %>% 
    dplyr::mutate(mean = ifelse(unidade == "h", as.numeric(calc_fator) * mean, mean))
}

# Faltar aplicar fator (obs.: fator 220)
# Verificar cálculos




