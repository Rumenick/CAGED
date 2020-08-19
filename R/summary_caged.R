#' @title Criar período de competências declaradas para filtrar na tabela caged
#' 
#' @param data.ref.coleta Data de coleta dos dados do CAGED, referencia o mês/ano até onde pegar os dados.
#' 
#' @param n.comp.coleta Números de meses do CAGED a serem filtrados em relação a \code{data.ref.coleta}. 
#' Usada para filtrar os dados do CAGED, por padrão o filtro é realizado para um período de 12 meses.
#' 
#' @noRd
select_period_caged <- function(data.ref.coleta, n.comp.coleta = 12) {
  format.Date(seq(from = as.Date(paste("01", data.ref.coleta, sep = "/"), format = "%d/%m/%Y"), 
                  by = "-1 month", 
                  length.out = n.comp.coleta), 
              format = "%Y%m")
}

#' @title Query de filtro na tabela caged
#' 
#' @details Considera índices (idx) baseados nas colunas competência declarada (comp_declarada) e CBO (cod_cbo) para acelarar
#' filtros dos dados.
#' 
#' @noRd
query_caged <- function(par.caged = list()) {
  query <- paste0("SELECT * FROM CAGED WHERE comp_declarada IN (", paste(par.caged$comp_declarada, collapse = ", "), ") AND ",
                  "cod_cbo IN(", paste0(par.caged$cod_cbo, collapse = ", ") ,") AND ")
  
  par.caged[["comp_declarada"]] <- NULL
  par.caged[["cod_cbo"]] <- NULL
  
  for (i in 1:(length(par.caged))) {
    query <- paste0(query, names(par.caged)[i], " IN (", paste(par.caged[[i]], collapse = ', '), ifelse(i < length(par.caged), ") AND ", ")"), collapse = "")
  }
  return(query)
}

#' @title Query de filtro na tabela DeProjParaCBO
#' @noRd
#' 
query_de_proj_para_cbo <- function(par.DeProjParaCBO = list()) {
  query <- "SELECT * FROM DeProjParaCBO WHERE "
  for (i in 1:(length(par.DeProjParaCBO))) {
    query <- paste0(query, names(par.DeProjParaCBO)[i], " IN (", paste("'", par.DeProjParaCBO[[i]], "'", collapse = ", ", sep = ""), ifelse(i < length(par.DeProjParaCBO), ") AND ", ")"), collapse = "")
  }
  return(query)
}

#' @title Filtrar os dados do CAGED de acordo com o projeto e data de referência
#' 
#' @description Acessa o banco de dados do caged e aplica filtros nos dados de acordo com os parâmetros 
#' CAGEG (par_caged) estabelecidos pela SIMG para um determinado projeto. 
#' 
#' @name filter_caged
#' 
#' @param conn Objeto DBIConnection produzido por `DBI::dbConnect()`.
#' 
#' @param par.caged Configurações usadas no projeto específico que implicam em filtros nos dados do caged.
#' 
#' @return um \code{tbl_sql} com os dados de acordo com os parâmetros selecionados de um dado projeto. 
#' Aplicar função \code{dplyr::collect(object)} para carregar os dados na memória.
#' 
#' @examples  
#' 
#' # Ex.: SICRO seção f (Alterar)
#' par_caged <- list(admitidos = c("1", "2"),
#'                    cor = c("1", "2", "4", "6", "8", "9"),
#'                  qtd_horas_contrat = 44,
#'                  ind_trab_parcial = c("1", "0"),
#'                   ind_trab_intermitente = c("1", "0"),
#'                  cod_uf = c("35", "33", "32", "31", "41",
#'                              "43", "42", "13", "14", "16", "15", "17", "11", "12", "21", "22",
#'                              "23", "24", "26", "25", "28", "27", "29", "51", "50", "52", "53"),
#'                   porte_empresa = c("5", "6", "7", "8", "9"),
#'                   escolaridade = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
#'                    sexo = c("1", "2"))
#' # Consultando CBO's do respectivo projeto (proj) na tabela DeProjParaCBO:
#' # par.caged$cod_cbo <-  
#' # conn %>% 
#' # dplyr::tbl(dplyr::sql(query_DeProjParaCBO(list(projeto = proj, data_ref = data.ref)))) %>% 
#' # dplyr::select(cod_cbo) %>% 
#' # dplyr::collect() %>% 
#' # unlist() %>% 
#' # unique() %>% 
#' # na.omit()
#' 
#' # con <- DBI::dbConnect(RSQLite::SQLite(), "/home/rumenick/Documentos/data/MDO.sqlite")
#' # SICRO_sec_f <- filter_caged(data.ref = "10/2019", n.ref = 12, par.caged = par_caged, conn = con)
#' # dplyr::collect(SICRO_sec_f) # Carregar dados na memória.
#' 
#'
#' @author Rumenick Pereira da Silva
#'
#' @import dplyr
#' @importFrom stats na.omit
#' @export
filter_caged <- function(conn, par.caged = list()) {
  # Consultando dados das CBO's na tabela CAGED:
  dt_filter <- DBI::dbGetQuery(conn, query_caged(par.caged))
  # conn %>% 
  # dplyr::tbl(dplyr::sql(query_caged(par.caged)))
  # return (use collect(dt_filter_proj))
  return(dt_filter)
}

fun_summary <- function(x, type) {
  switch(type,
         "Média" = mean(x),
         "Mediana" = median(x),
         "Desvio Padrão" = sd(x),
         "n" = length(x))
}

piso_por_valor <- function (.data, df.par.convencoes.piso.por.valor, cod.cliente, 
                            corte.piso) 
{
  print(cod.cliente)
  if (corte.piso != 2L) {
    dplyr::left_join(x = .data, 
                     y = dplyr::filter(df.par.convencoes.piso.por.valor, 
                                       cod_cliente == cod.cliente), 
                     by = c("cod_uf", "cod_cbo")) %>% 
      dplyr::filter(salario_mensal_com_reajuste >= piso_valor) %>% 
      dplyr::select(-piso_valor)
  }
  else {
    .data
  }
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
          filter(salario_mensal_com_reajuste <= quantile(salario_mensal_com_reajuste, probs = percentile, type = 2))
      } else {
        .data %>% 
          filter(salario_mensal_com_reajuste >= quantile(salario_mensal_com_reajuste, probs = percentile, type = 2))
      }
    }
  
  .data %>% 
    {
      if(ind_uf) { # Estatística por UF
        group_by(.data = ., cod_uf) %>% 
          summarise(mean = fun_summary(x = salario_mensal_com_reajuste, type = crits$calculus))
      } else {
        if(ind_reg) { 
          # Estatística por região:
          dplyr::mutate(.data = ., reg_uf = dplyr::case_when(cod_uf >= 11 & cod_uf <= 17 ~ "Norte",
                                                             cod_uf >= 21 & cod_uf <= 29 ~ "Nordeste",
                                                             cod_uf >= 31 & cod_uf <= 35 ~ "Sudeste",
                                                             cod_uf >= 41 & cod_uf <= 43 ~ "Sul",
                                                             cod_uf >= 50 & cod_uf <= 53 ~ "Centro-oeste")) %>% 
            dplyr::group_by(reg_uf) %>%
            dplyr::summarise(mean = fun_summary(x = salario_mensal_com_reajuste, type = crits$calculus)) %>% 
            dplyr::left_join(x = CAGED::info_ufs[, c("cod_uf", "sigla_uf", "reg_uf")], y = ., by = "reg_uf") %>% 
            dplyr::transmute(mean = mean, cod_uf = cod_uf)
        } else { 
          # Estatística Nacional:
          dplyr::summarise(.data = ., 
                           mean = fun_summary(x = salario_mensal_com_reajuste, type = crits$calculus), 
                           cod_uf = CAGED::info_ufs$cod_uf)
        }
      }
    }
} 

str_split_caged <- function(x) {
  unique(strsplit(paste0(x, collapse = "; "), split = "; ")[[1]])
}


#' @title Resume os dados por UF de acordo com o DeProjParaCBO
#' 
#' @description Permite obter as estatísticas da variável salário de acordo com o DeProjParaCBO do respectivo
#' projeto.
#' 
#' @name summary_caged
#' 
#' @param conn.caged Objeto DBIConnection produzido por DBI::dbConnect() do BD MDO.
#' 
#' @param conn.auxiliar Objeto DBIConnection produzido por DBI::dbConnect() do BD DadosAuxiliares.
#' 
#' @param proj Nome do projeto (Padronizar nomes no futuro).
#' 
#' @param data.ref.geral Data de referência do projeto.
#' 
#' @param par.caged Configurações usadas no projeto específico que implicam em filtros nos dados do caged.
#' 
#' @examples  
#' 
#' # system.time(summary_caged(conn.caged = DBI::dbConnect(RSQLite::SQLite(), "/home/rumenick/Documentos/MDO.sqlite"), 
#' # conn.auxiliar = DBI::dbConnect(RSQLite::SQLite(), "/home/rumenick/Documentos/MDODadosAuxiliares.sqlite"), 
#' # proj = "SICRO_secao_f", 
#' # data.ref.geral = "01/2020") -> teste)
#' 
#' @importFrom utils data
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @export
summary_caged <- function(conn.caged, conn.auxiliar, proj, data.ref.geral) {
  # Lendo a tabela MetadadosProjeto para o projeto e data de referência selecionado:
  metadados_projeto <- 
    conn.caged %>% 
    dplyr::tbl("MetadadosProjeto") %>%
    dplyr::filter(projeto == proj, 
                  data_ref_geral == data.ref.geral) %>% 
    dplyr::collect()
  
  # Lendo oa tabela DeProjParaCBO para o projeto e data de referência selecionado:
  de_proj_para_cbo <- 
    conn.caged %>% 
    dplyr::tbl("DeProjParaCBO") %>%
    dplyr::filter(projeto == proj, 
                  data_ref_geral == data.ref.geral) %>% 
    dplyr::collect()
  
  # Criando lsita de parâmetros do CAGED:
  
  par_caged <- list("admitidos" = str_split_caged(metadados_projeto$admitidos), 
                    "qtd_horas_contrat" = str_split_caged(metadados_projeto$qtd_horas_contrat), 
                    "porte_empresa" = str_split_caged(metadados_projeto$porte_empresa), 
                    "escolaridade" = str_split_caged(metadados_projeto$escolaridade), 
                    "cor" = str_split_caged(metadados_projeto$cor), 
                    "sexo" = str_split_caged(metadados_projeto$sexo), 
                    "tipo_mov" = str_split_caged(metadados_projeto$tipo_mov), 
                    "ind_contrato_parcial" = str_split_caged(metadados_projeto$ind_contrato_parcial), 
                    "ind_contrato_intermitente" = str_split_caged(metadados_projeto$ind_contrato_intermitente), 
                    "cod_uf" = str_split_caged(metadados_projeto$cod_uf),
                    # Add CBO's:
                    "cod_cbo" = str_split_caged(de_proj_para_cbo$cod_cbo),
                    # Add competências declarada:
                    "comp_declarada" = select_period_caged(data.ref.coleta = metadados_projeto$data_ref_coleta,
                                                           n.comp.coleta = metadados_projeto$ncompetencias))
  
  # Parâmetros gerais:
  
  # Parâmetros padrão:
  
  # Filtrando a tabela CAGED para o projeto e data de referência selecionado:
  caged <- 
    conn.caged %>% 
    filter_caged(par.caged = par_caged)
  
  # Filtrando a tabela CAGED de acordo com salário mínimo atual e anteriar:
  comp_declarada_ano_atual <-  par_caged$comp_declarada[grepl(pattern = strsplit(metadados_projeto$data_ref_coleta, "/")[[1]][2], par_caged$comp_declarada)]
  comp_declarada_ano_anterior <-  par_caged$comp_declarada[grepl(pattern = as.numeric(strsplit(metadados_projeto$data_ref_coleta, "/")[[1]][2]) - 1, par_caged$comp_declarada)]
  salario_minimo_atual <- metadados_projeto$salario_minimo_atual
  salario_minimo_anterior <- metadados_projeto$salario_minimo_anterior
  
  caged <- 
    caged %>%
    dplyr::filter(((comp_declarada %in% comp_declarada_ano_atual) & (salario_mensal >= salario_minimo_atual)) | 
                    ((comp_declarada %in% comp_declarada_ano_anterior) & (salario_mensal >= salario_minimo_anterior))) %>% 
    dplyr::mutate(salario_mensal_com_reajuste = salario_mensal)
  # Check
  # caged %>%
  #   group_by(comp_declarada) %>%
  #   summarise(min = min(salario_mensal)) %>% View()
  
  # Realizar reajustes no salário mensal considerando as condições sobre a
  # comp. declarada, salário mensal e tempo de permanência por UF:
  
  # Lendo a tabela ParConvencoesReajuste para o projeto e data de referência selecionado: 
  par_convencoes_reajuste <- 
    conn.auxiliar %>% 
    dplyr::tbl("ParConvencoesReajuste") %>% 
    dplyr::filter(projeto == proj, 
                  data_ref_geral == data.ref.geral,
                  status == "ativo") %>% 
    dplyr::collect() %>% 
    dplyr::mutate(cod_uf = as.numeric(cod_uf)) %>% 
    dplyr::select(cod_uf, dplyr::starts_with("crit"), reajuste)
  
  # Aplicando os reajustes: 
  if (nrow(par_convencoes_reajuste) > 0) {
    for (i in 1:nrow(par_convencoes_reajuste)) {
      crit_comp_declarada <- strsplit(as.character(par_convencoes_reajuste$crit_comp_declarada[i]), ",")[[1]]
      if (length(crit_comp_declarada) == 1) {
        crit_comp_declarada <- paste0("caged$comp_declarada ", crit_comp_declarada[1])
      } else {
        crit_comp_declarada <- paste0("caged$comp_declarada ", crit_comp_declarada[1], " & caged$comp_declarada ", crit_comp_declarada[2])
      }
      
      crit_tempo_emprego <- strsplit(as.character(par_convencoes_reajuste$crit_tempo_emprego[i]), ",")[[1]]
      if (length(crit_tempo_emprego) == 1) {
        crit_tempo_emprego <- paste0("caged$tempo_emprego ", crit_tempo_emprego[1])
      } else {
        crit_tempo_emprego <- paste0("caged$tempo_emprego ", crit_tempo_emprego[1], " & caged$tempo_emprego ", crit_tempo_emprego[2])
      }
      
      crit_salario_mensal <- strsplit(as.character(par_convencoes_reajuste$crit_salario_mensal[i]), ",")[[1]]
      if (length(crit_salario_mensal) == 1) {
        crit_salario_mensal <- paste0("caged$salario_mensal ", crit_salario_mensal[1])
      } else {
        crit_salario_mensal <- paste0("caged$salario_mensal ", crit_salario_mensal[1], " & caged$salario_mensal ", crit_salario_mensal[2])
      }
      
      crit_busca <- paste0("caged$salario_mensal_com_reajuste <- ifelse(caged$cod_uf == ", 
                           par_convencoes_reajuste$cod_uf[i], " & ", 
                           crit_comp_declarada, " & ", 
                           crit_tempo_emprego, " & ", 
                           crit_salario_mensal, ", caged$salario_mensal", 
                           par_convencoes_reajuste$reajuste[i], ", caged$salario_mensal_com_reajuste)")
      
      eval(parse(text = crit_busca))
    }
  }
  
  # Check
  # caged %>% 
  #   filter(cod_uf == "11", as.numeric(comp_declarada) <= 201904) %>% 
  #   select(salario_mensal, salario_mensal_com_reajuste) %>% 
  #   View()
  
  
  # Realizando os filtros baseados no piso por fator das categorias:
  par_convencoes_piso_por_fator <- 
    conn.auxiliar %>% 
    dplyr::tbl("ParConvencoesPisoPorFator") %>% 
    dplyr::filter(projeto == proj, 
                  data_ref_geral == data.ref.geral) %>% 
    dplyr::collect() %>% 
    dplyr::filter(cod_cliente %in% de_proj_para_cbo$cod_cliente)
  
  
  # Check
  # caged %>%
  #   filter(cod_cbo %in% str_split_caged(de_proj_para_cbo$cod_cbo[par_convencoes_piso_por_fator$cod_cliente[1] == de_proj_para_cbo$cod_cliente])) %>%
  #   group_by(cod_cbo) %>%
  #   summarise(min = min(salario_mensal_com_reajuste), max = max(salario_mensal_com_reajuste))
  
  par_convencoes_piso_por_valor <- 
    conn.auxiliar %>% 
    dplyr::tbl("ParConvencoesPisoPorValor") %>% 
    dplyr::filter(projeto == proj, 
                  data_ref_geral == data.ref.geral) %>% 
    dplyr::collect() %>% 
    dplyr::select(-projeto, -data_ref_geral) %>% 
    dplyr::right_join(x = ., 
                      y = de_proj_para_cbo, by = "cod_cliente") %>% 
    dplyr::select(cod_uf, cod_cliente, unidade, cod_cbo, piso_valor) %>% 
    dplyr::mutate(piso_valor = as.numeric(piso_valor),
                  piso_valor = if_else(unidade == "h", piso_valor * 220, piso_valor),
                  cod_uf  = as.integer(cod_uf)) %>% 
    dplyr::select(-unidade) %>% 
    tidyr::separate_rows(cod_cbo, convert = TRUE)
  
  # Obtendo salários médios por UF e critérios:
  de_proj_para_cbo %>% 
    mutate(cod_cliente_aux = cod_cliente) %>% 
    dplyr::group_by(cod_cliente) %>%
    tidyr::nest() %>% 
    dplyr::mutate(summary = lapply(data, FUN = function(df.cliente, 
                                                        df.caged, 
                                                        df.par.convencoes.piso.por.valor) { 
      df.caged %>% 
        dplyr::filter(cod_cbo %in% str_split_caged(df.cliente$cod_cbo)) %>%
        piso_por_valor(df.par.convencoes.piso.por.valor, 
                       cod.cliente = df.cliente$cod_cliente_aux,
                       corte.piso = metadados_projeto$corte_piso) %>%
        calc_means(crits = list(cut = df.cliente$crit_corte_percentil,
                                spatial = df.cliente$crit_espacial,
                                calculus = df.cliente$crit_calc))
    }, 
    df.caged = caged,
    df.par.convencoes.piso.por.valor = par_convencoes_piso_por_valor)) %>% 
    tidyr::unnest(cols = c(data, summary)) %>% 
    dplyr::mutate(mean = ifelse(unidade == "h", as.numeric(calc_fator) * mean, as.numeric(calc_fator)  * mean)) %>% 
    ungroup()
}

# Faltar aplicar fator (obs.: fator 220)
# Verificar cálculos

# conn.caged = DBI::dbConnect(RSQLite::SQLite(), "/home/rumenick/Documentos/MDO.sqlite")
# conn.auxiliar = DBI::dbConnect(RSQLite::SQLite(), "/home/rumenick/Documentos/MDODadosAuxiliares.sqlite")
# proj = "SICRO_secao_f"
# data.ref.geral = "01/2020"

# conn.caged = DBI::dbConnect(RSQLite::SQLite(), "C:/Users/rumen/Documents/MDO.sqlite")
# conn.auxiliar = DBI::dbConnect(RSQLite::SQLite(), "C:/Users/rumen/Documents/MDODadosAuxiliares.sqlite")
# proj = "SICRO_secao_f"
# data.ref.geral = "01/2020"


