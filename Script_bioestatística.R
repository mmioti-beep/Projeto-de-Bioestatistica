# ------------------------------------------------------------
# 1. Conexão com banco de dados e leitura dos dados SRAG
# ------------------------------------------------------------

pacotes <- c(
  "DBI",
  "RSQLite",
  "dplyr",
  "ggplot2",
  "lubridate",
  "tidyr",
  "purrr",
  "gtsummary",
  "flextable",
  "broom"
)

# Instala apenas o que estiver faltando
instalar <- pacotes[!pacotes %in% installed.packages()[, "Package"]]
if(length(instalar) > 0){
  install.packages(instalar)
}

# Carrega todos
invisible(lapply(pacotes, library, character.only = TRUE))

# Conexão com banco SQLite
con_ate25 <- dbConnect(RSQLite::SQLite(), "C:/Users/hecto/OneDrive/Documentos/Mari/Mestrado/Projeto/Dados SRAG/srag_ate25.sqlite")

# Colunas de interesse
cols_interesse <- c(
  "PCR_VSR",
  "TP_IDADE",
  "NU_IDADE_N",
  "CS_SEXO",
  "CS_RACA",
  "FEBRE",
  "TOSSE",
  "DISPNEIA",
  "SATURACAO",
  "CARDIOPATI",
  "SIND_DOWN",
  "NEUROLOGIC",
  "PNEUMOPATI",
  "IMUNODEPRE",
  "UTI",
  "SUPORT_VEN",
  "EVOLUCAO",
  "SG_UF",
  "SEM_NOT",
  "DT_EVOLUCA",
  "DT_ENTUTI",
  "DT_INTERNA",
  "SEM_PRI",
  "DT_NASC",
  "DT_NOTIFIC"
)

# Função para leitura e limpeza inicial dos dados
read_srag_clean <- function(con, tabela, cols_keep) {
  tbl(con, tabela) %>%
    filter(PCR_VSR == 1) %>%                # Apenas casos confirmados de VSR
    select(any_of(cols_keep)) %>%
    collect() %>%
    mutate(across(everything(), as.character)) # Garantir consistência de tipos
}

# Listar tabelas de 2019 a 2025
anos <- 2019:2025
tabelas <- paste0("srag", anos)

# Ler e combinar dados de todos os anos
dados_list <- map(tabelas, ~ read_srag_clean(con_ate25, .x, cols_interesse) %>%
                    mutate(ano = substr(.x, 5, 8)))
dados_rsv <- bind_rows(dados_list)

# ------------------------------------------------------------
# 2. Tratamento de datas e cálculo da idade em meses
# ------------------------------------------------------------
converter_data <- function(x) {
  suppressWarnings(
    case_when(
      grepl("T", x) ~ as.Date(ymd_hms(x)), # ISO 8601
      as.numeric(x) >= 1e9 ~ as.Date(as.POSIXct(as.numeric(x), origin="1970-01-01", tz="UTC")), # segundos
      TRUE ~ as.Date(as.numeric(x), origin="1970-01-01") # dias
    )
  )
}

dados_rsv <- dados_rsv %>%
  mutate(
    DT_INTERNA   = converter_data(DT_INTERNA),
    DT_EVOLUCA   = converter_data(DT_EVOLUCA),
    DT_NASC      = converter_data(DT_NASC),
    DT_NOTIFIC   = converter_data(DT_NOTIFIC),
    DT_ENTUTI    = converter_data(DT_ENTUTI),
    idade_meses  = interval(DT_NASC, DT_NOTIFIC) %/% months(1) # idade em meses
  )

crianca_rsv <- dados_rsv %>%  ##contagem de crianças e adolescentes para visualização##
  filter(
    !is.na(idade_meses),
    idade_meses > 0,
    idade_meses < 204
  )


# Contagem por mês
dist_idade <- crianca_rsv %>%
  count(idade_meses) %>%
  arrange(idade_meses) %>%
  mutate(
    cumulativo = cumsum(n),
    perc = 100 * cumulativo / sum(n)
  )


# gráfico cumulativo

ggplot(dist_idade, aes(x = idade_meses, y = perc)) +
  geom_line(color = "darkorange", size = 1.2) +
  geom_point(color = "red") +
  scale_x_continuous(breaks = seq(0, 204, by = 12)) +
  labs(
    title = "Proporção acumulada de casos de RSV",
    x = "Idade (meses)",
    y = "Percentual acumulado (%)"
  ) +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------
# 3. Seleção de crianças até 60 meses e definição de "grave"
# ------------------------------------------------------------
bebes_rsv <- dados_rsv %>%
  filter(!is.na(idade_meses), idade_meses > 0, idade_meses < 60) %>%
  mutate(
    grave = case_when(
      UTI == "1" | SUPORT_VEN == "1" | EVOLUCAO %in% c("2") ~ "Sim",
      TRUE ~ "Não"
    )
  )

# ------------------------------------------------------------
# 4. Recodificação de variáveis binárias e categóricas
# ------------------------------------------------------------
vars_binarias <- c("FEBRE", "TOSSE", "DISPNEIA", "SATURACAO",
                   "CARDIOPATI", "SIND_DOWN", "NEUROLOGIC",
                   "PNEUMOPATI", "IMUNODEPRE")

bebes_rsv <- bebes_rsv %>%
  mutate(
    across(all_of(vars_binarias), ~ case_when(. == "1" ~ "Sim", . == "2" ~ "Não")),
    sexo = case_when(CS_SEXO == "M" ~ "Masculino", CS_SEXO == "F" ~ "Feminino"),
    faixa_etaria = case_when(
      idade_meses < 3 ~ "0–2 meses",
      idade_meses < 6 ~ "3–5 meses",
      idade_meses < 12 ~ "6–11 meses",
      idade_meses < 24 ~ "1 ano",
      idade_meses < 36 ~ "2 anos",
      idade_meses < 48 ~ "3 anos",
      idade_meses < 60 ~ "4 anos"
    ),
    raca = case_when(
      CS_RACA == "1" ~ "Branca",
      CS_RACA == "2" ~ "Preta",
      CS_RACA == "3" ~ "Amarela",
      CS_RACA == "4" ~ "Parda",
      CS_RACA == "5" ~ "Indígena"
    ),
    regiao = case_when(
      SG_UF %in% c("RO","AC","AM","RR","PA","AP","TO") ~ "Norte",
      SG_UF %in% c("MA","PI","CE","RN","PB","PE","AL","SE","BA") ~ "Nordeste",
      SG_UF %in% c("MG","ES","RJ","SP") ~ "Sudeste",
      SG_UF %in% c("PR","SC","RS") ~ "Sul",
      SG_UF %in% c("MS","MT","GO","DF") ~ "Centro-Oeste",
      TRUE ~ "Outro"
    )
  )

# ------------------------------------------------------------
# 5. Estatística descritiva (Tabela 1)
# ------------------------------------------------------------

# Tabela com desfechos graves
tabela_desfechos <- bebes_rsv %>%
  summarise(
    UTI = sum(UTI == 1, na.rm = TRUE),
    Ventilacao = sum(SUPORT_VEN == 1, na.rm = TRUE),
    Obito = sum(EVOLUCAO == 2, na.rm = TRUE),
    Total = n()
  ) %>%
  mutate(
    UTI_label = paste0(UTI, " (", round(100 * UTI / Total, 1), "%)"),
    Ventilacao_label = paste0(Ventilacao, " (", round(100 * Ventilacao / Total, 1), "%)"),
    Obito_label = paste0(Obito, " (", round(100 * Obito / Total, 1), "%)")
  ) %>%
  select(UTI_label, Ventilacao_label, Obito_label, Total)

tabela_desfechos


tabela1 <- bebes_rsv %>%
  select(grave, sexo, faixa_etaria, raca, regiao,
         FEBRE, TOSSE, DISPNEIA, SATURACAO,
         CARDIOPATI, SIND_DOWN, NEUROLOGIC,
         PNEUMOPATI, IMUNODEPRE, idade_meses) %>%
  tbl_summary(
    by = grave,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 1,
    label = list(
      sexo ~ "Sexo", faixa_etaria ~ "Faixa etária", raca ~ "Raça", regiao ~ "Região",
      FEBRE ~ "Febre", TOSSE ~ "Tosse", DISPNEIA ~ "Dispneia", SATURACAO ~ "Saturação <95%",
      CARDIOPATI ~ "Cardiopatia", SIND_DOWN ~ "Síndrome de Down",
      NEUROLOGIC ~ "Doença neurológica", PNEUMOPATI ~ "Doença pulmonar crônica",
      IMUNODEPRE ~ "Imunodepressão", idade_meses ~ "Idade (meses)"
    ),
    missing = "no"
  ) %>%
  add_p(test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test")) %>%
  add_overall() %>%
  modify_header(label ~ "**Variável**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Evolução clínica**") %>%
  bold_labels()

tabela1_flex <- as_flex_table(tabela1)

### Visualização gráfica da Estatística descritiva #####

# Dicionário de rótulos
labels_vars <- c(
  FEBRE = "Febre",
  TOSSE = "Tosse",
  DISPNEIA = "Dispneia",
  SATURACAO = "Saturação <95%",
  CARDIOPATI = "Cardiopatia",
  SIND_DOWN = "Síndrome de Down",
  NEUROLOGIC = "Doença neurológica",
  PNEUMOPATI = "Doença pulmonar crônica",
  IMUNODEPRE = "Imunodepressão",
  sexo = "Sexo",
  raca = "Raça",
  regiao = "Região",
  faixa_etaria = "Faixa etária"
)

plot_categoricas <- function(vars, titulo){
  dados_long <- bebes_rsv %>%
    select(grave, all_of(vars)) %>%
    pivot_longer(cols = -grave, names_to = "Variavel", values_to = "Categoria") %>%
    filter(!is.na(Categoria)) %>%
    mutate(Variavel = recode(Variavel, !!!labels_vars))   # aplica rótulos
  
  ggplot(dados_long, aes(x = Categoria, fill = grave)) +
    geom_bar(position = "fill") +
    facet_wrap(~ Variavel, scales = "free_x", ncol = 2) +  # <- força uma coluna +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = titulo,
      x = "Categoria",
      y = "Proporção dentro do grupo"
    ) +
    theme_minimal(base_size = 14)
}

# Sintomas
plot_categoricas(c("FEBRE","TOSSE","DISPNEIA","SATURACAO"),
                 "Distribuição de sintomas por evolução clínica")

# Comorbidades
plot_categoricas(c("CARDIOPATI","SIND_DOWN","NEUROLOGIC","PNEUMOPATI","IMUNODEPRE"),
                 "Distribuição de comorbidades por evolução clínica")

# Sociodemográficas
plot_categoricas(c("regiao", "sexo","raca", "faixa_etaria"),
                 "Distribuição sociodemográfica por evolução clínica")
#idade
plot_categoricas(c("faixa_etaria"),
                 "Distribuição da faixa etária por evolução clínica")


# -----------------------------------------------------------------------------------
# 6. Testes de associação (Qui-quadrado) para confirmação dos resultados da tabela 1 
# ------------------------------------------------------------------------------------
avaliar_chisq <- function(var){
  tab <- bebes_rsv %>%
    filter(!is.na(.data[[var]])) %>%
    select(all_of(var), grave) %>%
    table()
  teste <- chisq.test(tab)
  tibble(Variavel = var,
         X2 = round(teste$statistic, 3),
         gl = teste$parameter,
         p_valor = signif(teste$p.value, 4))
}

vars_categoricas <- c("sexo","raca","faixa_etaria","regiao",
                      "FEBRE","TOSSE","DISPNEIA","SATURACAO",
                      "CARDIOPATI","SIND_DOWN","NEUROLOGIC",
                      "PNEUMOPATI","IMUNODEPRE")

resultados_chisq <- map_dfr(vars_categoricas, avaliar_chisq)

# ------------------------------------------------------------
# 7. Regressão logística multivariada + Forest plot único
# ------------------------------------------------------------

# Criar variável binária para evolução grave
bebes_rsv <- bebes_rsv %>%
  mutate(grave_bin = ifelse(grave == "Sim", 1, 0))

# Ajustar modelo multivariado
modelo_mult <- glm(
  grave_bin ~ faixa_etaria + sexo + raca + regiao +
    FEBRE + TOSSE + DISPNEIA + SATURACAO +
    CARDIOPATI + SIND_DOWN + NEUROLOGIC +
    PNEUMOPATI + IMUNODEPRE,
  data = bebes_rsv,
  family = binomial,
  na.action = na.omit
)

# Extrair resultados (OR, IC95, p-valor)
resultados_mult <- tidy(modelo_mult, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = round(estimate, 2),
    low = round(conf.low, 2),
    high = round(conf.high, 2),
    label_num = paste0(OR, " (", low, "–", high, ")")
  )

# Renomear variáveis para rótulos curtos
resultados_mult <- resultados_mult %>%
  mutate(term_label = recode(term,
                             "faixa_etaria1 ano" = "1 ano",
                             "faixa_etaria2 anos" = "2 anos",
                             "faixa_etaria3–5 meses" = "3–5 meses",
                             "faixa_etaria3 anos" = "3 anos",
                             "faixa_etaria4 anos" = "4 anos",
                             "faixa_etaria6–11 meses" = "6–11 meses",
                             "sexoMasculino" = "Masculino",
                             "racaBranca" = "Branca",
                             "racaIndígena" = "Indígena",
                             "racaParda" = "Parda",
                             "racaPreta" = "Preta",
                             "regiaoNordeste" = "Nordeste",
                             "regiaoNorte" = "Norte",
                             "regiaoOutro" = "Outro",
                             "regiaoSudeste" = "Sudeste",
                             "regiaoSul" = "Sul",
                             "FEBRESim" = "Febre",
                             "TOSSESim" = "Tosse",
                             "DISPNEIASim" = "Dispneia",
                             "SATURACAOSim" = "Sat <95%",
                             "CARDIOPATISim" = "Cardiopatia",
                             "SIND_DOWNSim" = "Síndrome de Down",
                             "NEUROLOGICSim" = "Doença neurológica",
                             "PNEUMOPATISim" = "Doença pulmonar crônica",
                             "IMUNODEPRESim" = "Imunodepressão"
  ))

# Criar grupos (igual à Tabela 1)
resultados_mult <- resultados_mult %>%
  mutate(grupo = case_when(
    grepl("ano|meses", term_label) ~ "Faixa etária",
    term_label %in% c("Masculino") ~ "Sexo",
    term_label %in% c("Branca","Indígena","Parda","Preta") ~ "Raça/cor",
    term_label %in% c("Nordeste","Norte","Outro","Sudeste","Sul") ~ "Região",
    term_label %in% c("Febre","Tosse","Dispneia","Sat <95%") ~ "Sintomas",
    TRUE ~ "Comorbidades"
  ))

plot_forest <- function(df, grupo_nome) {
  df %>%
    filter(grupo == grupo_nome) %>%
    ggplot(aes(x = term_label, y = OR, ymin = low, ymax = high)) +
    geom_pointrange(color = "darkblue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    # Coluna de valores à direita
    geom_text(aes(y = high + 0.5, label = label_num),
              hjust = 0, size = 3.5, color = "black") +
    coord_flip() +
    labs(y = "Odds Ratio (IC95%)", x = "",
         title = paste("Forest plot -", grupo_nome)) +
    theme_minimal(base_size = 12) +
    ylim(0,5)
}

# Exemplo de uso
plot_forest(resultados_mult, "Comorbidades")
plot_forest(resultados_mult, "Faixa etária")
plot_forest(resultados_mult, "Sintomas")
plot_forest(resultados_mult, "Raça/cor")
plot_forest(resultados_mult, "Região")
plot_forest(resultados_mult, "Sexo")



# Visualizar tabela final
print(resultados_mult, n = Inf)
