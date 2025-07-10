# -------------------------------------------------------------------------- #
# SCRIPT PARA ANÁLISE DE TÓPICOS ESTRUTURAIS (STM)
# Análise das Sugestões da Sociedade sobre Meio Ambiente para a Constituinte
# -------------------------------------------------------------------------- #

# ========================================================================== #
# 0. CONFIGURAÇÃO INICIAL (SETUP)
# ========================================================================== #

# --- Comentário Metodológico ---
# Carregamos todos os pacotes necessários para a análise. O 'stm' é o pacote
# central para a modelagem. O 'tidyverse' (com 'dplyr', 'ggplot2', etc.) é
# usado para a manipulação e visualização dos dados. 'tidytext' e 'broom'
# nos ajudam a extrair e organizar os resultados do modelo de forma limpa.

# Se for a primeira vez, instale os pacotes (remova o '#' da frente)
# install.packages(c("tidyverse", "stm", "lubridate", "janitor", "broom", "tidytext"))

library(tidyverse)
library(stm)
library(lubridate)
library(janitor)
library(broom)
library(tidytext)
library(forcats)

# ========================================================================== #
# 1. CARREGAMENTO E FILTRAGEM DOS DADOS
# ========================================================================== #

# --- Comentário Metodológico ---
# O ponto de partida é o conjunto de dados completo. Para focar a análise,
# filtramos o corpus para reter apenas as sugestões explicitamente relacionadas
# à pauta ambiental. Isso aumenta a coerência temática do modelo final.

# Define o diretório de trabalho (altere para o seu caminho)
setwd("C:/Users/Marcelo/Downloads")

# Carrega a base de dados
df <- read_csv2("Base SAIC.csv", locale = locale(encoding = "Latin1"))

# Limpa nomes das colunas e remove espaços em branco
cartas <- df %>%
  mutate(across(where(is.character), ~ str_trim(.))) %>%
  clean_names()

# Define as palavras-chave para filtrar o corpus ambiental
palavras_chave_ambiental <- c(
  "meio ambiente", "ecologia", "ecologica", "ecológica", "ecologico", "ecológico",
  "flora", "fauna", "poluicao", "poluição"
)
regex_busca <- str_c(palavras_chave_ambiental, collapse = "|")

# Filtra as cartas que contêm as palavras-chave no catálogo ou na indexação
cartas_filtradas <- cartas %>%
  filter(str_detect(str_to_lower(catalogo), regex_busca) |
           str_detect(str_to_lower(indexacao), regex_busca))


# ========================================================================== #
# 2. PRÉ-PROCESSAMENTO DAS COVARIÁVEIS
# ========================================================================== #

# --- Comentário Metodológico ---
# Nesta etapa, preparamos os metadados que serão usados no modelo.
# Variáveis categóricas são convertidas para o tipo 'fator'. É crucial
# definir as categorias de referência para a análise de regressão. As
# referências foram escolhidas para representar grupos de comparação
# relevantes (e.g., SP como polo populacional, MASCULINO como grupo
# majoritário na amostra, etc.). Valores ausentes (NA) são convertidos
# em uma categoria explícita para evitar a perda de dados.

df_processado <- cartas_filtradas %>%
  filter(!is.na(sugestao_texto), sugestao_texto != "") %>%
  mutate(
    uf = fct_relevel(as.factor(uf), "SP"),
    sexo = fct_relevel(fct_explicit_na(as.factor(sexo), na_level = "NA_desconhecido"), "MASCULINO"),
    morador = fct_relevel(fct_explicit_na(as.factor(morador), na_level = "NA_desconhecido"), "ZONA RURAL"),
    instrucao = fct_relevel(fct_explicit_na(as.factor(instrucao), na_level = "NA_desconhecido"), "SEGUNDO GRAU COMPLETO"),
    faixa_etaria = fct_relevel(fct_explicit_na(as.factor(faixa_etaria), na_level = "NA_desconhecido"), "20 A 24 ANOS"),
    estado_civil = fct_explicit_na(as.factor(estado_civil), na_level = "NA_desconhecido"),
    atividade = fct_explicit_na(as.factor(atividade), na_level = "NA_desconhecido"),
    ano = year(dmy(data))
  )


# ========================================================================== #
# 3. PREPARAÇÃO DOS DOCUMENTOS PARA O STM
# ========================================================================== #

# --- Comentário Metodológico ---
# O texto é processado para criar uma matriz de termos e documentos. Etapas
# incluem conversão para minúsculas, remoção de pontuação, números, stopwords
# e aplicação de stemming. O limiar inferior ('lower.thresh') remove termos
# muito raros, o que reduz o ruído e melhora a qualidade do modelo.

processed <- textProcessor(
  documents = df_processado$sugestao_texto,
  metadata = df_processado,
  language = "portuguese",
  stem = TRUE,
  removepunctuation = TRUE,
  removenumbers = TRUE,
  lowercase = TRUE,
  customstopwords = c("sugiro", "gostaria", "constituinte")
)

out <- prepDocuments(
  documents = processed$documents,
  vocab = processed$vocab,
  meta = processed$meta,
  lower.thresh = 10 # Remove palavras que aparecem em menos de 10 documentos
)


# ========================================================================== #
# 4. ESTIMAÇÃO DO MODELO STM
# ========================================================================== #

# --- Comentário Metodológico ---
# Estimamos o modelo STM final. O número de tópicos (K) foi definido como 15
# com base em análises prévias (searchK) e na interpretabilidade dos tópicos.
# A fórmula de 'prevalence' especifica quais covariáveis serão usadas para
# modelar a variação na proporção dos tópicos.

K_escolhido <- 15

stm_model <- stm(
  documents = out$documents,
  vocab = out$vocab,
  K = K_escolhido,
  prevalence = ~ uf + sexo + faixa_etaria + instrucao + morador,
  max.em.its = 100,
  data = out$meta,
  init.type = "Spectral",
  verbose = TRUE
)


# ========================================================================== #
# 5. ANÁLISE DOS EFEITOS DAS COVARIÁVEIS
# ========================================================================== #

# --- Comentário Metodológico ---
# Usamos estimateEffect() para rodar uma série de regressões, investigando
# o efeito de cada covariável na prevalência de cada um dos 15 tópicos.
# O resultado é então convertido para um formato 'tidy' (arrumado), o que
# facilita a filtragem e a visualização subsequente dos resultados.

efeitos <- estimateEffect(
  formula = 1:K_escolhido ~ uf + sexo + faixa_etaria + instrucao + morador,
  stmobj = stm_model,
  meta = out$meta,
  uncertainty = "Global"
)

# Tabela completa com todos os coeficientes, pronta para ser usada
tabela_coeficientes_completa <- tidy(efeitos)


# ========================================================================== #
# 6. VISUALIZAÇÃO DOS RESULTADOS
# ========================================================================== #

# -------------------------------------------------------------------------- #
# 6.1 FUNÇÃO REATORADA PARA PLOTAGEM
# -------------------------------------------------------------------------- #

# --- Comentário Metodológico ---
# Para evitar a repetição de código e facilitar a criação de múltiplos
# gráficos, criamos uma função chamada 'plot_efeito_covariavel'. Esta função
# recebe os dados, o tópico, a variável e os títulos desejados, e gera
# automaticamente um gráfico de "dot-and-whisker", padronizando a visualização.

plot_efeito_covariavel <- function(efeitos_df, num_topico, nome_variavel, ref_completa, titulo, subtitulo, cor) {
  
  # Prepara os dados específicos para o gráfico
  dados_plot <- efeitos_df %>%
    filter(topic == num_topico, grepl(nome_variavel, term)) %>%
    add_row(term = ref_completa, estimate = 0, std.error = 0) %>%
    mutate(term = gsub(nome_variavel, "", term))
  
  # Cria o gráfico com ggplot2
  ggplot(dados_plot, aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, color = "gray70", linetype = "dashed") +
    geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                       xmax = estimate + 1.96 * std.error),
                   height = 0.1, color = "gray50", linewidth = 0.8) +
    geom_point(color = cor, size = 4) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      y = nome_variavel,
      x = "Estimativa do Coeficiente",
      caption = "Intervalos de confiança de 95%."
    ) +
    theme_minimal(base_size = 14)
}


# -------------------------------------------------------------------------- #
# 6.2 GERAÇÃO DOS GRÁFICOS PARA TÓPICOS E VARIÁVEIS DE INTERESSE
# -------------------------------------------------------------------------- #

# --- Comentário Metodológico ---
# Agora, simplesmente chamamos a função que criamos para cada achado
# significativo que desejamos visualizar. Isso torna o código limpo,
# legível e fácil de adicionar novos gráficos no futuro.

# --- Gráficos para o Tópico 8: Pesca e Estrutura Administrativa ---

plot_efeito_covariavel(
  efeitos_tidy = tabela_coeficientes_completa,
  num_topico = 8,
  nome_variavel = "morador",
  ref_completa = "moradorZONA RURAL (Ref.)",
  titulo = "Efeito da Zona de Moradia no Tópico 8 (Pesca)",
  subtitulo = "Referência: ZONA RURAL",
  cor = "#0072B2"
)

plot_efeito_covariavel(
  efeitos_tidy = tabela_coeficientes_completa,
  num_topico = 8,
  nome_variavel = "instrucao",
  ref_completa = "instrucaoSEGUNDO GRAU COMPLETO (Ref.)",
  titulo = "Efeito da Instrução no Tópico 8 (Pesca)",
  subtitulo = "Referência: SEGUNDO GRAU COMPLETO",
  cor = "#CC79A7"
)


# --- Gráficos para o Tópico 9: Política Agrícola ---

plot_efeito_covariavel(
  efeitos_tidy = tabela_coeficientes_completa,
  num_topico = 9,
  nome_variavel = "uf",
  ref_completa = "ufSP (Ref.)",
  titulo = "Efeito do Estado (UF) no Tópico 9 (Política Agrícola)",
  subtitulo = "Referência: SP",
  cor = "#009E73"
)


# --- Gráficos para o Tópico 10: Gestão de Recursos Naturais ---

plot_efeito_covariavel(
  efeitos_tidy = tabela_coeficientes_completa,
  num_topico = 10,
  nome_variavel = "instrucao",
  ref_completa = "instrucaoSEGUNDO GRAU COMPLETO (Ref.)",
  titulo = "Efeito da Instrução no Tópico 10 (Recursos Naturais)",
  subtitulo = "Referência: SEGUNDO GRAU COMPLETO",
  cor = "#CC79A7"
)
