# --- SCRIPT PARA MODELAGEM DE TÓPICOS ESTRUTURAIS (STM) ---

# Passo 0: Instalar e Carregar Pacotes
# -------------------------------------------------------------------
# Se for a primeira vez, instale os pacotes (remova o '#' da frente)
# install.packages("tidyverse")
install.packages("stm")
install.packages("lubridate")
install.packages("janitor")

library(tidyverse) # Para manipulação de dados (dplyr, readr, etc.)
library(stm)       # O pacote principal para o modelo
library(lubridate) # Para trabalhar com datas
library(janitor)   # Para limpar nomes de colunas

# ---
# Passo 1: Carregar e Limpar os Dados
# -------------------------------------------------------------------
# Substitua "caminho/para/seu/arquivo.csv" pelo local do seu arquivo
# Assumimos que o seu dataframe se chama 'df' como no diagnóstico.
# Se já carregou os dados, pule esta linha.
setwd("C:/Users/mapereira/Downloads")

df <- read_csv2("Base SAIC.csv", locale = locale(encoding = "Latin1"))

cartas <- df |> 
  mutate(across(where(is.character), ~ str_trim(.)))  # remove espaços nas pontas

cartas <- cartas |> 
  clean_names()

# Lista de palavras
palavras <- c(
  "meio ambiente", "ecologia", "ecologica", "ecológica", "ecologico", "ecológico", 
  "flora", "fauna", "poluicao", "poluição"
)

# Monta expressão regular única, escapando acentos e espaços se necessário
regex_busca <- str_c(palavras, collapse = "|")

# Filtra as linhas com qualquer uma das palavras (case-insensitive)
cartas_filtradas <- cartas |> 
  filter(str_detect(str_to_lower(catalogo), str_to_lower(regex_busca)))

# ---
# Passo 2: Pré-processar as Covariáveis (Metadados)
# -------------------------------------------------------------------
# O STM precisa de metadados bem formatados.
df_processado <- cartas_filtradas %>%
  # Remover linhas onde o texto da sugestão está vazio ou é NA
  filter(!is.na(sugestao_texto), sugestao_texto != "") %>%
  
  # Converter as principais covariáveis categóricas para o tipo 'fator'
  # A função 'fct_explicit_na' transforma NAs em uma categoria "NA_desconhecido"
  # Isso é CRUCIAL para não perder dados no modelo.
  mutate(
    uf = as.factor(uf),
    sexo = fct_explicit_na(as.factor(sexo), na_level = "NA_desconhecido"),
    morador = fct_explicit_na(as.factor(morador), na_level = "NA_desconhecido"),
    instrucao = fct_explicit_na(as.factor(instrucao), na_level = "NA_desconhecido"),
    estado_civil = fct_explicit_na(as.factor(estado_civil), na_level = "NA_desconhecido"),
    faixa_etaria = fct_explicit_na(as.factor(faixa_etaria), na_level = "NA_desconhecido"),
    atividade = forcats::fct_explicit_na(as.factor(atividade), na_level = "NA_desconhecido"),
    
    
    # Processar a data
    data_formatada = dmy(data), # Converte "dd/mm/aaaa" para Data
    ano = year(data_formatada)  # Extrai o ano
  )

# Vamos verificar a estrutura dos dados processados
glimpse(df_processado)

# ---
# Passo 3: Processar o Texto
# -------------------------------------------------------------------
# Esta função do STM automatiza a limpeza do texto.
processed <- textProcessor(
  documents = df_processado$sugestao_texto,
  metadata = df_processado, # Anexa os metadados
  language = "portuguese", # Usa lista de stopwords em português
  stem = TRUE, # Reduz palavras ao seu radical (ex: politicos, politica -> politic)
  removepunctuation = TRUE,
  removenumbers = TRUE,
  lowercase = TRUE,
  customstopwords = c("sugiro", "gostaria", "constituinte") # Adicione palavras que você queira remover
)

# ---
# Passo 4: Preparar os Documentos para o STM
# -------------------------------------------------------------------
# Esta é a etapa final de preparação antes de rodar o modelo.
# 'lower.thresh = 10' remove palavras que aparecem em menos de 10 documentos.
# Isso ajuda a focar o modelo nos termos mais relevantes.
out <- prepDocuments(
  documents = processed$documents,
  vocab = processed$vocab,
  meta = processed$meta,
  lower.thresh = 10
)
# 'out' agora contém: out$documents, out$vocab, out$meta

# ---
# Passo 5: Estimar o Modelo STM
# -------------------------------------------------------------------
# 5.1 (Opcional, mas recomendado) Encontrar o número ideal de tópicos (K)
# O código abaixo testa vários valores de K e gera diagnósticos.
# Pode demorar bastante dependendo do tamanho da base.
# Se quiser rodá-lo, remova o '#' no início e no fim do bloco.
#
search_k <- searchK(
  out$documents,
  out$vocab,
  K = c(10, 15, 20, 25), # Valores de K que você quer testar
  prevalence = ~ uf + sexo + faixa_etaria,
  data = out$meta,
  verbose = TRUE
)
plot(search_k)
#

# 5.2 Estimar o modelo final
# Escolha um valor de K (número de tópicos). Vamos começar com K=20 como um bom ponto de partida.
# A fórmula de 'prevalence' define quais metadados influenciarão a frequência dos tópicos.
K_escolhido <- 20

stm_model <- stm(
  documents = out$documents,
  vocab = out$vocab,
  K = K_escolhido,
  prevalence = ~ uf + sexo + faixa_etaria + instrucao + morador + atividade, # Defina aqui suas covariáveis de interesse!
  max.em.its = 75, # Número máximo de iterações.
  data = out$meta,
  init.type = "Spectral", # Método de inicialização
  verbose = TRUE # Mostra o progresso da estimação
)

# ---
# Passo 6: Análise Inicial dos Resultados
# -------------------------------------------------------------------
# Ver um resumo do modelo
summary(stm_model)

# Visualizar as palavras mais prováveis de cada tópico
# Mostra as palavras mais importantes para cada um dos 20 tópicos
labelTopics(stm_model)

# Gráfico com a proporção esperada de cada tópico no corpus
plot(stm_model, type = "summary", text.cex = 0.8, main = "Proporção dos Tópicos no Corpus")

# Mais análises e visualizações virão na próxima etapa!