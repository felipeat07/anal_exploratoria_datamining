# Carregar o arquivo CSV
dados <- read.csv("students_mental_health_survey.csv")

# Visualizar as primeiras linhas
head(dados)


# Resumo básico dos dados
str(dados)

# Resumo estatístico das variáveis numéricas
summary(dados)

# Resumo mais completo com o pacote skimr
skim(dados)

# Verificar valores ausentes
colSums(is.na(dados))


# Estatísticas descritivas para variáveis numéricas
dados %>%
  select_if(is.numeric) %>%
  summary()

# Contagem de valores para variáveis categóricas
dados %>%
  select_if(is.factor) %>%
  summarise_all(~ list(table(.)))



