library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(ggplot2)

# Carregar o arquivo CSV (substitua pelo caminho correto do arquivo)
dados <- read.csv("students_mental_health_survey.csv", na.strings = "")

# Remover linhas com valores ausentes
dados <- na.omit(dados)

# Transformar variáveis categóricas em fatores
dados <- dados %>%
  mutate_if(is.character, as.factor)

# Verificar quais variáveis são numéricas e quais são categóricas
str(dados)


# Contagem de amostras em cada classe de Depression_Score
table(dados$Depression_Score)

# Definir a classe minoritária
minor_class_count <- min(table(dados$Depression_Score))

# Realizar undersampling para balancear as classes
dados_balanceados <- dados %>%
  group_by(Depression_Score) %>%
  sample_n(minor_class_count) %>%
  ungroup()

# Contagem das amostras após o balanceamento
table(dados_balanceados$Depression_Score)

# Plotar o histograma antes do balanceamento
ggplot(dados, aes(x = as.factor(Depression_Score), fill = as.factor(Depression_Score))) +
  geom_bar() +
  labs(title = "Distribuição das Classes de Depression_Score (Antes do Balanceamento)", 
       x = "Depression Score", y = "Contagem") +
  theme_minimal()

# Plotar o histograma após o balanceamento
ggplot(dados_balanceados, aes(x = as.factor(Depression_Score), fill = as.factor(Depression_Score))) +
  geom_bar() +
  labs(title = "Distribuição das Classes de Depression_Score (Após o Balanceamento)", 
       x = "Depression Score", y = "Contagem") +
  theme_minimal()










library(ggplot2)
library(dplyr)
library(caret)
library(reshape2)

# Criar novas features com combinações diferentes
dados <- dados %>%
  mutate(
    Stress_Anxiety = Stress_Level * Anxiety_Score,  # Multiplicação de estresse e ansiedade
    Stress_Sleep = Stress_Level + as.numeric(Sleep_Quality),  # Soma do estresse e qualidade do sono
    Physical_Support = as.numeric(Physical_Activity) + as.numeric(Social_Support),  # Soma de atividade física e suporte social
    Diet_Quality_Support = as.numeric(Diet_Quality) * as.numeric(Social_Support),  # Multiplicação de qualidade da dieta e suporte social
    CGPA_Depression = CGPA * (5 - as.numeric(Depression_Score))  # Produto do CGPA e inverso do escore de depressão
  )

# Normalizar as novas variáveis para que fiquem na mesma escala
dados_normalizados <- dados %>%
  mutate_at(vars(Stress_Anxiety, Stress_Sleep, Physical_Support, Diet_Quality_Support, CGPA_Depression), scale)

# Verificar as primeiras linhas com as novas features normalizadas
head(dados_normalizados)

# Selecionar apenas as colunas relevantes para a correlação
dados_selecionados <- dados_normalizados %>%
  select(Depression_Score, Stress_Anxiety, Stress_Sleep, Physical_Support, Diet_Quality_Support, CGPA_Depression)

# Transformar Depression_Score em numérico para calcular a correlação
dados_selecionados$Depression_Score <- as.numeric(as.character(dados_selecionados$Depression_Score))

# Calcular a correlação entre as novas variáveis e Depression_Score
correlacoes <- cor(dados_selecionados, use = "complete.obs")

# Exibir a matriz de correlação
print(correlacoes)

# Plotar um heatmap para visualizar a correlação
cor_data <- melt(correlacoes)
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlação") +
  theme_minimal() +
  labs(title = "Heatmap de Correlação entre Novas Features e Depression_Score")
