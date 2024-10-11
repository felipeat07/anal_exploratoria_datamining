# Carregar bibliotecas necessárias
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(ggplot2)
library(reshape2)

# Carregar o arquivo CSV (substitua pelo caminho correto do arquivo)
dados <- read.csv("students_mental_health_survey.csv", na.strings = "")

# Remover linhas com valores ausentes
dados <- na.omit(dados)

str(dados)
summary(dados)


# Transformar variáveis categóricas em fatores
dados <- dados %>%
  mutate_if(is.character, as.factor)

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

# Criar novas features com combinações diferentes
dados <- dados %>%
  mutate(
    Stress_Anxiety = Stress_Level * Anxiety_Score,  # Multiplicação de estresse e ansiedade
    Stress_Sleep = Stress_Level + as.numeric(Sleep_Quality),  # Soma do estresse e qualidade do sono
    Physical_Support = as.numeric(Physical_Activity) + as.numeric(Social_Support),  # Soma de atividade física e suporte social
    Diet_Quality_Support = as.numeric(Diet_Quality) * as.numeric(Social_Support)  # Multiplicação de qualidade da dieta e suporte social
  )

# Normalizar as novas variáveis para que fiquem na mesma escala
dados_normalizados <- dados %>%
  mutate_at(vars(Stress_Anxiety, Stress_Sleep, Physical_Support, Diet_Quality_Support), scale)

# Dividir os dados em conjunto de treino e teste
set.seed(123)  # Para reprodutibilidade
train_index <- createDataPartition(dados_normalizados$Depression_Score, p = 0.7, list = FALSE)
train_data <- dados_normalizados[train_index, ]
test_data <- dados_normalizados[-train_index, ]

# Treinar o modelo de árvore de decisão sem CGPA_Depression
modelo_arvore <- rpart(Depression_Score ~ Stress_Anxiety + Stress_Sleep + Physical_Support + Diet_Quality_Support,
                       data = train_data, method = "class")

# Plotar a árvore de decisão
rpart.plot(modelo_arvore, type = 3, extra = 102, fallen.leaves = TRUE, 
           main = "Árvore de Decisão para Prever Depression Score", 
           box.palette = "RdBu", shadow.col = "gray", branch.lty = 3)

# Fazer previsões no conjunto de teste
predicoes <- predict(modelo_arvore, newdata = test_data, type = "class")

# Criar a matriz de confusão
matriz_confusao <- confusionMatrix(predicoes, test_data$Depression_Score)

# Exibir a matriz de confusão
print(matriz_confusao)








