library("daltoolbox")
library("ggplot2")
library("RColorBrewer")
library("dplyr")

colors <- brewer.pal(9, 'Set1')
# setting the font size for all charts
font <- theme(text = element_text(size=16))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Carregar o arquivo CSV
dados <- read.csv("students_mental_health_survey.csv", na.strings = "")

# Visualizar as primeiras linhas
head(dados)

# Visualizar a estrutura dos dados
str(dados)

# Resumo das estatísticas descritivas para os dados numéricos
summary(select_if(dados, is.numeric))

# Visualizar dados faltantes
missing_data <- colSums(is.na(dados))
missing_data <- missing_data[missing_data > 0]
print(missing_data)

# Tratamento dos dados faltantes // Não sei se é nesse momento que é para fazer
# Preencher os CGPA com a média
# dados$CGPA[is.na(dados$CGPA)] <- mean(dados$CGPA, na.rm = TRUE)
# 
# # Preencher o Substance_Use com a moda // Não tem função de moda no R \0/
# dados$Substance_Use[is.na(dados$Substance_Use)] <- getmode(dados$Substance_Use)

# Esse gráfico mostra no eixo X o depression score e no eixo Y a média de idade (Não é possível identificar nada)
data <- dados |> group_by(Depression_Score) |> summarize(Age=mean(Age))
grf <- plot_bar(data, colors=colors[0:nrow(data)]) + font
plot(grf)

# Esse gráfico mostra no eixo X o depression score e no eixo Y o Coeficiente de rendimento  (Não é possível identificar nada)
data <- dados |> group_by(Depression_Score) |> summarize(CGPA=mean(CGPA))
grf <- plot_bar(data, colors=colors[0:nrow(data)]) + font
plot(grf)

# Esse gráfico mostra no eixo X o depression score e no eixo Y o   (Não é possível identificar nada)
data <- dados |> group_by(Depression_Score, Gender ) |> summarize(Count = n())
grf <- plot_groupedbar(data, colors=colors[1:2]) + font
plot(grf)

# Visualização de distribuições das variáveis numéricas // Distribuições ??? (Na verdade está mostrando só as colunas com variáveis numericas)
numeric_vars <- dados %>% select_if(is.numeric)
print(numeric_vars)


#plotando box_plot da variaveis numericas // Não me diz nada
grf <- plot_boxplot(numeric_vars, colors="white") + font
plot(grf)

# Filtrar apenas as colunas "Course" e "CGPA"
dados_course_cgpa <- dados %>%
  select(Course, CGPA)

# Grafico de barra do CGPA por curso // Não entendi o que é o eixo y desse gáfico // Agora acho que entendi, está com o somatório do CR dos estudantes por curso
grf <- plot_bar(dados_course_cgpa, label_x = "Course", label_y = "CGPA", colors=colors[1]) + font
plot(grf)


# Calcular média do Depression_Score por Course, Sleep_Quality e Physical_Activity // Não consegui retirar muita informação dessa análise
depression_analysis <- dados %>%
  group_by(Course, Sleep_Quality, Physical_Activity) %>%
  summarise(Média_Depression_Score = mean(Depression_Score, na.rm = TRUE), .groups = 'drop')
print(depression_analysis)


#Filtrar colunas curso e media de depressao
dados_depr_corse <- depression_analysis %>%
  select(Course, Média_Depression_Score)

# Aqui não fez sentido esse gráfico. Está fazendo o somatório agrupado por curso
grf <- plot_bar(dados_depr_corse, label_x = "Course", label_y = "Média_Depression_Score", colors=colors[1]) + font
plot(grf)


#Achar correlacao entre essas variaveis numericas Stress_Level, Depression_Score, Anxiety_Score, Financial_Stress, Semester_Credit_Load
# Não entendi esse resultado
numeric_var_filtred <- numeric_vars %>%
  select(Stress_Level, Depression_Score, Anxiety_Score, Financial_Stress, Semester_Credit_Load)
print(numeric_var_filtred)


