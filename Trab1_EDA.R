library("daltoolbox")
library("ggplot2")
library("dplyr")
library("tidyr")
library("reshape")
library("RColorBrewer")
library("corrplot")
library("WVPlots")
library("GGally")
library("aplpack")
library("gridExtra")

# Define fonte e esquema de cores para os gráficos
colors <- brewer.pal(9, 'Set1')
font <- theme(text = element_text(size=16))

## Carregar o arquivo CSV
# Dados preenchidos com strings vazias são considerados NA
dados <- read.csv("students_mental_health_survey.csv", na.strings = "")

# Visualizar as primeiras linhas
head(dados)

# Visualizar a estrutura dos dados
str(dados)

# Visualizar dados faltantes
missing_data <- colSums(is.na(dados))
missing_data <- missing_data[missing_data > 0]
print(missing_data)

# Resumo das estatísticas descritivas para os dados numéricos
summary(select_if(dados, is.numeric))

# Histograma da variável target
grf <- plot_hist(dados %>% dplyr::select(Depression_Score), 
                 label_x = "Depression Score", color=colors[2]) + font
plot(grf)

# Quantidade de cada sexo
data = table(dados$Gender)
grf <- plot_bar(data, colors=colors[0: nrow(data)]) + font
plot(grf)

# Histogramas
grf1 <- plot_hist(dados %>% dplyr::select(CGPA), 
                  label_x = "CGPA", color=colors[1]) + font
grf2 <- plot_hist(dados %>% dplyr::select(Semester_Credit_Load), 
                  label_x = "Semester_Credit_Load", color=colors[2]) + font  
grf3 <- plot_hist(dados %>% dplyr::select(Financial_Stress), 
                  label_x = "Financial_Stress", color=colors[3]) + font 
grf4 <- plot_hist(dados %>% dplyr::select(Stress_Level), 
                  label_x = "Stress_Level", color=colors[4]) + font
grf5 <- plot_hist(dados %>% dplyr::select(Age), 
                  label_x = "Age", color=colors[5]) + font
grf6 <- plot_hist(dados %>% dplyr::select(Anxiety_Score), 
                  label_x = "Anxiety_Score", color=colors[6]) + font

# Monta uma figura com vários gráficos
options(repr.plot.width=16, repr.plot.height=4)
grid.arrange(grf1, grf2, grf3, grf4, grf5, grf6, ncol=3, nrow=2)
options(repr.plot.width=4, repr.plot.height=4)

#Gráficos de densidade
grfA <- plot_density_class(dados %>% dplyr::select(Depression_Score, CGPA), 
                           class_label="Depression_Score", label_x = "CGPA", color=colors[c(1:6)]) + font
grfB <- plot_density_class(dados %>% dplyr::select(Depression_Score, Semester_Credit_Load), 
                           class_label="Depression_Score", label_x = "Semester_Credit_Load", color=colors[c(1:6)]) + font
grfC <- plot_density_class(dados %>% dplyr::select(Depression_Score, Financial_Stress),
                           class_label="Depression_Score", label_x = "Financial_Stress", color=colors[c(1:6)]) + font

options(repr.plot.width=8, repr.plot.height=8)
grid.arrange(grfA, grfB, grfC, ncol=3)
options(repr.plot.width=4, repr.plot.height=4)


grfD <- plot_density_class(dados %>% dplyr::select(Depression_Score, Stress_Level), 
                           class_label="Depression_Score", label_x = "Stress_Level", color=colors[c(1:6)]) + font
grfE <- plot_density_class(dados %>% dplyr::select(Depression_Score, Age), 
                           class_label="Depression_Score", label_x = "Age", color=colors[c(1:6)]) + font
grfF <- plot_density_class(dados %>% dplyr::select(Depression_Score, Anxiety_Score), 
                           class_label="Depression_Score", label_x = "Anxiety_Score", color=colors[c(1:6)]) + font
options(repr.plot.width=8, repr.plot.height=8)
grid.arrange( grfD, grfE, grfF, ncol=3)
options(repr.plot.width=4, repr.plot.height=4)

# Nível de depressão X genero.
data <- dados |> group_by(Depression_Score, Gender ) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Gender, values_from = Count)
grf1 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Gender", colors=colors[1:2]
  ) + font

# Nível de depressão X curso
data <- dados |> group_by(Depression_Score, Course) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Course, values_from = Count)
grf2 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Course", colors=colors[1:6]
) + font

# Nível de depressão X Sleep Quality
data <- dados |> group_by(Depression_Score, Sleep_Quality) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Sleep_Quality, values_from = Count)
grf3 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Sleep Quality", colors=colors[1:6]
) + font

# Nível de depressão X Physical Activity
data <- dados |> group_by(Depression_Score, Physical_Activity) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Physical_Activity, values_from = Count)
grf4 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Physical Activity", colors=colors[1:6]
) + font

# Nível de depressão X Diet Quality
data <- dados |> group_by(Depression_Score, Diet_Quality) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Diet_Quality, values_from = Count)
grf5 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Diet Quality", colors=colors[1:6]
) + font

# Nível de depressão X Social Support
data <- dados |> group_by(Depression_Score, Social_Support) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Social_Support, values_from = Count)
grf6 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Social Support", colors=colors[1:6]
) + font

# Nível de depressão X Relationship Status
data <- dados |> group_by(Depression_Score, Relationship_Status) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Relationship_Status, values_from = Count)
grf7 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Relationship Status", colors=colors[1:6]
) + font

# Nível de depressão X Substance Use
data <- dados |> group_by(Depression_Score, Substance_Use) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Substance_Use, values_from = Count)
grf8 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Substance Use", colors=colors[1:6]
) + font

# Nível de depressão X Counseling Service Use
data <- dados |> group_by(Depression_Score, Counseling_Service_Use) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Counseling_Service_Use, values_from = Count)
grf9 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Counseling Service Use", colors=colors[1:6]
) + font

# Nível de depressão X Family History
data <- dados |> group_by(Depression_Score, Family_History) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Family_History, values_from = Count)
grf10 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Family History", colors=colors[1:6]
) + font

# Nível de depressão X Chronic Illness
data <- dados |> group_by(Depression_Score, Chronic_Illness) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Chronic_Illness, values_from = Count)
grf11 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Chronic Illness", colors=colors[1:6]
) + font

# Nível de depressão X Extracurricular Involvement
data <- dados |> group_by(Depression_Score, Extracurricular_Involvement) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Extracurricular_Involvement, values_from = Count)
grf12 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Extracurricular Involvement", colors=colors[1:6]
) + font

# Nível de depressão X Residence Type
data <- dados |> group_by(Depression_Score, Residence_Type) |> summarize(Count = n())
data_wide <- data |> pivot_wider(names_from = Residence_Type, values_from = Count)
grf13 <- plot_groupedbar(
  data_wide, label_x = "Depression Score", label_y = "Residence Type", colors=colors[1:6]
) + font

options(repr.plot.width=8, repr.plot.height=8)
grid.arrange( grf1, grf13, grf3, grf4, ncol=2, nrow=2)
options(repr.plot.width=4, repr.plot.height=4)

options(repr.plot.width=8, repr.plot.height=8)
grid.arrange( grf5, grf6, grf7, grf8, ncol=2, nrow=2)
options(repr.plot.width=4, repr.plot.height=4)

options(repr.plot.width=8, repr.plot.height=8)
grid.arrange( grf9, grf10, grf11, grf12, ncol=2, nrow=2)
options(repr.plot.width=4, repr.plot.height=4)

options(repr.plot.width=8, repr.plot.height=8)
grid.arrange(grf2, ncol=1)
options(repr.plot.width=4, repr.plot.height=4)

cor_matrix = cor(
  select_if(dados, is.numeric), use = "pairwise.complete.obs"
)
corrplot(
  cor_matrix, method = "number", col = "black", cl.pos = "n",  type = "upper"
)

# grf <- plot_correlation(select_if(dados, is.numeric))
# options(repr.plot.width=8, repr.plot.height=8)
# grf
# options(repr.plot.width=4, repr.plot.height=4)

# Filtrar as colunas numéricas
numeric_vars <- dados %>% 
  select(Age, CGPA, Semester_Credit_Load)

# Criar três boxplots separados
grf_cgpa <- plot_boxplot(numeric_vars %>% select(CGPA), colors=colors[1]) + font 
grf_credit_load <- plot_boxplot(numeric_vars %>% select(Semester_Credit_Load), colors=colors[2]) + font
grf_age <- plot_boxplot(numeric_vars %>% select(Age), colors=colors[5]) + font 

# Organizar os três gráficos lado a lado ou empilhados
grid.arrange(grf_cgpa, grf_credit_load, grf_age, nrow = 1)  # Lado a lado


# Boxplots agrupados por target
grfA <- plot_boxplot_class(dados %>% dplyr::select(Depression_Score, Age), 
                           class_label="Depression_Score", label_x = "Age", color=colors[c(1:6)]) + font
grfB <- plot_boxplot_class(dados %>% dplyr::select(Depression_Score, CGPA), 
                           class_label="Depression_Score", label_x = "CGPA", color=colors[c(1:6)]) + font
grfC <- plot_boxplot_class(dados %>% dplyr::select(Depression_Score, Stress_Level), 
                           class_label="Depression_Score", label_x = "Stress_Level", color=colors[c(1:6)]) + font
grfD <- plot_boxplot_class(dados %>% dplyr::select(Depression_Score, Anxiety_Score), 
                           class_label="Depression_Score", label_x = "Anxiety_Score", color=colors[c(1:6)]) + font

options(repr.plot.width=8, repr.plot.height=8)
grid.arrange(grfA, grfB, grfC, grfD, ncol=2, nrow=2)
options(repr.plot.width=5, repr.plot.height=4)


grfE <- plot_boxplot_class(dados %>% dplyr::select(Depression_Score, Financial_Stress), 
                           class_label="Depression_Score", label_x = "Financial Stress", color=colors[c(1:6)]) + font
grfF <- plot_boxplot_class(dados %>% dplyr::select(Depression_Score, Semester_Credit_Load), 
                           class_label="Depression_Score", label_x = "Semester_Credit_Load", color=colors[c(1:6)]) + font

options(repr.plot.width=8, repr.plot.height=8)
grid.arrange(grfE, grfF, ncol=1, nrow=2)
options(repr.plot.width=5, repr.plot.height=4)
