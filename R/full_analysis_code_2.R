########################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if (!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)

if (!require(jpeg)) install.packages("jpeg")
library(jpeg)

if (!require(ggimage)) install.packages("ggimage")
library(ggimage)

if (!require(magick)) install.packages("magick")
library(magick)

# load fonts
if (!require(showtext)) install.packages("showtext")
library(showtext)

# load fonts
if (!require(extrafont)) install.packages("extrafont")
library(extrafont)

# rounded barplots
if (!require(ggchicklet)) install.packages("ggchicklet", repos = "https://cinc.rud.is")
library(ggchicklet)

# levene test
if (!require(car)) install.packages("car")
library(car)

# density plot
if (!require(lattice)) install.packages("lattice")
library(lattice)

# multiVDA
if (!require(rcompanion)) install.packages("rcompanion")
library(rcompanion)

# dunnTest
if (!require(FSA))install.packages("FSA")
library(FSA)

# First Part - Dataset 2 - Data Loading and Processing
df_path <- "../csv/Professor_Specific_Data.csv"
all_data_II <- read.csv(df_path, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

unique(all_data_II$situação)
all_data_II <- all_data_II[(all_data_II$situação != "Trancamento parcial" & all_data_II$situação != "Dispensado sem  nota" & all_data_II$situação != "Disciplina Não Concluída"),]

unique(all_data_II$curso)
all_data_II <- all_data_II[(all_data_II$curso != "ESPPOS" & all_data_II$curso != "BAMCA" & all_data_II$curso != "ESPG"),]
all_data_II$curso[all_data_II$curso == "BAEE-2010"] <- "BAEE"
unique(all_data_II$componente.curricular)
all_data_II <- all_data_II[(all_data_II$componente.curricular != "BA000184-CONFIABILIDADE DE PROCESSOS E PRODUTOS" & all_data_II$componente.curricular != "BA000258-GEOPROCESSAMENTO E TOPOGRAFIA" & all_data_II$componente.curricular != "BA000188-MODELAGEM DA INFORMACAO" & all_data_II$componente.curricular != "BA000254-SENSORIAMENTO REMOTO APLICADO A ENGENHARIA"),]
unique(all_data_II$ano.periodo)
unique(all_data_II$turma)
all_data_II[all_data_II$nota<0 | all_data_II$nota>10,]

sex_data_path <- "../csv/sex_data.csv"
sex_data <- read.csv(sex_data_path, sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

all_data_II <- cbind(all_data_II, sex_data)
colnames(all_data_II)
all_data_II <- all_data_II[,-c(1,5,6,7,8,9,15)]

all_data_II$componente.curricular[all_data_II$componente.curricular == "010801-DESENHO TECNICO I"] <- "DESENHO TECNICO I"
all_data_II$componente.curricular[all_data_II$componente.curricular == "010803-DESENHO TECNICO II"] <- "DESENHO TECNICO II"
all_data_II$componente.curricular[all_data_II$componente.curricular == "BA010801-DESENHO TECNICO I"] <- "DESENHO TECNICO I"
all_data_II$componente.curricular[all_data_II$componente.curricular == "BA010803-DESENHO TECNICO II"] <- "DESENHO TECNICO II"

all_data_II$ano <- as.numeric(substr(all_data_II$ano.periodo, 1, 4))
all_data_II$periodo <- substr(all_data_II$ano.periodo, 8, 18)
all_data_II <- all_data_II[,-2]
colnames(all_data_II)[6] <- "MEDIA_FINAL"

all_data_II %>% group_by(curso) %>% tally()
all_data_II %>% group_by(curso, componente.curricular) %>% tally()

all_data_II <- all_data_II[!(all_data_II$curso == "BALM"),]
all_data_II <- all_data_II[!(all_data_II$curso == "BALP"),]
all_data_II <- all_data_II[!(all_data_II$componente.curricular == "DESENHO TECNICO II" & all_data_II$curso == "BALQ"),]

# Second Part - Dataset 2 - Descriptive Statistics

df2_by_course_drawing_I <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", curso)

df2_by_year_drawing_I <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano)

df2_by_year_course_drawing_I <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, curso)

df2_by_year_semester_course_drawing_I <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, curso, periodo)

df2_by_year_semester_class_drawing_I <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, periodo, turma)

df2_by_course_drawing_II <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", curso)

df2_by_year_drawing_II <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano)

df2_by_year_course_drawing_II <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, curso)

df2_by_year_semester_course_drawing_II <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, curso, periodo)

df2_by_year_semester_class_drawing_II <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, periodo, turma)

dfII_without_failed_by_attendance <- all_data_II[!(all_data_II$situação == "Reprovado por frequencia"),]

df2_by_course_drawing_I_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", curso)

df2_by_year_drawing_I_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano)

df2_by_year_course_drawing_I_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, curso)

df2_by_year_semester_course_drawing_I_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, curso, periodo)

df2_by_year_semester_class_drawing_I_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, periodo, turma)

df2_by_course_drawing_II_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", curso)

df2_by_year_drawing_II_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano)

df2_by_year_course_drawing_II_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, curso)

df2_by_year_semester_course_drawing_II_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, curso, periodo)

df2_by_year_semester_class_drawing_II_2 <- statistics(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, periodo, turma)

# Third Part - Dataset 2 - Data Visualization

ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], 
       aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 8, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("Curso") + ylab("Media Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("Ano") + ylab("Media Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(periodo))) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) + 
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("Ano") + ylab("Media Final") + labs(fill = "Periodo") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], 
       aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("Curso") + ylab("Media Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.05) + 
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("Ano") + ylab("Media Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(periodo))) +
  geom_boxplot(width = 0.4, position = position_dodge(width = 0.9)) +
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("Ano") + ylab("Media Final") + labs(fill = "Periodo") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], 
       aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1) +
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
  xlab("Curso") + ylab("Media Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("Ano") + ylab("Media Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(periodo))) +
  geom_violin(trim = TRUE, position = dodge) + 
  geom_boxplot(width = 0.1, position = dodge) +
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("Ano") + ylab("Media Final") + labs(fill = "Periodo") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], 
       aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("Curso") + ylab("Media Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("Ano") + ylab("Media Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(periodo))) +
  geom_violin(trim = TRUE) + 
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("Ano") + ylab("Media Final") + labs(fill = "Periodo") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

aux <- statistics(all_data_II, "MEDIA_FINAL", SEXO, componente.curricular)      

ggplot(aux, aes(componente.curricular, round(mean,2), group = SEXO, fill = as.factor(SEXO))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_text(aes(label = round(mean, 2)), vjust = 2, color = "black", size = 5, position = position_dodge(0.9)) +
  scale_fill_manual("Sexo", values = c("F" = "deeppink", "M" = "deepskyblue")) +
  scale_y_continuous(breaks = seq(0, 7, by = 1)) +
  xlab("Disciplina") + ylab("Media da Nota Final") + ggtitle("Análise de Medias por Disciplina e Sexo") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 12))

aux <- statistics(all_data_II, "MEDIA_FINAL", ano, periodo, componente.curricular)

a <- ggplot(aux, aes(as.factor(ano), mean, group = periodo, fill = as.factor(periodo))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
  geom_text(aes(label = round(mean, 1)), vjust = 2, color = "black", size = 2.8, position = position_dodge(0.9)) +
  scale_fill_manual("Periodo", values = c("1. Semestre" = "cadetblue2", "2. Semestre" = "darkolivegreen2")) +
  xlab("Ano") + ylab("Media da Nota Final") + labs(fill = "Periodo") + ggtitle("Medias finais por Disciplina, Ano e Semestre") +
  theme(axis.text.x = element_text(face = "bold", size = 10), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~componente.curricular) 

b <- ggplot(aux, aes(as.factor(ano), n, group = periodo, fill = as.factor(periodo))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
  geom_text(aes(label = n), vjust = 2, color = "black", size = 2.8, position = position_dodge(0.9)) +
  scale_fill_manual("Periodo", values = c("1. Semestre" = "cadetblue2", "2. Semestre" = "darkolivegreen2")) +
  scale_y_continuous(breaks = round(seq(0, 60, by = 5), 1)) +
  xlab("Ano") + ylab("Número de Alunos") + labs(fill = "Periodo") + ggtitle("Número de Alunos por Disciplina, Periodo e Ano") +
  theme(axis.text.x = element_text(face = "bold", size = 10), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~componente.curricular)

ggarrange(a, b, nrow = 2)

aux <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, periodo, situação)

aux <- as.data.frame(aux %>% 
                       group_by(ano, periodo) %>% 
                       mutate(percent = 100*round(n/sum(n),8),
                              pos = 3.5 + (round(cumsum(percent) - (0.5 * percent), 8))
                       )
)

aux$periodo[aux$periodo == "1. Semestre"] <- "1 -"
aux$periodo[aux$periodo == "2. Semestre"] <- "2 -"
aux$situação <- factor(aux$situação, levels = rev(levels(as.factor(aux$situação))))

a <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(periodo), as.factor(ano)), y = percent, fill = as.factor(situação)), stat = "identity", color = "black") +
  scale_fill_manual(values = c("orange", "red", "green")) +
  geom_text(data = aux, aes(x = interaction(as.factor(periodo), as.factor(ano)), y = pos, label = paste0(round(percent, 1),"%(n=",n,")")), vjust = 2, color = "black", size = 3) +
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  xlab("Semestre - Ano") + ylab("Percentual de Alunos") + labs(fill = "Situação") +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title=element_text(hjust = 0.5),
        axis.text.x=element_text(colour = "black", angle = 45, size = 9, hjust = 1),
        axis.text.y=element_text(colour = "black", size = 10)) 

aux <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, periodo)

b <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(periodo), as.factor(ano)), y = n), stat = "identity", color = "black") +
  geom_text(data = aux, aes(x = interaction(as.factor(periodo), as.factor(ano)), y = n, label = n), vjust = 2, color = "black", size = 4) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  ylab("Número de Alunos") +
  ggtitle("Situação de Alunos por Periodo e Ano em Desenho Tecnico I") +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  aes(fill = I("darkseagreen2"))

ggarrange(b, a, nrow = 2, heights=c(1, 4))

aux <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, periodo, situação)

aux <- as.data.frame(aux %>% 
                       group_by(ano, periodo) %>% 
                       mutate(percent = 100*round(n/sum(n),8),
                              pos = 4.4 + (round(cumsum(percent) - (0.5 * percent), 8))
                       )
)

aux$periodo[aux$periodo == "1. Semestre"] <- "1 -"
aux$periodo[aux$periodo == "2. Semestre"] <- "2 -"
aux$situação <- factor(aux$situação, levels = rev(levels(as.factor(aux$situação))))

a <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(periodo), as.factor(ano)), y = percent, fill = as.factor(situação)), stat = "identity", color = "black") +
  scale_fill_manual(values = c("orange", "red", "green")) +
  geom_text(data = aux, aes(x = interaction(as.factor(periodo), as.factor(ano)), y = pos, label = paste0(round(percent, 1),"%")), vjust = 2, color = "black", size = 3) +
  geom_text(data = aux, aes(x = interaction(as.factor(periodo), as.factor(ano)), y = pos-2.5, label = paste0("n = ",n)), vjust = 2, color = "black", size = 2.5) +        
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  xlab("Semestre - Ano") + ylab("Percentual de Alunos") + labs(fill = "Situação") +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title=element_text(hjust = 0.5),
        axis.text.x=element_text(colour = "black", angle = 45, size = 9, hjust = 1),
        axis.text.y=element_text(colour = "black", size = 10)) 

aux <- statistics(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, periodo)

b <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(periodo), as.factor(ano)), y = n), stat = "identity", color = "black") +
  geom_text(data = aux, aes(x = interaction(as.factor(periodo), as.factor(ano)), y = n, label = n), vjust = 2, color = "black", size = 4) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  ylab("Número de Alunos") +
  ggtitle("Situação de Alunos por Periodo e Ano em Desenho Tecnico II") +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  aes(fill = I("darkseagreen2"))

ggarrange(b, a, nrow = 2, heights=c(1, 4))

ggplot(all_data_II[all_data_II$situação!="Reprovado por frequencia" & all_data_II$componente.curricular == "DESENHO TECNICO I",], aes(x = faltas, y = MEDIA_FINAL)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  scale_x_continuous(breaks = seq(0, 16, by = 4)) +
  ylab("Media Final") +
  ggtitle("Desenho Tecnico I") +
  geom_smooth(method = lm) +
  geom_jitter(width = 0.25) +
  theme(plot.title=element_text(hjust = 0.5))

ggplot(all_data_II[all_data_II$situação!="Reprovado por frequencia" & all_data_II$componente.curricular == "DESENHO TECNICO II",], aes(x = faltas, y = MEDIA_FINAL)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  scale_x_continuous(breaks = seq(0, 16, by = 2)) +
  ylab("Media Final") +
  ggtitle("Desenho Tecnico II") +
  geom_smooth(method = lm) +
  geom_jitter(width = 0.25) +
  theme(plot.title=element_text(hjust = 0.5))

# Fourth Part - Dataset 2 - Advanced Statistics

dfII_without_failed_by_attendance$periodo[dfII_without_failed_by_attendance$periodo == "1. Semestre"] <- 1
dfII_without_failed_by_attendance$periodo[dfII_without_failed_by_attendance$periodo == "2. Semestre"] <- 2
dfII_without_failed_by_attendance$periodo <- as.factor(dfII_without_failed_by_attendance$periodo)
dfII_without_failed_by_attendance$ano <- as.factor(dfII_without_failed_by_attendance$ano)

res = residuals(lm(MEDIA_FINAL ~ ano * periodo, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",]))
shapiro.test(res)
hist(res)

max(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ano, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])
leveneTest(MEDIA_FINAL ~ periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])
leveneTest(MEDIA_FINAL ~ ano * periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])

kruskal.test(MEDIA_FINAL ~ ano, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ interaction(ano, periodo), dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])

densityplot(~ MEDIA_FINAL | ano, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | periodo, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | ano*periodo, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",], xlab = "Media Final", ylab = "Densidade de Medias")


#multiVDA(MEDIA_FINAL ~ ano, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ periodo, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ SEXO, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ curso, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ano,
              data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",],
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
             data = PT$res,
             threshold = 0.05,
             remove.zero = FALSE)

ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 


res = residuals(lm(MEDIA_FINAL ~ ano * periodo, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",]))
shapiro.test(res)
hist(res)

max(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ano, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",])
leveneTest(MEDIA_FINAL ~ periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",])
leveneTest(MEDIA_FINAL ~ ano * periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",])

kruskal.test(MEDIA_FINAL ~ ano, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",])
kruskal.test(MEDIA_FINAL ~ periodo, dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",])
kruskal.test(MEDIA_FINAL ~ interaction(ano, periodo), dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",])

densityplot(~ MEDIA_FINAL | ano, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | periodo, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | ano*periodo, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",], xlab = "Media Final", ylab = "Densidade de Medias")

#multiVDA(MEDIA_FINAL ~ ano, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ periodo, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",])
multiVDA(MEDIA_FINAL ~ SEXO, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",])
multiVDA(MEDIA_FINAL ~ curso, data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",])

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ano,
              data = dfII_without_failed_by_attendance[dfII_without_failed_by_attendance$componente.curricular == "DESENHO TECNICO II",],
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
             data = PT$res,
             threshold = 0.05,
             remove.zero = FALSE)

ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 