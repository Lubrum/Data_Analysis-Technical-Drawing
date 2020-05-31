########################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if (!require(EnvStats)) install.packages("EnvStats")
library(EnvStats)

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

##############################################################################################################
##############################################################################################################
##############################################################################################################

# First Part - Dataset 2 - Data Loading and Processing
df_path <- "../csv/Professor_Specific_Data.csv"
all_data_II <- read.csv(df_path, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

colnames(all_data_II)

colnames(all_data_II)[2] <- "DISCIPLINA"
colnames(all_data_II)[3] <- "ANO.PERIODO"
colnames(all_data_II)[4] <- "TURMA"
colnames(all_data_II)[13] <- "MEDIA_FINAL"
colnames(all_data_II)[11] <- "CURSO"
colnames(all_data_II)[14] <- "SITUACAO"

unique(all_data_II$SITUACAO)
all_data_II <- all_data_II[(all_data_II$SITUACAO != "Trancamento parcial" & all_data_II$SITUACAO != "Dispensado sem  nota" & all_data_II$SITUACAO != "Disciplina Não Concluída"),]

unique(all_data_II$CURSO)
all_data_II <- all_data_II[(all_data_II$CURSO != "ESPPOS" & all_data_II$CURSO != "BAMCA" & all_data_II$CURSO != "ESPG"),]
all_data_II$CURSO[all_data_II$CURSO == "BAEE-2010"] <- "BAEE"

unique(all_data_II$DISCIPLINA)
all_data_II <- all_data_II[(all_data_II$DISCIPLINA != "BA000184-CONFIABILIDADE DE PROCESSOS E PRODUTOS" & all_data_II$DISCIPLINA != "BA000258-GEOPROCESSAMENTO E TOPOGRAFIA" & all_data_II$DISCIPLINA != "BA000188-MODELAGEM DA INFORMACAO" & all_data_II$DISCIPLINA != "BA000254-SENSORIAMENTO REMOTO APLICADO A ENGENHARIA"),]

unique(all_data_II$ANO.PERIODO)

unique(all_data_II$TURMA)

all_data_II[all_data_II$nota<0 | all_data_II$nota>10,]

sex_data_path <- "../csv/sex_data.csv"
sex_data <- read.csv(sex_data_path, sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

all_data_II <- cbind(all_data_II, sex_data)
all_data_II <- all_data_II[,-c(1,5,6,7,8,9,15)]

all_data_II$DISCIPLINA[all_data_II$DISCIPLINA == "010801-DESENHO TECNICO I"] <- "DESENHO TECNICO I"
all_data_II$DISCIPLINA[all_data_II$DISCIPLINA == "010803-DESENHO TECNICO II"] <- "DESENHO TECNICO II"
all_data_II$DISCIPLINA[all_data_II$DISCIPLINA == "BA010801-DESENHO TECNICO I"] <- "DESENHO TECNICO I"
all_data_II$DISCIPLINA[all_data_II$DISCIPLINA == "BA010803-DESENHO TECNICO II"] <- "DESENHO TECNICO II"

all_data_II$ANO <- as.numeric(substr(all_data_II$ANO.PERIODO, 1, 4))
all_data_II$PERIODO <- substr(all_data_II$ANO.PERIODO, 8, 18)
all_data_II <- all_data_II[,-2]

all_data_II %>% group_by(CURSO) %>% tally()
all_data_II %>% group_by(CURSO, DISCIPLINA) %>% tally()

all_data_II <- all_data_II[!(all_data_II$CURSO == "BALM"),]
all_data_II <- all_data_II[!(all_data_II$CURSO == "BALP"),]
all_data_II <- all_data_II[!(all_data_II$CURSO == "BALQ"),]

##############################################################################################################
##############################################################################################################
##############################################################################################################

# Second Part - Dataset 2 - Descriptive Statistics

statistics <- function(dataframe, response, ...) {
  group_var <- enquos(...)
  result <- as.data.frame(
    dataframe %>% 
      group_by(!!!group_var) %>% 
      summarise(mean = mean(!!sym(response)), 
                median = median(!!sym(response)), 
                sd = sd(!!sym(response)), 
                min = min(!!sym(response)), 
                max = max(!!sym(response)),
                n = NROW(!!sym(response)),
                quartil_1st = quantile(!!sym(response), 0.25),
                quartil_3rd = quantile(!!sym(response), 0.75),
                IQR = quantile(!!sym(response), 0.75) - quantile(!!sym(response), 0.25),
                var_coef = 100*(sd(!!sym(response))/mean(!!sym(response))),
                kurtosis = kurtosis(!!sym(response)),
                skewness = skewness(!!sym(response))
      )
  )
  return(result)
}

DT1 <- all_data_II[all_data_II$DISCIPLINA == "DESENHO TECNICO I",]
DT2 <- all_data_II[all_data_II$DISCIPLINA == "DESENHO TECNICO II",]

df2_by_course_drawing_I <- statistics(DT1, "MEDIA_FINAL", CURSO)

df2_by_year_drawing_I <- statistics(DT1, "MEDIA_FINAL", ANO)

df2_by_year_course_drawing_I <- statistics(DT1, "MEDIA_FINAL", ANO, CURSO)

df2_by_year_semester_course_drawing_I <- statistics(DT1, "MEDIA_FINAL", ANO, CURSO, PERIODO)

df2_by_year_semester_class_drawing_I <- statistics(DT1, "MEDIA_FINAL", ANO, PERIODO, TURMA)

df2_by_course_drawing_II <- statistics(DT2, "MEDIA_FINAL", CURSO)

df2_by_year_drawing_II <- statistics(DT2, "MEDIA_FINAL", ANO)

df2_by_year_course_drawing_II <- statistics(DT2, "MEDIA_FINAL", ANO, CURSO)

df2_by_year_semester_course_drawing_II <- statistics(DT2, "MEDIA_FINAL", ANO, CURSO, PERIODO)

df2_by_year_semester_class_drawing_II <- statistics(DT2, "MEDIA_FINAL", ANO, PERIODO, TURMA)

partial_data <- all_data_II[!(all_data_II$SITUACAO == "Reprovado por Frequência"),]
DT1_partial <- partial_data[partial_data$DISCIPLINA == "DESENHO TECNICO I",]
DT2_partial <- partial_data[partial_data$DISCIPLINA == "DESENHO TECNICO II",]

df2_by_course_drawing_I_2 <- statistics(DT1_partial, "MEDIA_FINAL", CURSO)

df2_by_year_drawing_I_2 <- statistics(DT1_partial, "MEDIA_FINAL", ANO)

df2_by_year_course_drawing_I_2 <- statistics(DT1_partial, "MEDIA_FINAL", ANO, CURSO)

df2_by_year_semester_course_drawing_I_2 <- statistics(DT1_partial, "MEDIA_FINAL", ANO, CURSO, PERIODO)

df2_by_year_semester_class_drawing_I_2 <- statistics(DT1_partial, "MEDIA_FINAL", ANO, PERIODO, TURMA)

df2_by_course_drawing_II_2 <- statistics(DT2_partial, "MEDIA_FINAL", CURSO)

df2_by_year_drawing_II_2 <- statistics(DT2_partial, "MEDIA_FINAL", ANO)

df2_by_year_course_drawing_II_2 <- statistics(DT2_partial, "MEDIA_FINAL", ANO, CURSO)

df2_by_year_semester_course_drawing_II_2 <- statistics(DT2_partial, "MEDIA_FINAL", ANO, CURSO, PERIODO)

df2_by_year_semester_class_drawing_II_2 <- statistics(DT2_partial, "MEDIA_FINAL", ANO, PERIODO, TURMA)

##############################################################################################################
##############################################################################################################
##############################################################################################################

# Third Part - Dataset 2 - Data Visualization

img = "../images/dark-background-1.jpg"

font_family <- "AlloyInk"
font_add("Bold Hollow", "../fonts/Raindrops BOLD.ttf")
font_add("AlloyInk", "../fonts/AlloyInk-nRLyO.ttf")
font_add("Ogowey", "../fonts/OgoweyDemo-owxDV.ttf")
showtext_auto()

# normal theme
#normal_theme <- theme(plot.title = element_text(hjust = 0.5))

#dark theme
normal_theme <- theme(
  axis.text = element_text(family = font_family, size = 22, color = "#cccccc"),
  axis.title = element_text(family = font_family, size = 30, color = "#cccccc"),
  axis.ticks = element_line(colour = "#cccccc"),
  axis.ticks.length = unit(0.5, "cm"),
  plot.caption = element_text(family = font_family, size = 16, color = "#cccccc"),
  plot.title = element_text(family = font_family, size = 30, hjust = 0.5, color = "#ffffff"),
  plot.subtitle = element_text(hjust = 0.5),
  plot.background = element_rect(fill = "black"),
  panel.grid.minor.y = element_line(size =.1, color = "grey"),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(size =.1,  color = "grey"),
  panel.grid.major.x = element_blank(),
  panel.background = element_rect(fill = 'black'),
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect(fill = "black"),
  legend.text = element_text(family = font_family, size = 20, color = "#cccccc"),
  text = element_text(family = font_family, color = "#cccccc", size = 22)
)

# Manually open a graphics device if you run this code in RStudio
x11(width = 11, height = 11)

fig <- ggplot(DT1, aes(x = CURSO, y = MEDIA_FINAL, fill = CURSO)) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(color = "black",  width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
  stat_n_text(color = "white", size = 8) + 
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("\nCurso\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "Curso") +
  ggtitle("\nMedias finais em Desenho Tecnico I\n") + 
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_1.png", type = "png", device = dev.cur())

fig <- ggplot(DT1, aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(ANO))) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("\nAno\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "Ano") + 
  ggtitle("\nMedias finais em Desenho Tecnico I\n") +
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_2.png", type = "png", device = dev.cur())

fig <- ggplot(DT1, aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("\nAno\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "PERIODO") + 
  ggtitle("\nMedias finais em Desenho Tecnico I\n") +
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_3.png", type = "png", device = dev.cur())

fig <- ggplot(DT2, 
  aes(x = CURSO, y = MEDIA_FINAL, fill = CURSO)) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("\nCurso\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "CURSO") + 
  ggtitle("\nMedias finais em Desenho Tecnico II\n") +
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_4.png", type = "png", device = dev.cur())

fig <- ggplot(DT2, 
  aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(ANO))) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.05, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("\nAno\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "ANO") + 
  ggtitle("\nMedias finais em Desenho Tecnico II\n") +
  theme(axis.text.x = element_text(size = 15)) + 
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_5.png", type = "png", device = dev.cur())

fig <- ggplot(DT2, 
  aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
  geom_boxplot(width = 0.4, position = position_dodge(width = 0.9), outlier.shape = 21, outlier.size = 2, outlier.fill = "white") +
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("\nAno\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "PERIODO") + 
  ggtitle("\nMedias finais em Desenho Tecnico II\n") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + 
  normal_theme 

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_6.png", type = "png", device = dev.cur())

fig <- ggplot(DT1_partial, 
  aes(x = CURSO, y = MEDIA_FINAL, fill = CURSO)) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) +
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
  xlab("\nCurso\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "CURSO") + 
  ggtitle("\nMedias finais em Desenho Tecnico I", subtitle = "Sem reprovados por frequencia\n") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_7.png", type = "png", device = dev.cur())

fig <- ggplot(DT1_partial, 
  aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(ANO))) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("\nAno\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "Ano") + 
  ggtitle("\nMedias finais em Desenho Tecnico I", subtitle = "Sem reprovados por frequencia\n") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_8.png", type = "png", device = dev.cur())

fig <- ggplot(DT1_partial, 
  aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.8), outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) +
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("\nAno\n") +
  ylab("\nMedia Final\n") + 
  labs(fill = "Periodo") + 
  ggtitle("\nMedias finais em Desenho Tecnico I", subtitle = "Sem reprovados por frequencia\n") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_9.png", type = "png", device = dev.cur())

fig <- ggplot(DT2_partial, 
  aes(x = CURSO, y = MEDIA_FINAL, fill = CURSO)) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("\nCurso\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "CURSO") + 
  ggtitle("\nMedias finais em Desenho Tecnico II", subtitle = "Sem reprovados por frequencia\n") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_10.png", type = "png", device = dev.cur())

fig <- ggplot(DT2_partial, 
  aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(ANO))) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("\nAno\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "Ano") + 
  ggtitle("\nMedias finais em Desenho Tecnico II", subtitle = "Sem reprovados por frequencia\n") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_11.png", type = "png", device = dev.cur())

fig <- ggplot(DT2_partial, 
  aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.8), outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) +
  stat_n_text(color = "white", size = 6) +
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red", size = 0.25) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("\nAno\n") + 
  ylab("\nMedia Final\n") + 
  labs(fill = "Periodo") + 
  ggtitle("\nMedias finais em Desenho Tecnico II", subtitle = "Sem reprovados por frequencia\n") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_12.png", type = "png", device = dev.cur())

aux <- statistics(all_data_II, "MEDIA_FINAL", SEXO, DISCIPLINA)      

fig <- ggplot(aux, aes(DISCIPLINA, round(mean,2), group = SEXO, fill = as.factor(SEXO))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge(), alpha = 0.5) +
  geom_text(aes(label = round(mean, 2)), vjust = 2, color = "white", size = 5, position = position_dodge(0.9)) +
  scale_fill_manual("Sexo", values = c("F" = "deeppink", "M" = "deepskyblue")) +
  scale_y_continuous(breaks = seq(0, 7, by = 1)) +
  xlab("\nDisciplina\n") + 
  ylab("\nMedia da Nota Final\n") + 
  ggtitle("\nAnalise de Medias por Disciplina e Sexo\n") +
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_13.png", type = "png", device = dev.cur())

aux <- statistics(all_data_II, "MEDIA_FINAL", ANO, PERIODO, DISCIPLINA)

aux$PERIODO[aux$PERIODO == "1. Semestre"] <- "1"
aux$PERIODO[aux$PERIODO == "2. Semestre"] <- "2"

a <- ggplot(aux, aes(as.factor(ANO), mean, group = PERIODO, color = PERIODO)) + 
  geom_bar(stat = "identity", fill = "black", position = position_dodge2(preserve = "single")) +
  scale_color_manual("PERIODO", values = c("1" = "darkolivegreen2", "2" = "mediumorchid1")) +
  xlab("") + 
  ylab("\nMedia da Nota Final\n") + 
  guides(color = FALSE) + 
  ggtitle("\nMedias finais por Disciplina, Ano e Semestre\n") +
  normal_theme +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 18), axis.text = element_text(size = 11),
        legend.title =  element_text(size = 12), legend.text = element_text(size = 12)) +
  facet_wrap(~DISCIPLINA) 

b <- ggplot(aux, aes(as.factor(ANO), n, group = PERIODO, color = PERIODO)) + 
  geom_bar(stat = "identity", fill = "black", position = position_dodge2(preserve = "single")) +
  scale_color_manual("Semestre - ", values = c("1" = "darkolivegreen2", "2" = "mediumorchid1")) +
  scale_y_continuous(breaks = round(seq(0, 60, by = 5), 1)) +
  xlab("\nAno") + 
  ylab("\nNumero de Alunos\n") + 
  ggtitle("\nNumero de Alunos por Disciplina, Periodo e Ano\n") +
  normal_theme +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 18), axis.text = element_text(size = 11), 
        legend.title =  element_text(size = 12), legend.text = element_text(size = 12), legend.position = "bottom") +
  facet_wrap(~DISCIPLINA)

ggbackground(ggarrange(a, b, nrow = 2), img)
savePlot(filename = "../images/figure_specific_14.png", type = "png", device = dev.cur())


aux <- statistics(DT1, "MEDIA_FINAL", ANO, PERIODO, SITUACAO)
aux <- as.data.frame(aux %>% 
                       group_by(ANO, PERIODO) %>% 
                       mutate(percent = 100*round(n/sum(n),8),
                              pos = 3.5 + (round(cumsum(percent) - (0.5 * percent), 8))
                       )
)
aux$PERIODO[aux$PERIODO == "1. Semestre"] <- "1 -"
aux$PERIODO[aux$PERIODO == "2. Semestre"] <- "2 -"
aux$SITUACAO[aux$SITUACAO == "Aprovado com nota"] <- "Aprovado"
aux$SITUACAO[aux$SITUACAO == "Reprovado com nota"] <- "Reprovado"
aux$SITUACAO[aux$SITUACAO == "Reprovado por Frequência"] <- "Reprovado por Freq."
aux$SITUACAO <- factor(aux$SITUACAO, levels = rev(levels(as.factor(aux$SITUACAO))))

a <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = percent, fill = as.factor(SITUACAO)), stat = "identity", color = "black") +
  scale_fill_manual(values = c("orange4", "red4", "seagreen"), labels = c("Reprovado por Freq.     ", "Reprovado  ", "Aprovado")) +
  geom_text(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = pos, label = paste0(round(percent, 1),"%")), vjust = 2, color = "white", size = 4) +
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  xlab("\nSemestre - Ano\n") + 
  ylab("\nPercentual de Alunos\n") + 
  labs(fill = FALSE) +
  normal_theme +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text.x = element_text(colour = "white", angle = 45, size = 18, hjust = 1),
        axis.text.y = element_text(colour = "white", size = 18),
        plot.title = element_text(size = 15), axis.title = element_text(size = 18), axis.text = element_text(size = 11), 
        legend.title = element_blank(), legend.text = element_text(size = 12)) 

aux <- statistics(DT1, "MEDIA_FINAL", ANO, PERIODO)

b <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = n), stat = "identity", color = "black") +
  geom_text(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = n, label = n), vjust = 2, color = "black", size = 5) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  ylab("\nAlunos\n") +
  ggtitle("\nSituacao de Alunos por Periodo e Ano em Desenho I\n") +
  normal_theme +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_text(size = 18), axis.title = element_text(size = 18), axis.text = element_text(size = 13), 
        legend.title =  element_text(size = 12), legend.text = element_text(size = 12)) +
  aes(fill = I("darkseagreen2"))

ggbackground(ggarrange(b, a, nrow = 2, heights = c(1, 4)), img)
savePlot(filename = "../images/figure_specific_15.png", type = "png", device = dev.cur())


aux <- statistics(DT2, "MEDIA_FINAL", ANO, PERIODO, SITUACAO)
aux <- as.data.frame(aux %>% 
                       group_by(ANO, PERIODO) %>% 
                       mutate(percent = 100*round(n/sum(n),8),
                              pos = 3 + (round(cumsum(percent) - (0.5 * percent), 8))
                       )
)
aux$PERIODO[aux$PERIODO == "1. Semestre"] <- "1 -"
aux$PERIODO[aux$PERIODO == "2. Semestre"] <- "2 -"
aux$SITUACAO[aux$SITUACAO == "Aprovado com nota"] <- "Aprovado"
aux$SITUACAO[aux$SITUACAO == "Reprovado com nota"] <- "Reprovado"
aux$SITUACAO[aux$SITUACAO == "Reprovado por Frequência"] <- "Reprovado por Freq."
aux$SITUACAO <- factor(aux$SITUACAO, levels = rev(levels(as.factor(aux$SITUACAO))))

a <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = percent, fill = as.factor(SITUACAO)), stat = "identity", color = "black") +
  scale_fill_manual(values = c("orange4", "red4", "seagreen"), labels = c("Reprovado por Freq.     ", "Reprovado  ", "Aprovado")) +
  geom_text(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = pos, label = paste0(round(percent, 1),"%")), vjust = 2, color = "white", size = 3) +
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  xlab("\nSemestre - Ano") + 
  ylab("\nPercentual de Alunos\n") + 
  labs(fill = FALSE) +
  normal_theme +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text.x = element_text(colour = "white", angle = 45, size = 15, hjust = 1),
        axis.text.y = element_text(colour = "white", size = 15),
        plot.title = element_text(size = 15), axis.title = element_text(size = 18), axis.text = element_text(size = 11), 
        legend.title = element_blank(), legend.text = element_text(size = 12)) 

aux <- statistics(DT2, "MEDIA_FINAL", ANO, PERIODO)

b <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = n), stat = "identity", color = "black") +
  geom_text(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = n, label = n), vjust = 2, color = "black", size = 5) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  ylab("\nAlunos\n") +
  ggtitle("\nSituacao de Alunos por Periodo e Ano em Desenho II") +
  normal_theme +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_text(size = 18), axis.title = element_text(size = 18), axis.text = element_text(size = 13), 
        legend.title =  element_text(size = 12), legend.text = element_text(size = 12)) +
  aes(fill = I("darkseagreen2"))

ggbackground(ggarrange(b, a, nrow = 2, heights = c(1, 4)), img)
savePlot(filename = "../images/figure_specific_16.png", type = "png", device = dev.cur())


a <- ggplot(all_data_II[all_data_II$SITUACAO!="Reprovado por Frequência" & all_data_II$DISCIPLINA == "DESENHO TECNICO I",], aes(x = faltas, y = MEDIA_FINAL)) + 
  geom_point(color = "white") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  scale_x_continuous(breaks = seq(0, 16, by = 4)) +
  ylab("\nMedia Final\n") +
  ggtitle("\nDesenho Tecnico I\n") +
  geom_smooth(method = lm) +
  geom_jitter(width = 0.25, color = "white") +
  normal_theme 

ggbackground(a, img)
savePlot(filename = "../images/figure_specific_17.png", type = "png", device = dev.cur())

a <- ggplot(all_data_II[all_data_II$SITUACAO!="Reprovado por Frequência" & all_data_II$DISCIPLINA == "DESENHO TECNICO II",], aes(x = faltas, y = MEDIA_FINAL)) + 
  geom_point(color = "white") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  scale_x_continuous(breaks = seq(0, 16, by = 4)) +
  ylab("\nMedia Final\n") +
  ggtitle("\nDesenho Tecnico II\n") +
  geom_smooth(method = lm) +
  geom_jitter(width = 0.25, color = "white") +
  normal_theme 

ggbackground(a, img)
savePlot(filename = "../images/figure_specific_18.png", type = "png", device = dev.cur())

##############################################################################################################
##############################################################################################################
##############################################################################################################

# Fourth Part - Dataset 2 - Advanced Statistics

# Technical Drawing I

all_data_II$PERIODO[all_data_II$PERIODO == "1. Semestre"] <- 1
all_data_II$PERIODO[all_data_II$PERIODO == "2. Semestre"] <- 2
all_data_II$PERIODO <- as.factor(all_data_II$PERIODO)
all_data_II$ANO <- as.factor(all_data_II$ANO)
DT1 <- all_data_II[all_data_II$DISCIPLINA == "DESENHO TECNICO I",]

# Variance analysis
aov_result <- aov(MEDIA_FINAL ~ ANO * PERIODO, data = DT1)

# Normality of errors test
shapiro.test(residuals(aov_result))

res <- as.data.frame(residuals(aov_result))
colnames(res)[1] <- "value"

fig1 <- ggplot(data = res, aes(value)) + 
  geom_histogram(color = "yellow") +
  ggtitle("\nHistograma de Residuos\n") +
  ylab("\nFrequencia\n") + 
  xlab("\nResiduo\n") + 
  labs(fill = "Densidade") +   
  normal_theme      

ggbackground(fig1, img)
savePlot(filename = "../images/figure_specific_19.png", type = "png", device = dev.cur())

# Homogeneity of Variance between the groups
max(aggregate(MEDIA_FINAL ~ ANO + PERIODO, DT1, var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ANO + PERIODO, DT1, var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ANO, DT1, center = median)
leveneTest(MEDIA_FINAL ~ PERIODO, DT1, center = median)
leveneTest(MEDIA_FINAL ~ ANO * PERIODO, DT1, center = median)

# Distributions between the groups of years and semesters

kruskal.test(MEDIA_FINAL ~ ANO, DT1)
kruskal.test(MEDIA_FINAL ~ PERIODO, DT1)
kruskal.test(MEDIA_FINAL ~ interaction(ANO, PERIODO), DT1)


fig1 <- ggplot(DT1, aes(x = MEDIA_FINAL , fill = PERIODO, color = PERIODO)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT1$MEDIA_FINAL),  max(DT1$MEDIA_FINAL), by = 5), 10)) +
  xlab("\nDistribuicao de Medias") + 
  ylab("\nDensidade de Medias\n") + 
  labs(fill = "Semestre", color = "Semestre") +
  ggtitle("\nDistribuicao das Medias em Desenho I\n") +
  normal_theme +       
  theme(axis.title = element_text(size = 25), 
        plot.title = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        legend.position = "bottom", 
        legend.text = element_text(size = 18)) 

ggbackground(fig1, img)
savePlot(filename = "../images/figure_specific_20.png", type = "png", device = dev.cur())


fig1 <- ggplot(DT1, aes(x = MEDIA_FINAL , fill = ANO, color = ANO)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT1$MEDIA_FINAL), max(DT1$MEDIA_FINAL), by = 5), 10)) +
  xlab("\nDistribuicao de Medias") + 
  ylab("\nDensidade de Medias\n") + 
  labs(fill = "Ano", color = "Ano") +
  ggtitle("\nDistribuicao das Medias em Desenho I\n") +
  normal_theme +       
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.title = element_text(size = 25), 
        plot.title = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        legend.position = "bottom", 
        legend.text = element_text(size = 18)) +
  facet_wrap(. ~ ANO, nrow = 4)

ggbackground(fig1, img)
savePlot(filename = "../images/figure_specific_21.png", type = "png", device = dev.cur())


fig1 <- ggplot(DT1, aes(x = MEDIA_FINAL , fill = ANO, color = PERIODO)) +
  geom_density(alpha = 0.7, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT1$MEDIA_FINAL), max(DT1$MEDIA_FINAL), by = 5), 10)) +
  xlab("\nDistribuicao de Medias") + 
  ylab("\nDensidade de Medias\n") + 
  labs(fill = "Ano", color = "Semestre") +
  ggtitle("\nDistribuicao das Medias em Desenho I\n") +
  normal_theme +       
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        legend.position = "bottom", 
        legend.text = element_text(size = 15)) +
  facet_wrap(ANO ~ PERIODO, nrow = 5)

ggbackground(fig1, img)
savePlot(filename = "../images/figure_specific_22.png", type = "png", device = dev.cur())


multiVDA(MEDIA_FINAL ~ ANO, data = DT1)
multiVDA(MEDIA_FINAL ~ PERIODO, data = DT1)
multiVDA(MEDIA_FINAL ~ SEX, data = DT1)
multiVDA(MEDIA_FINAL ~ CURSO, data = DT1)

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ANO, data = DT1, method = "bh") 
O <- cldList(P.adj ~ Comparison, data = PT$res, threshold = 0.05, remove.zero = FALSE)

fig <- ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) +
  xlab("\nGrupo\n") +
  ylab("\nAno\n") +
  labs(fill = "Grupo", color = "Grupo") +
  geom_point(size = 4) +
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_23.png", type = "png", device = dev.cur())

merged_data <- transform(DT1, ANO_PERIODO = paste(ANO, '.', PERIODO))

PT = dunnTest(MEDIA_FINAL ~ ANO_PERIODO, data = merged_data, method = "bh") 
O <- cldList(P.adj ~ Comparison, data = PT$res, threshold = 0.05, remove.zero = FALSE)

fig <- ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) +
  xlab("\nGrupo\n") +
  ylab("\nAno\n") +
  labs(fill = "Grupo", color = "Grupo") +
  geom_point(size = 4) +
  normal_theme +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(family = "TT Times New Roman", size = 14), 
        legend.position = "bottom", 
        legend.text = element_text(size = 15)) 

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_24.png", type = "png", device = dev.cur())

##############################################################################################################

# Technical Drawing II
DT2 <- all_data_II[all_data_II$DISCIPLINA == "DESENHO TECNICO II",]

# Variance analysis
aov_result <- aov(MEDIA_FINAL ~ ANO * PERIODO, data = DT2)

# Normality of errors test
shapiro.test(residuals(aov_result))

res <- as.data.frame(residuals(aov_result))
colnames(res)[1] <- "value"

fig1 <- ggplot(data = res, aes(value)) + 
  geom_histogram(color = "blue") +
  ggtitle("\nHistograma de Residuos\n") +
  ylab("\nFrequencia\n") + 
  xlab("\nResiduo\n") + 
  labs(fill = "Densidade") +   
  normal_theme      

ggbackground(fig1, img)
savePlot(filename = "../images/figure_specific_25.png", type = "png", device = dev.cur())

# Homogeneity of Variance between the groups
max(aggregate(MEDIA_FINAL ~ ANO + PERIODO, DT2, var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ANO + PERIODO, DT2, var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ANO,DT2, center = median)
leveneTest(MEDIA_FINAL ~ PERIODO,DT2, center = median)
leveneTest(MEDIA_FINAL ~ ANO * PERIODO, DT2, center = median)

# Distributions between the groups of years and semesters

kruskal.test(MEDIA_FINAL ~ ANO, DT2)
kruskal.test(MEDIA_FINAL ~ PERIODO, DT2)
kruskal.test(MEDIA_FINAL ~ interaction(ANO, PERIODO), DT2)

fig1 <- ggplot(DT2, aes(x = MEDIA_FINAL , fill = PERIODO, color = PERIODO)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT2$MEDIA_FINAL), max(DT2$MEDIA_FINAL), by = 5), 10)) +
  xlab("\nDistribuicao de Medias") + 
  ylab("\nDensidade de Medias\n") + 
  labs(fill = "Semestre", color = "Semestre") +
  ggtitle("\nDistribuicao das Medias em Desenho II\n") +
  normal_theme +       
  theme(axis.title = element_text(size = 25), 
        plot.title = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        legend.position = "bottom", 
        legend.text = element_text(size = 18)) 

ggbackground(fig1, img)
savePlot(filename = "../images/figure_specific_26.png", type = "png", device = dev.cur())


fig1 <- ggplot(DT2, aes(x = MEDIA_FINAL , fill = ANO, color = ANO)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT2$MEDIA_FINAL), max(DT2$MEDIA_FINAL), by = 5), 10)) +
  xlab("\nDistribuicao de Medias") + 
  ylab("\nDensidade de Medias\n") + 
  labs(fill = "Semestre", color = "Semestre") +
  ggtitle("\nDistribuicao das Medias em Desenho II\n") +
  normal_theme +       
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.title = element_text(size = 25), 
        plot.title = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        legend.position = "bottom", 
        legend.text = element_text(size = 18)) +
  facet_wrap(. ~ ANO, nrow = 4)

ggbackground(fig1, img)
savePlot(filename = "../images/figure_specific_27.png", type = "png", device = dev.cur())


fig1 <- ggplot(DT2, aes(x = MEDIA_FINAL , fill = ANO, color = PERIODO)) +
  geom_density(alpha = 0.7, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT2$MEDIA_FINAL), max(DT2$MEDIA_FINAL), by = 5), 10)) +
  xlab("\nDistribuicao de Medias") + 
  ylab("\nDensidade de Medias\n") + 
  labs(fill = "Ano", color = "Semestre") +
  ggtitle("\nDistribuicao das Medias em Desenho II\n") +
  normal_theme +       
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        legend.position = "bottom", 
        legend.text = element_text(size = 15)) +
  facet_wrap(ANO ~ PERIODO, nrow = 5)

ggbackground(fig1, img)
savePlot(filename = "../images/figure_specific_28.png", type = "png", device = dev.cur())

multiVDA(MEDIA_FINAL ~ ANO, data = DT2)
multiVDA(MEDIA_FINAL ~ PERIODO, data = DT2)
multiVDA(MEDIA_FINAL ~ SEX, data = DT2)
multiVDA(MEDIA_FINAL ~ CURSO, data = DT2)

merged_data <- transform(DT2, ANO_PERIODO = paste(ANO, '.', PERIODO))

PT = dunnTest(MEDIA_FINAL ~ ANO_PERIODO, data = merged_data, method = "bh") 
O <- cldList(P.adj ~ Comparison, data = PT$res, threshold = 0.05, remove.zero = FALSE)

fig <- ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) +
  xlab("\nGrupo\n") +
  ylab("\nAno\n") +
  labs(fill = "Grupo", color = "Grupo") +
  geom_point(size = 4) +
  normal_theme +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(family = "sans", size = 11), 
        legend.position = "bottom", 
        legend.text = element_text(size = 15)) 

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_29.png", type = "png", device = dev.cur())