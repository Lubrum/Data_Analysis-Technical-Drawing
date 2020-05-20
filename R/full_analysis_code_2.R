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
                quartil_1st = summary(!!sym(response))[2],
                quartil_3rd = summary(!!sym(response))[5],
                IQR = summary(!!sym(response))[5] - summary(!!sym(response))[2]
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

partial_data <- all_data_II[!(all_data_II$SITUACAO == "Reprovado por frequencia"),]
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
  axis.text = element_text(family = font_family, size = 16, color = "#cccccc"),
  axis.title = element_text(family = font_family, size = 20, color = "#cccccc"),
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
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("\nCurso\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Curso") +
        ggtitle("\nDesenho Tecnico I\n") + 
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure_specific_1.png", type = "png", device = dev.cur())

ggplot(DT1, 
       aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(ANO))) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("ANO") + ylab("Media Final") + labs(fill = "ANO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT1, 
       aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) + 
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("ANO") + ylab("Media Final") + labs(fill = "PERIODO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT2, 
       aes(x = CURSO, y = MEDIA_FINAL, fill = CURSO)) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("CURSO") + ylab("Media Final") + labs(fill = "CURSO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT2, 
       aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(ANO))) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.05) + 
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("ANO") + ylab("Media Final") + labs(fill = "ANO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT2, 
       aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
  geom_boxplot(width = 0.4, position = position_dodge(width = 0.9)) +
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("ANO") + ylab("Media Final") + labs(fill = "PERIODO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT1_partial, 
       aes(x = CURSO, y = MEDIA_FINAL, fill = CURSO)) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1) +
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
  xlab("CURSO") + ylab("Media Final") + labs(fill = "CURSO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT1_partial, 
       aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(ANO))) +
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("ANO") + ylab("Media Final") + labs(fill = "ANO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT1_partial, 
       aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
  geom_violin(trim = TRUE, position = dodge) + 
  geom_boxplot(width = 0.1, position = dodge) +
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("ANO") + ylab("Media Final") + labs(fill = "PERIODO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT2_partial, 
       aes(x = CURSO, y = MEDIA_FINAL, fill = CURSO)) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("CURSO") + ylab("Media Final") + labs(fill = "CURSO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT2_partial, 
       aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(ANO))) +
  geom_violin(trim = TRUE)+ 
  geom_boxplot(width = 0.1) + 
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("ANO") + ylab("Media Final") + labs(fill = "ANO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(DT2_partial, 
       aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
  geom_violin(trim = TRUE) + 
  geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "bottom") +
  xlab("ANO") + ylab("Media Final") + labs(fill = "PERIODO") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II (Sem reprovados por frequencia)") +
  theme(plot.title = element_text(hjust = 0.5))

aux <- statistics(all_data_II, "MEDIA_FINAL", SEXO, DISCIPLINA)      

ggplot(aux, aes(DISCIPLINA, round(mean,2), group = SEXO, fill = as.factor(SEXO))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_text(aes(label = round(mean, 2)), vjust = 2, color = "black", size = 5, position = position_dodge(0.9)) +
  scale_fill_manual("Sexo", values = c("F" = "deeppink", "M" = "deepskyblue")) +
  scale_y_continuous(breaks = seq(0, 7, by = 1)) +
  xlab("DISCIPLINA") + ylab("Media da Nota Final") + ggtitle("Análise de Medias por DISCIPLINA e Sexo") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 12))

aux <- statistics(all_data_II, "MEDIA_FINAL", ANO, PERIODO, DISCIPLINA)

a <- ggplot(aux, aes(as.factor(ANO), mean, group = PERIODO, fill = as.factor(PERIODO))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
  geom_text(aes(label = round(mean, 1)), vjust = 2, color = "black", size = 2.8, position = position_dodge(0.9)) +
  scale_fill_manual("PERIODO", values = c("1. Semestre" = "cadetblue2", "2. Semestre" = "darkolivegreen2")) +
  xlab("ANO") + ylab("Media da Nota Final") + labs(fill = "PERIODO") + ggtitle("Medias finais por DISCIPLINA, ANO e Semestre") +
  theme(axis.text.x = element_text(face = "bold", size = 10), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~DISCIPLINA) 

b <- ggplot(aux, aes(as.factor(ANO), n, group = PERIODO, fill = as.factor(PERIODO))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
  geom_text(aes(label = n), vjust = 2, color = "black", size = 2.8, position = position_dodge(0.9)) +
  scale_fill_manual("PERIODO", values = c("1. Semestre" = "cadetblue2", "2. Semestre" = "darkolivegreen2")) +
  scale_y_continuous(breaks = round(seq(0, 60, by = 5), 1)) +
  xlab("ANO") + ylab("Número de Alunos") + labs(fill = "PERIODO") + ggtitle("Número de Alunos por DISCIPLINA, PERIODO e ANO") +
  theme(axis.text.x = element_text(face = "bold", size = 10), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~DISCIPLINA)

ggarrange(a, b, nrow = 2)

aux <- statistics(DT1, "MEDIA_FINAL", ANO, PERIODO, SITUACAO)

aux <- as.data.frame(aux %>% 
                       group_by(ANO, PERIODO) %>% 
                       mutate(percent = 100*round(n/sum(n),8),
                              pos = 3.5 + (round(cumsum(percent) - (0.5 * percent), 8))
                       )
)

aux$PERIODO[aux$PERIODO == "1. Semestre"] <- "1 -"
aux$PERIODO[aux$PERIODO == "2. Semestre"] <- "2 -"
aux$SITUACAO <- factor(aux$SITUACAO, levels = rev(levels(as.factor(aux$SITUACAO))))

a <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = percent, fill = as.factor(SITUACAO)), stat = "identity", color = "black") +
  scale_fill_manual(values = c("orange", "red", "green")) +
  geom_text(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = pos, label = paste0(round(percent, 1),"%(n=",n,")")), vjust = 2, color = "black", size = 3) +
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  xlab("Semestre - ANO") + ylab("Percentual de Alunos") + labs(fill = "SITUACAO") +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title=element_text(hjust = 0.5),
        axis.text.x=element_text(colour = "black", angle = 45, size = 9, hjust = 1),
        axis.text.y=element_text(colour = "black", size = 10)) 

aux <- statistics(DT1, "MEDIA_FINAL", ANO, PERIODO)

b <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = n), stat = "identity", color = "black") +
  geom_text(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = n, label = n), vjust = 2, color = "black", size = 4) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  ylab("Número de Alunos") +
  ggtitle("SITUACAO de Alunos por PERIODO e ANO em Desenho Tecnico I") +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  aes(fill = I("darkseagreen2"))

ggarrange(b, a, nrow = 2, heights=c(1, 4))

aux <- statistics(DT2, "MEDIA_FINAL", ANO, PERIODO, SITUACAO)

aux <- as.data.frame(aux %>% 
                       group_by(ANO, PERIODO) %>% 
                       mutate(percent = 100*round(n/sum(n),8),
                              pos = 4.4 + (round(cumsum(percent) - (0.5 * percent), 8))
                       )
)

aux$PERIODO[aux$PERIODO == "1. Semestre"] <- "1 -"
aux$PERIODO[aux$PERIODO == "2. Semestre"] <- "2 -"
aux$SITUACAO <- factor(aux$SITUACAO, levels = rev(levels(as.factor(aux$SITUACAO))))

a <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = percent, fill = as.factor(SITUACAO)), stat = "identity", color = "black") +
  scale_fill_manual(values = c("orange", "red", "green")) +
  geom_text(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = pos, label = paste0(round(percent, 1),"%")), vjust = 2, color = "black", size = 3) +
  geom_text(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = pos-2.5, label = paste0("n = ",n)), vjust = 2, color = "black", size = 2.5) +        
  scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
  xlab("Semestre - ANO") + ylab("Percentual de Alunos") + labs(fill = "SITUACAO") +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title=element_text(hjust = 0.5),
        axis.text.x=element_text(colour = "black", angle = 45, size = 9, hjust = 1),
        axis.text.y=element_text(colour = "black", size = 10)) 

aux <- statistics(DT2, "MEDIA_FINAL", ANO, PERIODO)

b <- ggplot() + 
  geom_bar(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = n), stat = "identity", color = "black") +
  geom_text(data = aux, aes(x = interaction(as.factor(PERIODO), as.factor(ANO)), y = n, label = n), vjust = 2, color = "black", size = 4) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  ylab("Número de Alunos") +
  ggtitle("SITUACAO de Alunos por PERIODO e ANO em Desenho Tecnico II") +
  theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  aes(fill = I("darkseagreen2"))

ggarrange(b, a, nrow = 2, heights=c(1, 4))

ggplot(all_data_II[all_data_II$SITUACAO!="Reprovado por frequencia" & all_data_II$DISCIPLINA == "DESENHO TECNICO I",], aes(x = faltas, y = MEDIA_FINAL)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  scale_x_continuous(breaks = seq(0, 16, by = 4)) +
  ylab("Media Final") +
  ggtitle("Desenho Tecnico I") +
  geom_smooth(method = lm) +
  geom_jitter(width = 0.25) +
  theme(plot.title=element_text(hjust = 0.5))

ggplot(all_data_II[all_data_II$SITUACAO!="Reprovado por frequencia" & all_data_II$DISCIPLINA == "DESENHO TECNICO II",], aes(x = faltas, y = MEDIA_FINAL)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  scale_x_continuous(breaks = seq(0, 16, by = 2)) +
  ylab("Media Final") +
  ggtitle("Desenho Tecnico II") +
  geom_smooth(method = lm) +
  geom_jitter(width = 0.25) +
  theme(plot.title=element_text(hjust = 0.5))

# Fourth Part - Dataset 2 - Advanced Statistics

dfII_without_failed_by_attendance$PERIODO[dfII_without_failed_by_attendance$PERIODO == "1. Semestre"] <- 1
dfII_without_failed_by_attendance$PERIODO[dfII_without_failed_by_attendance$PERIODO == "2. Semestre"] <- 2
dfII_without_failed_by_attendance$PERIODO <- as.factor(dfII_without_failed_by_attendance$PERIODO)
dfII_without_failed_by_attendance$ANO <- as.factor(dfII_without_failed_by_attendance$ANO)

res = residuals(lm(MEDIA_FINAL ~ ANO * PERIODO, data = DT1_partial))
shapiro.test(res)
hist(res)

max(aggregate(MEDIA_FINAL ~ ANO + PERIODO, DT1_partial, var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ANO + PERIODO, DT1_partial, var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ANO, DT1_partial)
leveneTest(MEDIA_FINAL ~ PERIODO, DT1_partial)
leveneTest(MEDIA_FINAL ~ ANO * PERIODO, DT1_partial)

kruskal.test(MEDIA_FINAL ~ ANO, DT1_partial)
kruskal.test(MEDIA_FINAL ~ PERIODO, DT1_partial)
kruskal.test(MEDIA_FINAL ~ interaction(ANO, PERIODO), DT1_partial)

densityplot(~ MEDIA_FINAL | ANO, data = DT1_partial, xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | PERIODO, data = DT1_partial, xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | ANO*PERIODO, data = DT1_partial, xlab = "Media Final", ylab = "Densidade de Medias")


#multiVDA(MEDIA_FINAL ~ ANO, data = DT1_partial)
multiVDA(MEDIA_FINAL ~ PERIODO, data = DT1_partial)
multiVDA(MEDIA_FINAL ~ SEXO, data = DT1_partial)
multiVDA(MEDIA_FINAL ~ CURSO, data = DT1_partial)

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ANO,
              data = DT1_partial,
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
             data = PT$res,
             threshold = 0.05,
             remove.zero = FALSE)

ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 


res = residuals(lm(MEDIA_FINAL ~ ANO * PERIODO, data = DT2_partial))
shapiro.test(res)
hist(res)

max(aggregate(MEDIA_FINAL ~ ANO + PERIODO, DT2_partial, var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ANO + PERIODO, DT2_partial, var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ANO, DT2_partial)
leveneTest(MEDIA_FINAL ~ PERIODO, DT2_partial)
leveneTest(MEDIA_FINAL ~ ANO * PERIODO, DT2_partial)

kruskal.test(MEDIA_FINAL ~ ANO, DT2_partial)
kruskal.test(MEDIA_FINAL ~ PERIODO, DT2_partial)
kruskal.test(MEDIA_FINAL ~ interaction(ANO, PERIODO), DT2_partial)

densityplot(~ MEDIA_FINAL | ANO, data = DT2_partial, xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | PERIODO, data = DT2_partial, xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | ANO*PERIODO, data = DT2_partial, xlab = "Media Final", ylab = "Densidade de Medias")

#multiVDA(MEDIA_FINAL ~ ANO, data = DT1_partial)
multiVDA(MEDIA_FINAL ~ PERIODO, data = DT2_partial)
multiVDA(MEDIA_FINAL ~ SEXO, data = DT2_partial)
multiVDA(MEDIA_FINAL ~ CURSO, data = DT2_partial)

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ANO,
              data = DT2_partial,
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
             data = PT$res,
             threshold = 0.05,
             remove.zero = FALSE)

ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 