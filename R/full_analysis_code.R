setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

#if (!require(xlsx)) install.packages("xlsx")
#library(xlsx)

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

if (!require(showtext)) install.packages("showtext")
library(showtext)

if (!require(extrafont)) install.packages("extrafont")
library(extrafont)

#if(!require(RColorBrewer)) install.packages("RColorBrewer")
#library(RColorBrewer)

#if(!require(viridis)) install.packages("viridis")
#library(viridis)

#if (!require(matlab)) install.packages("matlab")
#library(matlab)

#if (!require(car)) install.packages("car")
#library(car)

#if (!require(lattice)) install.packages("lattice")
#library(lattice)

#if (!require(FSA))install.packages("FSA")
#library(FSA)

#if (!require(rcompanion)) install.packages("rcompanion")
#library(rcompanion)

#if (!require(coin)) install.packages("coin")
#library(coin)

#if (!require(gridExtra)) install.packages("gridExtra")
#library(gridExtra)

#if (!require(grid)) install.packages("grid")
#library(grid)

#if (!require(reshape)) install.packages("reshape")
#library(reshape)

#if (!require(extrafont)) install.packages("extrafont")
#library(extrafont)

#if (!require(viridis)) install.packages("viridis")
#library(viridis)

#if (!require(nortest)) install.packages("nortest")
#library(nortest)

#if (!require(cowplot)) install.packages("cowplot")
#library(cowplot)

#if (!require(ggExtra)) install.packages("ggExtra")
#library(ggExtra)

# First Part - Dataset 1 - Loading and Processing

BAEA_path <- "../csv/Technical_Drawing_I_II - BAEA.csv"
BAEE_path <- "../csv/Technical_Drawing_I_II - BAEE.csv"
BAEC_path <- "../csv/Technical_Drawing_I_II - BAEC.csv"
BAEP_path <- "../csv/Technical_Drawing_I_II - BAEP.csv"
BAEQ_path <- "../csv/Technical_Drawing_I_II - BAEQ.csv"
BALF_path <- "../csv/Technical_Drawing_I_II - BALF.csv"

BAEA <- read.csv(BAEA_path, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
BAEE <- read.csv(BAEE_path, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
BAEC <- read.csv(BAEC_path, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
BAEP <- read.csv(BAEP_path, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
BAEQ <- read.csv(BAEQ_path, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
BALF <- read.csv(BALF_path, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

BAEA <- BAEA[,-10]

colnames(BAEA) == colnames(BAEE)
colnames(BAEA) == colnames(BAEC)
colnames(BAEA) == colnames(BAEP)
colnames(BAEA) == colnames(BAEQ)
colnames(BAEA) == colnames(BALF)

colnames(BAEC)[10] <- colnames(BAEA)[10]

all_data <- rbind(BAEA, BAEE)
all_data <- rbind(all_data, BAEC)
all_data <- rbind(all_data, BAEP)
all_data <- rbind(all_data, BAEQ)
all_data <- rbind(all_data, BALF)

all_data$MATR_ALUNO[is.na(all_data$MATR_ALUNO)]
all_data$MATR_ALUNO[all_data$MATR_ALUNO == ""]

all_data$COD_CURSO[is.na(all_data$COD_CURSO)]
all_data$COD_CURSO[all_data$COD_CURSO == ""]
unique(all_data$COD_CURSO)

all_data$ANO[is.na(all_data$ANO)]
unique(all_data$ANO)

all_data$PERIODO[is.na(all_data$PERIODO)]
all_data$PERIODO[all_data$PERIODO == ""]
unique(all_data$PERIODO)

all_data$COD_ATIV_CURRIC[is.na(all_data$COD_ATIV_CURRIC)]
all_data$COD_ATIV_CURRIC[all_data$COD_ATIV_CURRIC == ""]
unique(all_data$COD_ATIV_CURRIC)

all_data$NOME_ATIV_CURRIC[is.na(all_data$NOME_ATIV_CURRIC)]
all_data$NOME_ATIV_CURRIC[all_data$NOME_ATIV_CURRIC == ""]
unique(all_data$NOME_ATIV_CURRIC)

all_data$CREDITOS[is.na(all_data$CREDITOS)]
unique(all_data$CREDITOS)

all_data$MEDIA_FINAL[is.na(all_data$MEDIA_FINAL)]
all_data$MEDIA_FINAL[all_data$MEDIA_FINAL > 10 | all_data$MEDIA_FINAL < 0] 
all_data$MEDIA_FINAL[all_data$MEDIA_FINAL > 10 | all_data$MEDIA_FINAL < 0] <- 0

all_data$DESCR_SITUACAO[all_data$DESCR_SITUACAO == ""]
all_data$DESCR_SITUACAO[is.na(all_data$DESCR_SITUACAO)]
unique(all_data$DESCR_SITUACAO)

all_data[is.na(all_data$TOTAL_CARGA_HORARIA),]
unique(all_data$TOTAL_CARGA_HORARIA)

all_data$FORMA_INGRESSO[all_data$FORMA_INGRESSO == ""]
all_data$FORMA_INGRESSO[is.na(all_data$FORMA_INGRESSO)]
unique(all_data$FORMA_INGRESSO)

all_data$ANO_INGRESSO[is.na(all_data$ANO_INGRESSO)]
unique(all_data$ANO_INGRESSO)

all_data$FORMA_EVASAO[all_data$FORMA_EVASAO == ""]
all_data$FORMA_EVASAO[is.na(all_data$FORMA_EVASAO)]
unique(all_data$FORMA_EVASAO)

all_data$ANO_EVASAO[is.na(all_data$ANO_EVASAO)]
all_data$ANO_EVASAO[is.na(all_data$ANO_EVASAO)] <- 0
unique(all_data$ANO_EVASAO)

all_data <- all_data[all_data$DESCR_SITUACAO != "Dispensado sem nota",]
all_data <- all_data[!(all_data$DESCR_SITUACAO == "Aproveitamento" & all_data$MEDIA_FINAL == 0),]
all_data <- all_data[(all_data$DESCR_SITUACAO != "Trancamento parcial"),]
all_data <- all_data[(all_data$DESCR_SITUACAO != "Matrícula"),]
all_data <- all_data[(all_data$DESCR_SITUACAO != "Disciplina Não Concluída"),]

unique(all_data$FORMA_EVASAO)
all_data$FORMA_EVASAO[(all_data$FORMA_EVASAO == "Transf. Interna Por Reopção de Curso")] <- "Reopção"
all_data$FORMA_EVASAO[(all_data$FORMA_EVASAO == "Transferência Interna")] <- "Transf. Interna"

# Second Part - Dataset 1 - Descriptive Statistics

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

by_course_drawing_I <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", COD_CURSO)

by_year_drawing_I <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", ANO)

by_year_course_drawing_I <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_I <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)

by_course_drawing_II <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", COD_CURSO)

by_year_drawing_II <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO)

by_year_course_drawing_II <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_II <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)

all_data_without_frequency_dropout <- all_data[!(all_data$DESCR_SITUACAO == "Reprovado por frequencia"),]

by_course_drawing_I_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", COD_CURSO)

by_year_drawing_I_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", ANO)

by_year_course_drawing_I_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_I_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)

by_course_drawing_II_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", COD_CURSO)

by_year_drawing_II_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO)

by_year_course_drawing_II_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_II_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)

# Third Part - Dataset 1 - Data Visualization

x <- rep(1:length(unique(all_data$FORMA_EVASAO)))

boxplot(MEDIA_FINAL ~ FORMA_EVASAO,
        data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",],
        main = "Disciplina de Desenho Tecnico I",
        xlab = "Formas de Evasão",
        ylab = "Media Final",
        col = rainbow(length(unique(x))),
        border = "black"
)

boxplot(MEDIA_FINAL ~ FORMA_EVASAO,
        data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",],
        main = "Disciplina de Desenho Tecnico II",
        xlab = "Formas de Evasão",
        ylab = "Media Final",
        col = rainbow(length(unique(x))),
        border = "black"
)

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
  axis.title = element_text(family = font_family, size = 35, color = "#cccccc"),
  axis.ticks = element_line(colour = "#cccccc"),
  axis.ticks.length = unit(0.5, "cm"),
  plot.caption = element_text(family = font_family, size = 16, color = "#cccccc"),
  plot.title = element_text(family = font_family, size = 45, hjust = 0.5, color = "#ffffff"),
  plot.background = element_rect(fill = "black"),
  panel.grid.minor.y = element_line(size =.1, color = "grey"),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(size =.1, color = "grey"),
  panel.grid.major.x = element_blank(),
  panel.background = element_rect(fill = 'black'),
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect(color = "gray", fill = "black"),
  legend.text = element_text(family = font_family, size = 20, color = "#cccccc"),
  text = element_text(family = font_family, color = "#cccccc", size = 22)
)

## Manually open a graphics device if you run this code in RStudio
x11()

first <- ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = COD_CURSO, y = MEDIA_FINAL, fill = COD_CURSO)) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(color = "black", width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("\nCurso\n") + ylab("\nMedia Final\n") + 
        labs(fill = "Curso") +
        ggtitle("\nDesenho Tecnico I\n") + 
        normal_theme

ggbackground(first, img, alpha=.9)
savePlot(filename = "figure1.png", type = "png", device = dev.cur())

ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = ANO, y = MEDIA_FINAL, fill = as.factor(ANO))) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black", width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
        geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(min(all_data$ANO), max(all_data$ANO), by = 1)) +
        xlab("\nAno\n") + ylab("\nMedia Final\n") + labs(fill = "Ano") + ggtitle("\nDesenho Tecnico I\n") +
        normal_theme

ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE) +
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("\nAno\n") + ylab("\nMedia Final\n") + labs(fill = "Periodo") + ggtitle("\nDesenho Tecnico I\n") +
        normal_theme

ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = COD_CURSO, y = MEDIA_FINAL, fill = COD_CURSO)) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(color = "black", width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("\nCurso\n") + ylab("\nMedia Final\n") + labs(fill = "Curso") + ggtitle("\nDesenho Tecnico II\n") +
        normal_theme

ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = ANO, y = MEDIA_FINAL, fill = as.factor(ANO), group = ANO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black", width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
        geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
        xlab("\nAno\n") + ylab("\nMedia Final\n") + labs(fill = "Ano") + ggtitle("\nDesenho Tecnico II\n") +
        normal_theme

ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE) +
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("\nAno\n") + ylab("\nMedia Final\n") + labs(fill = "Periodo") + ggtitle("\nDesenho Tecnico II\n") +
        normal_theme

ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = COD_CURSO, y = MEDIA_FINAL, fill = COD_CURSO)) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(color = "black", width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
        xlab("\nCurso\n") + ylab("\nMedia Final\n") + labs(fill = "Curso") + 
        ggtitle("\nMedias em Desenho Tecnico I (Sem reprovados por frequencia)\n") +
        normal_theme

ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = ANO, y = MEDIA_FINAL, fill = as.factor(ANO), group = ANO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black", width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
        geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
        xlab("\nAno\n") + ylab("\nMedia Final\n") + labs(fill = "Ano") + 
        ggtitle("\nMedias em Desenho Tecnico I (Sem reprovados por frequencia)\n") +
        normal_theme

ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE) + 
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("\nAno\n") + ylab("\nMedia Final\n") + labs(fill = "Periodo") + 
        ggtitle("\nMedias em Desenho Tecnico I (Sem reprovados por frequencia)\n") +
        normal_theme

ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = COD_CURSO, y = MEDIA_FINAL, fill = COD_CURSO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black", width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("\nCurso\n") + ylab("\nMedia Final\n") + labs(fill = "Curso") + 
        ggtitle("\nMedias em Desenho Tecnico II (Sem reprovados por frequencia)\n") +
        normal_theme

ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = ANO, y = MEDIA_FINAL, fill = as.factor(ANO), group = ANO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black", width = 0.1, outlier.shape = 21, outlier.size = 2, outlier.fill = "white", show.legend = FALSE) + 
        geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
        xlab("\nAno\n") + ylab("\nMedia Final\n") + labs(fill = "Ano") + 
        ggtitle("\nMedias em Desenho Tecnico II (Sem reprovados por frequencia)\n") +
        normal_theme

ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE) +
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("\nAno\n") + ylab("\nMedia Final\n") + labs(fill = "Periodo") + 
        ggtitle("\nMedias em Desenho Tecnico II (Sem reprovados por frequencia)\n") +
        normal_theme

by_sex <- statistics(all_data, "MEDIA_FINAL", SEXO)
by_sex_dropout <- statistics(all_data[(all_data$DESCR_SITUACAO == "Reprovado por frequencia"),], "MEDIA_FINAL", SEXO)

by_sex$per[by_sex$SEXO == "M"] <- round((by_sex_dropout$n[by_sex_dropout$SEXO == "M"] / by_sex$n[by_sex$SEXO == "M"]) * 100, 2)
by_sex$per[by_sex$SEXO =="F"] <- round((by_sex_dropout$n[by_sex_dropout$SEXO == "F"] / by_sex$n[by_sex$SEXO == "F"]) * 100, 2) 
by_sex$ndrop[by_sex$SEXO == "M"] <- round((by_sex_dropout$n[by_sex_dropout$SEXO == "M"]), 2)
by_sex$ndrop[by_sex$SEXO == "F"] <- round((by_sex_dropout$n[by_sex_dropout$SEXO == "F"]), 2) 

a <- ggplot(by_sex, aes(x = SEXO, y = ndrop, color = SEXO)) + 
        geom_bar(fill = "#101010", stat = "identity") +
        geom_text(data = by_sex, aes(label = ndrop), hjust = 0.5, vjust = 1.5, color = "white", size = 5) +
        scale_fill_hue(c = 40) +
        xlab("\nSexo\n") + ylab("\nNúmero de reprovados\n") + labs(fill = "Sexo") + 
        ggtitle("\nreprovados por frequencia (Desenho Tecnico I e II)\n") +
        normal_theme

b <- ggplot(data = by_sex, aes(x = SEXO, y = per, color = SEXO)) + 
        geom_bar(fill = "#101010", stat = 'identity') +
        geom_text(data = by_sex, aes(label = paste(per, '%')), hjust = 0.5, vjust = 1.5, color = "white", size = 5) +
        scale_fill_hue(c = 40) +
        xlab("\nSexo\n") + ylab("\nPercentual de reprovados em Relação ao Total\n") + labs(fill = "Sexo") + 
        ggtitle("\nreprovados por frequencia (Desenho Tecnico I e II)\n") +
        normal_theme

ggarrange(a, b, ncol = 2)

frequency_dropout_data <- all_data[(all_data$DESCR_SITUACAO == "Reprovado por frequencia"),]

by_sex_discipline <- statistics(all_data, "MEDIA_FINAL", SEXO, NOME_ATIV_CURRIC)
by_sex_discipline_dropout <- statistics(all_data[(all_data$DESCR_SITUACAO == "Reprovado por frequencia"),], "MEDIA_FINAL", SEXO, NOME_ATIV_CURRIC)
by_sex_discipline_dropout$per <- round((by_sex_discipline_dropout$n[by_sex_discipline_dropout$SEXO == by_sex_discipline$SEXO && by_sex_discipline_dropout$NOME_ATIV_CURRIC == by_sex_discipline$NOME_ATIV_CURRIC] / 
                by_sex_discipline$n[by_sex_discipline$SEXO == by_sex_discipline_dropout$SEXO && by_sex_discipline$NOME_ATIV_CURRIC == by_sex_discipline_dropout$NOME_ATIV_CURRIC]) * 100, 2) 

ggplot(by_sex_discipline_dropout, aes(NOME_ATIV_CURRIC, n, group = SEXO, color = SEXO)) + 
        geom_bar(stat = "identity", fill = "#101010", position = position_dodge2()) +
        geom_text(data = by_sex_discipline_dropout, aes(label = paste(n,"(",per,"% )")), hjust = 0.5, vjust = 1.5, color = "white", size = 5, position = position_dodge2(width = 0.9)) +
        scale_fill_hue(c = 40) +
        scale_y_continuous(breaks = seq(0, 400, by = 50)) +
        xlab("\nDisciplina\n") + ylab("\nNúmero de reprovados\n") + labs(fill = "Sexo") + 
        ggtitle("\nreprovados por frequencia\n\n") +
        normal_theme

by_sex_discipline_year <- statistics(all_data, "MEDIA_FINAL", SEXO, NOME_ATIV_CURRIC, ANO)
by_sex_discipline_dropout_year  <- statistics(all_data[(all_data$DESCR_SITUACAO == "Reprovado por frequencia"),], "MEDIA_FINAL", SEXO, NOME_ATIV_CURRIC, ANO)
by_sex_discipline_dropout_year$per <- round((by_sex_discipline_dropout_year$n[by_sex_discipline_dropout_year$SEXO == by_sex_discipline_year$SEXO && by_sex_discipline_dropout_year$NOME_ATIV_CURRIC == by_sex_discipline_year$NOME_ATIV_CURRIC && by_sex_discipline_dropout_year$ANO == by_sex_discipline_year$ANO] / 
                                               by_sex_discipline_year$n[by_sex_discipline_year$SEXO == by_sex_discipline_dropout_year$SEXO && by_sex_discipline_year$NOME_ATIV_CURRIC == by_sex_discipline_dropout_year$NOME_ATIV_CURRIC && by_sex_discipline_dropout_year$ANO == by_sex_discipline_year$ANO]) * 100, 2) 

a <- ggplot(by_sex_discipline_dropout_year, aes(ANO, n, color = SEXO)) + 
        geom_bar(stat = "identity", fill = "#101010", position = position_dodge2()) +
        scale_fill_hue(c = 40) +
        ylab("\nfrequencia\n") + labs(fill = "Sexo") + ggtitle("\nreprovados por frequencia") +
        normal_theme +
        scale_y_continuous(breaks = seq(0, 50, by = 5)) +
        scale_x_continuous(breaks = round(seq(min(aux$ANO), max(aux$ANO), by = 1), 1)) +
        facet_wrap(~NOME_ATIV_CURRIC) 

b <- ggplot(by_sex_discipline_dropout_year, aes(ANO, per, color = SEXO)) + 
        geom_bar(stat = "identity",  fill = "#101010", position = position_dodge2()) +
        scale_fill_hue(c = 40) +
        ylab("\nPercentual (%)\n") + labs(fill = "Sexo") +
        normal_theme +
        scale_y_continuous(breaks = seq(0, 50, by = 5)) +
        scale_x_continuous(breaks = round(seq(min(aux$ANO), max(aux$ANO), by = 1), 1)) +
        facet_wrap(~NOME_ATIV_CURRIC) 

ggarrange(a, b, nrow = 2)

aux <- frequency_dropout_data %>%
        group_by(NOME_ATIV_CURRIC,PERIODO,ANO) %>%
        tally()

ggplot(aux, aes(NOME_ATIV_CURRIC, n, fill=as.factor(PERIODO))) + 
        geom_bar(stat="identity", color="black", position=position_dodge()) +
        scale_fill_hue(c = 50) +
        ylab("frequencia") + xlab("Discipline") + labs(fill = "Disciplina") + ggtitle("reprovados por frequencia") +
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 8)) +
        facet_wrap(~ANO) 

aux <- frequency_dropout_data %>%
        group_by(NOME_ATIV_CURRIC,FORMA_EVASAO) %>%
        tally()

ggplot(aux, aes(NOME_ATIV_CURRIC, n, fill=as.factor(FORMA_EVASAO))) + 
        geom_bar(stat="identity", width = 0.5, color = "black", position=position_dodge()) +
        scale_fill_hue(c = 50) +
        ylab("frequencia") + labs(fill = "Tipo de Evasão")  + xlab("Disciplina") + ggtitle("Situação de Alunos Reprovados por frequencia") 

aux <- all_data %>%
        group_by(NOME_ATIV_CURRIC,DESCR_SITUACAO,ANO) %>%
        tally()

ggplot(aux, aes(NOME_ATIV_CURRIC, n, fill=as.factor(DESCR_SITUACAO))) + 
        geom_bar(stat="identity", width = 0.5, color = "black", position=position_dodge()) +
        scale_fill_hue(c = 50) +
        ylab("frequencia") + labs(fill = "Disciplina") + xlab("Disciplina") + ggtitle("Aprovações e reprovados por Ano e Disciplina") +
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 7)) +
        facet_wrap(~ANO) 

aux <- as.data.frame(all_data %>%
        group_by(ANO,DESCR_SITUACAO,NOME_ATIV_CURRIC) %>%
        tally())

ggplot(aux, aes(ANO, n, group = DESCR_SITUACAO, color = DESCR_SITUACAO)) + 
        geom_line(size=1.5) +
        geom_point(size=3, shape=21, fill="white") +
        scale_x_continuous(breaks = round(seq(min(aux$ANO), max(aux$ANO), by = 1),1)) +
        scale_y_continuous(breaks = round(seq(0, max(aux$n), by = 10),1)) +
        ylab("frequencia") + labs(color = "Situação do Aluno") + ggtitle("Aprovações e reprovados por Ano e Disciplina") +
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 8)) +
        facet_wrap(~NOME_ATIV_CURRIC) 

aux <- as.data.frame(all_data %>%
        group_by(ANO,DESCR_SITUACAO,NOME_ATIV_CURRIC) %>%
        tally())

aux <- as.data.frame(aux %>% 
        group_by(ANO, NOME_ATIV_CURRIC) %>% 
        mutate(percent = 100*(n/sum(n))))

ggplot(aux, aes(ANO, percent, group = DESCR_SITUACAO, color = DESCR_SITUACAO)) + 
        geom_line(size=1.5) +
        geom_point(size=3, shape=21, fill="white") +
        scale_x_continuous(breaks = round(seq(min(aux$ANO), max(aux$ANO), by = 1),1)) +
        scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
        ylab("Percentual de Alunos (%)") + labs(color = "Situação do Aluno") + ggtitle("Aprovações e reprovados por Ano e Disciplina") +
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 8)) +
        facet_wrap(~NOME_ATIV_CURRIC) 

ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = MEDIA_FINAL , fill = as.factor(ANO))) +
        geom_density(color = "black") + xlab("Media Final") + ylab("Densidade") + labs(fill = "Ano") +
        ggtitle("Distribuição das Medias em Desenho Tecnico I (Sem reprovados por frequencia)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_wrap(c("ANO","PERIODO"))

ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = MEDIA_FINAL , fill = as.factor(ANO))) +
        geom_density(color = "black") + xlab("Media Final") + ylab("Densidade") + labs(fill = "Ano") +
        ggtitle("Distribuição das Medias em Desenho Tecnico II (Sem reprovados por frequencia)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_wrap(c("ANO","PERIODO"))

ggplot(all_data_without_frequency_dropout,
        aes(x = ANO, y = MEDIA_FINAL)) +
        ggtitle("Densidade das Medias por Ano (Sem reprovados por frequencia)") +
        ylab("Media Final") + xlab("Ano") + labs(fill = "Densidade") +      
        stat_density2d(aes(fill = ..density..), contour = F, geom = 'tile') +
        scale_fill_distiller(palette = 7) +
        geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), linetype = "dotted", colour = "black") +
        geom_segment(aes(x = 2007, y = 7, xend = 2019, yend = 7), linetype = "dotted", colour = "black") +
        geom_segment(aes(x = 2016, y = 0, xend = 2016, yend = 10), linetype = "dotted", colour = "black") +
        geom_segment(aes(x = 2018, y = 0, xend = 2018, yend = 10), linetype = "dotted", colour = "black") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(2007, 2019, by = 1)) +
        theme(panel.spacing = unit(2, "lines"), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 8)) +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 10, by = 1)) +
        facet_wrap(~NOME_ATIV_CURRIC)

# Fourth Part - Dataset 1 - Advanced Statistics

all_data$PERIODO[all_data$PERIODO == "1. Semestre"] <- 1
all_data$PERIODO[all_data$PERIODO == "2. Semestre"] <- 2
all_data$PERIODO <- as.factor(all_data$PERIODO)
all_data$ANO <- as.factor(all_data$ANO)

aov_result <- aov(MEDIA_FINAL ~ ANO * PERIODO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
shapiro.test(residuals(aov_result))
hist(res, main = "Histogram of residuals", xlab = "Residuals")

max(aggregate(MEDIA_FINAL ~ ANO + PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ANO + PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ANO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], center = median)
leveneTest(MEDIA_FINAL ~ PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], center = median)
leveneTest(MEDIA_FINAL ~ ANO * PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], center = median)

kruskal.test(MEDIA_FINAL ~ ANO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ interaction(ANO, PERIODO), all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

densityplot(~ MEDIA_FINAL | ANO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Distribuição de Medias por Ano (Desenho Tecnico I)", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | PERIODO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], ylab = "Densidade de Medias", xlab = "Distribuição de Medias por Semestre (Desenho Tecnico I)")
densityplot(~ MEDIA_FINAL | ANO * PERIODO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Distribuição de Medias por Ano e Semestre (Desenho Tecnico I)", ylab = "Densidade de Medias")

multiVDA(MEDIA_FINAL ~ ANO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ PERIODO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ SEX, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ COD_CURSO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ANO,
          data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",],
          method = "bh") 

O <- cldList(P.adj ~ Comparison,
          data = PT$res,
          threshold = 0.05,
          remove.zero = FALSE)

ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4)       

all_data_without_frequency_dropout$PERIODO[all_data_without_frequency_dropout$PERIODO == "1. Semestre"] <- 1
all_data_without_frequency_dropout$PERIODO[all_data_without_frequency_dropout$PERIODO == "2. Semestre"] <- 2
all_data_without_frequency_dropout$PERIODO <- as.factor(all_data_without_frequency_dropout$PERIODO)
all_data_without_frequency_dropout$ANO <- as.factor(all_data_without_frequency_dropout$ANO)

res = residuals(lm(MEDIA_FINAL ~ ANO * PERIODO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",]))
shapiro.test(res)
hist(res, main = "Histogram of residuals", xlab = "Residuals")


max(aggregate(MEDIA_FINAL ~ ANO + PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ANO + PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ANO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
leveneTest(MEDIA_FINAL ~ PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
leveneTest(MEDIA_FINAL ~ ANO * PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

kruskal.test(MEDIA_FINAL ~ ANO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ interaction(ANO, PERIODO), all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

densityplot(~ MEDIA_FINAL | ANO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | PERIODO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | ANO * PERIODO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Media Final", ylab = "Densidade de Medias")

multiVDA(MEDIA_FINAL ~ ANO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ PERIODO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ SEX, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ COD_CURSO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ANO,
          data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",],
          method = "bh") 

O <- cldList(P.adj ~ Comparison,
          data = PT$res,
          threshold = 0.05,
          remove.zero = FALSE)

ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 

# First Part - Dataset 2 - Data Loading and Processing

all_data_II <- read.csv("csv/Professor_Specific_Data.csv", sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

unique(all_data_II$situação)
all_data_II <- all_data_II[(all_data_II$situação != "Trancamento parcial" & all_data_II$situação != "Dispensado sem  nota" & all_data_II$situação != "Disciplina Não Concluída"),]

sex_data <- read.csv("csv/sex_data.csv", sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

all_data_II <- cbind(all_data_II, sex_data)
all_data_II <- all_data_II[,-c(1,5,6,7,8,9,15)]

all_data_II$componente.curricular[all_data_II$componente.curricular == "010801-DESENHO TECNICO I"] <- "DESENHO TECNICO I"
all_data_II$componente.curricular[all_data_II$componente.curricular == "010803-DESENHO TECNICO II"] <- "DESENHO TECNICO II"
all_data_II$componente.curricular[all_data_II$componente.curricular == "BA010801-DESENHO TECNICO I"] <- "DESENHO TECNICO I"
all_data_II$componente.curricular[all_data_II$componente.curricular == "BA010803-DESENHO TECNICO II"] <- "DESENHO TECNICO II"

all_data_II$ano <- as.numeric(substr(all_data_II$ano.periodo, 1, 4))
all_data_II$periodo <- substr(all_data_II$ano.periodo, 8, 18)
all_data_II <- all_data_II[,-2]
colnames(all_data_II)[6] <- "MEDIA_FINAL"

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

dfII_without_dropout <- all_data_II[!(all_data_II$situação == "Reprovado por frequencia"),]

df2_by_course_drawing_I_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", curso)

df2_by_year_drawing_I_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano)

df2_by_year_course_drawing_I_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, curso)

df2_by_year_semester_course_drawing_I_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, curso, periodo)

df2_by_year_semester_class_drawing_I_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], "MEDIA_FINAL", ano, periodo, turma)

df2_by_course_drawing_II_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", curso)

df2_by_year_drawing_II_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano)

df2_by_year_course_drawing_II_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, curso)

df2_by_year_semester_course_drawing_II_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, curso, periodo)

df2_by_year_semester_class_drawing_II_2 <- statistics(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], "MEDIA_FINAL", ano, periodo, turma)

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

ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], 
        aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(width = 0.1) +
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
        xlab("Curso") + ylab("Media Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I (Sem reprovados por frequencia)") +
        theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], 
        aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Ano") + ylab("Media Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I (Sem reprovados por frequencia)") +
        theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], 
        aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(periodo))) +
        geom_violin(trim = TRUE, position = dodge) + 
        geom_boxplot(width = 0.1, position = dodge) +
        geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("Ano") + ylab("Media Final") + labs(fill = "Periodo") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I (Sem reprovados por frequencia)") +
        theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], 
        aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Curso") + ylab("Media Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II (Sem reprovados por frequencia)") +
        theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], 
        aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Ano") + ylab("Media Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico II (Sem reprovados por frequencia)") +
        theme(plot.title = element_text(hjust = 0.5))

ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], 
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

dfII_without_dropout$periodo[dfII_without_dropout$periodo == "1. Semestre"] <- 1
dfII_without_dropout$periodo[dfII_without_dropout$periodo == "2. Semestre"] <- 2
dfII_without_dropout$periodo <- as.factor(dfII_without_dropout$periodo)
dfII_without_dropout$ano <- as.factor(dfII_without_dropout$ano)

res = residuals(lm(MEDIA_FINAL ~ ano * periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",]))
shapiro.test(res)
hist(res)

max(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ano, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
leveneTest(MEDIA_FINAL ~ periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
leveneTest(MEDIA_FINAL ~ ano * periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

kruskal.test(MEDIA_FINAL ~ ano, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ interaction(ano, periodo), dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

densityplot(~ MEDIA_FINAL | ano, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | ano*periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], xlab = "Media Final", ylab = "Densidade de Medias")


#multiVDA(MEDIA_FINAL ~ ano, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ SEXO, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ curso, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ano,
        data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",],
        method = "bh") 

O <- cldList(P.adj ~ Comparison,
        data = PT$res,
        threshold = 0.05,
        remove.zero = FALSE)

ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 


res = residuals(lm(MEDIA_FINAL ~ ano * periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",]))
shapiro.test(res)
hist(res)

max(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], var)$MEDIA_FINAL)

leveneTest(MEDIA_FINAL ~ ano, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])
leveneTest(MEDIA_FINAL ~ periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])
leveneTest(MEDIA_FINAL ~ ano * periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

kruskal.test(MEDIA_FINAL ~ ano, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])
kruskal.test(MEDIA_FINAL ~ periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])
kruskal.test(MEDIA_FINAL ~ interaction(ano, periodo), dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

densityplot(~ MEDIA_FINAL | ano, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], xlab = "Media Final", ylab = "Densidade de Medias")
densityplot(~ MEDIA_FINAL | ano*periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], xlab = "Media Final", ylab = "Densidade de Medias")

#multiVDA(MEDIA_FINAL ~ ano, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
multiVDA(MEDIA_FINAL ~ periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])
multiVDA(MEDIA_FINAL ~ SEXO, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])
multiVDA(MEDIA_FINAL ~ curso, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ano,
        data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",],
        method = "bh") 

O <- cldList(P.adj ~ Comparison,
        data = PT$res,
        threshold = 0.05,
        remove.zero = FALSE)

ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 