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

colnames(all_data)[10] <- "CARGA_HORARIA"
colnames(all_data)[9] <- "SITUACAO"
colnames(all_data)[5] <- "COD_DISCIPLINA"
colnames(all_data)[6] <- "DISCIPLINA"

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

all_data$COD_DISCIPLINA[is.na(all_data$COD_DISCIPLINA)]
all_data$COD_DISCIPLINA[all_data$COD_DISCIPLINA == ""]
unique(all_data$COD_DISCIPLINA)

all_data$DISCIPLINA[is.na(all_data$DISCIPLINA)]
all_data$DISCIPLINA[all_data$DISCIPLINA == ""]
unique(all_data$DISCIPLINA)

all_data$CREDITOS[is.na(all_data$CREDITOS)]
unique(all_data$CREDITOS)

all_data$MEDIA_FINAL[is.na(all_data$MEDIA_FINAL)]
all_data$MEDIA_FINAL[all_data$MEDIA_FINAL > 10 | all_data$MEDIA_FINAL < 0] 
all_data$MEDIA_FINAL[all_data$MEDIA_FINAL > 10 | all_data$MEDIA_FINAL < 0] <- 0

all_data$SITUACAO[all_data$SITUACAO == ""]
all_data$SITUACAO[is.na(all_data$SITUACAO)]
unique(all_data$SITUACAO)

all_data[is.na(all_data$CARGA_HORARIA),]
unique(all_data$CARGA_HORARIA)

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

all_data <- all_data[all_data$SITUACAO != "Dispensado sem nota",]
all_data <- all_data[!(all_data$SITUACAO == "Aproveitamento" & all_data$MEDIA_FINAL == 0),]
all_data <- all_data[(all_data$SITUACAO != "Trancamento parcial"),]
all_data <- all_data[(all_data$SITUACAO != "Matrícula"),]
all_data <- all_data[(all_data$SITUACAO != "Disciplina Não Concluída"),]

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

DT1 <- all_data[all_data$DISCIPLINA == "DESENHO TECNICO I",]
DT2 <- all_data[all_data$DISCIPLINA == "DESENHO TECNICO II",]

by_course_drawing_I <- statistics(DT1, "MEDIA_FINAL", COD_CURSO)

by_year_drawing_I <- statistics(DT1, "MEDIA_FINAL", ANO)

by_year_course_drawing_I <- statistics(DT1, "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_I <- statistics(DT1, "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)

by_course_drawing_II <- statistics(DT2, "MEDIA_FINAL", COD_CURSO)

by_year_drawing_II <- statistics(DT2, "MEDIA_FINAL", ANO)

by_year_course_drawing_II <- statistics(DT2, "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_II <- statistics(DT2, "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)

partial_data <- all_data[!(all_data$SITUACAO == "Reprovado por Frequência"),]
DT1_partial <- partial_data[partial_data$DISCIPLINA == "DESENHO TECNICO I",]
DT2_partial <- partial_data[partial_data$DISCIPLINA == "DESENHO TECNICO II",]

by_course_drawing_I_2 <- statistics(DT1_partial, "MEDIA_FINAL", COD_CURSO)

by_year_drawing_I_2 <- statistics(DT1_partial, "MEDIA_FINAL", ANO)

by_year_course_drawing_I_2 <- statistics(DT1_partial, "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_I_2 <- statistics(DT1_partial, "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)

by_course_drawing_II_2 <- statistics(DT2_partial, "MEDIA_FINAL", COD_CURSO)

by_year_drawing_II_2 <- statistics(DT2_partial, "MEDIA_FINAL", ANO)

by_year_course_drawing_II_2 <- statistics(DT2_partial, "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_II_2 <- statistics(DT2_partial, "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)

# Third Part - Dataset 1 - Data Visualization

x <- rep(1:length(unique(all_data$FORMA_EVASAO)))

boxplot(MEDIA_FINAL ~ FORMA_EVASAO,
        data = DT1,
        main = "Disciplina de Desenho Tecnico I",
        xlab = "Formas de Evasão",
        ylab = "Media Final",
        col = rainbow(length(unique(x))),
        border = "black"
)

boxplot(MEDIA_FINAL ~ FORMA_EVASAO,
        data = DT2,
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
  plot.title = element_text(family = font_family,  size = 45, 
                              hjust = 0.5, color = "#ffffff"),
  plot.background = element_rect(fill = "black"),
  panel.grid.minor.y = element_line(size =.1, color = "grey"),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(size =.1, color = "grey"),
  panel.grid.major.x = element_blank(),
  panel.background = element_rect(fill = 'black'),
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect(fill = "black"),
  legend.text = element_text(family = font_family,size = 20,color = "#cccccc"),
  text = element_text(family = font_family, color = "#cccccc", size = 22)
)

# Manually open a graphics device if you run this code in RStudio
x11(width = 13, height = 11.25)
















# Tech. Drawing I - violin plot + boxplot - by undergraduate course  
fig <- ggplot(DT1, 
        aes(x = COD_CURSO, 
            y = MEDIA_FINAL, 
            fill = COD_CURSO)) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(color = "black",
                     width = 0.1, 
                     outlier.shape = 21, 
                     outlier.size = 2, 
                     outlier.fill = "white", 
                     show.legend = FALSE) + 
        geom_segment(aes(x = 0, 
                         y = 6, 
                         xend = 7, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("\nCurso\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Curso") +
        ggtitle("\nDesenho Tecnico I\n") + 
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure1.png", type = "png", device = dev.cur())










# Tech. Drawing I - violin plot + boxplot - by year
fig <- ggplot(DT1, 
        aes(x = ANO, 
            y = MEDIA_FINAL, 
            fill = as.factor(ANO))) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black",
                     width = 0.1,
                     outlier.shape = 21, 
                     outlier.size = 2, 
                     outlier.fill = "white", 
                     show.legend = FALSE) + 
        geom_segment(aes(x = 2007, 
                         y = 6, 
                         xend = 2019, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(min(all_data$ANO), 
                                        max(all_data$ANO), 
                                        by = 1)) +
        xlab("\nAno\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Ano") + 
        ggtitle("\nDesenho Tecnico I\n") +
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure2.png", type = "png", device = dev.cur())











# Tech. Drawing I - violin plot - by semester and year
fig <- ggplot(DT1, 
        aes(x = as.factor(ANO), 
            y = MEDIA_FINAL, 
            fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE) +
        geom_segment(aes(x = 0, 
                         y = 6, 
                         xend = 13, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("\nAno\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Periodo") + 
        ggtitle("\nDesenho Tecnico I\n") +
        normal_theme +
        theme(legend.position = "bottom") 

ggbackground(fig, img)
savePlot(filename = "../images/figure3.png", type = "png", device = dev.cur())
















# Tech. Drawing II - violin plot + boxplot - by undergraduate course  
fig <- ggplot(DT2, 
        aes(x = COD_CURSO, 
            y = MEDIA_FINAL, 
            fill = COD_CURSO)) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(color = "black", 
                     width = 0.1, 
                     outlier.shape = 21, 
                     outlier.size = 2, 
                     outlier.fill = "white", 
                     show.legend = FALSE) + 
        geom_segment(aes(x = 0, 
                         y = 6, 
                         xend = 7, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("\nCurso\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Curso") + 
        ggtitle("\nDesenho Tecnico II\n") +
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure4.png", type = "png", device = dev.cur())













# Tech. Drawing II - violin plot + boxplot - by year
fig <- ggplot(DT2, 
        aes(x = ANO, 
            y = MEDIA_FINAL, 
            fill = as.factor(ANO), 
            group = ANO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black", 
                     width = 0.1, 
                     outlier.shape = 21, 
                     outlier.size = 2, 
                     outlier.fill = "white", 
                     show.legend = FALSE) + 
        geom_segment(aes(x = 2007, 
                         y = 6, 
                         xend = 2019, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
        xlab("\nAno\n") +
        ylab("\nMedia Final\n") + 
        labs(fill = "Ano") + 
        ggtitle("\nDesenho Tecnico II\n") +
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure5.png", type = "png", device = dev.cur())















# Tech. Drawing II - violin plot - by semester and year
fig <- ggplot(DT2, 
        aes(x = as.factor(ANO), 
            y = MEDIA_FINAL, 
            fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE) +
        geom_segment(aes(x = 0, 
                         y = 6, 
                         xend = 13, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("\nAno\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Periodo") + 
        ggtitle("\nDesenho Tecnico II\n") +
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure6.png", type = "png", device = dev.cur())













# Tech. Drawing I - violin plot + boxplot - by course (without who failed by attendance)
fig <- ggplot(DT1_partial, 
        aes(x = COD_CURSO, 
            y = MEDIA_FINAL, 
            fill = COD_CURSO)) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(color = "black", 
                     width = 0.1, 
                     outlier.shape = 21, 
                     outlier.size = 2, 
                     outlier.fill = "white", 
                     show.legend = FALSE) + 
        geom_segment(aes(x = 0, 
                         y = 6, 
                         xend = 7, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
        xlab("\nCurso\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Curso") + 
        ggtitle("\nMedias em Desenho Tecnico I", subtitle = "Sem reprovados por frequencia\n") +
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure7.png", type = "png", device = dev.cur())












# Tech. Drawing I - violin plot + boxplot - by year (without who failed by attendance)
fig <- ggplot(DT1_partial, 
        aes(x = ANO, 
            y = MEDIA_FINAL, 
            fill = as.factor(ANO), 
            group = ANO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black", 
                     width = 0.1, 
                     outlier.shape = 21, 
                     outlier.size = 2, 
                     outlier.fill = "white", 
                     show.legend = FALSE) + 
        geom_segment(aes(x = 2007, 
                         y = 6, 
                         xend = 2019, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
        xlab("\nAno\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Ano") + 
        ggtitle("\nMedias em Desenho Tecnico I", subtitle = "Sem reprovados por frequencia\n") +
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure8.png", type = "png", device = dev.cur())












# Tech. Drawing I - violin plot - by semester and year (without who failed by attendance)
fig <- ggplot(DT1_partial, 
        aes(x = as.factor(ANO), 
            y = MEDIA_FINAL, 
            fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE) + 
        geom_segment(aes(x = 0, 
                         y = 6, 
                         xend = 13,
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("\nAno\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Periodo") + 
        ggtitle("\nMedias em Desenho Tecnico I", subtitle = "Sem reprovados por frequencia\n") +
        normal_theme +
        theme(plot.title = element_text(hjust = 0))

ggbackground(fig, img)
savePlot(filename = "../images/figure9.png", type = "png", device = dev.cur())














# Tech. Drawing II - violin plot + boxplot- by undergraduate course 
# (without who failed by attendance)
fig <- ggplot(DT2_partial, 
        aes(x = COD_CURSO, 
            y = MEDIA_FINAL, 
            fill = COD_CURSO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black",
                     width = 0.1, 
                     outlier.shape = 21, 
                     outlier.size = 2, 
                     outlier.fill = "white", 
                     show.legend = FALSE) + 
        geom_segment(aes(x = 0, 
                         y = 6, 
                         xend = 7, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("\nCurso\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Curso") + 
        ggtitle("\nMedias em Desenho Tecnico II", subtitle = "Sem reprovados por frequencia\n") +
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure10.png", type = "png", device = dev.cur())














# Tech. Drawing II - violin plot + boxplot - by year (without who failed by attendance)
fig <- ggplot(DT2_partial, 
        aes(x = ANO, 
            y = MEDIA_FINAL, 
            fill = as.factor(ANO), 
            group = ANO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(color = "black",
                     width = 0.1, 
                     outlier.shape = 21, 
                     outlier.size = 2, 
                     outlier.fill = "white", 
                     show.legend = FALSE) + 
        geom_segment(aes(x = 2007,
                         y = 6, 
                         xend = 2019, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
        xlab("\nAno\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Ano") + 
        ggtitle("\nMedias em Desenho Tecnico II", subtitle = "Sem reprovados por frequencia\n") +
        normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure11.png", type = "png", device = dev.cur())













# Tech. Drawing II - violin plot - by semester and year (without who failed by attendance)
fig <- ggplot(DT2_partial, 
        aes(x = as.factor(ANO), 
            y = MEDIA_FINAL, 
            fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE) +
        geom_segment(aes(x = 0, 
                         y = 6, 
                         xend = 13, 
                         yend = 6), 
                     linetype = "dotted", 
                     colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("\nAno\n") + 
        ylab("\nMedia Final\n") + 
        labs(fill = "Periodo") + 
        ggtitle("\nMedias em Desenho Tecnico II", subtitle = "Sem reprovados por frequencia\n") +
        normal_theme +
        theme(plot.title = element_text(hjust = 0))

ggbackground(fig, img)
savePlot(filename = "../images/figure12.png", type = "png", device = dev.cur())









# Bar plot - who failed by attendance by sex (n and %)

by_sex <- statistics(all_data, "MEDIA_FINAL", SEXO)
by_sex_failed_by_attendance <- statistics(all_data[(all_data$SITUACAO == "Reprovado por Frequência"),], "MEDIA_FINAL", SEXO)

by_sex$per[by_sex$SEXO == "M"] <- round((by_sex_failed_by_attendance$n[by_sex_failed_by_attendance$SEXO == "M"] / by_sex$n[by_sex$SEXO == "M"]) * 100, 2)
by_sex$per[by_sex$SEXO =="F"] <- round((by_sex_failed_by_attendance$n[by_sex_failed_by_attendance$SEXO == "F"] / by_sex$n[by_sex$SEXO == "F"]) * 100, 2) 

by_sex$ndrop[by_sex$SEXO == "M"] <- round((by_sex_failed_by_attendance$n[by_sex_failed_by_attendance$SEXO == "M"]), 2)
by_sex$ndrop[by_sex$SEXO == "F"] <- round((by_sex_failed_by_attendance$n[by_sex_failed_by_attendance$SEXO == "F"]), 2) 






# Bar plot - who failed by attendance by sex (n and %)

a <- ggplot(by_sex, aes(x = SEXO, y = ndrop, fill = SEXO)) + 
        geom_chicklet(radius = grid::unit(10, 'mm')) +
        scale_fill_manual(name = "Sexo", values = c("#bc7b87","#7b87bc")) + 
        geom_text(data = by_sex, aes(label = ndrop), fontface = "bold", hjust = 0.5, 
                  vjust = 1.5, color = "white", size = 5) +
        xlab("\nSexo\n") + 
        ylab("\nNumero de Reprovados\n") + 
        ggtitle("\nReprovados por Frequencia", subtitle = "Desenho Tecnico I e II\n") +
        normal_theme + 
        theme(plot.title = element_text(size = 25)) 

b <- ggplot(data = by_sex, aes(x = SEXO, y = per, fill = SEXO)) + 
        geom_chicklet(radius = grid::unit(10, 'mm')) +
        scale_fill_manual(name = "Sexo", values = c("#bc7b87","#7b87bc")) +
        geom_text(data = by_sex, aes(label = paste(per, '%')), fontface = "bold", hjust = 0.5, 
                  vjust = 1.5, color = "white", size = 5) +
        xlab("\nSexo\n") + 
        ylab("\nPercentual de Reprovados\n") +
        ggtitle("\nReprovados por Frequencia", subtitle = "Desenho Tecnico I e II\n") +
        normal_theme + 
        theme(plot.title = element_text(size = 25))
        
c <- ggarrange(a, b, ncol = 2)

ggbackground(c, img)
savePlot(filename = "../images/figure13.png", type = "png", device = dev.cur())





# Bar plot - who failed by attendance - by sex and discipline (n and %)

frequency_failed_by_attendance_data <- all_data[(all_data$SITUACAO == "Reprovado por Frequência"),]

# by sex + course
a <- statistics(all_data, "MEDIA_FINAL", SEXO, DISCIPLINA)

# who failed by attendance - by sex + course
b <- statistics(all_data[(all_data$SITUACAO == "Reprovado por Frequência"),], "MEDIA_FINAL", SEXO, DISCIPLINA)

b$per <- round((b$n[b$SEXO == a$SEXO && b$DISCIPLINA == a$DISCIPLINA] / 
                a$n[a$SEXO == b$SEXO && a$DISCIPLINA == b$DISCIPLINA]) * 100, 2) 









# Bar plot - who failed by attendance - by sex and discipline (n and %)
fig <- ggplot(b, aes(DISCIPLINA, 
                     n, 
                     group = SEXO, 
                     color = SEXO)) + 
        geom_bar(stat = "identity", 
                 fill = "#101010", 
                 position = position_dodge2()) +
        geom_text(data = b, aes(label = paste(n,"(",per,"% )")), 
                  hjust = 0.5, 
                  vjust = 1.5, 
                  color = "white", 
                  size = 5, 
                  position = position_dodge2(width = 0.9)) +
        scale_fill_hue(c = 40) +
        scale_y_continuous(breaks = seq(0, 400, by = 50)) +
        xlab("\nDisciplina\n") + 
        ylab("\nNumero de reprovados\n") + 
        labs(fill = "Sexo") + 
        ggtitle("\nReprovados por frequencia\n\n") +
        normal_theme 

ggbackground(fig, img)
savePlot(filename = "../images/figure14.png", type = "png", device = dev.cur())









# Bar plot - who failed by attendance - by sex, discipline and year (n and %)

# by sex + course + year
a <- statistics(all_data, "MEDIA_FINAL", SEXO, DISCIPLINA, ANO)

# who failed by attendance by sex + course + year
b  <- statistics(all_data[(all_data$SITUACAO == "Reprovado por Frequência"),], "MEDIA_FINAL", SEXO, DISCIPLINA, ANO)

b$per <- round((b$n[b$SEXO == a$SEXO && b$DISCIPLINA == a$DISCIPLINA && b$ANO == a$ANO] / 
                a$n[a$SEXO == b$SEXO && a$DISCIPLINA == b$DISCIPLINA && b$ANO == a$ANO]) * 100, 2) 







# Bar plot - who failed by attendance - by sex, discipline and year (n and %)
fig1 <- ggplot(b, aes(ANO, n, color = SEXO)) + 
        geom_bar(stat = "identity", fill = "#101010", position = position_dodge2()) +
        scale_y_continuous(breaks = seq(0, 50, by = 5)) +
        scale_x_continuous(breaks = round(seq(min(b$ANO), max(b$ANO), by = 1), 1)) +
        scale_fill_hue(c = 40) +
        ylab("\nNum. de alunos\n") + xlab("\nAno\n") + labs(fill = "Sexo") + 
        ggtitle("\nReprovados por frequencia\n") +
        normal_theme +
        theme(plot.title = element_text(size = 25), axis.title = element_text(size = 18),
              axis.text = element_text(size = 12), strip.background = element_rect(fill = "#111111"), 
              strip.text = element_text(colour = 'white')) +
        facet_wrap(~DISCIPLINA) 

fig2 <- ggplot(b, aes(ANO, per, color = SEXO)) + 
        geom_bar(stat = "identity",  fill = "#101010", position = position_dodge2()) +
        scale_y_continuous(breaks = seq(0, 50, by = 5)) +
        scale_x_continuous(breaks = round(seq(min(b$ANO), max(b$ANO), by = 1), 1)) +
        scale_fill_hue(c = 40) +
        ylab("\nPercentual (%)\n") + xlab("\nAno\n") + labs(fill = "Sexo") +
        normal_theme +
        theme(plot.title = element_text(size = 25), axis.title = element_text(size = 18),
              axis.text = element_text(size = 12), strip.background = element_rect(fill = "#111111"), 
              strip.text = element_text(colour = 'white')) +
        facet_wrap(~DISCIPLINA) 

c <- ggarrange(fig1, fig2, nrow = 2)
x11(width = 15, height = 11.25)
ggbackground(c, img)
savePlot(filename = "../images/figure15.png", type = "png", device = dev.cur())







# Bar plot - who failed by attendance - by semester, year and discipline (n)

# by semester + year + course
a <- statistics(all_data, "MEDIA_FINAL", PERIODO, DISCIPLINA, ANO)

# who failed by attendance - by semester + year + course
b  <- statistics(all_data[(all_data$SITUACAO == "Reprovado por Frequência"),], "MEDIA_FINAL", PERIODO, DISCIPLINA, ANO)

b$per <- round((b$n[b$PERIODO == a$PERIODO && b$DISCIPLINA == a$DISCIPLINA && b$ANO == a$ANO] / 
                  a$n[a$PERIODO == b$PERIODO && a$DISCIPLINA == b$DISCIPLINA && b$ANO == a$ANO]) * 100, 2) 

b$DISCIPLINA[b$DISCIPLINA == "DESENHO TECNICO I"] <- "DESENHO I"
b$DISCIPLINA[b$DISCIPLINA == "DESENHO TECNICO II"] <- "DESENHO II"








# Bar plot - who failed by attendance - by semester, year and discipline (n)
fig1 <- ggplot(b, aes(DISCIPLINA, 
                      n, 
                      fill = as.factor(PERIODO),
                      color = as.factor(PERIODO))) + 
        geom_bar(stat = "identity", 
                 position = position_dodge(),
                 alpha = 0.2) +
        scale_fill_hue(c = 50) +
        ylab("\nNum. de alunos\n") + 
        xlab("Disciplina\n") + 
        labs(fill = "", 
             color = "") + 
        ggtitle("\nReprovados por frequencia\n") +
        normal_theme + 
        theme(plot.title = element_text(size = 35), 
              axis.text = element_text(size = 12),
              legend.position = "top",
              strip.background = element_rect(fill = "#111111"), 
              strip.text = element_text(colour = 'white')) +
        facet_wrap(~ANO) 

ggbackground(fig1, img)
savePlot(filename = "../images/figure16.png", type = "png", device = dev.cur())












# Bar plot - who failed by attendances - by semester, year and discipline (%)

fig1 <- ggplot(b, aes(DISCIPLINA, 
                      per, 
                      fill = as.factor(PERIODO), 
                      color = as.factor(PERIODO))) + 
        geom_bar(stat = "identity", 
                 position = position_dodge(),
                 alpha = 0.2) +
        scale_fill_hue(c = 50) +
        ylab("\nPercentual\n") + 
        xlab("Disciplina") + 
        labs(fill = "", color = "") + 
        ggtitle("\nReprovados por frequencia\n") +
        normal_theme + 
        theme(plot.title = element_text(size = 35), 
              axis.text = element_text(size = 12),
              legend.position = "top",
              strip.background = element_rect(fill = "#111111"), 
              strip.text = element_text(colour = 'white')) +
        facet_wrap(~ANO) 

ggbackground(fig1, img)
savePlot(filename = "../images/figure17.png", type = "png", device = dev.cur())
















# Bar plot - who failed by attendance - by discipline and dropout mode (n)

# who failed by attendance - by discipline + dropout mode
b  <- statistics(all_data[(all_data$SITUACAO == "Reprovado por Frequência"),], "MEDIA_FINAL", DISCIPLINA, FORMA_EVASAO)

b$FORMA_EVASAO[b$FORMA_EVASAO == "Reopção"] <- "Reopcao"
b$FORMA_EVASAO[b$FORMA_EVASAO == "Transferência"] <- "Transferencia"










# Bar plot - who failed by attendance - by discipline and dropout mode (n)
fig1 <- ggplot(b, aes(DISCIPLINA, 
                      n, 
                      color = as.factor(FORMA_EVASAO),
                      fill = as.factor(FORMA_EVASAO))) + 
        geom_bar(stat = "identity", 
                 width = 0.5, 
                 alpha = 0.2,
                 position = position_dodge()) +
        scale_fill_hue(c = 50) + 
        ylab("\nNum. de alunos\n") + 
        labs(color = "Situacao atual",
             fill = "Situacao atual")  + 
        xlab("\nDisciplina\n") + 
        ggtitle("\nReprovados por frequencia por situacao\n") +
        normal_theme +
        theme(plot.title = element_text(size = 35)) 
       
ggbackground(fig1, img)
savePlot(filename = "../images/figure18.png", type = "png", device = dev.cur())








# Bar plot - all students - by discipline, current situation and year (n)

# by discipline + current situation + year
b <- statistics(all_data, "MEDIA_FINAL", DISCIPLINA, SITUACAO, ANO)

b$DISCIPLINA[b$DISCIPLINA == "DESENHO TECNICO I"] <- "DESENHO I"
b$DISCIPLINA[b$DISCIPLINA == "DESENHO TECNICO II"] <- "DESENHO II"
b$SITUACAO[b$SITUACAO == "Reprovado por Frequência"] <- "Reprovado por Frequencia"






# Bar plot - all students - by discipline, current situation and year (n)
fig1 <- ggplot(b, aes(DISCIPLINA, 
                      n, 
                      fill = as.factor(SITUACAO),
                      color = as.factor(SITUACAO))) + 
        geom_bar(stat = "identity", 
                 width = 0.5, 
                 alpha = 0.2,
                 position = position_dodge()) +
        scale_fill_hue(c = 50) + 
        ylab("\nNum. de alunos\n") + 
        labs(color = "Situacao", 
             fill = "Situacao") + 
        xlab("\nDisciplina") + 
        ggtitle("\nAlunos por Ano, Disciplina e Situacao\n") +
        guides(fill = guide_legend(nrow = 3)) +
        normal_theme + 
        theme(plot.title = element_text(size = 30), 
              axis.text = element_text(size = 10), 
              legend.position = "top",
              strip.background = element_rect(fill = "#111111"), 
              strip.text = element_text(colour = 'white')) +
        facet_wrap(~ANO, nrow = 3) 

ggbackground(fig1, img)
savePlot(filename = "../images/figure19.png", type = "png", device = dev.cur())





# Line plot - all students - by current situation, discipline and year (n)
fig1 <- ggplot(b, aes(ANO, 
                      n, 
                      group = SITUACAO, 
                      color = SITUACAO)) + 
        geom_line(size = 1.5) +
        geom_point(size = 3, 
                   shape = 21, 
                   fill = "white") +
        scale_x_continuous(breaks = round(seq(min(b$ANO),
                                              max(b$ANO), 
                                              by = 1), 1)) +
        scale_y_continuous(breaks = round(seq(0, 
                                              max(b$n), 
                                              by = 10), 1)) +
        ylab("\nNum. de Alunos\n") + 
        xlab("\nAno") +
        labs(color = "Situacao do Aluno") + 
        ggtitle("\nAlunos por Ano, Disciplina e Situacao\n") +
        guides(color = guide_legend(nrow = 3)) +
        normal_theme +  
        theme(plot.title = element_text(size = 25), 
              axis.text = element_text(size = 12), 
              legend.position = "bottom",
              strip.background = element_rect(fill = "#111111"), 
              strip.text = element_text(colour = 'white')) +
        facet_wrap(~DISCIPLINA, nrow = 2) 

ggbackground(fig1, img)
savePlot(filename = "../images/figure20.png", type = "png", device = dev.cur())




# Line plot - all students - by current situation, discipline and year (%)

b <- as.data.frame(b %>% 
        group_by(ANO, DISCIPLINA) %>% 
        mutate(percent = 100*(n/sum(n))))






# Line plot - all students - by current situation, discipline and year (%)
fig1 <- ggplot(b, aes(ANO, 
                      percent, 
                      group = SITUACAO, 
                      color = SITUACAO)) + 
        geom_line(size = 1.5) +
        geom_point(size = 3, 
                   shape = 21, 
                   fill = "white") +
        scale_x_continuous(breaks = round(seq(min(b$ANO), 
                                              max(b$ANO), 
                                              by = 1),1)) +
        scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
        ylab("\nPercentual de Alunos (%)\n") + 
        xlab("\nAno") +
        labs(color = "Situacao do Aluno") + 
        ggtitle("\n% de Alunos por Situacao em cada Disciplina e Ano") +
        guides(color = guide_legend(nrow = 3)) +
        normal_theme +  
        theme(plot.title = element_text(size = 25), 
              axis.text = element_text(size = 12), 
              legend.position = "bottom", 
              legend.text = element_text(size = 14),
              strip.background = element_rect(fill = "#111111"), 
              strip.text = element_text(colour = 'white')) +
        facet_wrap(~DISCIPLINA, nrow = 2) 

ggbackground(fig1, img)
savePlot(filename = "../images/figure21.png", type = "png", device = dev.cur())











# Density curve plot - without failed by attendance - grade by semester and year - technical drawing I

fig1 <- ggplot(DT1_partial, 
        aes(x = MEDIA_FINAL , 
            fill = as.factor(ANO), 
            colour = PERIODO)) +
        geom_density(alpha = 0.9, 
                     size = 1) + 
        scale_x_continuous(breaks = round(seq(min(partial_data$MEDIA_FINAL), 
                                              max(partial_data$MEDIA_FINAL), 
                                              by = 5), 10)) +
        xlab("\nMedia Final") + 
        ylab("\nDensidade\n") + 
        labs(fill = "Ano", 
             colour = "Semestre") +
        ggtitle("\nDistribuicao das Medias em Desenho I (Sem rep. por frequencia)\n") +
        normal_theme +       
        theme(strip.background = element_blank(), 
              strip.text = element_blank(), 
              axis.title = element_text(size = 18), 
              plot.title = element_text(size = 18), 
              axis.text = element_text(size = 15), 
              legend.position = "bottom", 
              legend.text = element_text(size = 15)) +
        facet_wrap(ANO ~ PERIODO, nrow = 5)

ggbackground(fig1, img)
savePlot(filename = "../images/figure22.png", type = "png", device = dev.cur())










# Density curve plot - without failed by attendance - grade by semester and year - technical drawing II

fig1 <- ggplot(DT2_partial, 
        aes(x = MEDIA_FINAL , 
            fill = as.factor(ANO), 
            colour = PERIODO)) +
        geom_density(alpha = 0.9, 
                     size = 1) +
        scale_x_continuous(breaks = round(seq(min(partial_data$MEDIA_FINAL), 
                                              max(partial_data$MEDIA_FINAL), 
                                              by = 5), 10)) +
        xlab("\nMedia Final") + 
        ylab("\nDensidade") + 
        labs(fill = "Ano", 
             colour = "Semestre") +
        ggtitle("\nDistribuicao das Medias em Desenho II (Sem rep. por frequencia)\n") +
        normal_theme +       
        theme(strip.background = element_blank(), 
              strip.text = element_blank(), 
              axis.title = element_text(size = 18), 
              plot.title = element_text(size = 18), 
              axis.text = element_text(size = 15), 
              legend.position = "bottom", 
              legend.text = element_text(size = 15)) +
        facet_wrap(ANO ~ PERIODO, nrow = 5)

ggbackground(fig1, img)
savePlot(filename = "../images/figure23.png", type = "png", device = dev.cur())





# Density plot - without failed by attendance - grade by year - technical drawing I + II
fig1 <- ggplot(partial_data,
        aes(x = ANO, y = MEDIA_FINAL)) +
        ggtitle("\nDensidade das Medias por Ano (Sem reprovados por frequencia)\n") +
        ylab("\nMedia Final\n") + xlab("\nAno") + 
        labs(fill = "Densidade") +      
        stat_density2d(aes(fill = ..density..), 
                       contour = F, 
                       geom = 'tile') +
      scale_fill_gradient(low = "black", high = "white") +
        geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), 
                     linetype = "dotted", colour = "black") +
        geom_segment(aes(x = 2007, y = 7, xend = 2019, yend = 7), 
                     linetype = "dotted", colour = "black") +
        geom_segment(aes(x = 2016, y = 0, xend = 2016, yend = 10), 
                     linetype = "dotted", colour = "black") +
        geom_segment(aes(x = 2018, y = 0, xend = 2018, yend = 10), 
                     linetype = "dotted", colour = "black") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(2007, 2019, by = 1)) +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 10, by = 1)) +
        normal_theme +       
        theme(axis.title = element_text(size = 18), 
              axis.text = element_text(size = 15), 
              panel.spacing = unit(2, "lines"), 
              plot.title = element_text(hjust = 0.5, size = 18), 
              axis.text.x = element_text(face = "bold", size = 8), 
              legend.key.width = unit(2, "cm"), 
              legend.position = "bottom", legend.text = element_text(size = 11)) +
        facet_wrap(~DISCIPLINA)

ggbackground(fig1, img)
savePlot(filename = "../images/figure24.png", type = "png", device = dev.cur())









# Fourth Part - Dataset 1 - Advanced Statistics

all_data$PERIODO[all_data$PERIODO == "1. Semestre"] <- 1
all_data$PERIODO[all_data$PERIODO == "2. Semestre"] <- 2
all_data$PERIODO <- as.factor(all_data$PERIODO)
all_data$ANO <- as.factor(all_data$ANO)
DT1 <- all_data[all_data$DISCIPLINA == "DESENHO TECNICO I",]

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
savePlot(filename = "../images/figure25.png", type = "png", device = dev.cur())

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
        scale_x_continuous(breaks = round(seq(min(DT1$MEDIA_FINAL), 
                                              max(DT1$MEDIA_FINAL), 
                                              by = 5), 10)) +
        xlab("\nDistribuicao de Medias") + 
        ylab("\nDensidade de Medias\n") + 
        labs(fill = "Semestre", color = "Semestre") +
        ggtitle("\nDistribuicao das Medias em Desenho Tecnico I\n") +
        normal_theme +       
        theme(axis.title = element_text(size = 25), 
              plot.title = element_text(size = 30), 
              axis.text = element_text(size = 15), 
              legend.position = "bottom", 
              legend.text = element_text(size = 18)) 

ggbackground(fig1, img)
savePlot(filename = "../images/figure26.png", type = "png", device = dev.cur())


fig1 <- ggplot(DT1, aes(x = MEDIA_FINAL , fill = ANO, color = ANO)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT1$MEDIA_FINAL), 
                                        max(DT1$MEDIA_FINAL), 
                                        by = 5), 10)) +
  xlab("\nDistribuicao de Medias") + 
  ylab("\nDensidade de Medias\n") + 
  labs(fill = "Ano", color = "Ano") +
  ggtitle("\nDistribuicao das Medias em Desenho Tecnico I\n") +
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
savePlot(filename = "../images/figure27.png", type = "png", device = dev.cur())


fig1 <- ggplot(DT1, aes(x = MEDIA_FINAL , fill = ANO, color = PERIODO)) +
  geom_density(alpha = 0.7, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT1$MEDIA_FINAL), 
                                        max(DT1$MEDIA_FINAL), 
                                        by = 5), 10)) +
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
savePlot(filename = "../images/figure28.png", type = "png", device = dev.cur())


multiVDA(MEDIA_FINAL ~ ANO, data = DT1)
multiVDA(MEDIA_FINAL ~ PERIODO, data = DT1)
multiVDA(MEDIA_FINAL ~ SEX, data = DT1)
multiVDA(MEDIA_FINAL ~ COD_CURSO, data = DT1)

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ANO,
          data = DT1,
          method = "bh") 

O <- cldList(P.adj ~ Comparison,
          data = PT$res,
          threshold = 0.05,
          remove.zero = FALSE)

fig <- ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) +
  xlab("\nGrupo\n") +
  ylab("\nAno\n") +
  labs(fill = "Grupo", color = "Grupo") +
  geom_point(size = 4) +
  normal_theme

ggbackground(fig, img)
savePlot(filename = "../images/figure29.png", type = "png", device = dev.cur())
  
merged_data <- transform(DT1, ANO_PERIODO = paste(ANO, '.', PERIODO))

PT = dunnTest(MEDIA_FINAL ~ ANO_PERIODO,
              data = merged_data,
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
             data = PT$res,
             threshold = 0.05,
             remove.zero = FALSE)

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
savePlot(filename = "../images/figure30.png", type = "png", device = dev.cur())



# Technical Drawing II
DT2 <- all_data[all_data$DISCIPLINA == "DESENHO TECNICO II",]

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
savePlot(filename = "../images/figure31.png", type = "png", device = dev.cur())

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
  scale_x_continuous(breaks = round(seq(min(DT2$MEDIA_FINAL), 
                                        max(DT2$MEDIA_FINAL), 
                                        by = 5), 10)) +
  xlab("\nDistribuicao de Medias") + 
  ylab("\nDensidade de Medias\n") + 
  labs(fill = "Semestre", color = "Semestre") +
  ggtitle("\nDistribuicao das Medias em Desenho Tecnico II\n") +
  normal_theme +       
  theme(axis.title = element_text(size = 25), 
        plot.title = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        legend.position = "bottom", 
        legend.text = element_text(size = 18)) 

ggbackground(fig1, img)
savePlot(filename = "../images/figure32.png", type = "png", device = dev.cur())


fig1 <- ggplot(DT2, aes(x = MEDIA_FINAL , fill = ANO, color = ANO)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT2$MEDIA_FINAL), 
                                        max(DT2$MEDIA_FINAL), 
                                        by = 5), 10)) +
  xlab("\nDistribuicao de Medias") + 
  ylab("\nDensidade de Medias\n") + 
  labs(fill = "Semestre", color = "Semestre") +
  ggtitle("\nDistribuicao das Medias em Desenho Tecnico II\n") +
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
savePlot(filename = "../images/figure33.png", type = "png", device = dev.cur())


fig1 <- ggplot(DT2, aes(x = MEDIA_FINAL , fill = ANO, color = PERIODO)) +
  geom_density(alpha = 0.7, size = 1) +
  geom_vline(aes(xintercept = 6), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(DT2$MEDIA_FINAL), 
                                        max(DT2$MEDIA_FINAL), 
                                        by = 5), 10)) +
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
savePlot(filename = "../images/figure34.png", type = "png", device = dev.cur())

multiVDA(MEDIA_FINAL ~ ANO, data = DT2)
multiVDA(MEDIA_FINAL ~ PERIODO, data = DT2)
multiVDA(MEDIA_FINAL ~ SEX, data = DT2)
multiVDA(MEDIA_FINAL ~ COD_CURSO, data = DT2)

merged_data <- transform(DT2, ANO_PERIODO = paste(ANO, '.', PERIODO))

PT = dunnTest(MEDIA_FINAL ~ ANO_PERIODO,
              data = merged_data,
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
             data = PT$res,
             threshold = 0.05,
             remove.zero = FALSE)

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
savePlot(filename = "../images/figure35.png", type = "png", device = dev.cur())

















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