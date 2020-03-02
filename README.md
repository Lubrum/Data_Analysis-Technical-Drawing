# Summary 

In this project, I will explore some data about Technical Drawing disciplines, from a Brazilian University. I will show all the steps we must do to perform an apropriate data analysis to extract useful insights from the data we have.

# What is data about?

The data is about students from Technical Drawing disciplines from a Brazilian University. 

# What data do I have?

Two different datasets were provided:

1- Student data separated by undergraduating course, from all professors (without the information about who ministered the discipline).

2- Student data from one specific professor from the discipline.

We have the following variables/columns for the dataset 1 by student:
- Student Name;
- Student ID;
- Course Code;
- Year;
- Semester;
- Discipline Code;
- Discipline Name;
- Workload by Week in Hours;
- Final Grade;
- Student Situation;
- Course - Student ID;
- Total Workload in Hours;
- Type of University Admission;
- Year of Admission;
- Evasion Type (Including Enrolled Students);
- Evasion Year (If Applicable);
- Sex;

We have the following variables/columns for the dataset 2 by student:
- Discipline Code;
- Discipline Name;
- Professor Name;
- Year and Period;
- Class Code;
- Total Workload in Hours;
- Theoretical Workload by Week in Hours;
- Practical Workload by Week in Hours;
- Semipresential Workload by Week in Hours;
- Other Workloads by Week in Hours;
- Student ID;
- Course Code;
- Student Name;
- Student Absences;
- Final Grade;
- Student Situation;

# First Part - Dataset 1 - Sensible Information
 
All sensible information from the datasets were removed first, like students names. The students identification were encrypted to preserve the university sensible data. 

# Second Part - Dataset 1 - Loading and Processing

The first thing to do is to install/load the packages that will be used.

```R
if (!require(xlsx)) install.packages("xlsx")
library(xlsx)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if(!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)

if(!require(viridis)) install.packages("viridis")
library(viridis)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(matlab)) install.packages("matlab")
library(matlab)

if (!require(car)) install.packages("car")
library(car)

if (!require(lattice)) install.packages("lattice")
library(lattice)

if (!require(FSA))install.packages("FSA")
library(FSA)

if (!require(rcompanion)) install.packages("rcompanion")
library(rcompanion)

if (!require(coin)) install.packages("coin")
library(coin)

if (!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)

if (!require(grid)) install.packages("grid")
library(grid)

if (!require(reshape)) install.packages("reshape")
library(reshape)

if (!require(extrafont)) install.packages("extrafont")
library(extrafont)

if (!require(viridis)) install.packages("viridis")
library(viridis)

if (!require(nortest)) install.packages("nortest")
library(nortest)

if (!require(cowplot)) install.packages("cowplot")
library(cowplot)

if (!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)

if (!require(ggExtra)) install.packages("ggExtra")
library(ggExtra)
```

Now the dataset 1 is loaded, it is separated by each course. There are the following courses:
- BAEA: Food Engineering
- BAEE: Renewable Energy Engineering
- BAEC: Computer Engineering
- BAEP: Production Engineering
- BAEQ: Chemical Engineering
- BALF: Physics Degree

This step can be called **data loading**.

```R
BAEA <- read.csv("csv/Technical_Drawing_I_II - BAEA.csv", sep=";", stringsAsFactors=FALSE, encoding = "Latin-1")
BAEE <- read.csv("csv/Technical_Drawing_I_II - BAEE.csv", sep=";", stringsAsFactors=FALSE, encoding = "Latin-1")
BAEC <- read.csv("csv/Technical_Drawing_I_II - BAEC.csv", sep=";", stringsAsFactors=FALSE, encoding = "Latin-1")
BAEP <- read.csv("csv/Technical_Drawing_I_II - BAEP.csv", sep=";", stringsAsFactors=FALSE, encoding = "Latin-1")
BAEQ <- read.csv("csv/Technical_Drawing_I_II - BAEQ.csv", sep=";", stringsAsFactors=FALSE, encoding = "Latin-1")
BALF <- read.csv("csv/Technical_Drawing_I_II - BALF.csv", sep=";", stringsAsFactors=FALSE, encoding = "Latin-1")
```

It is possible to see that BAEA dataset has one extra column compared to the others, so I removed that column (column 11). I also changed the name of column 11 to the same name of the related column in other datasets.

```R
BAEA <- BAEA[,-10]
colnames(BAEC)[10] <- "TOTAL_CARGA_HORARIA"
```

All data from each dataset/dataframe is joined to just one variable (in dataframe format).

```R
all_data <- rbind(BAEA, BAEE)
all_data <- rbind(all_data, BAEC)
all_data <- rbind(all_data, BAEP)
all_data <- rbind(all_data, BAEQ)
all_data <- rbind(all_data, BALF)
```

I removed the columns that have useless or redundant information about the students.
Now all columns are checked, looking for errors, inconsistent values, missing values and so on. This step is called **data cleaning**.

```R
all_data$NOME_PESSOA[is.na(all_data$NOME_PESSOA)]
all_data$NOME_PESSOA[all_data$NOME_PESSOA == ""]

all_data$MATR_ALUNO[is.na(all_data$MATR_ALUNO)]
all_data$MATR_ALUNO[all_data$NOME_PESSOA == ""]

all_data$COD_CURSO[is.na(all_data$COD_CURSO)]
all_data$COD_CURSO[all_data$COD_CURSO == ""]
unique(all_data$COD_CURSO)

all_data$ANO[is.na(all_data$ANO)]
all_data$ANO <- as.numeric(all_data$ANO)
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

all_data$CREDITOS <- as.numeric(all_data$CREDITOS)
all_data$CREDITOS[is.na(all_data$CREDITOS)]
unique(all_data$CREDITOS)

all_data$MEDIA_FINAL <- as.numeric(all_data$MEDIA_FINAL)
all_data$MEDIA_FINAL[is.na(all_data$MEDIA_FINAL)]
all_data$MEDIA_FINAL[all_data$MEDIA_FINAL > 10 | all_data$MEDIA_FINAL < 0] 
all_data$MEDIA_FINAL[all_data$MEDIA_FINAL > 10 | all_data$MEDIA_FINAL < 0] <- 0

all_data$DESCR_SITUACAO[all_data$DESCR_SITUACAO == ""]
all_data$DESCR_SITUACAO[is.na(all_data$DESCR_SITUACAO)]
unique(all_data$DESCR_SITUACAO)

all_data$TOTAL_CARGA_HORARIA <- as.numeric(all_data$TOTAL_CARGA_HORARIA)
all_data[is.na(all_data$TOTAL_CARGA_HORARIA),]
all_data$TOTAL_CARGA_HORARIA[is.na(all_data$TOTAL_CARGA_HORARIA)] <- 60
unique(all_data$TOTAL_CARGA_HORARIA)

all_data$FORMA_INGRESSO[all_data$FORMA_INGRESSO == ""]
all_data$FORMA_INGRESSO[is.na(all_data$FORMA_INGRESSO)]
unique(all_data$FORMA_INGRESSO)

all_data$ANO_INGRESSO <- as.numeric(all_data$ANO_INGRESSO)
all_data$ANO_INGRESSO[is.na(all_data$ANO_INGRESSO)]
unique(all_data$ANO_INGRESSO)

all_data$FORMA_EVASAO[all_data$FORMA_EVASAO == ""]
all_data$FORMA_EVASAO[is.na(all_data$FORMA_EVASAO)]
unique(all_data$FORMA_EVASAO)

all_data$ANO_EVASAO <- as.numeric(all_data$ANO_EVASAO)
all_data$ANO_EVASAO[is.na(all_data$ANO_EVASAO)]
all_data$ANO_EVASAO[is.na(all_data$ANO_EVASAO)] <- 0
unique(all_data$ANO_EVASAO)
```

Now I need to keep only valid students data. For that, I analyse the field "Situation", keeping only Approved and Disapproved students. Registers in situations like "enrollment canceled" or "enrolled students" were removed. There are some exceptional Approval cases ("Aproveitamento") where a final grade is available and in some cases is not. I keep the cases where a final grade is available, because it is a case where a student did the discipline in an engineering course distinct of its own, so this information is valid.

```R
all_data <- all_data[all_data$DESCR_SITUACAO != "Dispensado sem nota",]
all_data <- all_data[!(all_data$DESCR_SITUACAO == "Aproveitamento" & all_data$MEDIA_FINAL == 0),]
all_data <- all_data[!(all_data$DESCR_SITUACAO == "Trancamento parcial"),]
all_data <- all_data[!(all_data$DESCR_SITUACAO == "Matrícula"),]
all_data <- all_data[!(all_data$DESCR_SITUACAO == "Disciplina Não Concluída"),]
```

I will also do a change in the dropout way column ("FORMA_EVASAO"), allowing us to visualize the information in the graphics next. 

```R
unique(all_data$FORMA_EVASAO)
all_data$FORMA_EVASAO[(all_data$FORMA_EVASAO == "Transf. Interna Por Reopção de Curso")] <- "Reopção"
all_data$FORMA_EVASAO[(all_data$FORMA_EVASAO == "Transferência Interna")] <- "Transf. Interna"
```

# Third Part - Dataset 1 - Descriptive Statistics

Now I begin to use descriptive statistics to better know our dataset. Our analysis focus only the student final grade as a dependent variable, being affected by all other information available. 
I will separate the analysis by discipline. There are the following cases: analysis by course; by year; by course and year; by course, year and semester.
I calculated the following information for each case: mean, median, standart deviation, min and max value, number of samples, first and third quartil and interquartil range. For that, I created the function "statistics".

```R
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
```

How this last data table looks?

![Alt text](images/image1.png?raw=true "image")

```R
by_course_drawing_II <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", COD_CURSO)

by_year_drawing_II <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO)

by_year_course_drawing_II <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_II <- statistics(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)
```

How this last data table looks?

![Alt text](images/image2.png?raw=true "image")

Now I perform the same analysis but without the students that dropout.

```R
all_data_without_frequency_dropout <- all_data[!(all_data$DESCR_SITUACAO == "Reprovado por Frequência"),]

by_course_drawing_I_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", COD_CURSO)

by_year_drawing_I_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", ANO)

by_year_course_drawing_I_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_I_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)
```

How this last data table looks?

![Alt text](images/image3.png?raw=true "image")

```R
by_course_drawing_II_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", COD_CURSO)

by_year_drawing_II_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO)

by_year_course_drawing_II_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO, COD_CURSO)

by_year_semester_course_drawing_II_2 <- statistics(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], "MEDIA_FINAL", ANO, COD_CURSO, PERIODO)
```

How this last data table looks?

![Alt text](images/image4.png?raw=true "image")


# Fourth Part - Dataset 1 - Data Visualization

I will create separated boxplots and violin plots analysing students final grade for each discipline and for each possible dimension (years, semesters, courses and course dropout type), considering all samples and samples without the dropout students.

```R
# Is there a relation between final grade and way the student left university? (Technical Drawing I)
x <- rep(1:length(unique(all_data$FORMA_EVASAO)))
boxplot(MEDIA_FINAL~FORMA_EVASAO,
        data=all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",],
        main="Disciplina de Desenho Técnico I",
        xlab="Formas de Evasão",
        ylab="Média Final",
        col=rainbow(length(unique(x))),
        border="black"
)
```

![Alt text](images/image5.png?raw=true "image")

```R
# Is there a relation between final grade and way the student left university? (Technical Drawing II)
boxplot(MEDIA_FINAL~FORMA_EVASAO,
        data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",],
        main = "Disciplina de Desenho Técnico II",
        xlab = "Formas de Evasão",
        ylab = "Média Final",
        col = rainbow(length(unique(x))),
        border = "black"
)
```

![Alt text](images/image6.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the graduation courses? (Technical Drawing I)
ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
       aes(x = COD_CURSO, y = MEDIA_FINAL, fill = COD_CURSO)) +
       geom_violin(trim = TRUE) + 
       geom_boxplot(width = 0.1) + 
       geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
       scale_y_continuous(breaks = seq(0, 10, by = 1)) +
       xlab("Curso") + ylab("Media Final") + labs(fill = "Curso") + ggtitle("Desenho Tecnico I") +
       theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image7.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the years? (Technical Drawing I)
ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
       aes(x = ANO, y = MEDIA_FINAL, fill = as.factor(ANO), group = ANO)) +
       geom_violin(trim = TRUE)+ 
       geom_boxplot(width = 0.1) + 
       geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), linetype = "dotted", colour = "red") +
       scale_y_continuous(breaks = seq(0, 10, by = 1)) +
       xlab("Ano") + ylab("Média Final") + labs(fill = "Ano") + ggtitle("Desenho Técnico I") +
       theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image8.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the years and semesters? (Technical Drawing I)      
ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
       aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
       geom_violin(trim = TRUE) +
       geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
       scale_y_continuous(breaks = seq(0, 10, by = 1)) +
       theme(legend.position = "bottom") +
       xlab("ANO") + ylab("Média Final") + labs(fill = "Período") + ggtitle("Desenho Técnico I") +
       theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image9.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the graduation courses? (Technical Drawing II)
ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = COD_CURSO, y = MEDIA_FINAL, fill = COD_CURSO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Curso") + ylab("Média Final") + labs(fill = "Curso") + ggtitle("Desenho Técnico II") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image10.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the years? (Technical Drawing II)
ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = ANO, y = MEDIA_FINAL, fill = as.factor(ANO), group = ANO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
        xlab("Ano") + ylab("Média Final") + labs(fill = "Ano") + ggtitle("Desenho Técnico II") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image11.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the years and semesters? (Technical Drawing II)  
ggplot(all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
         aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
         geom_violin(trim = TRUE) +
         geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
         scale_y_continuous(breaks = seq(0, 10, by = 1)) +
         theme(legend.position = "bottom") +
         xlab("Ano") + ylab("Média Final") + labs(fill = "Período") + ggtitle("Desenho Técnico II") +
         theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image12.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the graduation courses? (Technical Drawing I - without dropout students)
ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = COD_CURSO, y = MEDIA_FINAL, fill = COD_CURSO)) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(width = 0.1) +
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
        xlab("Curso") + ylab("Média Final") + labs(fill = "Curso") + ggtitle("Médias em Desenho Técnico I (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image13.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the years? (Technical Drawing I - without dropout students)
ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = ANO, y = MEDIA_FINAL, fill = as.factor(ANO), group = ANO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
        xlab("Ano") + ylab("Média Final") + labs(fill = "Ano") + ggtitle("Médias em Desenho Técnico I (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image14.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the years and semesters? (Technical Drawing I - without dropout students)
ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE, position = dodge) + 
        geom_boxplot(width = 0.1, position = dodge) +
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("ANO") + ylab("Média Final") + labs(fill = "Período") + ggtitle("Médias em Desenho Técnico I (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image15.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the graduation courses? (Technical Drawing II - without dropout students)
ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = COD_CURSO, y = MEDIA_FINAL, fill = COD_CURSO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Curso") + ylab("Média Final") + labs(fill = "Curso") + ggtitle("Médias em Desenho Técnico II (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image16.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the years? (Technical Drawing II - without dropout students)
ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = ANO, y = MEDIA_FINAL, fill = as.factor(ANO), group = ANO)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 2007, y = 6, xend = 2019, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
        xlab("Ano") + ylab("Média Final") + labs(fill = "Ano") + ggtitle("Médias em Desenho Técnico II (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image17.png?raw=true "image")

```R
# Are there relevant differences in the final grade between the years and semesters? (Technical Drawing II - without dropout students)
dodge <- position_dodge(width = 0.8)
ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = as.factor(ANO), y = MEDIA_FINAL, fill = as.factor(PERIODO))) +
        geom_violin(trim = TRUE)+
        geom_boxplot(width = 0.2, position = dodge) + 
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("ANO") + ylab("Média Final") + labs(fill = "Período") + ggtitle("Médias em Desenho Técnico II (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image18.png?raw=true "image")


Now I analyse specifically the dropout cases.

```R
frequency_dropout_data <- all_data[(all_data$DESCR_SITUACAO == "Reprovado por Frequência"),]

## Who abandons the disciplines of Technical Drawing I and II the most?
ggplot(frequency_dropout_data, 
        aes(SEXO, fill=as.factor(SEXO))) + 
        geom_bar() +
        scale_fill_hue(c = 40) +
        xlab("Sexo") + ylab("Número de Reprovações") + labs(fill = "Sexo") + ggtitle("Reprovações por Frequência (Desenho Técnico I e II)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image19.png?raw=true "image")

```R
## how much students abandons by discipline and sex?
aux <- frequency_dropout_data %>%
        group_by(SEXO,NOME_ATIV_CURRIC) %>%
        tally()

ggplot(aux, aes(NOME_ATIV_CURRIC, n, group = SEXO, fill=as.factor(SEXO))) + 
        geom_bar(stat="identity", color="black", position=position_dodge()) +
        scale_fill_hue(c = 40) +
        scale_y_continuous(breaks = seq(0, 400, by = 50)) +
        xlab("Disciplina") + ylab("Número de Reprovações") + labs(fill = "Sexo") + ggtitle("Reprovações por Frequência") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image20.png?raw=true "image")

```R
## how much students abandons by discipline, sex, and year?
aux <- frequency_dropout_data %>%
        group_by(SEXO,NOME_ATIV_CURRIC,ANO) %>%
        tally()

ggplot(aux, aes(ANO, n, fill=as.factor(SEXO))) + 
        geom_bar(stat="identity", color="black", position=position_dodge()) +
        scale_fill_hue(c = 40) +
        ylab("Frequência") + labs(fill = "Sexo") + ggtitle("Reprovações por Frequência") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(breaks = seq(0, 50, by = 5)) +
        scale_x_continuous(breaks = round(seq(min(aux$ANO), max(aux$ANO), by = 1),1)) +
        facet_wrap(~NOME_ATIV_CURRIC) 
```

![Alt text](images/image21.png?raw=true "image")

```R
## how much students abandons by discipline, semester, and year?
aux <- frequency_dropout_data %>%
        group_by(NOME_ATIV_CURRIC,PERIODO,ANO) %>%
        tally()
  
ggplot(aux, aes(NOME_ATIV_CURRIC, n, fill=as.factor(PERIODO))) + 
        geom_bar(stat="identity", color="black", position=position_dodge()) +
        scale_fill_hue(c = 50) +
        ylab("Frequência") + xlab("Discipline") + labs(fill = "Disciplina") + ggtitle("Reprovações por Frequência") +
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 8)) +
        facet_wrap(~ANO) 
```

![Alt text](images/image22.png?raw=true "image")

```R
## What is the evasion type of dropout students by year?
aux <- frequency_dropout_data %>%
  group_by(NOME_ATIV_CURRIC,FORMA_EVASAO) %>%
  tally()

ggplot(aux, aes(NOME_ATIV_CURRIC, n, fill=as.factor(FORMA_EVASAO))) + 
        geom_bar(stat="identity", width = 0.5, color = "black", position=position_dodge()) +
        scale_fill_hue(c = 50) +
        ylab("Frequência") + labs(fill = "Tipo de Evasão")  + xlab("Disciplina") + ggtitle("Situação de Alunos Reprovados por Frequência") 
```

![Alt text](images/image23.png?raw=true "image")

```R
# Comparing Approved and Disapproved Students
aux <- all_data %>%
        group_by(NOME_ATIV_CURRIC,DESCR_SITUACAO,ANO) %>%
        tally()

ggplot(aux, aes(NOME_ATIV_CURRIC, n, fill=as.factor(DESCR_SITUACAO))) + 
        geom_bar(stat = "identity", width = 0.5, color = "black", position = position_dodge()) +
        scale_fill_hue(c = 50) +
        ylab("Frequência") + labs(fill = "Disciplina") + xlab("Disciplina") + ggtitle("Aprovações e Reprovações por Ano e Disciplina") +
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 7)) +
        facet_wrap(~ANO) 
```

![Alt text](images/image24.png?raw=true "image")

```R
# Comparing Approved and Disapproved Students (with lines)
aux <- as.data.frame(all_data %>%
        group_by(ANO,DESCR_SITUACAO,NOME_ATIV_CURRIC) %>%
        tally())

ggplot(aux, aes(ANO, n, group = DESCR_SITUACAO, color = DESCR_SITUACAO)) + 
        geom_line(size = 1.5) +
        geom_point(size = 3, shape = 21, fill = "white") +
        scale_x_continuous(breaks = round(seq(min(aux$ANO), max(aux$ANO), by = 1),1)) +
        scale_y_continuous(breaks = round(seq(0, max(aux$n), by = 10),1)) +
        ylab("Frequência") + labs(color = "Situação do Aluno") + ggtitle("Aprovações e Reprovações por Ano e Disciplina") +
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 8)) +
        facet_wrap(~NOME_ATIV_CURRIC) 
```

![Alt text](images/image25.png?raw=true "image")

```R
# Comparing Approved and Disapproved Students (percentual)
aux <- as.data.frame(all_data %>%
        group_by(ANO,DESCR_SITUACAO,NOME_ATIV_CURRIC) %>%
        tally())
  
aux <- as.data.frame(aux %>% 
        group_by(ANO, NOME_ATIV_CURRIC) %>% 
        mutate(percent = 100*(n/sum(n))))

ggplot(aux, aes(ANO, percent, group = DESCR_SITUACAO, color = DESCR_SITUACAO)) + 
        geom_line(size = 1.5) +
        geom_point(size = 3, shape = 21, fill = "white") +
        scale_x_continuous(breaks = round(seq(min(aux$ANO), max(aux$ANO), by = 1),1)) +
        scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
        ylab("Percentual de Alunos (%)") + labs(color = "Situação do Aluno") + ggtitle("Aprovações e Reprovações por Ano e Disciplina") +
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 8)) +
        facet_wrap(~NOME_ATIV_CURRIC) 
```

![Alt text](images/image26.png?raw=true "image")

```R
# Final Grade Distribution by semester and year (Technical Drawing I)
ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], 
        aes(x = MEDIA_FINAL , fill = as.factor(ANO))) +
        geom_density(color = "black") + xlab("Média Final") + ylab("Densidade") + labs(fill = "Ano") +
        ggtitle("Distribuição das Médias em Desenho Técnico I (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_wrap(c("ANO","PERIODO"))
```

![Alt text](images/image27.png?raw=true "image")

```R
# Final Grade Distribution by semester and year (Technical Drawing II)
ggplot(all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO II",], 
        aes(x = MEDIA_FINAL , fill = as.factor(ANO))) +
        geom_density(color = "black") + xlab("Média Final") + ylab("Densidade") + labs(fill = "Ano") +
        ggtitle("Distribuição das Médias em Desenho Técnico II (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_wrap(c("ANO","PERIODO"))
```

![Alt text](images/image28.png?raw=true "image")

```R
# Final Grade 2D Distribution by Year (Technical Drawing I)
ggplot(all_data_without_frequency_dropout,
        aes(x = ANO, y = MEDIA_FINAL)) +
        ggtitle("Densidade das Médias por Ano (Sem Reprovações por Frequência)") +
        ylab("Média Final") + xlab("Ano") + labs(fill = "Densidade") +      
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
```

![Alt text](images/image29.png?raw=true "image")


# Fifth Part - Dataset 1 - Advanced Statistics

In this part I begin to test some hyphotesis about the independent variables and the students final grade. First I will separate our analysis, one including all students and other without dropout students. I will show how to do with Technical Drawing I.

I first turn the data from period column to numbers, representing the first and second semesters. After, the period and the year are turned into factors.

```R
all_data$PERIODO[all_data$PERIODO == "1. Semestre"] <- 1
all_data$PERIODO[all_data$PERIODO == "2. Semestre"] <- 2
all_data$PERIODO <- as.factor(all_data$PERIODO)
all_data$ANO <- as.factor(all_data$ANO)
```

## Case I: Students Performance by semester and year in Technical Drawing I, with all samples.

```R
# Test I: Normality of errors test.
aov_result <- aov(MEDIA_FINAL ~ ANO * PERIODO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
shapiro.test(residuals(aov_result))

#	Shapiro-Wilk normality test
#
# data:  residuals(aov_result)
# W = 0.93032, p-value < 2.2e-16

# p-value < 0.05 : H0 Rejected, the evidence that residuals distribution do not follow a normal distribution is statistically significant.

# Result: Residuals have a not normal distribution.

hist(res, main = "Histogram of residuals", xlab = "Residuals")
#plot(aov_result, 1)
```

![Alt text](images/statistic1.png?raw=true "image")

```R
# Test II: Homogeneity of Variance between the groups.
max(aggregate(MEDIA_FINAL ~ ANO + PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ANO + PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], var)$MEDIA_FINAL)

#Rule of thumb: Variances not exceeding the 1.5:1 rate between the maximun and minimun variance. Result: 1.62. The variance between the groups has some discrepancy.

leveneTest(MEDIA_FINAL ~ ANO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], center = median)

# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value    Pr(>F)    
# group   12  4.6556 1.579e-07 ***
#       2346                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

leveneTest(MEDIA_FINAL ~ PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], center = median)

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value  Pr(>F)  
# group    1  6.2454 0.01252 *
#       2357                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

leveneTest(MEDIA_FINAL ~ ANO * PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], center = median)

# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value    Pr(>F)    
# group   24  2.2232 0.0005779 ***
#       2334                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# p-value < 0.05 : H0 rejected in all tests above, the evidence that the variances between groups (years, semesters and both) are different is statistically significant.

# Result: Different variances between the groups of Years and Semesters.


# Test III: Distributions between the groups of years and semesters.

kruskal.test(MEDIA_FINAL ~ ANO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# Kruskal-Wallis rank sum test
# data:  MEDIA_FINAL by ANO
# Kruskal-Wallis chi-squared = 134.28, df = 12, p-value < 2.2e-16

kruskal.test(MEDIA_FINAL ~ PERIODO, all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# Kruskal-Wallis rank sum test
# data:  MEDIA_FINAL by PERIODO
# Kruskal-Wallis chi-squared = 4.8196, df = 1, p-value = 0.02814

kruskal.test(MEDIA_FINAL ~ interaction(ANO, PERIODO), all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

#	Kruskal-Wallis rank sum test
# data:  MEDIA_FINAL by interaction(ANO, PERIODO)
# Kruskal-Wallis chi-squared = 172.89, df = 24, p-value < 2.2e-16

# p-value < 0.05 : H0 rejected in all tests above, the difference of distributions between the years and semesters is statistically significant.

# Result: Different distributions between the groups of years and semesters.

densityplot(~ MEDIA_FINAL | ANO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Distribuição de Médias por Ano (Desenho Técnico I)", ylab = "Densidade de Médias")
```

![Alt text](images/statistic2.png?raw=true "image")

```R
densityplot(~ MEDIA_FINAL | PERIODO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], ylab = "Densidade de Médias", xlab = "Distribuição de Médias por Semestre (Desenho Técnico I)")
```

![Alt text](images/statistic3.png?raw=true "image")

```R
densityplot(~ MEDIA_FINAL | ANO * PERIODO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Distribuição de Médias por Ano e Semestre (Desenho Técnico I)", ylab = "Densidade de Médias")
```

![Alt text](images/statistic4.png?raw=true "image")

```R

# Test IV: Size effect evaluation test

multiVDA(MEDIA_FINAL ~ ANO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

Results: VDA ranges from 0 to 1, with 0.5 indicating stochastic equality, and 1 indicating that the first group dominates the second. CD ranges from -1 to 1, with 0 indicating stochastic equality, and 1 indicating that the first group dominates the second. r ranges from approximately, -0.86 to 0.86, depending on sample size, with 0 indicating no effect, and a positive result indicating that values in the first group are greater than in the second.

# 78.5% probability that the values from 2008 being greater than the 2014.

multiVDA(MEDIA_FINAL ~ PERIODO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# 47.4% probability that the values from first semester being greater than the second.

multiVDA(MEDIA_FINAL ~ SEX, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# 51.4% probability that the values from sex F being greater than the sex M.

multiVDA(MEDIA_FINAL ~ COD_CURSO, data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# 60.1% probability that the values from BAEQ course being greater than the BALF course.

# Which groups of years are different?

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ANO,
              data = all_data[all_data$NOME_ATIV_CURRIC == "DESENHO TECNICO I",],
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
        data = PT$res,
        threshold = 0.05,
        remove.zero = FALSE)
        
ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4)       
```

![Alt text](images/statistic5.png?raw=true "image")


## Case II: Students Performance by semester and year in Technical Drawing I, without dropout students.

```R
all_data_without_frequency_dropout$PERIODO[all_data_without_frequency_dropout$PERIODO == "1. Semestre"] <- 1
all_data_without_frequency_dropout$PERIODO[all_data_without_frequency_dropout$PERIODO == "2. Semestre"] <- 2
all_data_without_frequency_dropout$PERIODO <- as.factor(all_data_without_frequency_dropout$PERIODO)
all_data_without_frequency_dropout$ANO <- as.factor(all_data_without_frequency_dropout$ANO)

# Test I: Normality of errors test.
res = residuals(lm(MEDIA_FINAL ~ ANO * PERIODO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",]))
shapiro.test(res)

# Shapiro-Wilk normality test
# data:  res
# W = 0.95118, p-value < 2.2e-16

# p-value < 0.05 : H0 Rejected, the evidence that residuals distribution do not follow a normal distribution is statistically significant.
# Result: Residuals have a not normal distribution.
hist(res, main = "Histogram of residuals", xlab = "Residuals")
```

![Alt text](images/statistic6.png?raw=true "image")

```R
# Test II: Homogeneity of Variance between the groups.
max(aggregate(MEDIA_FINAL ~ ANO + PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ANO + PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], var)$MEDIA_FINAL)

# Result: 6.73. The variance between the groups has high discrepancy.

leveneTest(MEDIA_FINAL ~ ANO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

leveneTest(MEDIA_FINAL ~ PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

leveneTest(MEDIA_FINAL ~ ANO * PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# Results: H0 rejected in tests for Year and interaction Year*Semester above, the evidence that the variances between groups (years and both) are different is statistically significant. I failed to reject HO for Semesters, there is no evidence that the variances between semesters are statistically different.


# Test III: Distributions between the groups of years and semesters.

kruskal.test(MEDIA_FINAL ~ ANO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ PERIODO, all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ interaction(ANO, PERIODO), all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# Results: H0 rejected in tests for Year and interaction Year*Semester above, the evidence that the distributions between groups (years and both) are different is statistically significant. I failed to reject HO for Semesters, there is no evidence that the distributions between semesters are statistically different.

densityplot(~ MEDIA_FINAL | ANO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Média Final", ylab = "Densidade de Médias")
```

![Alt text](images/statistic7.png?raw=true "image")

```R
densityplot(~ MEDIA_FINAL | PERIODO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Média Final", ylab = "Densidade de Médias")
```

![Alt text](images/statistic8.png?raw=true "image")

```R
densityplot(~ MEDIA_FINAL | ANO * PERIODO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",], xlab = "Média Final", ylab = "Densidade de Médias")
```

![Alt text](images/statistic9.png?raw=true "image")

```R

# Test IV: Size effect evaluation test

multiVDA(MEDIA_FINAL ~ ANO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# 83.7% probability that the values from 2008 being greater than the 2014.

multiVDA(MEDIA_FINAL ~ PERIODO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# 51.3% probability that the values from first semester being greater than the second.

multiVDA(MEDIA_FINAL ~ SEX, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# 46.2% probability that the values from sex F being greater than the sex M.

multiVDA(MEDIA_FINAL ~ COD_CURSO, data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",])

# 61.6% probability that the values from BAEC course being greater than the BALF course.

# Which groups of years are different?

options("scipen" = 100, "digits" = 4)
PT = dunnTest(MEDIA_FINAL ~ ANO,
              data = all_data_without_frequency_dropout[all_data_without_frequency_dropout$NOME_ATIV_CURRIC == "DESENHO TECNICO I",],
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
        data = PT$res,
        threshold = 0.05,
        remove.zero = FALSE)
        
ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 
```

![Alt text](images/statistic10.png?raw=true "image")


The statistical analysis for Technical Drawing II is left for further analysis in the future.

# First Part - Dataset 2 - Sensible Information

All sensible information from the datasets were removed first, like students names. The student identification was encrypted to preserve the university sensible data. 

# Second Part - Dataset 1 - Loading and Processing

Now I load the dataset 2 from the spreadsheet. This step is called **data loading**.

```R
all_data_II <- read.csv("csv/Professor_Specific_Data.csv", sep = ";", stringsAsFactors = FALSE, encoding = "utf8")
```

Now I explore the data and the columns, beginning with the **data exploration and processing** step.
I will only keep Approved and Disapproved students. Other situations will be removed. 

```R
unique(all_data_II$situação)
all_data_II <- all_data_II[(all_data_II$situação != "Trancamento parcial" & all_data_II$situação != "Dispensado sem  nota" & all_data_II$situação != "Disciplina Não Concluída"),]
```

Our goal is to analyse the Technical Drawing I and II, so I removed other disciplines and related courses from this professor.

```R
unique(all_data_II$curso)
all_data_II <- all_data_II[(all_data_II$curso != "ESPPOS" & all_data_II$curso != "BAMCA" & all_data_II$curso != "ESPG"),]
all_data_II$curso[all_data_II$curso == "BAEE-2010"] <- "BAEE"
unique(all_data_II$componente.curricular)
all_data_II <- all_data_II[(all_data_II$componente.curricular != "BA000184-CONFIABILIDADE DE PROCESSOS E PRODUTOS" & all_data_II$componente.curricular != "BA000258-GEOPROCESSAMENTO E TOPOGRAFIA" & all_data_II$componente.curricular != "BA000188-MODELAGEM DA INFORMACAO" & all_data_II$componente.curricular != "BA000254-SENSORIAMENTO REMOTO APLICADO A ENGENHARIA"),]
unique(all_data_II$ano.periodo)
unique(all_data_II$turma)
all_data_II[all_data_II$nota<0 | all_data_II$nota>10,]
```

One student information not available in this dataset is the sex. I can get the sex data from the first dataset, using the student id as the key. I will show how I did it using student id, but this operation cannot be done anymore because of the way I encrypted student id before. Some sex information is still missing, showing to us that the second dataset has students not present in the first (data integration problem or data missing). This is a very difficult problem to check and address, but being aware of it is important for know what to do to correct the academic system.
I saved the information about student sex in a csv file. 

```R
all_data <- rbind(BAEA, BAEE)
all_data <- rbind(all_data, BAEC)
all_data <- rbind(all_data, BAEP)
all_data <- rbind(all_data, BAEQ)
all_data <- rbind(all_data, BALF)
sex_data <- all_data[,c(2,16)]
sex_data <- sex_data %>% distinct(MATR_ALUNO, .keep_all = TRUE)
all_data_II <- merge(x = all_data_II, y = sex_data, by.x = "matrícula", by.y = "MATR_ALUNO", all.x = TRUE)
all_data_II <- all_data_II[,c(13,17)] #keep only names and sex, to correct missing data manually in the spreadsheet
write.table(all_data_II,'csv/sex_data.csv',row.names = TRUE, sep = ";")
```

Now I loaded the sex dataset and merge with our second dataset. 

```R
sex_data <- read.csv("csv/sex_data.csv", sep = ";", stringsAsFactors = FALSE, encoding = "utf8")
```
I also removed information not usefull for our next analysis.

```R
all_data_II <- cbind(all_data_II, sex_data)
all_data_II <- all_data_II[,-c(1,5,6,7,8,9,15)]
```

I also corrected the names of Technical Drawing disciplines in cases that the code information was concatenated with it.

```R
all_data_II$componente.curricular[all_data_II$componente.curricular == "010801-DESENHO TECNICO I"] <- "DESENHO TECNICO I"
all_data_II$componente.curricular[all_data_II$componente.curricular == "010803-DESENHO TECNICO II"] <- "DESENHO TECNICO II"
all_data_II$componente.curricular[all_data_II$componente.curricular == "BA010801-DESENHO TECNICO I"] <- "DESENHO TECNICO I"
all_data_II$componente.curricular[all_data_II$componente.curricular == "BA010803-DESENHO TECNICO II"] <- "DESENHO TECNICO II"
```

Finally, I separated the year and semester information from the column "ano.periodo". I also changed the column name "nota" to "MEDIA_FINAL" (just to keep the independent variable standart).

```R
all_data_II$ano <- as.numeric(substr(all_data_II$ano.periodo, 1, 4))
all_data_II$periodo <- substr(all_data_II$ano.periodo, 8, 18)
all_data_II <- all_data_II[,-2]
colnames(all_data_II)[6] <- "MEDIA_FINAL"
```

# Third Part - Dataset 2 - Descriptive Statistics

I proceed the same way I did previously with the first dataset. First, I removed the samples from BALM course (mathematics), because there are just three samples, being almost impossible to infer any general information from the math students performance. I did the same for BALP and BALQ in case of Technical Drawing II.

```R
all_data_II <- all_data_II[!(all_data_II$curso == "BALM"),]
all_data_II <- all_data_II[!(all_data_II$curso == "BALP"),]
all_data_II <- all_data_II[!(all_data_II$componente.curricular == "DESENHO TECNICO II" & all_data_II$curso == "BALQ"),]

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
```

And now the same statistical analysis without dropout students.

```R
dfII_without_dropout <- all_data_II[!(all_data_II$situação == "Reprovado por Frequência"),]

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
```

# Fourth Part - Dataset 1 - Data Visualization

I will create separated boxplots and violin plots analysing students final grade for each discipline and for each possible dimension (years, semesters, courses and classes), considering all samples and samples without the dropout students.

```R
# Are there relevant differences in the final grade between the graduation courses? (Technical Drawing I)
ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], 
       aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
       geom_violin(trim = TRUE) + 
       geom_boxplot(width = 0.1) + 
       geom_segment(aes(x = 0, y = 6, xend = 8, yend = 6), linetype = "dotted", colour = "red") +
       scale_y_continuous(breaks = seq(0, 10, by = 1)) +
       xlab("Curso") + ylab("Media Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Tecnico I") +
       theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image30.png?raw=true "image")


```
# Are there relevant differences in the final grade between the years? (Technical Drawing I)
ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
       geom_violin(trim = TRUE) + 
       geom_boxplot(width = 0.1) + 
       geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
       scale_y_continuous(breaks = seq(0, 10, by = 1)) +
       xlab("Ano") + ylab("Média Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Técnico I") +
       theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image31.png?raw=true "image")


```
# Are there relevant differences in the final grade between the years and semesters? (Technical Drawing I)      
ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO I",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(periodo))) +
       geom_violin(trim = TRUE) +
       geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) + 
       geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
       scale_y_continuous(breaks = seq(0, 10, by = 1)) +
       theme(legend.position = "bottom") +
       xlab("Ano") + ylab("Média Final") + labs(fill = "Período") + ggtitle("Desempenho dos Estudantes em Desenho Técnico I") +
       theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image32.png?raw=true "image")


```
# Are there relevant differences in the final grade between the graduation courses? (Technical Drawing II)
ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], 
        aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Curso") + ylab("Média Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Técnico II") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image33.png?raw=true "image")


```
# Are there relevant differences in the final grade between the years? (Technical Drawing II)
ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], 
        aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.05) + 
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Ano") + ylab("Média Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Técnico II") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image34.png?raw=true "image")


```
# Are there relevant differences in the final grade between the years and semesters? (Technical Drawing II)  
ggplot(all_data_II[all_data_II$componente.curricular == "DESENHO TECNICO II",], 
       aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(periodo))) +
       geom_boxplot(width = 0.4, position = position_dodge(width = 0.9)) +
       geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
       scale_y_continuous(breaks = seq(0, 10, by = 1)) +
       theme(legend.position = "bottom") +
       xlab("Ano") + ylab("Média Final") + labs(fill = "Período") + ggtitle("Desempenho dos Estudantes em Desenho Técnico II") +
       theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image35.png?raw=true "image")


```
# Are there relevant differences in the final grade between the graduation courses? (Technical Drawing I - without dropout students)
ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], 
        aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(width = 0.1) +
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
        xlab("Curso") + ylab("Média Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Técnico I (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image36.png?raw=true "image")


```
# Are there relevant differences in the final grade between the years? (Technical Drawing I - without dropout students)
ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], 
        aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
        geom_violin(trim = TRUE) + 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Ano") + ylab("Média Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Técnico I (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image37.png?raw=true "image")


```
# Are there relevant differences in the final grade between the years and semesters? (Technical Drawing I - without dropout students)
ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], 
        aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(periodo))) +
        geom_violin(trim = TRUE, position = dodge) + 
        geom_boxplot(width = 0.1, position = dodge) +
        geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("Ano") + ylab("Média Final") + labs(fill = "Período") + ggtitle("Desempenho dos Estudantes em Desenho Técnico I (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image38.png?raw=true "image")


```
# Are there relevant differences in the final grade between the graduation courses? (Technical Drawing II - without dropout students)
ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], 
        aes(x = curso, y = MEDIA_FINAL, fill = curso)) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 0, y = 6, xend = 7, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Curso") + ylab("Média Final") + labs(fill = "Curso") + ggtitle("Desempenho dos Estudantes em Desenho Técnico II (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image39.png?raw=true "image")


```
# Are there relevant differences in the final grade between the years? (Technical Drawing II - without dropout students)
ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], 
        aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(ano))) +
        geom_violin(trim = TRUE)+ 
        geom_boxplot(width = 0.1) + 
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        xlab("Ano") + ylab("Média Final") + labs(fill = "Ano") + ggtitle("Desempenho dos Estudantes em Desenho Técnico II (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image40.png?raw=true "image")


```
# Are there relevant differences in the final grade between the years and semesters? (Technical Drawing II - without dropout students)
ggplot(dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], 
        aes(x = as.factor(ano), y = MEDIA_FINAL, fill = as.factor(periodo))) +
        geom_violin(trim = TRUE) + 
        geom_segment(aes(x = 0, y = 6, xend = 13, yend = 6), linetype = "dotted", colour = "red") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme(legend.position = "bottom") +
        xlab("Ano") + ylab("Média Final") + labs(fill = "Período") + ggtitle("Desempenho dos Estudantes em Desenho Técnico II (Sem Reprovações por Frequência)") +
        theme(plot.title = element_text(hjust = 0.5))
```

![Alt text](images/image41.png?raw=true "image")


```
#Are there relevant differences in the final grade means between the students sex and disciplines? (Technical Drawing I)
aux <- statistics(all_data_II, "MEDIA_FINAL", SEXO, componente.curricular)      
      
ggplot(aux, aes(componente.curricular, round(mean,2), group = SEXO, fill = as.factor(SEXO))) + 
        geom_bar(stat = "identity", color = "black", position = position_dodge()) +
        geom_text(aes(label = round(mean, 2)), vjust = 2, color = "black", size = 5, position = position_dodge(0.9)) +
        scale_fill_manual("Sexo", values = c("F" = "deeppink", "M" = "deepskyblue")) +
        scale_y_continuous(breaks = seq(0, 7, by = 1)) +
        xlab("Disciplina") + ylab("Média da Nota Final") + ggtitle("Análise de Médias por Disciplina e Sexo") +
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 12))
```

![Alt text](images/image42.png?raw=true "image")


```
#Students number and final grades by year, semester and discipline
aux <- statistics(all_data_II, "MEDIA_FINAL", ano, periodo, componente.curricular)

a <- ggplot(aux, aes(as.factor(ano), mean, group = periodo, fill = as.factor(periodo))) + 
        geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
        geom_text(aes(label = round(mean, 1)), vjust = 2, color = "black", size = 2.8, position = position_dodge(0.9)) +
        scale_fill_manual("Período", values = c("1. Semestre" = "cadetblue2", "2. Semestre" = "darkolivegreen2")) +
        xlab("Ano") + ylab("Média da Nota Final") + labs(fill = "Período") + ggtitle("Médias finais por Disciplina, Ano e Semestre") +
        theme(axis.text.x = element_text(face = "bold", size = 10), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
        facet_wrap(~componente.curricular) 

b <- ggplot(aux, aes(as.factor(ano), n, group = periodo, fill = as.factor(periodo))) + 
        geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
        geom_text(aes(label = n), vjust = 2, color = "black", size = 2.8, position = position_dodge(0.9)) +
        scale_fill_manual("Período", values = c("1. Semestre" = "cadetblue2", "2. Semestre" = "darkolivegreen2")) +
        scale_y_continuous(breaks = round(seq(0, 60, by = 5), 1)) +
        xlab("Ano") + ylab("Número de Alunos") + labs(fill = "Período") + ggtitle("Número de Alunos por Disciplina, Período e Ano") +
        theme(axis.text.x = element_text(face = "bold", size = 10), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
        facet_wrap(~componente.curricular)
  
ggarrange(a, b, nrow = 2)

```

![Alt text](images/image43.png?raw=true "image")


```
# Students situation by year and semester in Technical Drawing I
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
        ggtitle("Situação de Alunos por Período e Ano em Desenho Técnico I") +
        theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.border = element_blank(), panel.background = element_blank(),
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank()) +
        aes(fill = I("darkseagreen2"))

ggarrange(b, a, nrow = 2, heights=c(1, 4))
```

![Alt text](images/image44.png?raw=true "image")


```
# Students situation by year and semester in Technical Drawing II
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
        ggtitle("Situação de Alunos por Período e Ano em Desenho Técnico II") +
        theme(legend.position = "bottom", axis.line = element_line(size = 1, colour = "black"),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.border = element_blank(), panel.background = element_blank(),
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank()) +
        aes(fill = I("darkseagreen2"))

ggarrange(b, a, nrow = 2, heights=c(1, 4))
```

![Alt text](images/image45.png?raw=true "image")


```
# Is there a relation between the number of classes missed and the final grade? (of course, without dropout students)

ggplot(all_data_II[all_data_II$situação!="Reprovado por Frequência" & all_data_II$componente.curricular == "DESENHO TECNICO I",], aes(x = faltas, y = MEDIA_FINAL)) + 
        geom_point() +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(0, 16, by = 4)) +
        ylab("Média Final") +
        ggtitle("Desenho Técnico I") +
        geom_smooth(method = lm) +
        geom_jitter(width = 0.25) +
        theme(plot.title=element_text(hjust = 0.5))
```

![Alt text](images/image46.png?raw=true "image")


```
ggplot(all_data_II[all_data_II$situação!="Reprovado por Frequência" & all_data_II$componente.curricular == "DESENHO TECNICO II",], aes(x = faltas, y = MEDIA_FINAL)) + 
        geom_point() +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        scale_x_continuous(breaks = seq(0, 16, by = 2)) +
        ylab("Média Final") +
        ggtitle("Desenho Técnico II") +
        geom_smooth(method = lm) +
        geom_jitter(width = 0.25) +
        theme(plot.title=element_text(hjust = 0.5))
```

![Alt text](images/image47.png?raw=true "image")


```  
# Fifth Part - Dataset 1 - Advanced Statistics

In this part I begin to test some hyphotesis about the independent variables and the students final grade. First I will separate our analysis by discipline, including all students (except dropout students). I started with Technical Drawing I.

First, I turn the data from period column to numbers, and them to factor. I did the same for the years.

```R
dfII_without_dropout$periodo[dfII_without_dropout$periodo == "1. Semestre"] <- 1
dfII_without_dropout$periodo[dfII_without_dropout$periodo == "2. Semestre"] <- 2
dfII_without_dropout$periodo <- as.factor(dfII_without_dropout$periodo)
dfII_without_dropout$ano <- as.factor(dfII_without_dropout$ano)
```

# Case I: Students Performance by semester and year in Technical Drawing I, without dropout students.

```R
# Test I: Normality of errors test.
res = residuals(lm(MEDIA_FINAL ~ ano * periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",]))
shapiro.test(res)
# p-value < 0.05 : H0 Rejected, the evidence that residuals distribution do not follow a normal distribution is statistically significant.
# Result: Residuals have a not normal distribution.
hist(res)
```

![Alt text](images/statistic11.png?raw=true "image")


```R
# Test II: Homogeneity of Variance between the groups.
max(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], var)$MEDIA_FINAL)

# Result: 9.26. The variance between the groups has high discrepancy.
leveneTest(MEDIA_FINAL ~ ano, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

leveneTest(MEDIA_FINAL ~ periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

leveneTest(MEDIA_FINAL ~ ano * periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

# Results: H0 rejected all tests for Year and Semester, the evidence that the variances between groups of years and semesters are different is statistically significant. 

# Test III: Distributions between the groups of years and semesters.

kruskal.test(MEDIA_FINAL ~ ano, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])
kruskal.test(MEDIA_FINAL ~ interaction(ano, periodo), dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

# Results: H0 rejected all tests for Years and Semesters, the evidence that the distributions between groups are different is statistically significant. 

densityplot(~ MEDIA_FINAL | ano, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], xlab = "Média Final", ylab = "Densidade de Médias")
```

![Alt text](images/statistic11.png?raw=true "image")


```R
densityplot(~ MEDIA_FINAL | periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], xlab = "Média Final", ylab = "Densidade de Médias")
```

![Alt text](images/statistic12.png?raw=true "image")


```R
densityplot(~ MEDIA_FINAL | ano*periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",], xlab = "Média Final", ylab = "Densidade de Médias")
```

![Alt text](images/statistic13.png?raw=true "image")


```R
# Test IV: Size effect evaluation test

#multiVDA(MEDIA_FINAL ~ ano, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

# 2008-2014 has the greatest probability of the groups being statistically different.

multiVDA(MEDIA_FINAL ~ periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

# 1-2 semesters have 42% of probability that the values from the first group being greater than the second (semesters).

multiVDA(MEDIA_FINAL ~ SEXO, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

# f - M have a almost 50% probability that the values from one group being greater than the other.

multiVDA(MEDIA_FINAL ~ curso, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

# BAEE - BALF have the greatest difference between the values of each group.

# Which groups of years are different?

options("scipen"=100, "digits"=4)
PT = dunnTest(MEDIA_FINAL ~ ano,
              data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",],
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
        data = PT$res,
        threshold = 0.05,
        remove.zero = FALSE)
        
ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 
```

![Alt text](images/statistic15.png?raw=true "image")


The statistical analysis for Technical Drawing II were made below.

```R
# Test I: Normality of errors test.
res = residuals(lm(MEDIA_FINAL ~ ano * periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",]))
shapiro.test(res)
# p-value < 0.05 : H0 Rejected, the evidence that residuals distribution do not follow a normal distribution is statistically significant.
# Result: Residuals have a not normal distribution.
hist(res)
```

![Alt text](images/statistic16.png?raw=true "image")

```R
# Test II: Homogeneity of Variance between the groups.
max(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], var)$MEDIA_FINAL)/min(aggregate(MEDIA_FINAL ~ ano + periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], var)$MEDIA_FINAL)

# Result: 30.49. The variance between the groups has high discrepancy.
leveneTest(MEDIA_FINAL ~ ano, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

leveneTest(MEDIA_FINAL ~ periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

leveneTest(MEDIA_FINAL ~ ano * periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

# Results: H0 rejected all tests for Year and Semester, the evidence that the variances between groups of years and semesters are different is statistically significant. 

# Test III: Final Grade Distributions between the groups of years and semesters.

kruskal.test(MEDIA_FINAL ~ ano, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])
kruskal.test(MEDIA_FINAL ~ periodo, dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])
kruskal.test(MEDIA_FINAL ~ interaction(ano, periodo), dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

# Results: H0 rejected in tests for Years and interaction between years and semesters, the evidence that the distributions between these groups are different is statistically significant. I failed to reject H0 for semesters, the evidence that the distributions between semesters are different is not statistically significant.

densityplot(~ MEDIA_FINAL | ano, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], xlab = "Média Final", ylab = "Densidade de Médias")
```

![Alt text](images/statistic17.png?raw=true "image")

```R
densityplot(~ MEDIA_FINAL | periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], xlab = "Média Final", ylab = "Densidade de Médias")
```

![Alt text](images/statistic18.png?raw=true "image")

```R
densityplot(~ MEDIA_FINAL | ano*periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",], xlab = "Média Final", ylab = "Densidade de Médias")
```

![Alt text](images/statistic19.png?raw=true "image")

```R

# Test IV: Size effect evaluation test

#multiVDA(MEDIA_FINAL ~ ano, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO I",])

# 2008-2014 has the greatest probability of the groups being statistically different.

multiVDA(MEDIA_FINAL ~ periodo, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

# 52.5% of probability that the values from the first semester being greater than the second.

multiVDA(MEDIA_FINAL ~ SEXO, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

# 53% probability that the values from F group being greater than the M group (sex).

multiVDA(MEDIA_FINAL ~ curso, data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",])

# 75% probability that the values from BAEC group being greater than the BALF group (course).

# Which groups of years are different?

options("scipen"=100, "digits"=4)
PT = dunnTest(MEDIA_FINAL ~ ano,
              data = dfII_without_dropout[dfII_without_dropout$componente.curricular == "DESENHO TECNICO II",],
              method = "bh") 

O <- cldList(P.adj ~ Comparison,
        data = PT$res,
        threshold = 0.05,
        remove.zero = FALSE)

ggplot(O, aes(x = Letter, y = Group, fill = Letter, color = Letter)) + geom_point(size = 4) 
```

![Alt text](images/statistic20.png?raw=true "image")


# Conclusions 

There is a lot of results presented here that can provide insights about how the disciplines of Technical Drawing I and II is going, considering the students performance and dropouts, what is happening by year, semester and courses, and the statistical differences considering these possibilities. A more detailed written resume will be provided here, with time. 

This analysis reached it's goal, to extract usefull information from a lot of data stored in spreadsheets provided by the university. I give a special thanks to professor Alexandro Schaffer, who trusted me to do this analysis and gave me the raw data to perform this work.
