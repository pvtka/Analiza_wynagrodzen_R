# Databricks notebook source
# Załadowanie potrzebnych bibliotek
library(SparkR)
library(ggplot2)
library(dplyr)
library(rpart)
library(pROC)
install.packages("Metrics")
library(Metrics)
install.packages("rattle")
library(rattle)

# COMMAND ----------

# MAGIC %md
# MAGIC #Źródło danych

# COMMAND ----------

# Zbiór danych składa się z informacji dotyczących osób zatrudnionych. 
 
# Poniżej zostały przedstawione zmienne brane pod uwagę w badaniu wraz z opisującąmi je skalami.
 
#       1. salary - wysokość wynagrodznia
 
#       2. jobType - rodzaj stanowsika
#           - CEO
#           - CFO
#           - CTO 
#           - VICE-PRESIDENT
#           - MANAGER
#           - SENIOR
#           - JUNIOR 
#           - JANITOR

#       4. degree - poziom wykszatałcenia wykształcenia
#           - DOCTORAL
#           - MASTER
#           - BACHELORS
#           - HIGH_SCHOOL
#           - NONE
 
#      5. industry - gałąź przemysłu
#           - OIL
#           - FINANCE
#           - WEB
#           - HEALTH
#           - AUTO
#           - SERVICE
#           - EDUCATION
  
#       6. yearsExperience - doświadczenie zawodowe w latach

#       7. milesFromMetropolis - odległość od miasta

#       6. major - główna dziedzina, która zajmuje się pracownik
#           - MATH
#           - PHYSICS
#           - CHEMISTRY
#           - COMPSCI
#           - BIOLOGY
#           - LITERATURE
#           - BUSINESS
#           - ENGINEERING
#           - NONE

#       7. jobId
 
#       8. companyId
 
 
 
# Dane zostały zaczerpnięte ze strony internetowej "kaggle". Są one dostępne pod linkiem: https://www.kaggle.com/datasets/pavanelisetty/salarypredictions.

# COMMAND ----------

# MAGIC %md
# MAGIC #Problem badawczy

# COMMAND ----------

# Problem badawczy: Analiza czynników wpływających na wysokość wynagrodzenia.
# Pytania badawcze: 
#     Czy typ stanowiska ma wpływ na wynagrodzenie? 
#     Czy kwalifikacje zawodowe wpływają na wynagrodzenie? 
#     Czy branża, w której pracuje dana osoba wpływa na wynagrodzenie? 
#     Czy doświadczenie zawodowe ma wpływ na wynagrodzenie? Czy odległość od miasta ma wpływ na wynagrodzenie? 
#     Czy wpływ wszystkich powyższych czynników na wynagrodzenie jest równoważny, czy też jeden z nich ma większy wpływ?

# COMMAND ----------

# MAGIC %md
# MAGIC #Analiza eksploracyjna

# COMMAND ----------

# MAGIC %md
# MAGIC ##Wczytanie danych
# MAGIC

# COMMAND ----------

#Wczytanie danych
d <- read.df('/FileStore/tables/train_dataset.csv', source='csv', header='true' ,inferSchema = 'true')
dsal <- read.df('/FileStore/tables/train_salaries.csv', source='csv', header='true' ,inferSchema = 'true')

# COMMAND ----------

data <- as.data.frame(d)
datasal <- as.data.frame(dsal)

# COMMAND ----------

#Połączenie danych z dwóch tabeli.
data <- inner_join(data, datasal, by = "jobId")

# COMMAND ----------

colnames(data)

# COMMAND ----------

nrow(data)

# COMMAND ----------

#Z uwagi na rozmiary tabeli zawierającej 1000000 wierszy, zdecydowano się na wyselekcjonowanie próby losowej składającej się z 30% wierszy. Taki proces pozwala na utworzenie reprezentatywnego wzoru dla całego zbioru, co zapewnia wyniki bardziej adekwatne do rzeczywistości. Ponadto, taka próba jest łatwiejsza do interpretacji i wizualizacji, a jej analiza jest mniej czasochłonna i mniej obciążająca.
data <- data %>% sample_frac(0.3)

# COMMAND ----------

nrow(data)

# COMMAND ----------

# MAGIC %md
# MAGIC ##Analiza wykresów

# COMMAND ----------

ggplot(data=data) +
  geom_histogram(aes(x=salary), fill="yellow", alpha=0.5) +
  ggtitle('Histogram przedstawiający wynagrodznia') +
  xlab("Wysokość wynagrodznia") +
  ylab("Liczebność") +
  theme(plot.title = element_text(hjust = 0.5))

# COMMAND ----------

ggplot(data=data)+
  geom_histogram(aes(x=salary, y=..density..), fill="purple3", alpha=0.5, binwidth=2)+
  geom_density(aes(x=salary), col="black", fill="grey", alpha=0.2) +
  geom_vline(aes(xintercept = mean(salary)), lty=3) +
  xlab("Wysokość wynagrodzenia") +
  ylab("Udział") +
  ggtitle("Rozkład wysokości wynagrodznia") +
  theme(plot.title = element_text(hjust = 0.5))

# COMMAND ----------

mean(data$salary)

# COMMAND ----------

median(data$salary)

# COMMAND ----------

# MAGIC %md
# MAGIC ##Badanie zależności pomiędzy wynagrodzeniem a zmiennymi objaśniającymi

# COMMAND ----------

data_industry_avg_salary <- data %>% 
  group_by(industry) %>% 
  summarize(average_salary = mean(salary, na.rm = TRUE))

ggplot(data = data_industry_avg_salary, aes(x = industry, y = average_salary)) +
  geom_col(aes(fill=industry), alpha=0.5) +
  ggtitle('Średnie wynagrodzenie dla poszczególnych branż') +
  xlab("Branża") +
  ylab("Średnie wynagrodzenie") +
  theme(plot.title = element_text(hjust = 0.5))


# COMMAND ----------

ggplot(data=data, aes(x=industry, y=salary)) +
  geom_violin(scale="area", color=12, alpha=0.5) +
  geom_jitter(alpha=0.5, aes(color=industry),position = position_jitter(width = 0.1)) +
  xlab("Branża") +
  ylab("Wynagrodzenie") + 
  scale_color_discrete(name = "Branża")

# COMMAND ----------

data_degree_avg_salary <- data %>% 
  group_by(degree) %>% 
  summarize(average_salary = mean(salary, na.rm = TRUE))

ggplot(data=data_degree_avg_salary, aes(x=degree, y=average_salary)) + 
  geom_bar(stat = "identity",  aes(fill=degree), alpha=0.5) +
  ggtitle('Średnie wynagrodzenie w zależności od poziomu wykształcenia') +
  xlab("Poziom wykształcenia") +
  ylab("Średnie wynagrodzenie") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "Tytuł naukowy")


# COMMAND ----------

ggplot(data=data, aes(x=degree, y=salary)) +
  geom_violin(scale="area", color=1, alpha=0.5) +
  geom_jitter(alpha=0.5, aes(color=degree),position = position_jitter(width = 0.1)) +
  xlab("Tytuł naukowy") +
  ylab("Wynagrodzenie") + 
  scale_color_discrete(name = "Tytuł naukowy")


# COMMAND ----------

data_jobType_avg_salary <- data %>% 
  group_by(jobType) %>% 
  summarize(average_salary = mean(salary, na.rm = TRUE))

ggplot(data=data_jobType_avg_salary, aes(x=jobType, y=average_salary)) + 
  geom_bar(stat = "identity",  aes(fill=jobType), alpha=0.5) +
  ggtitle('Średnie wynagrodzenie w zależności od rodzaju stanowiska') +
  xlab("Stanowisko") +
  ylab("Średnie wynagrodzenie") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_discrete(name = "Stanowisko")


# COMMAND ----------

ggplot(data=data, aes(x=jobType, y=salary)) +
  geom_violin(scale="area", color=3, alpha=0.5) +
  geom_jitter(alpha=0.5, aes(color=jobType),position = position_jitter(width = 0.1)) +
  xlab("Stanowisko") +
  ylab("Wynagrodzenie") + 
  scale_color_discrete(name = "Stanowisko")


# COMMAND ----------

data_yearsExperience_avg_salary <- data %>% 
  group_by(yearsExperience) %>% 
  summarize(average_salary = mean(salary, na.rm = TRUE))

ggplot(data=data_yearsExperience_avg_salary, aes(x=yearsExperience, y=average_salary)) + 
  geom_bar(stat = "identity",  fill="pink", alpha=0.6) +
  ggtitle('Średnie wynagrodzenie w zależności od doświadczenia zawodowego') +
  xlab("Doświadczenie zawodowe") +
  ylab("Średnie wynagrodzenie") +
  theme(plot.title = element_text(hjust = 0.5)) 

# COMMAND ----------

data_milesFromMetropolis_avg_salary <- data %>% 
  group_by(milesFromMetropolis) %>% 
  summarize(average_salary = mean(salary, na.rm = TRUE))

ggplot(data=data_milesFromMetropolis_avg_salary, aes(x=milesFromMetropolis, y=average_salary)) + 
  geom_bar(stat = "identity",  fill="purple4", alpha=0.5) +
  ggtitle('Średnie wynagrodzenie w zależności od odległości od miasta') +
  xlab("Odległość od miasta") +
  ylab("Średnie wynagrodzenie") +
  theme(plot.title = element_text(hjust = 0.5))


# COMMAND ----------

bins <- seq(0, 100, by = 25)
intervals <- cut(data$milesFromMetropolis, breaks = bins, include.lowest = TRUE)

# COMMAND ----------

ggplot(data=data, aes(x=intervals, y=salary)) +
  geom_violin(scale="area", color=12, alpha=0.5) +
  geom_jitter(alpha=0.5, aes(color=milesFromMetropolis),position = position_jitter(width = 0.1)) +
  scale_color_gradient(low = "thistle", high = "light blue", name = "Odległość") +
  xlab("Odległość od miasta") +
  ylab("Wynagrodzenie") 


# COMMAND ----------

avg_salary_per_experience_level_industry <- data %>% 
                                  group_by(yearsExperience, industry) %>% 
                                  summarize(average_salary = mean(salary))
ggplot(data = avg_salary_per_experience_level_industry) + 
  geom_area(mapping = aes(x = yearsExperience,y = average_salary, fill = industry)) + 
  xlab("Doświadczenie (w latach)") +
  ylab("Średnie wynagrodzenie") +
  scale_fill_discrete(name = "Branża") +
  ggtitle("Średnie wynagrodzenie w zależności od lat doświadczenia i branży") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~industry)

# COMMAND ----------

salary_per_experience_level_jobType <- data %>% 
                                  group_by(yearsExperience, jobType) %>% 
                                  summarize(average_salary = mean(salary))
ggplot(data = salary_per_experience_level_jobType) + 
  geom_col(mapping = aes(x = yearsExperience, y = average_salary, fill = jobType)) + 
  facet_wrap(~jobType) +
  xlab("Doświadczenie (w latach)") +
  ylab("Średnie wynagrodzenie") +
  ggtitle("Średnie wynagrodzenie w zależności od doświadczenia i stanowiska pracy") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Stanowisko pracy")

# COMMAND ----------

 ggplot(data, aes(x=salary, y=intervals, fill=intervals)) + 
  geom_boxplot() +
  xlab("Wysokość wynagrodzenia") +
  ylab("Doświadczenie zawodowe") +
  ggtitle('Zależność pomiędzy wysokością wynagrodzenia a doświadczeniem zawodowym')+
    labs(fill = 'Odległość od miasta')

# COMMAND ----------

ggplot(data[data$jobType == "SENIOR", ], aes(x = yearsExperience, y = salary)) +
geom_point() +
geom_smooth() +
ggtitle(paste("Wynagrodzenie według lat doświadczenia dla pracowników na stanowisku: Senior")) +
xlab("Lat doświadczenia") +
ylab("Wynagrodzenie") +
theme(plot.title = element_text(hjust = 0.5)) 

# COMMAND ----------

data1 <- data %>% sample_frac(0.02)
ggplot(data1, aes(x = milesFromMetropolis, y = salary, color = jobType)) + 
  geom_point() + 
  ggtitle("Wynagrodzenie według dystansu od metropolii dla różnych typów pracy") +
  xlab("Dystans od metropolii") + 
  ylab("Wynagrodzenie") +
  theme(plot.title = element_text(hjust = 0.5))

# COMMAND ----------

data_major <- data %>% 
  group_by(major) %>% 
  summarise(
    n = n()
  )

data_major$proportion <- data_major$n / sum(data_major$n)

ggplot(data=data_major, aes(x="", y=proportion, fill=major)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle('Wykres kołowy przedstawiający udział poszczególnych dziedzin wśród pracowników') +
  labs(fill = 'Dziedzina') +
  coord_polar("y", start=0) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent)


# COMMAND ----------

data_degree_industry <- data %>% 
  group_by(degree, industry) %>% 
  summarise(
    n = n()
  )
 
ggplot(data_degree_industry, aes(fill=degree, y=n, x=industry)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle('Wykres słupkowy przedstawiający liczbę sprzedanych lokali mieszkalnych z uwzględnieniem oceny stanu')+
    xlab("Rodzaj lokalu mieszkalnego") +
    ylab("Liczebność") +
    labs(fill = 'Poziom wykształcenia')

# COMMAND ----------

# MAGIC %md 
# MAGIC #Analiza zaawansowana

# COMMAND ----------

# MAGIC %md
# MAGIC ##Drzewo decyzyjne

# COMMAND ----------

# Podział danych na zbiór treningowy i testowy
set.seed(13)
n = nrow(data)
i = floor(0.75 * n)
s = sample.int(n, i, replace=F)
 
train_data = data[s,]
test_data = data[-s,]

# COMMAND ----------

str(data)

# COMMAND ----------

model_drzewo <- rpart(data = train_data, 
                      salary ~ jobType + degree + industry + yearsExperience + milesFromMetropolis
                     )
 
model_drzewo

# COMMAND ----------

printcp(model_drzewo)

# COMMAND ----------

plotcp(model_drzewo)

# COMMAND ----------

predict.model_drzewo <- predict(model_drzewo, newdata=test_data, type='vector')

# COMMAND ----------


rmse.model_drzewo <- rmse(test_data$salary, predict.model_drzewo)
rmse.model_drzewo

# COMMAND ----------

install.packages("rpart.plot")
library(rpart.plot)

# COMMAND ----------

plotcp(model_drzewo)

# COMMAND ----------

fancyRpartPlot(model_drzewo, cex=0.50)

# COMMAND ----------

# MAGIC %md
# MAGIC # Podsumowanie
# MAGIC

# COMMAND ----------

# Podjęta w niniejszym badaniu próba określenia wpływu wybranych uwarunkowań na wysokość wynagrodznia wykazała istotny wpływ wszystkich badanych zmiennych. Typ stanowiska, kwalifikacje zawodowe, branża, doświadczenie zawodowe i odległość od miasta wpływają na wysokość wynagrodzenia.
