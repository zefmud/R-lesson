# Завантажуємо потрібні бібліотеки
library(dplyr)
library(ggplot2)
library(ggthemes)

# Встановлюємо робочу директорію
setwd("/home/pavlo/R-lesson")

# Вантажимо файли
download.file('http://opendata.rada.gov.ua/?q=node/871/download', 'bills.csv')
download.file('http://opendata.rada.gov.ua/?q=node/793/download', 'initiators.csv')
download.file('http://opendata.rada.gov.ua/?q=node/636/download', 'associations.csv')
download.file('http://opendata.rada.gov.ua/?q=node/648/download', 'mp_factions.csv')
download.file('http://opendata.rada.gov.ua/?q=node/652/download', 'mps.csv')

# Читаємо завантажені файли
options(stringsAsFactors = FALSE)
bills <- read.csv('bills.csv')
initiators <- read.csv('initiators.csv')
associations <- read.csv('associations.csv')
mps_associations <- read.csv('mp_factions.csv')
mps <- read.csv('mps.csv')

# Обираємо проекти законів із масиву законопроектів
bills <- filter(bills, type == "Проект Закону")

# З усіх асоціацій обираємо фракції
associations <- filter(associations, is_fr == 1)

# В усіх табличках лишаємо лише потрібні нам змінні
initiators <- select(initiators, bill_id, person)
mps <- select(mps, fullname:id)
bills <- select(bills, bill_id, currentPhase_title)
mps_associations <- select(mps_associations, mp_id, fr_association_id)
associations <- select(associations, id, name)

# Отримуємо потрібну нам таблицю з завантажених таблиць
mps_associations <- inner_join(mps_associations, associations, by = c('fr_association_id' = 'id'))
initiators <- inner_join(initiators, mps, by = c('person' = 'fullname'))
initiators <- inner_join(initiators, mps_associations, by = c('id' = 'mp_id'))
bills <- inner_join(bills, initiators, by = 'bill_id')

# "Чистимо" назви фракцій
bills$name[grepl("Батьківщина", bills$name)] <- "Батьківщина"
bills$name[grepl("ПОРОШЕНКА", bills$name)] <- "БПП"
bills$name[grepl("Ляшка", bills$name)] <- "Радикальна партія"
bills$name[grepl("ФРОНТ", bills$name)] <- "Народний Фронт"
bills$name[grepl("Опозиційний", bills$name)] <- "Опоблок"
bills$name[grepl("Відродження", bills$name)] <- "Відродження"
bills$name[grepl("Воля", bills$name)] <- "Воля народу"
bills$name[grepl("САМОПОМІЧ", bills$name)] <- "Самопоміч"

#Виділяємо статуси із малою кількістю законопроектів в групу "Інше"
major_statuses <- c(names(sort(table(bills$currentPhase_title), decreasing = TRUE)[1:7]), "Інше")
bills$currentPhase_title[!(bills$currentPhase_title %in% major_statuses)] <- "Інше"
bills$currentPhase_title<- factor(bills$currentPhase_title, levels = major_statuses)

# Отримуємо табличку ініціювання фракціями законопроектів
phases_by_factions <- 
  bills %>%
    group_by(name, currentPhase_title) %>%
    mutate(phase_bill_number = n()) %>%
    select(name, currentPhase_title, phase_bill_number) %>%
    distinct(.keep_all = TRUE) %>%
    group_by(name) %>%
    mutate(faction_bill_number = sum(phase_bill_number)) %>%
    select(name, currentPhase_title, phase_bill_number, faction_bill_number) %>%
    mutate(percent = round((phase_bill_number / faction_bill_number) * 100) )

#Створюємо кольорову шкалу
cols = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Будуємо графік
 faction_plot <- ggplot(phases_by_factions, aes(x = currentPhase_title, y = faction_bill_number, fill = currentPhase_title)) +
   geom_bar(aes(y = phase_bill_number), stat = 'identity',position = "dodge") +
   theme_minimal() +
   facet_grid(~name) +
   scale_fill_manual(values = cols)+
   theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
   theme(legend.title = element_blank(), legend.position="bottom") +
   labs(y = "Кількість проектів законів")
   


