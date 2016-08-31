# Завантажуємо потрібну бібліотеку
library(dplyr)

# Встановлюємо робочу директорію
setwd("/home/pavlo/R-lesson")

# Вантажимо потрібні файли
download.file('http://opendata.rada.gov.ua/?q=node/871/download', 'bills.csv')
download.file('http://opendata.rada.gov.ua/?q=node/796/download', 'committees.csv')

# Читаємо завантажені файли
options(stringsAsFactors = FALSE)
bills <- read.csv('bills.csv')
committees <- read.csv('committees.csv')

# Лишаємо лише потрібні стовпчики
bills <- select(bills, bill_id, currentPhase_title)
committees <- select(committees, bill_id, department)

# Отримуємо потрібну нам таблицю з завантажених таблиць
bills_by_committee <- inner_join(bills, committees, by = 'bill_id')

# Отримуємо табличку із статистикою по комітетах
percent_committees <-
  bills_by_committee %>%
    group_by(department, currentPhase_title) %>%
    mutate(phase_bills_number = n()) %>%
    select(department, currentPhase_title, phase_bills_number) %>%
    distinct(.keep_all = TRUE) %>%
    group_by(department) %>%
    mutate(committee_bills_number = sum(phase_bills_number)) %>%
    mutate(percent = round((phase_bills_number / committee_bills_number) * 100)) %>%
    filter(currentPhase_title == "Опрацьовується в комітеті")

  
  
  
  


