install.packages('readxl')
library(readxl)

setwd("C:/Users/21806538/Downloads")
base <- read_xlsx('base.xlsx', na = c(",", ".", "Non renseigné"))
base <- base[base$statut_juridique == "Personne physique", 1:99]
base <- base[(base$age >= 18) & (base$age <= 80),]


summary(as.factor(base$top_def_12m_90j))

base$statut_juridique
sum(is.na(base))


table(base$CSP)
summary(base)
paste("Na :", sum(is.na(base$CSP)))

as.matrix(sort(summary(as.factor(base$CSP))))
base[c("CSP", "sit_familiale")]
summary(as.factor(base$top_compte_joint))

summary(as.factor(base$top_DAV))


sample()

#les étapes:
#population
#cirètre à expliquer tcp.def impaye sampling
#split train/test
#preselection
#Analyse varibales