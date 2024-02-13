#chargement des librairies nécessaires
library(readxl)
set.seed(2024)
#Importation de la base
setwd("C:/Users/21806538/Downloads")
base <- read_excel("base.xlsx", na = c("",".","Non renseigné"))
base <- base[base$statut_juridique=="Personne physique",]
hist(base$age, breaks = 137)
max(na.omit(base$age))
base <- base[(base$age > 18) & (base$age < 80), ]
base <- base[(!(is.na(base$matricule))),]

#Nombre de valeurs non définies
sum(is.na(base))

#Statistiques de base

#top_def_12m_90j (variable à prédire)
top_def_12m_90j_S <- as.matrix(sort(summary(as.factor(base$top_def_12m_90j))))
colnames(top_def_12m_90j_S)<-"Nombre de matricules"
print(top_def_12m_90j_S)

#CSP class
CSP_S <- as.matrix(sort(summary(as.factor(base$CSP))))
colnames(CSP_S)<-"Nombre de matricules"
print(CSP_S)

#Situation Matrimoniale
sit_matrimonial_S <- as.matrix(sort(summary(as.factor(base$sit_matrimonial))))
colnames(sit_matrimonial_S)<-"Nombre de matricules"
print(sit_matrimonial_S)

#Statut Economique
statut_eco_S <- as.matrix(sort(summary(as.factor(base$statut_eco))))
colnames(statut_eco_S)<-"Nombre de matricules"
print(statut_eco_S)

#Situation Familiale
sit_familiale_S <- as.matrix(sort(summary(as.factor(base$sit_familiale))))
colnames(sit_familiale_S)<-"Nombre de matricules"
print(sit_familiale_S)

summary(as.factor(base[base$capital_restant_du=="0.00",]$anciennete_pret))

#base_161<-base[base$capital_restant_du=="0.00",]
#base_161<-base_161[((base_161$anciennete_pret==161) & (!(is.na(base_161$anciennete_pret)))),]

#base_162<-base[base$capital_restant_du=="0.00",]
#base_162<-base_162[((base_162$anciennete_pret==162) & (!(is.na(base_162$anciennete_pret)))),]

#base_193<-base[base$capital_restant_du=="0.00",]
#base_193<-base_193[((base_193$anciennete_pret==193) & (!(is.na(base_193$anciennete_pret)))),]

#base_211<-base[base$capital_restant_du=="0.00",]
#base_211<-base_211[((base_211$anciennete_pret==211) & (!(is.na(base_211$anciennete_pret)))),]

##############
##Resampling##
##############

#Nombre d'observation en 20% et 80%
num_obs_80_percent <- nrow(base[base$top_def_12m_90j==0,])
num_obs_20_percent <- round(num_obs_80_percent * 0.25)

#Sous-bases
base_80_percent <- base[base$top_def_12m_90j == 0, ]
base_resampled_20_percent <- base[sample(which(base$top_def_12m_90j==1),size=num_obs_20_percent,replace = T),]

#Base résultante
base_resampled <- rbind(base_resampled_20_percent, base_80_percent)
base_resampled <- base_resampled[sample(nrow(base_resampled)), ]

#vérification des proportions
summary(as.factor(base_resampled$top_def_12m_90j))/nrow(base_resampled)
summary(as.factor(base_resampled$top_def_12m_90j))

####################
##Train-Test split##
####################

library(caret)
library(ggplot2)

index <- createDataPartition(base_resampled$top_def_12m_90j, p = 0.7, list = FALSE)
train_set <- base_resampled[index, ]
test_set <- base_resampled[-index, ]

#vérification des proportions
summary(as.factor(train_set$top_def_12m_90j))/nrow(train_set)
summary(as.factor(test_set$top_def_12m_90j))/nrow(test_set)

summary(as.factor(train_set$top_def_12m_90j))
summary(as.factor(test_set$top_def_12m_90j))



install.packages('vcd')
library(vcd)


for (i in 1:20){
  coupure = seq(18, 80, i)
  age_c = cut(base_resampled$age, breaks = coupure, include.lowest = TRUE)
  age_c_1 = cut(base_resampled[base_resampled$top_def_12m_90j == 1,]$age, breaks = coupure, include.lowest = TRUE)
  percent_class = table(age_c)/table(age_c_1)
  condition = sum(!(percent_class>=0.05))== 0
  
  decoupage_cramer = c()
  if ( condition == T ) {
    con_table = table(age_c, base_resampled$top_def_12m_90j)
    result = assocstats(con_table)
    decoupage_cramer[i] = result$cramer
  }
}

meilleur_coupure = which(decoupage_cramer ==  max(decoupage_cramer))
