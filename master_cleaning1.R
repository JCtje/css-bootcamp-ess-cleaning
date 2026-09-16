ess.data <- read.csv("ess_messy_extract.csv")

install.packages("questionr")
#Part I: Demographics
table (ess.data$gndr)
ess.data$gndr_clean <- ess.data$gndr
ess.data$gndr_clean[!(ess.data$gndr_clean %in% c(1, 2))] <- NA
table (ess.data$gndr_clean)

table (ess.data$agea)
ess.data$agea_clean <- ess.data$agea
ess.data$agea_clean[ess.data$agea_clean == 999] <- NA
ess.data$agea_clean[ess.data$agea_clean < 0] <- NA
ess.data$agea_clean[ess.data$agea_clean > 120] <- NA
table (ess.data$agea_clean)

table (ess.data$eduyrs)
ess.data$eduyrs_clean <- ess.data$eduyrs
ess.data$eduyrs_clean[ess.data$eduyrs_clean %in% c(77, 88, 99)] <- NA
ess.data$eduyrs_clean[ess.data$eduyrs_clean < 0] <- NA
ess.data$eduyrs_clean[ess.data$eduyrs_clean > 70] <- NA
table (ess.data$eduyrs_clean)
range(ess.data$eduyrs_clean, na.rm = TRUE)
#Part II: Political variables
css.data<-read.csv("ess_messy_extract.csv")
View(css.data)

library(dplyr)
library(forcats)
class(css.data$prtclgfr)

css.data <- css.data %>%
 mutate(
    prtclgfr = case_when(
      prtclgfr == 1  ~ "PCF (Parti Communiste Français)",
      prtclgfr == 2  ~ "FI (La France Insoumise)",
      prtclgfr == 3  ~ "PS (Parti Socialiste)",
      prtclgfr == 4  ~ "EELV (Europe Écologie Les Verts)",
      prtclgfr == 5  ~ "Renaissance (ex La République en Marche !)",
      prtclgfr == 6  ~ "Horizons",
      prtclgfr == 7  ~ "MODEM (Mouvement Démocrate)",
      prtclgfr == 8  ~ "LR (Les Républicains)",
      prtclgfr == 9  ~ "Le Rassemblement National (RN, ex Front National)",
      prtclgfr == 10 ~ "Reconquête !",
      prtclgfr == 31 ~ "Autre",
      prtclgfr == 66 ~ "Not applicable",
      prtclgfr == 77 ~ "Refusal",
      prtclgfr == 88 ~ "Don't know",
      prtclgfr == 99 ~ "No answer",
      TRUE        ~ as.character(prtclgfr) # Sécurité pour conserver les données manquantes ou hors liste
    ),
    # Conversion finale en facteur pour les analyses statistiques
    prtclgfr = as.factor(prtclgfr) 
  )


table(css.data$prtclgfr)

#Part III: Trust variables
library(questionr)

ess.data$trstprl_character <- as.character(ess.data$trstprl)
is.numeric(ess.data$trstprl_character)
na.omit(ess.data$trstprl_character)
irec(ess.data$trstprl_character)

## Recoding ess.data$trstprl_character into ess.data$trstprl_character_rec
ess.data$trstprl_character_rec <- ess.data$trstprl_character
ess.data$trstprl_character_rec[ess.data$trstprl_character == "20"] <- "2"
ess.data$trstprl_character_rec[ess.data$trstprl_character == "30"] <- "3"
ess.data$trstprl_character_rec[ess.data$trstprl_character == "40"] <- "4"
ess.data$trstprl_character_rec[ess.data$trstprl_character == "50"] <- "5"
ess.data$trstprl_character_rec[ess.data$trstprl_character == "60"] <- "6"
ess.data$trstprl_character_rec[ess.data$trstprl_character == "70"] <- "7"
ess.data$trstprl_character_rec[ess.data$trstprl_character == "77"] <- NA
ess.data$trstprl_character_rec[ess.data$trstprl_character == "770"] <- NA
ess.data$trstprl_character_rec[ess.data$trstprl_character == "80"] <- "8"
ess.data$trstprl_character_rec[ess.data$trstprl_character == "88"] <- NA
ess.data$trstprl_character_rec[ess.data$trstprl_character == "880"] <- NA
ess.data$trstprl_character_rec[ess.data$trstprl_character == "99"] <- NA

ess.data$trstprl_numeric <- as.numeric(ess.data$trstprl_character_rec)
is.numeric(ess.data$trstprl_numeric)
table(ess.data$trstprl_numeric)

mean(ess.data$trstprl_numeric)
summary(ess.data$trstprl_numeric)

# Mean trust score in the parliament : 4.153
# Additional infos : Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#                   0.000   2.000   5.000   4.153   6.000  10.000     150