ess.data <- read.csv("ess_messy_extract.csv")

#Part I: Demographics

#Part II: Political variables

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