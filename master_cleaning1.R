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

#Part III: Trust variables
