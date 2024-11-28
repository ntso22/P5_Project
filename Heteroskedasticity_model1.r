library(dplyr)

load('HomeData.rda')

vars <- c(
        "Pris_Salg",
        "Areal_Bolig",
        "Areal_Grund",
        "Salgsaar",
        "StorGrund",
        "Dist_skole",
        "Dist_raadhus",
        "Alder",
        "AntalFremvisninger",
        "Areal_GarageCarport",
        "Ejd_AntalPlan",
        "Salgstid",
        "KommuneNavn"
)

cities <- c(
        "Aalborg",
        "Odense",
        "Aarhus",
        "København"
)

# Cleaning data and treating Salgsaar as a qualitative variable, removing outliers using multiple hypothesis test
DataSet <- homedata %>%
    select(vars) %>%
    na.omit() %>%
    mutate(Salgsaar = as.character(Salgsaar)) %>%
    subset(KommuneNavn %in% cities) %>%
    select(-KommuneNavn)

model <- lm(formula = Pris_Salg ~ Salgsaar + 
        Areal_Bolig + Salgsaar * Areal_Bolig + 
        Areal_Grund + Salgsaar * Areal_Grund + 
        Dist_skole + Salgsaar * Dist_skole + 
        Dist_raadhus + Salgsaar * Dist_raadhus + 
        Alder + Salgsaar * Alder + 
        AntalFremvisninger + Salgsaar * AntalFremvisninger + 
        Areal_GarageCarport + Salgsaar * Areal_GarageCarport + 
        Ejd_AntalPlan + Salgsaar * Ejd_AntalPlan + 
        Salgstid + Salgsaar * Salgstid, 
        data = DataSet)

summary <- summary(model)

H_diag <- influence(model)$hat

#Standardising residuals and adding to data frame
p_values <- 2 * (1 - pnorm(abs(model$residuals / (summary$sigma * sqrt(1 - H_diag))))) # Remember the definition of standardised residuals
DataSet <- DataSet %>% mutate(p_value = p_values) %>%
        arrange(p_value)
   
m <- nrow(DataSet)
alpha <- .05
k <- 0
harmonic <- sum(1 / (1:m))
for (i in 1:m) {
        alpha_i <- i * alpha / (m * harmonic)
        if (DataSet$p_value[i] <= alpha_i) {
            k <- i
        }
}
DataSet_1 <- DataSet[-(1:k),] # Observations corresponding to rejected hypotheses are removed from the data set

    # The model is constructed from the data not containing outliers
    model_1 <- lm(formula = Pris_Salg ~ Salgsaar + 
        Areal_Bolig + Salgsaar * Areal_Bolig + 
        Areal_Grund + Salgsaar * Areal_Grund + 
        Dist_skole + Salgsaar * Dist_skole + 
        Dist_raadhus + Salgsaar * Dist_raadhus + 
        Alder + Salgsaar * Alder + 
        AntalFremvisninger + Salgsaar * AntalFremvisninger + 
        Areal_GarageCarport + Salgsaar * Areal_GarageCarport + 
        Ejd_AntalPlan + Salgsaar * Ejd_AntalPlan + 
        Salgstid + Salgsaar * Salgstid, 
        data = DataSet_1)

par(mfrow=c(2,2))
plot(model_1)

#Extracting the residuals and squaring each entry

model_1_residuals <- (residuals(model_1))^2

#Executing the Breusch-Pagan test

Breusch_Pagan_1 <- lm(formula = model_1_residuals ~ Salgsaar + 
        Areal_Bolig + Salgsaar * Areal_Bolig + 
        Areal_Grund + Salgsaar * Areal_Grund + 
        Dist_skole + Salgsaar * Dist_skole + 
        Dist_raadhus + Salgsaar * Dist_raadhus + 
        Alder + Salgsaar * Alder + 
        AntalFremvisninger + Salgsaar * AntalFremvisninger + 
        Areal_GarageCarport + Salgsaar * Areal_GarageCarport + 
        Ejd_AntalPlan + Salgsaar * Ejd_AntalPlan + 
        Salgstid + Salgsaar * Salgstid, 
        data = DataSet_1)

summary(Breusch_Pagan_1)
