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
        "Aarhus",
        "Odense",
        "København"
)

# Cleaning data and treating Salgsaar as a qualitative variable, removing outliers using multiple hypothesis test
DataSet <- homedata %>%
        select(vars) %>%
        na.omit() %>%
        mutate(Salgsaar = as.character(Salgsaar)) %>%
        subset(KommuneNavn %in% cities)

model <- lm(formula = Pris_Salg ~ Salgsaar +  KommuneNavn + 
        Areal_Bolig + Salgsaar * Areal_Bolig + KommuneNavn * Areal_Bolig + 
        Areal_Grund + Salgsaar * Areal_Grund + KommuneNavn * Areal_Grund +
        Dist_skole + Salgsaar * Dist_skole + KommuneNavn * Dist_skole +
        Dist_raadhus + Salgsaar * Dist_raadhus + KommuneNavn * Dist_raadhus +
        Alder + Salgsaar * Alder + KommuneNavn * Alder +
        AntalFremvisninger + Salgsaar * AntalFremvisninger + KommuneNavn  * AntalFremvisninger +
        Areal_GarageCarport + Salgsaar * Areal_GarageCarport + KommuneNavn * Areal_GarageCarport +
        Ejd_AntalPlan + Salgsaar * Ejd_AntalPlan + KommuneNavn * Ejd_AntalPlan +
        Salgstid + Salgsaar * Salgstid + KommuneNavn * Salgstid, 
        data = DataSet)

summary <- summary(model)

H_diag <- influence(model)$hat

print(head(H_diag))

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
DataSet_2 <- DataSet[-(1:k),] # Observations corresponding to rejected hypotheses are removed from the data set


    # The model is constructed from the data not containing outliers
    model_2 <- lm(formula = Pris_Salg ~ Salgsaar +  KommuneNavn + 
        Areal_Bolig + Salgsaar * Areal_Bolig + KommuneNavn * Areal_Bolig + 
        Areal_Grund + Salgsaar * Areal_Grund + KommuneNavn * Areal_Grund +
        Dist_skole + Salgsaar * Dist_skole + KommuneNavn * Dist_skole +
        Dist_raadhus + Salgsaar * Dist_raadhus + KommuneNavn * Dist_raadhus +
        Alder + Salgsaar * Alder + KommuneNavn * Alder +
        AntalFremvisninger + Salgsaar * AntalFremvisninger + KommuneNavn  * AntalFremvisninger +
        Areal_GarageCarport + Salgsaar * Areal_GarageCarport + KommuneNavn * Areal_GarageCarport +
        Ejd_AntalPlan + Salgsaar * Ejd_AntalPlan + KommuneNavn * Ejd_AntalPlan +
        Salgstid + Salgsaar * Salgstid + KommuneNavn * Salgstid, 
        data = DataSet_2)

par(mfrow=c(2,2))
plot(model_2)

#Extracting the residuals and squaring each entry

model_2_residuals <- (residuals(model_2))^2

#Executing the Breusch-Pagan test
Breusch_Pagan_2 <- lm(formula = model_2_residuals ~ -1 + Salgsaar + KommuneNavn + 
        Areal_Bolig + Salgsaar * Areal_Bolig + KommuneNavn * Areal_Bolig + 
        Areal_Grund + Salgsaar * Areal_Grund + KommuneNavn * Areal_Grund +
        Dist_skole + Salgsaar * Dist_skole + KommuneNavn * Dist_skole +
        Dist_raadhus + Salgsaar * Dist_raadhus + KommuneNavn * Dist_raadhus +
        Alder + Salgsaar * Alder + KommuneNavn * Alder +
        AntalFremvisninger + Salgsaar * AntalFremvisninger + KommuneNavn  * AntalFremvisninger +
        Areal_GarageCarport + Salgsaar * Areal_GarageCarport + KommuneNavn * Areal_GarageCarport +
        Ejd_AntalPlan + Salgsaar * Ejd_AntalPlan + KommuneNavn * Ejd_AntalPlan +
        Salgstid + Salgsaar * Salgstid + KommuneNavn * Salgstid, 
        data = DataSet_2)

summary(Breusch_Pagan_2)
