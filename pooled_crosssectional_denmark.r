main <- function() {
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
    DataSet <- DataSet[-(1:k),] # Observations corresponding to rejected hypotheses are removed from the data set

    write.csv(DataSet, 'clean_data2.csv')

    # The model is constructed from the data not containing outliers
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

    write.csv(summary$coefficients, 'linear_model_1.csv')
    return (summary)
}

main()
