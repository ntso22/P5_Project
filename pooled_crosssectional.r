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
        "Aarhus",
        "Odense",
        "København"
    )

    # Cleaning data and treating Salgsaar as a qualitative variable
    DataSet <- homedata %>%
        select(vars) %>%
        na.omit() %>%
        mutate(Salgsaar = as.character(Salgsaar)) %>%
        subset(KommuneNavn %in% cities)

    # Possibly need to combine Salgsaar and KommuneNavn as well
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
    write.csv(summary$coefficients, 'linear_model_1.csv')
    return (summary)
}