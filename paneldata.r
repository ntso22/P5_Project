library(dplyr)

load(file = "HomeData.rda")

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

panel_data <- homedata %>%
    select(vars) %>%
    na.omit() %>%
    group_by(KommuneNavn, Salgsaar) %>%
    summarise(Areal_Bolig = mean(Areal_Bolig), Salgstid = mean(Salgstid), Pris_Salg = mean(Pris_Salg), Areal_Grund = mean(Areal_Grund), StorGrund = mean(StorGrund), Dist_skole = mean(Dist_skole), Dist_raadhus = mean(Dist_raadhus), Alder = mean(Alder), AntalFremvisninger = mean(AntalFremvisninger), Areal_GarageCarport = mean(Areal_GarageCarport), Ejd_AntalPlan = mean(Ejd_AntalPlan))

municipalities <- panel_data %>%
    select(KommuneNavn) %>%
    group_by(KommuneNavn) %>%
    summarise()

for (municipality in municipalities$KommuneNavn) {
    years <- nrow(subset(panel_data, KommuneNavn == municipality))
    if (years < 12) {
        panel_data <- panel_data %>% subset(KommuneNavn != municipality) %>% subset(Salgsaar != 2010)
    }
}

write.csv(panel_data, "panel_data.csv")
