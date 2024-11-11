panel_data <- read.csv("panel_data.csv")

panel_data %>%
    group_by(KommuneNavn) %>%
    arrange(Salgsaar) %>%
    mutate(Areal_Bolig = Areal_Bolig - lag(Areal_Bolig), Salgstid = Salgstid - lag(Salgstid), Pris_Salg = Pris_Salg - lag(Pris_Salg), Areal_Grund = Areal_Grund - lag(Areal_Grund), StorGrund = StorGrund - lag(StorGrund), Dist_skole = Dist_skole - lag(Dist_skole), Dist_raadhus = Dist_raadhus - lag(Dist_raadhus), Alder = Alder - lag(Alder), AntalFremvisninger - lag(AntalFremvisninger), Areal_GarageCarport = Areal_GarageCarport - lag(Areal_GarageCarport), Ejd_AntalPlan = Ejd_AntalPlan - lag(Ejd_AntalPlan)) %>%
    arrange(KommuneNavn) %>%
    subset(Salgsaar != 2011) %>%
    write.csv("first_differenced_panel_data.csv")


# diff = Areal_Bolig - lag(Areal_Bolig)
