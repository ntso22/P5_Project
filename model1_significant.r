BY <- function(data, ftest) {
    m <- nrow(data)
    alpha <- .05
    k <- 0
    harmonic <- sum(1 / (1:m))
    for (i in 1:m) {
        alpha_i <- i * alpha / (m * harmonic)
        if (data$p_value[i] <= alpha_i) {
            k <- i
        }
    }
    if (ftest == FALSE) {
        data <- data[-(1:k),]
    } else {
        data <- data[1:k,] 
    }

    return (data)
}

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
        mutate(Salgsaar = as.character(Salgsaar), Pris_Salg = log(Pris_Salg)) %>%
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
   
    DataSet <- BY(DataSet, FALSE) # Observations corresponding to rejected hypotheses are removed from the data set

    write.csv(DataSet, 'clean_data1.csv')

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

    quantitative <- c(
        "Areal_Bolig",
        "Areal_Grund",
        "StorGrund",
        "Dist_skole",
        "Dist_raadhus",
        "Alder",
        "AntalFremvisninger",
        "Areal_GarageCarport",
        "Ejd_AntalPlan",
        "Salgstid"
    )
    qualitative <- c(
        "Salgsaar"
    )
    interaction <- c()
    for (i in 1:length(qualitative)) {
        for (j in 1:length(quantitative)) {
            index <- (i - 1) * length(quantitative) + j
            interaction[index] <- paste(qualitative[i], quantitative[j], sep=':')
        }
    }
    response <- "Pris_Salg"
    explanatory <- append(append(qualitative, quantitative), interaction)

    n <- length(explanatory)

    final_df <- data.frame(variable = c(), p_value = c())

    for (i in 1:(n-1)) {
        p_values <- c()
        for (j in 1:length(explanatory)) {
            variable <- explanatory[j]
            formula1 <- as.formula(
                paste("Pris_Salg", paste(explanatory, collapse=' + '), sep=' ~ ')
            )
            formula2 <- as.formula(
                paste("Pris_Salg", paste(explanatory[-j], collapse=' + '), sep=' ~ ')
            )

            # Skal lige have fjernet outliers her
            model1 <- lm(formula=formula1, data=DataSet)
            model2 <- lm(formula=formula2, data=DataSet)

            p_value <- anova(model1, model2, test='F')$'Pr(>F)'[2]
            p_values[j] <- p_value
        }
        
        current_df <- data.frame(variable = explanatory, p_value = p_values) %>%
            arrange(desc(p_value))

        final_df[i, 1:2] <-  current_df[1, 1:2]


        explanatory <- explanatory[! explanatory == current_df[1, 1]]
        
        # Bare lige så man kan følge med i hvor langt den er
        progress <- (n * (n + 1) - (n-i) * (n-i + 1))/((n * (n + 1)) - 1)
        print(paste('Progress: ', progress))
    }
    final_df <- final_df %>% arrange(p_value)
    # Benjamini-Yekutieli procedure
    final_df <- BY(final_df, TRUE)

    # Make final model

    final_formula <- as.formula(
        paste(
            paste("Pris_Salg", paste(final_df$variable, collapse=' + '), sep=' ~ ')
        )
    )
    final_model <- lm(formula=final_formula, data=DataSet)
    return(summary(final_model))
}
main()
