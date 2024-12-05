# Benjamini Yekutieli procedure
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
    if (m <= 10) {
        print(paste('m = ', m))
        print(data)
    }
    if (ftest == FALSE) {
        data <- data[-(1:k),]
        return (data)
    } else {
        data <- data[1:k,]
        return (data)
    }
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
        "Aarhus",
        "Odense",
        "København"
    )

    # Cleaning data and treating Salgsaar as a qualitative variable, removing outliers using multiple hypothesis test
    DataSet <- homedata %>%
        select(vars) %>%
        na.omit() %>%
        mutate(Salgsaar = as.character(Salgsaar), Pris_Salg = log(Pris_Salg)) %>%
        subset(KommuneNavn %in% cities)

    # Creating initial model
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

    #Standardising residuals and adding to data frame
    p_values <- 2 * (1 - pnorm(abs(model$residuals / (summary$sigma * sqrt(1 - H_diag))))) # Remember the definition of standardised residuals
    DataSet <- DataSet %>% mutate(p_value = p_values) %>%
        arrange(p_value)
   
    DataSet <- BY(DataSet, FALSE)

    write.csv(DataSet, 'clean_data2.csv')

    # The model is constructed from the data not containing outliers
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

    # Quantitative and qualitative variables are defined, such that the correct interaction terms can also be defined
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
        "Salgsaar",
        "KommuneNavn"
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

    # Each null hypothesis is evaluated.
    p_values <- c()

    control_formula <- as.formula(
            paste("Pris_Salg", paste(explanatory, collapse=' + '), sep=' ~ ')
    )
    model1 <- lm(formula=control_formula, data=DataSet)

    for (j in 1:n) {
        variable <- explanatory[j]
        
        if (grepl(":", variable) | variable == "Salgsaar" | variable == "KommuneNavn") {
            formula <- as.formula(
                paste("Pris_Salg", paste(explanatory[-j], collapse=' + '), sep=' ~ ')
            )
            model2 <- lm(formula=formula, data=DataSet)

            p_value <- anova(model2, model1, test='F')$'Pr(>F)'[2]
        } else {
            p_value <- summary(model1)$coefficients[variable, "Pr(>|t|)"]
        }
                       
        p_values[j] <- p_value
    }

    # Including iteration that tests the significance of the intercept
    p_values[n+1] <- summary(model1)$coefficients["(Intercept)", "Pr(>|t|)"]
    explanatory <- append(explanatory, "(Intercept)")

    # Using Benjamini Yekutieli procedure to determine significance of null hypotheses
    df <- data.frame(variable = explanatory, p_value = p_values) %>%
        arrange(p_value)

    updated_df <- BY(df, TRUE)

    print(updated_df)

    # Exclude interaction terms where main effects are insignificant
    final_variables <- c()
    for (variable in updated_df$variable) {
        if (! grepl(":", variable)) {
            final_variables <- append(final_variables, variable)
            next
        }
        main_variables <- strsplit(variable, ":")[[1]]
        condition <- main_variables %in% updated_df$variable
        if (condition[1] && condition[2]) {
            final_variables <- append(final_variables, variable)
        } else {
            print(
                paste(variable, " was discarded due to omitted main variable(s)")
            )
        }
    }

    # Make final model
    final_formula <- paste(
            paste("Pris_Salg", paste(final_variables[! final_variables == "(Intercept)"], collapse=' + '), sep=' ~ ')
    )

    if (! "(Intercept)" %in% final_variables) {
        final_formula <- paste(final_formula_string, "-1")
    }
    
    final_model <- lm(formula=as.formula(final_formula), data=DataSet)
    return(summary(final_model))
}
main()
