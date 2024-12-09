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
    
    if (grepl(":", variable) | variable == "Salgsaar" | variable == "KommuneNavn") {
      p_value <- anova(model2, model1, test='F')$'Pr(>F)'[2]
    } else {
      p_value <- summary(model1)$coefficients[variable, "Pr(>|t|)"]
    }
    
    p_values[j] <- p_value
  }
  
  df <- data.frame(variable = explanatory, p_value = p_values) %>%
    arrange(p_value)
  
  updated_df <- BY(df, TRUE)
  
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
  print(updated_df$variable)
  
  # Make final model
  final_formula <- as.formula(
    paste(
      paste("Pris_Salg", paste(final_variables, collapse=' + '), sep=' ~ ')
    )
  )
  final_model <- lm(formula=final_formula, data=DataSet)
  return(final_model)
}
x = main() #Take the linear regression model

#Defining new variables that needs to be predicted and determine the 95% prediction intervals
hus_1_2015_obs = data.frame(
  "Areal_Bolig" = 175,
  "Salgsaar" = as.factor(2015),
  "KommuneNavn" = "Aalborg",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0
)
hus_1_2018_obs = data.frame(
  "Areal_Bolig" = 175,
  "Salgsaar" = as.factor(2018),
  "KommuneNavn" = "Aalborg",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0
)
hus_1_2020_obs = data.frame(
  "Areal_Bolig" = 175,
  "Salgsaar" = as.factor(2020),
  "KommuneNavn" = "Aalborg",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0
)
hus_2_2014_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2014),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)

hus_2_2017_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2017),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)

hus_2_2020_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2020),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0
)
#Create a dataframe with all the new observed values
new_obs_DF = data.frame(rbind(hus_1_2015_obs,hus_1_2018_obs,hus_1_2020_obs,hus_2_2014_obs,hus_2_2017_obs,hus_2_2020_obs))
#Predict the price and the 95% prediction intervals
prediction_intervals = exp(predict(x, newdata = new_obs_DF, interval = "prediction", level = 0.95))

#Create prediction intervals and plots for hus_2 troughout the period.
hus_2_2011_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2011),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)
hus_2_2012_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2012),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)
hus_2_2013_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2013),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)
hus_2_2014_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2014),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)
hus_2_2015_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2015),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)
hus_2_2016_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2016),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)
hus_2_2017_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2017),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)
hus_2_2018_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2018),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)
hus_2_2019_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2019),
  "KommuneNavn" = "København",
  "Ejd_AntalPlan" = 1,
  "StorGrund" = 0)
hus_2_2020_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2020),
  "Ejd_AntalPlan" = 1,
  "KommuneNavn" = "København",
  "StorGrund" = 0
)
hus_2_2021_obs = data.frame(
  "Areal_Bolig" = 80,
  "Salgsaar" = as.factor(2021),
  "Ejd_AntalPlan" = 1,
  "KommuneNavn" = "København",
  "StorGrund" = 0
)
hus_2_2022_obs = data.frame(
  "Areal_Bolig" = 80,
  "Ejd_AntalPlan" = 1,
  "Salgsaar" = as.factor(2022),
  "KommuneNavn" = "København",
  "StorGrund" = 0
)
#Create a dataframe with all the new observed values
new_obs_DF = data.frame(rbind(hus_2_2011_obs,hus_2_2012_obs,hus_2_2013_obs,hus_2_2014_obs,hus_2_2015_obs,hus_2_2016_obs,hus_2_2017_obs,hus_2_2018_obs,hus_2_2019_obs,hus_2_2020_obs,hus_2_2021_obs,hus_2_2022_obs))
#Predict the price and the 95% prediction intervals
prediction_intervals = data.frame(Index = rbind(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022),
                                  exp(predict(x, newdata = new_obs_DF, interval = "prediction", level = 0.95)))
actual_price = data.frame(Index = rbind(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022),
                          Price = rbind(NA,NA,NA,2680000,NA,NA,4795000,NA,NA,4375000,NA,NA))

library(ggplot2)

ggplot(prediction_intervals, aes(x = Index)) +
  geom_line(aes(y = lwr), color = "red") +    # Lower prediction interval
  geom_line(aes(y = upr), color = "blue") +   # Upper prediction interval
  geom_point(data = actual_price, aes(x = Index, y = Price), color = "green") +  # Actual prices
  labs(x = "Index", y = "Value", title = "Plot of Predicted Values and Actual Prices" ) +
  scale_x_continuous(breaks = seq(2011, 2022, by = 1)) +
  theme_minimal()
