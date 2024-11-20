main <- function() {
    library(dplyr)

    # Setting up data frame properly
    data <- read.csv("linear_model_1.csv")
    names <- data$X
    rownames(data) <- names
    data <- data[-1]
    colnames(data)[4] <- "p_value"

    # Ordering data frame with respect to p-values to prepare for Benjamini-Yekutieli procedure
    data <- arrange(data, p_value)

    # Benjamini-Yekutieli procedure
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

    # Creating column describing whether or not the null hypothesis is rejected and writing the result to csv
    is_rejected <- append(rep('Rejected', k), rep('Not rejected', m - k))
    data <- mutate(data, BY_test_result = is_rejected)
    write.csv(data, 'Benjamini_Yekutieli.csv')

    return(data)
}

main()
