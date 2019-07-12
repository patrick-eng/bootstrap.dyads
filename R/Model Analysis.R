#' Analysis of bootstrapped dyad-ratios loading scores
#'
#' Runs post-hoc analysis of bootstrapped dyad-ratios model to identify poorly-fitting and unreliable variable items and fit a bootstrap-suggested dyad-ratios model.
#'
#' @param model Define the name of the object in which bootstrap results are stored.
#' @param max.diff Define the maximum acceptable difference between single-estimated and bootstrapped mean loading scores for items to be passed into the final model. Default is 0.1.
#' @param threshold Define the lower limit of bootstrapped loading scores for items to be passed into the final model. Default is 0.4.
#' @param print Logical. Define whether summary of analysis should be returned to the console.
#'
#' @details This function runs the dyad-ratios bootstrap model, which takes the results of a single dyad-ratios estimation outcome and produces bootstrapped estimations of the variable loading scores.
#'
#' The analysis function analyses the differences between the bootstrapped means and single-estimation means of variable loading scores and makes suggestions on which items ought to be dropped from the data for either lack of reliability (difference between loading estimations greater than user-defined or default max.diff) or passing below any substantive level of commonality with the main series (bootstrapped mean loading score below the user-defined or default threshold either on the positive or minus side of the scale).
#'
#' The model inherits all formula arguments from the original extract function output (pre-bootstrapping).
#'
#' Assigning the output to an object creates a list of ten items, including the most over-estimated and under-estimated loading scores according to the bootstrapping, a data-frame of suggested data input, the results of this suggested input when passed into the extract function, and a graph plotting the latent dimension estimated in the bootstrap-suggested data.
#'
#' @export analyse.model

analyse.model <- function(model, max.diff=0.1, threshold=0.4, print=TRUE){

    ## Extract objects
    results <- model[["Full Results"]]
    data <- model[["Input Data"]]
    varname <- model[["Variable Name"]]
    output <- model[["Output"]]



    ## Top unreliable
    DiffOrderedUp <- results[order(results$Difference, decreasing=TRUE),c("Variable", "Difference")]
    DiffOrderedDown <- results[order(results$Difference, decreasing=FALSE),c("Variable", "Difference")]

    rownames(DiffOrderedDown) <- NULL
    rownames(DiffOrderedUp) <- NULL

    AList <- as.list("Bootstrap Analysis Results")

    AList[["Most Unreliable Items"]] <- cbind(head(DiffOrderedUp), head(DiffOrderedDown))

    ## Construct threshold data
    ThresholdData <- results[((results$`Bootstrapped Mean` > threshold | results$`Bootstrapped Mean` < (threshold*-1))
                              & results$Difference < max.diff), c("Variable", "Bootstrapped Mean")]


    ### Replace formula call to point at subsets - this is a crazy workaround, but I've spent hours trying to
    # figure out how to replace data call from extract and just cannot do it any other way than this (without introducing)
    # characters into the list

    # make sub_data and run a regression to get a formula with a data call to 'sub data'
    SuggestedData <- as.data.frame(cbind(c(1,2,3),c(3,5,4)))

    sum <- summary(lm(data=SuggestedData, V1 ~ V2))

    L1 <- as.list(sum$call)

    # get call from extract as a list
    formula_list <- as.list(model[["Formula"]])

    # replace call to original data with 'sub data'
    formula_list[[2]] <- L1[[3]]

    # convert back to formula
    formula <- as.call(formula_list)


    ## Construct the suggested data
    SuggestedData <- data[data[,varname] %in% ThresholdData[,"Variable"],]

    rownames(SuggestedData) <- NULL

    AList[["Suggested Input"]] <- SuggestedData


    ## Run the model
    BootResult <- eval(formula)

    ## Graph code

    g <- ggplot2::ggplot(data=NULL, ggplot2::aes(x=c(1:BootResult$T), y=BootResult$latent1))

    EstGraph <- g + ggplot2::geom_line() + ggplot2::theme_bw() +
      ggplot2::labs(subtitle="Bootstrap Model Suggested Estimated Latent Dimensions",
           x="Time period",
           y="Estimated latent value")


    ## Send results to list object

    AList[["T"]] <- BootResult$T

    AList[["Bootstrap Suggested Model Result"]] <- BootResult

    AList[["Single-Run Estimate"]] <- output

    AList

    AList[["Graph"]] <- EstGraph

    AList[["Mean Difference"]] <- mean(output[["latent1"]]) -  mean(BootResult[["latent1"]])

    AList[["Bootstrap Model Suggested Latent Dimension"]] <- BootResult$latent1

    AList[["Suggested Items to Drop"]] <- as.character(bootstraps$`Full Results`$Variable[(results$`Bootstrapped Mean` < threshold |
                        results$Difference > max.diff)])

    if(print==TRUE){
      ### To print
      cat(paste("Results from Analysis of Bootstrap Estimates:", "\n"))
      cat("", '\n')
      cat(paste("Single Run Model latent mean:", round(mean(output[["latent1"]]), 2), '\n'))
      cat(paste("Bootstrap Suggested Model latent mean:", round(mean(BootResult[["latent1"]]), 2), '\n'))
      cat(paste("Difference in means:", round(AList$`Mean Difference`, 2), '\n'))
      cat(paste("The start-point for the bootstrap-suggested estimated series is:", BootResult$period[1],'\n'))
      cat("", '\n')
      cat(paste("Suggested items to drop:", '\n'))
      ifelse(rapportools::is.empty(AList[["Suggested Items to Drop"]])==TRUE, cat("None"),
             print(AList[["Suggested Items to Drop"]]))
      cat("", '\n')
      cat("Most Unreliable Items:", '\n')
      print(AList[["Most Unreliable Items"]])
      print(EstGraph)
    }

    return(AList)

}

