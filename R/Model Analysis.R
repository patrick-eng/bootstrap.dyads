#' Analysis of bootstrapped dyad-ratios loading scores
#'
#' Runs post-hoc analysis of bootstrapped dyad-ratios model to identify poorly-fitting and unreliable variable items and fit a bootstrap-suggested dyad-ratios model.
#'
#' @param model Define the name of the object in which bootstrap results are stored.
#' @param max.diff Define the maximum acceptable difference between single-estimated and bootstrapped mean loading scores for items to be passed into the final model. Default is 0.1.
#' @param threshold Define the lower limit of bootstrapped loading scores for items to be passed into the final model. Default is 0.4.
#' @param print Logical. Define whether summary of analysis should be returned to the console.
#'
#' @details This function runs analysis of the outputs from the dyad-ratios bootstrap model, which takes the results of a single dyad-ratios estimation outcome and produces bootstrapped estimations of the variable loading scores.
#'
#' The analysis function analyses the differences between the bootstrapped means and single-estimation means of variable loading scores and makes suggestions on which items ought to be dropped from the data for either lack of reliability (difference between loading estimations greater than user-defined or default max.diff) or passing below any substantive level of commonality with the main series (bootstrapped mean loading score below the user-defined or default threshold either on the positive or minus side of the scale).
#'
#' The model inherits formula arguments from the original extract function output (pre-bootstrapping) and data from the output of the bootstrap model.
#'
#' Assigning the output to an object creates a list of nine items, including the most over-estimated and under-estimated loading scores according to the bootstrapping, a data-frame of suggested data input, the results of this suggested input when passed into the extract function, and a graph plotting the latent dimension estimated in the bootstrap-suggested data.
#'
#' @export analyse.model

analyse.model <- function(model, max.diff=0.05, threshold=0.3, sd.cut=0.2, print=TRUE){

    ## Extract objects
    results <- model[["Full Results"]]
    data <- model[["Input Data"]]
    varname <- model[["Variable Name"]]
    output <- model[["Output"]]



    ## Top over- and under-estimated
    DiffOrderedUp <- results[order(results$Difference, decreasing=TRUE),c("Variable", "Difference")]
    DiffOrderedDown <- results[order(results$Difference, decreasing=FALSE),c("Variable", "Difference")]
    HighSds <- results[order(results$`Standard Deviation`, decreasing=TRUE),c("Variable", "Standard Deviation")]

    rownames(DiffOrderedDown) <- NULL
    rownames(DiffOrderedUp) <- NULL
    rownames(HighSds) <- NULL

    diffs <- cbind(head(DiffOrderedUp), head(DiffOrderedDown), head(HighSds))

    report <- as.data.frame(diffs)

    names(report) <- c("Most Over-Estimated", "", "Most Under-Estimated", "", "Highest Standard Deviation", "")

    AList <- as.list("Bootstrap Analysis Results")

    AList[["Problematic Items"]] <- report


    ## Construct threshold data
    ThresholdData <- results[((results$`Bootstrapped Mean` > threshold
                              | results$`Bootstrapped Mean` < (threshold*-1))
                              & (results$Difference < max.diff)
                              & results$`Standard Deviation` < sd.cut), c("Variable", "Bootstrapped Mean")]


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

    AList[["Graph"]] <- EstGraph

    AList[["Mean Difference"]] <- mean(output[["latent1"]]) -  mean(BootResult[["latent1"]])

    AList[["Bootstrap Model Suggested Latent Dimension"]] <- BootResult$latent1

    AList[["Suggested Items to Drop"]] <- as.character(
      bootstraps$`Full Results`$Variable[(results$`Bootstrapped Mean` < threshold
                                          | results$`Bootstrapped Mean` < (threshold*-1)
                                          | results$Difference > max.diff
                                          | results$`Standard Deviation` > sd.cut)])

    if(print==TRUE){
      ### To print
      cat(paste("Results from Analysis of Bootstrap Estimates:", "\n"))
      cat("", '\n')
      cat(paste("Single Run Model latent mean:", round(mean(output[["latent1"]]), 2), '\n'))
      cat(paste("Bootstrap Suggested Model latent mean:", round(mean(BootResult[["latent1"]]), 2), '\n'))
      cat(paste("Difference in means:", round(AList$`Mean Difference`, 2), '\n'))
      cat("")
      cat(paste("Suggested items to drop:", '\n'))
      ifelse(rapportools::is.empty(AList[["Suggested Items to Drop"]])==TRUE, cat("None"),
             print(AList[["Suggested Items to Drop"]]))
      cat("", '\n')
      cat("Most Problematic Items:", '\n', '\n')
      print(AList[["Problematic Items"]])
      print(EstGraph)
    }

    return(AList)

}

