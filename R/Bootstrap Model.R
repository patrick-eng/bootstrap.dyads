#' Bootstrap model for the Stimson dyad ratios algorithm.
#' 
#' Runs bootstrap model for repeated estimation of variable loading scores in the dyad-ratios algorithm. 
#' 
#' @param data Define the data
#' @param reps Define number of bootstrapped replications. Default is 500.
#' @param draw Define the proportion of items to be dropped in each bootstrap replication. Default is 0.1.
#' @param varname Define the variable name indicating the input series (as in extract function)
#' @param output Define the object name that extract function results are stores in.
#' @param print Logical. Define whether or not the function should display active count of replication progress in the console.
#' 
#' @details This function runs the dyad-ratios bootstrap model, which takes the results of a single dyad-ratios estimation outcome and produces bootstrapped estimations of the variable loading scores.
#' 
#' The bootstrap model removes a pre-defined proportion of random variables for each estimation, extracts the variable loading scores, and averages them across all trials. 
#' 
#' The model inherits all formula arguments from the extract function output. 
#' 
#' Assigning the output to an object creates a list of five items, including a dataframe called 'Full Results' which contrains the bootstrapped mean loading score, single-run estimated loading score, and the difference between the two for each variable input.
#' 
#' It is strongly suggested (but not essential) for the speed of the bootstrap estimation that the options print and log in the extract function (to produce the results object) are set to FALSE.

bootstrapped_extraction <- function(data,reps=500,draw=0.1,varname,output,print=FALSE){

  ### Work out the levels and lengths of objects, build base objects for later filling, assign call to object
  
  set.seed(12345)
  
  d <- data 
  
  items <- levels(d[,varname])
  
  L <- (length(items)-round((length(items)*draw)))
  selections <- as.data.frame(matrix(nrow=L, ncol=1))
  
  formula <- output[["formula"]]
  
  ### Randomly select items and make a database with only randomly drawn items
  L2 <- length(levels(d[,varname]))
  loadings <- as.data.frame(levels(d[,varname]))
  names(loadings) <- "Variable"
  
  for(i in 1:reps){
    x <- sample(items, size = L)
    selections[i] <- x
    
  }
  
  ### Replace formula call to point at subsets - this is a crazy workaround, but I've spent hours trying to
  # figure out how to replace data call from extract and just cannot do it any other way than this (without introducing)
  # characters into the list 
  
  # make sub_data and run a regression to get a formula with a data call to 'sub data'
  sub_data <- as.data.frame(cbind(c(1,2,3),c(3,5,4)))
  
  sum <- summary(lm(data=sub_data, V1 ~ V2))
  
  L1 <- as.list(sum$call)
  
  # get call from extract as a list
  formula_list <- as.list(output[["formula"]])
  
  # replace call to original data with 'sub data'
  formula_list[[2]] <- L1[[3]]
  
  # convert back to formula
  formula <- as.call(formula_list)
  
  
  # set counter for reps
  ticker <- 0
  
  
  ### Run the original Stimson code on each database, store loading scores, merge into loadings object
  
  for(i in 1:length(selections)){
    
    sub_data <- d[d[,varname] %in% selections[,i], ]
    
    sub_result <- eval(formula)
    
    results_new <- as.data.frame(cbind(sub_result[["varname"]], sub_result[["loadings1"]]))
    
    names(results_new) <- c("Variable", paste("LoadingScoreTrial", i, sep=""))
    
    loadings <- merge(loadings, results_new, by="Variable", all.x = TRUE)
    
    if(print==TRUE){
      
      ticker <- ticker+1
      cat(paste(ticker, "..."))
      
      if(ticker == reps){
        cat("Done!")
      }
      
    }
    
  }


  ### Calculate mean of scores for each item and rank
  
  ## change scores back to numeric
  for(i in 2:length(loadings)){
    
    loadings[,i] <- as.numeric(as.character(loadings[,i]))
    
  }
  
  ## find means
  loadings$mean <- rowMeans(loadings[,2:length(loadings)], na.rm=TRUE)
  
  
  ## extract full results
  results_full <- as.data.frame(cbind(output[["varname"]], output[["loadings1"]]))
  
  ## change loadings back to numeric
  results_full$V2 <- as.numeric(as.character(results_full$V2))
  
  ## rename object
  names(results_full) <- c("Variable", "FullDataScore")
  
  ## combine objects
  loadings <- merge(loadings, results_full, by="Variable")
  
  ## Find distance from mean for each item
  loadings$Difference <- loadings$FullDataScore-loadings$mean
  
  ## Subset to summarisable object
  FullResults <- loadings[,c("Variable", "mean", "FullDataScore", "Difference")]
  
  names(FullResults) <- c("Variable", "Bootstrapped Mean", "Single-Run Estimate", "Difference")
  
  FullResults <- FullResults[order(FullResults$`Bootstrapped Mean`, decreasing=TRUE),]
  
  ## Construct list object
  ListResults <- as.list("Bootstrap Results")
  
  ListResults[["Full Results"]] <- FullResults
  
  ListResults[["Input Data"]] <- d
  
  ListResults[["Variable Name"]]  <- varname
  
  ListResults[["Formula"]] <- formula
  
  ListResults[["Output"]] <- output

  
  return(ListResults)

}

