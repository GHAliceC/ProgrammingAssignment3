## Set working directory to destinated file
setwd("./Documents/R_programming/rprog_data_ProgAssignment3-data/")

## Find the best hospital in a state
best <- function(state, outcome){
  ## Read file data and pass the result to vector 'care_measures'
  care_measures <- read.csv('./outcome-of-care-measures.csv', colClasses = 'character')
  
  ## Change mortality rate type to numeric, and avoid warnings when introducing NAs by coercion
  care_measures[, 11] <- suppressWarnings(as.numeric(care_measures[, 11]))
  care_measures[, 17] <- suppressWarnings(as.numeric(care_measures[, 17]))
  care_measures[, 23] <- suppressWarnings(as.numeric(care_measures[, 23]))
  
  ## Check if input values are valid
  if(!state %in% care_measures$State){
    stop('invalid state')
  }else if(!outcome %in% c('heart attack', 'heart failure', 'pneumonia')){
    stop('invalid outcome')
  }else{
    
    ## Create a subset to store the selected state and its corresponding data 
    sub_state <- care_measures[care_measures$State == state, ]
    ## Rank hospital names in alphabetical order
    sub_state <- sub_state[order(sub_state[,2]), ]
    
    if(outcome == 'heart attack'){
      ind <- 11
    }else if(outcome == 'heart failure'){
      ind <- 17  
    }else{
      ind <- 23
    }
    ## Rank the mortality rate in an ascending order and remove all the NAs
    sub_state <- sub_state[order(sub_state[, ind], na.last = NA) ,]

    ## return the given ranking hospital name
    h_name <- sub_state[1, 2]
    return(h_name)
  }
}
