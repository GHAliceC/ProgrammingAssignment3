## Set working directory to destinated file
setwd("./Documents/R_programming/rprog_data_ProgAssignment3-data/")

## Rank hospitals by outcome in a state
rankall <- function(outcome, num = "best"){
  
  ## Read file data and pass the result to vector 'care_measures'
  care_measures <- read.csv('./outcome-of-care-measures.csv', colClasses = 'character')
  
  ## Change mortality rate type to numeric, and avoid warnings when introducing NAs by coercion
  care_measures[, 11] <- suppressWarnings(as.numeric(care_measures[, 11]))
  care_measures[, 17] <- suppressWarnings(as.numeric(care_measures[, 17]))
  care_measures[, 23] <- suppressWarnings(as.numeric(care_measures[, 23]))
  
  ## Check if input values are valid
 if(!outcome %in% c('heart attack', 'heart failure', 'pneumonia')){
    stop('invalid outcome')
  }else{
    
    ## Sort all the states in alphabetical order
    care_measures <- care_measures[order(care_measures[, 7]), ]
    state_names <- unique(care_measures[, 7])
    
    ## Initialize the hospital and state lists
    hospital <- list()
    state <- list()
    
    ## Decide which column is selected
    if(outcome == 'heart attack'){
      ind <- 11
    }else if(outcome == 'heart failure'){
      ind <- 17  
    }else{
      ind <- 23
    }
    
    ## Find all the hospital names matching the given ranking in all states 
    for(i in 1:length(state_names)){
      sub_state <- care_measures[care_measures$State == state_names[i], ]
      sub_state <- sub_state[order(sub_state[,2]), ]
      sub_state <- sub_state[order(sub_state[,ind], na.last = NA), ]
      
      ## Consider request for the worst hospital for a given mortality cause
        if(num == "worst"){
          num_set <- length(sub_state[, 2])
        }else if(num == "best"){
          num_set <- 1
        }else{
          num_set <- num
        }
      
        hospital[i] <- sub_state[num_set, 2]
        state[i] <- state_names[i]
    }
    result <- cbind(hospital, state)
    return(result)
  }
}  
  

  
