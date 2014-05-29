best <- function(state, outcome) {
        
        ## Read outcome data
        list <-list.files()
        ## Check that state and outcome are valid
        
        File <- c("outcome-of-care-measures.csv")
        
        DataFile <- read.csv(File, colClasses = "character")
        DataFile <- na.omit(DataFile)
        
        outcomes <- c("heart attack","heart failure","pneumonia")
        if (is.element(outcome, outcomes)==FALSE) stop("invalid outcome")
        
        stateVector <- DataFile[,7]
        if (is.element(state, stateVector)==FALSE) stop("invalid state")
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if (identical(outcome, outcomes[1])){
                
                DataFile1 <- subset(DataFile, DataFile$State==state)
                
                
                sortHeartAttack <- DataFile1[order(DataFile1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, with(DataFile1, order(DataFile1$Hospital.Name)), na.last=TRUE) , ]
                HospitalNames <-sortHeartAttack["Hospital.Name"]
                
                HospitalNames[1,]
        }

        else if (identical(outcome, outcomes[2])){
                
                DataFile2 <- subset(DataFile, DataFile$State==state)
                
                
                sortHeartFailure <- DataFile2[order(DataFile2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, with(DataFile2, order(DataFile2$Hospital.Name)), na.last=NA) , ]
                HospitalNames1 <-sortHeartFailure["Hospital.Name"]
                
                HospitalNames1[1,]
        }
        else if (identical(outcome, outcomes[3])){
                
                DataFile3 <- subset(DataFile, DataFile$State==state)
                
                
                sortPneumonia <- DataFile3[order(DataFile3$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,with(DataFile3, order(DataFile3$Hospital.Name)), na.last=NA) , ]
                HospitalNames <-sortPneumonia["Hospital.Name"]
                
                HospitalNames[1,]
        }
}       
        
