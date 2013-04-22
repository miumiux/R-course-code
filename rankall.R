#|-----------------------------------------------------------------------
#| This is a function for finding hospital of certain rank in certain state, return= c(HOSPTIAL, STATE)
#|-----------------------------------------------------------------------

rankall <- function(outcome, num = "best") {

  dat <- read.csv("C:/Users/miu/Dropbox/courses/CDA-cr/Programming/outcome-of-care-measures.csv",colClasses = "character")
  
  #Rename
	names(dat)[names(dat)=="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]<-"pneumonia"
	names(dat)[names(dat)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]<-"heart attack"
	names(dat)[names(dat)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]<-"heart failure"
	
	# --- Coerce character into numeric
	dat$"pneumonia"<-as.numeric(dat$"pneumonia")
	dat$"heart attack"<-as.numeric(dat$"heart attack")
	dat$"heart failure"<-as.numeric(dat$"heart failure")
	
  # --- Check that state and outcome are valid
  if(outcome %in% c("pneumonia","heart attack","heart failure") == 0)
    stop("invalid outcome")
	
	## For each state, find the hospital of the given rank
	#Remove the NA outcome from ALL states
	dat_<-dat[!is.na(dat[,outcome]),]
	dat2<-split(dat1,dat1$State)
	datOrd<-lapply(dat2,function(z){
		z1<-z[order(z[,outcome],z[,"Hospital.Name"]),]
		if (num == "best")
		{
			hosp<-z1[1,]$"Hospital.Name"
		}
  		else if (num == "worst")
  		{
  			hosp<-z1[nrow(z),]$"Hospital.Name"
  		}
      #Check if ranking is greater than of hospitals in this state 
		     else if (num>nrow(z)) hosp <- "NA"
		          
            else {hosp<-z1[num,]$"Hospital.Name"}
		state<-z1$State[1]
		return(c(hosp,state))
	})
	
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	best<-as.data.frame(datOrd)
	best1<-as.data.frame(t(best))	
	names(best1)<-c("hospital","state")
	best1<-best1[order(best1$state),]
	return(best1)
}
