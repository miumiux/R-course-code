rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
	dat <- read.csv("C:/Users/miu/Dropbox/courses/CDA-cr/Programming/outcome-of-care-measures.csv",colClasses = "character")
	names(dat)[names(dat)=="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]<-"pneumonia"
	names(dat)[names(dat)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]<-"heart attack"
	names(dat)[names(dat)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]<-"heart failure"
	dat$"pneumonia"<-as.numeric(dat$"pneumonia")
	dat$"heart attack"<-as.numeric(dat$"heart attack")
	dat$"heart failure"<-as.numeric(dat$"heart failure")
	
	## Check that state and outcome are valid
	if(state %in% dat[,7]==0) stop("invalid state")
	if(outcome %in% c("pneumonia","heart attack","heart failure") == 0) stop("invalid outcome")
	dat1<-subset(dat,State==state)
	
	dat2<-dat1[!is.na(dat1[,outcome]),]
	dat3<-dat2[order(dat2[,outcome],dat2[,"Hospital.Name"]),]

	## Return hospital name in that state with the given rank
	if (num == "best")
		{
		best<-dat3[1,]$"Hospital.Name"
	}
	
	else if (num == "worst")
	{
		best<-dat3[nrow(dat2),]$"Hospital.Name"
	}
	else if (num>nrow(dat1)) best<-"NA"
	else best<-dat3[num,]$"Hospital.Name"
	return(best)
}


