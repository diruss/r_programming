corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        id <- 1:332
        corrs <- numeric()        
        for(i in id){
                filename <- paste(c(directory,"/",sprintf("%03d", i),".csv"),collapse="")
                mydata <- read.csv(filename, header=TRUE)
                nobs <- nrow(mydata[complete.cases(mydata),])
                if(nobs > threshold){
                        corrs<-c(corrs, cor(mydata$nitrate, mydata$sulfate, use="complete.obs"))
                }
        }        
        as.numeric(corrs)
        
        
}