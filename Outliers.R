#this function helps to detetct outliers in the data
outlier<- function(data)
{
  UpperCount <- c() #empty vector
  LowerCount <- c()
  Log_UpperCount <- c()
  Log_LowerCount <- c()
  names <- c()

  for(i in 1:ncol(data)) #for each columns in the data
  {
    if(is.numeric((data[,i]))) #if numeric
    {
      x <- data[,i][!is.na(data[,i])] #if value is not null add in x
      l <- log(data[,i][!is.na(data[,i])]) #same with log

      #calculating 25th,75th percentiles of the data
      q25 = quantile(x)[2]
      q75 = quantile(x)[4]
      l25 = quantile(l)[2] #for log
      l75 = quantile(l)[4]

      #calculating the upper limit and lower limits
      lower <- q25-1.5*(q75-q25)
      upper <- q75+1.5*(q75-q25)
      l_lower <- l25-1.5*(l75-l25) #for log
      l_upper <- l75+1.5*(l75-l25)

      names[i] = names(data[i])
      UpperCount[i] = sum(x>upper) #sum of observation greater than upper limit
      LowerCount[i] = sum(x<lower) #sum of observation greater than lower limit
      Log_UpperCount[i] = sum(l>l_upper) #sum of observation greater than upper limit
      Log_LowerCount[i] = sum(l<l_lower) #sum of observation greater than lower limit

    }
    else
    {
      next
    }
  }

  outlierTable = cbind(UpperCount,LowerCount,Log_UpperCount,Log_LowerCount)
  rownames(outlierTable) = names

  print("* Outlier detection for only numerical variables")
  return(outlierTable[complete.cases(outlierTable), ])

}

data <- read.csv("C:\\Users\\kishor\\Desktop\\Praxis\\SEM 2\\R Pracs\\Assignments\\Outliers\\merchants.csv")
outlier(data)
