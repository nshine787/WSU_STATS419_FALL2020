doMode <- function(x)
{
	result = c();
	# freq ... # high frequencies
	# ties ... store all of the ties
	unique_x <- unique(x);
	#unique_x[which.max(tabulate(match(x,unique_x)))];
	counts <- tabulate(match(x, unique_x))
	maxCount <- max(counts)
	result <- unique_x[which(counts==maxCount)]
	result;
}

# result should return the sum, sumsq and var for naive; and sum, sum2 and var for two-pass
sampleVariance <- function(x, method)
{
	result <- c()
	if(method=='naive')
	{
		n <- 0;
		sum <- 0;
		sumsq <- 0;
		
		for(i in x)
		{
			n <- n + 1
			sum <- sum + i
			sumsq <- sumsq + (i*i)
		}
		var = (sumsq - (sum*sum)/n)/(n-1)
		result = c(Sum=sum, SumSq=sumsq, Variance=var)
	}

# two-pass algorithm
	else
	{
		n <- 0
		sum <- 0
		sum2 <- 0
		
		for(i in x)
		{
			n <- n + 1
			sum <- sum + i
		}
		mean <- sum/n
		
		for(i in x)
		{
			sum2 <- sum2 + ((i - mean) * (i-mean))
		}
		var <- sum2/(n-1)
		result <- c(Sum=sum, Sum2=sum2, Variance=var)
	}
	result;
}

doSummary = function(x)
{
	x <- as.numeric(as.vector(x))
	# length
	result <- c(Length=length(x));
	
	# number of NAs
	result <- c(result, NumNA=sum(is.na(x)));
	
	# mean
	result <- c(result, Mean=mean(x))
	
	# median 
	result <- c(result, Median=median(x))
		
	# mode # custom function
	modes <- doMode(x)
	for(i in 1:length(modes))
	{
		result <- c(result, Mode=modes[i])
	}
	
	# variance # custom function ...
	vMethod = 'naive'

	result <- c(result, sampleVariance(x, vMethod))
	
	# standard deviation ... built in function but compare it to the custom function
	result <- c(result, BuildInSD=sd(x))
	customSDResult <- sqrt(result['Variance'])
	result <- c(result, CustomSD=customSDResult)
	result;
}
	
#doSummary(df$V01)
##monte.shaffer@gmail.com
#monteShafferRecord <- unlist(df[1,3:62])
#test2 <- doSummary(monteShafferRecord)

doZScores <- function(x, info)
{
	x <- as.numeric(as.vector(x))
	result = c();
	for(i in x)
	{
		zScore <- (i-info['Mean'])/info['BuildInSD']
		result <- c(result, zScore)
	}
	unname(result);
}