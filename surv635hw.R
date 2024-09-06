install.packages("faraway", dependencies = TRUE)
install.packages("minqa")

library(faraway)

#Question 1
hprice$homeprice<-exp(hprice$narsp)*1000

  mean(hprice$homeprice)
  #The mean of home price is $94,411.42 which is the 
  #average price of homes in the 36 metropolitican statistical areas studied by the survey
  sd(hprice$homeprice, na.rm=T)
  #The standard deviation is 39,788.32 which is the average of how far data points are away from the mean
  
  
  
  ##############Question 2
  #"The 95 % CI for the point estimate 94411.42 with standard error 39788.32 is ( 16427.75 to 172395.09 )"
  my_CI3 <- function(point_estimate, standard_deviation, confidence_level) {
    # Calculate the Z value for the given confidence level
    z_value <- qnorm(1 - (1 - confidence_level) / 2)
    
    # Calculate the margin of error
    margin_of_error <- z_value * standard_deviation
    
    # Calculate the lower and upper bounds of the confidence interval
    lower_bound <- point_estimate - margin_of_error
    upper_bound <- point_estimate + margin_of_error
    
    # Print the confidence interval in a descriptive sentence
    print(paste("The", confidence_level * 100, "% CI for the point estimate", point_estimate, 
                "with standard error", standard_deviation, "is (", 
                round(lower_bound, 2), "to", round(upper_bound, 2), ")"))
    
    # Return the confidence interval as a vector
    return(c(lower_bound, upper_bound))
  }
  
  my_CI3(94411.42, 39788.32, .95)
  
  
  #######Question 3
  library(tidyverse)
  nearwater <- hprice%>%
    filter(hprice$ajwtr==1)
  
  notnearwater <- hprice%>%
    filter(hprice$ajwtr==0)
  mean(nearwater$homeprice)
  mean(notnearwater$homeprice)
  
  std_error_nearwater <- sd(nearwater$homeprice) / sqrt(length(nearwater$homeprice))
  
  std_error_notnearwater <- sd(notnearwater$homeprice) / sqrt(length(notnearwater$homeprice))
  
  
  #The mean home price near water is 11,243 and standard error is 4655.88 while away from water is 82,388.89, and stadnard error is 1228.66
  
  #Question 4 in the difference of means test H0: u=k Ha u!=k two sided test if the two means are the same
  # at a signifance level of p<.01 we can reject the null hypothesis, that there is a difference betwenn these two means
  t.test(nearwater$homeprice, notnearwarer$homeprice)
  #Question5
  #The pearson coefficent is .7437
  pearson_corr <- cor(hprice$homeprice, hprice$ypc, method = "pearson")
  
  #Question6
  #alternative hpyothesis is true, it is not =0
    cor.test(hprice$homeprice, hprice$ypc)
  #Question 7
    #Reject the null hypothesis, the correlation coefficent is different from 0
    #at a significance level of P<.01 which means that income and house price are corellated
  #question 8
    #P<.01 so fail to reject null, home price is not normally distrbuted.
    shapiro.test(hprice$homeprice)
    #This would change my answer because tests like the Pearson test assume normal distrubtion
    #we would have to use alternative tests to account for a non-normal distribution
    
  
  
  
  