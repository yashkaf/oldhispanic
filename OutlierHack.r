outlier_hack=function(n=30, hacks=1) {
  # n is the sample size, hacks is the number of outliers to remove
  x=rnorm(n)
  y=rnorm(n)
  # Two independent variables
  for(i in 1:hacks) {
    z=x*y
  # If x and y are positively correlated we remove the negatively correlated outliers, and vice versa.
    if(cor(x,y)>0) {
      x=x[-which.min(z)]
      y=y[-which.min(z)]
    }
    else {
      x=x[-which.max(z)]
      y=y[-which.max(z)]
    }
  }
  m=lm(y~x)
  return(summary(m)$fstatistic)
  # Return the F-statistic of regressing y on x with the outliers removed.
}

hackability=function(n=30, hacks=1, sims=1000, alpha=0.05) {
  # sims is the number of simulations to run, alpha is the original false-positive threshold
  fstats=numeric(0)
  # Linear models in R return F-statistics and not p-values directly
  for(i in 1:sims) fstats=rbind(fstats,outlier_hack(n,hacks)[1])
  threshold=qf(alpha,1,n-hacks-2,0,FALSE)
  # Converting a p-value threshold (alpha) to an F-statistic threshold
  return(length(fstats[fstats>threshold])/sims)
  # This is the number of trials that were p-hacked to the alpha threshold
}
