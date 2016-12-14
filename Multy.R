genpop <- function(n=1) {
# Creates a data frame of n*24 subjects with sex/age/race characteristics and a column for binary results.
  nid=n*24
  id <- c(1:nid)
  sex <- c(rep('female',n*12),rep('male',n*12))
  age <- rep(c(rep('young',n*4),rep('adult',n*4),rep('old',n*4)),2)
  race <- rep(c('asian','black','hispanic','white'),n*6)
  result <- rep(0,n*24)
  return(data.frame(id,sex,age,race,result,stringsAsFactors = F))
}

pval <- function(results,pv=0.5) {
# Calculates the p-value for a single vector of binary successes, compared to a null hypothesis of 'pv' proportion of successes.
  1-pbinom(sum(results),length(results),pv)
}

phack <-function(pop) {
# Calculates the p-values for each possible subgroup of the population data frame
  allps <-data.frame(depth=0, pvalue=pval(pop$result),stringsAsFactors = F)
  for (s in unique(pop$sex)) {
    allps <- rbind(allps,c(1,pval(pop[pop$sex==s,"result"])))
    for (a in unique(pop$age)) {
      allps <- rbind(allps,c(1,pval(pop[pop$age==a,"result"])))
      allps <- rbind(allps,c(2,pval(pop[pop$sex==s & pop$age==a,"result"])))
      for (r in unique(pop$race)) {
        allps <- rbind(allps,c(1,pval(pop[pop$race==r,"result"])))
        allps <- rbind(allps,c(2,pval(pop[pop$sex==s & pop$race==r,"result"])))
        allps <- rbind(allps,c(2,pval(pop[pop$age==a & pop$race==r,"result"])))
        allps <- rbind(allps,c(3,pval(pop[pop$sex==s & pop$age==a & pop$race==r,"result"])))
      }
    }
  }
  return(allps)
}

psimulation <- function(n=1, sims=20, pdrug=0.5) {
# Simulates p-hacking to return the best p-value of any subgroup in each simulation run. 
# The success probability for each individual is held constant at 'pdrug'.
  population=genpop(n)
  bestps <- data.frame(depth=integer(), pvalue=numeric(),stringsAsFactors = F)
  for (iter in seq_len(sims)) {
    population$result <- rbinom(n*24,1,pdrug)
    simresult = phack(population)
    bestps <- rbind(bestps,simresult[order(simresult$pvalue,simresult$depth),][1,])
  }
  return(bestps)
}

psimulation2 <- function(n=1, sims=20, pdrug=0.5) {
# Simulates p-hacking to return the best p-value of any subgroup in each simulation run. 
# The population porportion of successes is held constant at 'pdrug'.
  population=genpop(n)
  tot=n*24
  cures=as.integer(pdrug*tot)
  baseresult=c(rep(1,cures),rep(0,tot-cures))
  bestps <- data.frame(depth=integer(), pvalue=numeric(),stringsAsFactors = F)
  for (iter in seq_len(sims)) {
    population$result <- sample(baseresult,tot)
    simresult = phack(population)
    bestps <- rbind(bestps,simresult[order(simresult$pvalue,simresult$depth),][1,])
  }
  return(bestps)
}

