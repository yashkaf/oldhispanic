simsurv <- function(n, total, probs, score){
  ifelse(sum(sample(score,n,TRUE,probs))==total,1,0)
}

simmult <- function(n=100, total, probs, score, sims=10000000){
  hits=0
  for (i in seq_len(sims)) {
    hits=hits+simsurv(n,total,probs,score)
  }
  return(hits)
}

runsims <- function(n=100,sims=10000000,ascores,aprobs) {
  results=matrix(data=numeric(15),nrow=5,ncol=3)
  for (r in 1:5){
    for (c in 1:3) {
      results[r,c]=simmult(n,ascores[c],aprobs[[r]])
    }
  }
  return(results)
}

probsa=c(.59,.3,.1,.01)
probsb=c(.48,.38,.13,.01)
probsc=c(.28,.55,.15,.02)
probsd=c(.22,.57,.18,.03)
probse=c(.38,.47,.14,.01)
scores=c(3,1,0,-1)

allprobs=list(probsa,probsb,probsc,probsd,probse)
allscores=c(173,132,176)

longscores=c(2,1,0)
gapscores=c(1,0,-1)
longprobsa=c(.388,.474,.138)
longprobsb=c(.263,.474,.263)
