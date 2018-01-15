

dml100k <- defineData(ml100k_array, T)

e100k <- evalModel(dml100k, 5)

e <- evalModel(d,5)


neigh <- 100* c(0.2, 1:10)/2

res_ub_mf <- c()

for (i in neigh){
  r <- evalRec(e, "ub", simFunct = "cos", positiveThreshold = 5, topNGen = "mf", coRatedThreshold = 20, neigh = i, topN = 10)
  res_ub_mf <- c(res_ub_mf, r)
}


lapply(res_ub_mf, function(x) mean(x@nDCG))

res_ub_hpr <- c()

for (i in neigh){
  r <- evalRec(e, "ub", simFunct = "cos", positiveThreshold = 5, topNGen = "hpr", coRatedThreshold = 20, neigh = i, topN = 10)
  res_ub_hpr <- c(res_ub_hpr, r)
}

lapply(res_ub_hpr, function(x) mean(x@nDCG))




res_ib_mf <- c()

for (i in neigh){
  r <- evalRec(e, "ib", simFunct = "cos", positiveThreshold = 5, topNGen = "mf", coRatedThreshold = 25, neigh = i, topN = 10)
  res_ib_mf <- c(res_ib_mf, r)
}



lapply(res_ib_mf, function(x) mean(x@nDCG))

res_ib_hpr <- c()

for (i in neigh){
  r <- evalRec(e, "ib", simFunct = "cos", positiveThreshold = 5, topNGen = "hpr", coRatedThreshold = 25, neigh = i, topN = 10)
  res_ib_hpr <- c(res_ib_hpr, r)
}


lapply(res_ib_hpr, function(x) mean(x@nDCG))


topNNN <- c(1,5,10)

ub_mf <- c()
ub_hpr <- c()
ib_mf <- c()
ib_hpr <- c()


for (i in topNNN) {
  #r <-  evalRec(e, "ub", simFunct = "cos", positiveThreshold = 5, topNGen = "mf", coRatedThreshold = 20, neigh = 50, topN = i)
  #ub_mf <- c(ub_mf, r)

 # r <-  evalRec(e, "ub", simFunct = "cos", positiveThreshold = 5, topNGen = "hpr", coRatedThreshold = 20, neigh = 50, topN = i)
  #ub_hpr <- c(ub_hpr, r)
  
  r <-  evalRec(e, "ib", simFunct = "cos", positiveThreshold = 5, topNGen = "mf", coRatedThreshold = 70, neigh = 50, topN = i)
  ib_mf <- c(ib_mf, r)
  
  r <-  evalRec(e, "ib", simFunct = "cos", positiveThreshold = 5, topNGen = "hpr", coRatedThreshold = 70, neigh = 50, topN = i)
  ib_hpr <- c(ib_hpr,r)
}





lapply(ub_mf, function(x) mean(x@nDCG))
lapply(ub_hpr, function(x) mean(x@nDCG))
lapply(ib_mf, function(x) mean(x@nDCG))
lapply(ib_hpr, function(x) mean(x@nDCG))
