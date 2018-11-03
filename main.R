rm(list=ls())
setwd("~/SiBRank")

source(paste(getwd(),'/Common code.R',sep=""))
source(paste(getwd(),'/SiBRank.R',sep=""))
source(paste(getwd(),'/Common code-knn.R',sep=""))
source(paste(getwd(),'/SiBRank-graphplussim.R',sep=""))



rating<- read.delim(paste(getwd(),'/rating.data',sep=""), header=FALSE)
trdir=paste(getwd(),'/tr-1-10.csv',sep="")
simdir=paste(getwd(),'/sim-tr-1-10.csv',sep="")

rmat=makeRatmat(rating)
usersequence=makeuserSequence(rating);
res=rankEvaluation_Yu_sibrank(rmat,userSeq = usersequence,trl=10,direc = "~/g-res.csv",firstRun = 1,simflag = 0,damping = 0.85,trainflag = 1,trdirec = trdir,simdirec=simdir)
