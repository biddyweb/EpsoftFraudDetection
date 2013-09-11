library(TraMineR)
library(xtable)

dir.create("Graphiques", showWarnings = FALSE)
graphdir <- "Graphiques/"

install.packages("TraMineR", repos="http://mephisto.unige.ch/traminer/R")
library("TraMineR")
data("mvad")
mvad.alphab <- c("employment", "FE", "HE", "joblessness", "school", "training")
mvad.seq <- seqdef(mvad, 17:86, xtstep=6, alphabet=mvad.alphab)
mvad.om <- seqdist(mvad.seq, method = "OM", indel = 1, sm = "TRATE")

library("cluster")
clusterward <- agnes(mvad.om, diss=TRUE, method="ward")
mvad.cl4 <- cutree(clusterward, k=4)
cl4.lab <- factor(mvad.cl4, labels = paste("Cluster",1:4))

mvad.seqe <- seqecreate(mvad.seq)
fsubseq <- seqefsub(mvad.seqe,pMinSupport=0.05)
plot(fsubseq[1:15])