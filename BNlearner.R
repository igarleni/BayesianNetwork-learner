#
library ("bnlearn")

emergency <- read.csv("urgencias11v-class-ms-R.txt")
emergency[] <- lapply( emergency, factor)

#################################
# Bayesian network using "Hill Climbing" method and BIC, K2, BDe score

#bic
bn.hc.bic <- hc(emergency, score = "bic")
modelstring(bn.hc.bic)
plot(bn.hc.bic)

#k2
bn.hc.k2 <- hc(emergency, score = "k2")
modelstring(bn.hc.k2)
plot(bn.hc.k2)

#bde
bn.hc.bde <- hc(emergency, score = "bde")
modelstring(bn.hc.bde)
plot(bn.hc.bde)
score(bn.hc.bde, emergency, type = "bde", iss = 1)

#################################
# Bayesian network using independece test

#Max-min parents and children (MMPC)
bn.mmpc <- mmpc(emergency)

#Mutual information test test
ci.test("tipofinancia", "motivo_alta", "dia", data = emergency, test = "mi")$p.value
ci.test("tipofinancia", "motivo_alta", "dia", data = emergency, test = "mc-x2")$p.value


###############
# Comparison

# BIC vs K2
compare(bn.hc.bic, bn.hc.k2)

#BIC vs BDe
compare(bn.hc.bic, bn.hc.bde)

#K2 vs BDe
compare(bn.hc.bde, bn.hc.k2)
