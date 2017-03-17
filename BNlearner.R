#
library ("bnlearn")

emergency <- read.csv("urgencias11v-class-ms-R.txt")
emergency[] <- lapply( emergency, factor)

#################################
# Bayesian network using "Hill Climbing" method and BIC, K2, BDe score

#bic
bn.hc.bic <- hc(emergency, score = "bic")
modelstring(bn.hc.bic)
##Result
#"[tipofinancia][tipo_patologia|tipofinancia][motivo_ingr|tipo_patologia]
#[centro|motivo_ingr:tipo_patologia][tipo_docu|tipofinancia:centro][turno|motivo_ingr:centro]
#[ser_real|centro][p10|tipo_docu:centro][motivo_alta|ser_real][horas_estancia|ser_real][dia|p10]"
plot(bn.hc.bic)

#k2
bn.hc.k2 <- hc(emergency, score = "k2")
modelstring(bn.hc.k2)
##Result
#"[ser_real][motivo_alta|ser_real][centro|ser_real][motivo_ingr|centro]
#[horas_estancia|motivo_alta:ser_real][tipo_patologia|motivo_ingr:centro]
#[tipofinancia|tipo_patologia][tipo_docu|tipofinancia:motivo_ingr:centro]
#[p10|tipo_docu:ser_real]
#[turno|motivo_ingr:tipo_patologia:tipo_docu:motivo_alta:horas_estancia:centro:ser_real]
#[dia|motivo_ingr:tipo_patologia:p10:tipo_docu:turno:centro]"
plot(bn.hc.k2)

#bde
bn.hc.bde <- hc(emergency, score = "bde")
modelstring(bn.hc.bde)
##Result
#"[centro][ser_real|centro][tipo_patologia|ser_real][p10|ser_real][motivo_alta|ser_real]
#[tipofinancia|tipo_patologia][motivo_ingr|tipo_patologia:centro][dia|p10]
#[horas_estancia|motivo_alta:ser_real][tipo_docu|tipofinancia:motivo_ingr:p10:centro]
#[turno|motivo_ingr:p10:centro]"
plot(bn.hc.bde)
score(bn.hc.bde, emergency, type = "bde", iss = 1)
#Score result = -229797.1

#################################
# Bayesian network using independece test

#Max-min parents and children (MMPC)
bn.mmpc <- mmpc(emergency)

#Mutual information test test
ci.test("tipofinancia", "motivo_alta", "dia", data = emergency, test = "mi")$p.value
##Result --> 0.07435552
ci.test("tipofinancia", "motivo_alta", "dia", data = emergency, test = "mc-x2")$p.value
##Result --> 0.0148

###############
# Comparison

# BIC vs K2
compare(bn.hc.bic, bn.hc.k2)
##Result
#tp = 8
#fp = 18
#fn = 6

#BIC vs BDe
compare(bn.hc.bic, bn.hc.bde)
##Result
#tp = 9
#fp = 8
#fn = 5

#K2 vs BDe
compare(bn.hc.bde, bn.hc.k2)
##Result
#tp = 12
#fp = 14
#fn = 5
