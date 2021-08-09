chain<-reshape_data[-1,]
#install.packages("markovchain")
library("markovchain")
base_sequence<-chain[,-1]

mcFitMap <- markovchainFit(base_sequence, method = "map")
mcFitMap
TPM=mcFitMap$estimate
TPM
#row.names(TPM)
#names(TPM)=colnames("-3","-2","-1","1","2","3","4","5","6","7","8","9","10","11")
#str(TPM)
s=round(steadyStates(TPM),digits=3)

write.csv(round(TPM@transitionMatrix, digits=2), "tpm1y.csv")
tpm=round(TPM@transitionMatrix, digits=2)
tpm2=round(TPM@transitionMatrix %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm2, digits=2), "tpm2year.csv")
tpm3=round(tpm2 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm3, digits=2), "tpm3year.csv")
tpm4=round(tpm3 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm4, digits=2), "tpm4year.csv")
tpm5=round(tpm4 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm5, digits=2), "tpm5year.csv")
tpm6=round(tpm5 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm6, digits=2), "tpm6year.csv")
tpm7=round(tpm6 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm7, digits=2), "tpm7year.csv")
tpm8=round(tpm7 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm8, digits=2), "tpm8year.csv")
tpm9=round(tpm8 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm9, digits=2), "tpm9year.csv")
tpm10=round(tpm9 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm10, digits=2), "tpm10year.csv")
tpm11=round(tpm10 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm11, digits=2), "tpm11year.csv")
tpm12=round(tpm11 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm12, digits=2), "tpm12year.csv")
tpm13=round(tpm12 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm13, digits=2), "tpm13year.csv")
tpm14=round(tpm13 %*% TPM@transitionMatrix,digits=2)
write.csv(round(tpm14, digits=2), "tpm14year.csv")
tpm15=round(tpm14 %*% TPM@transitionMatrix,digits=2)

tpm16=round(tpm15 %*% TPM@transitionMatrix,digits=2)

tpm17=round(tpm16 %*% TPM@transitionMatrix,digits=2)
