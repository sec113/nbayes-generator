##Function
PrecRec = function(predicted, actual_labels){
  #feed the data in columns
  xTab <- table(predicted, actual_labels)
  accuracy= sum(xTab[1,1],xTab[2,2])/sum(xTab)
  precision= xTab[1,1]/sum(xTab[1,])
  recall= xTab[1,1]/sum(xTab[,1])
  f1measure=(2*precision*recall)/sum(precision,recall)

  cat('recall:  ')
  cat(recall)
  cat('\n') 
  
  cat('precision:  ')
  cat(precision)
  cat('\n')
  
  cat('f1measure:  ')
  cat(f1measure)
  cat('\n')
  
  cat('accuracy:  ')
  cat(accuracy)
  cat('\n')
  
  cat('TruePositives: ')
  cat(xTab[1,1])
  cat('\n')
  
  cat('TrueNegatives: ')
  cat(xTab[2,2])
  cat('\n')
  
  cat('FalseNegatives: ')
  cat(xTab[2,1])
  cat('\n')
  
  cat('FalsePositives: ')
  cat(xTab[1,2])
  cat('\n')
  
}


#read the file
b_base=read.csv(file = "c:/users/SergioMC/Documents/Project course/baselinevaluation_4.csv",header = T)

PrecRec(predicted = b_base$broklb,actual_labels = b_base$humanlb=='0')
PrecRec(predicted = b_base$broklb,actual_labels = b_base$humanlb=='1')
PrecRec(predicted = b_base$broklb,actual_labels = b_base$humanlb=='2')
PrecRec(predicted = b_base$broklb,actual_labels = b_base$humanlb=='3')
PrecRec(predicted = b_base$broklb,actual_labels = b_base$humanlb=='4')
PrecRec(predicted = b_base$broklb,actual_labels = b_base$humanlb=='5')
PrecRec(predicted = b_base$broklb,actual_labels = b_base$humanlb=='6')

PrecRec(predicted = b_base$broklb,actual_labels = b_base$humanlb)

PrecRec(predicted = b_base$brokrb,actual_labels = b_base$humanrb=='0')
PrecRec(predicted = b_base$brokrb,actual_labels = b_base$humanrb=='1')
PrecRec(predicted = b_base$brokrb,actual_labels = b_base$humanrb=='2')
PrecRec(predicted = b_base$brokrb,actual_labels = b_base$humanrb=='3')
PrecRec(predicted = b_base$brokrb,actual_labels = b_base$humanrb=='4')
PrecRec(predicted = b_base$brokrb,actual_labels = b_base$humanrb=='5')
PrecRec(predicted = b_base$brokrb,actual_labels = b_base$humanrb=='6')

PrecRec(predicted = b_base$brokrb,actual_labels = b_base$humanrb)

PrecRec(predicted = birads$BROK.BIRADS.LEFT,actual_labels = birads$FINAL.BIRADS.Left.Breast.Assessment)
PrecRec(predicted = birads$BROK.BIRADS.RIGHT,actual_labels = birads$FINAL.BIRADS.Right.Breast.Assessment)

write.csv(birads,"Project course/b.csv")


aja=read.csv(file="C:/Users/SergioMC/Documents/Project course/b.csv")
PrecRec(predicted =aja$BROK.BIRADS.LEFT,actual_labels = aja$FINAL.BIRADS.Left.Breast.Assessment )
PrecRec(predicted = aja$BROK.BIRADS.RIGHT,actual_labels = aja$FINAL.BIRADS.Right.Breast.Assessment)


prime=read.csv(file="C:/Users/SergioMC/Documents/Project course/mierda.csv")
PrecRec(predicted =prime$primeprimeleft,actual_labels = prime$humanleft)
PrecRec(predicted = prime$primeprimeright,actual_labels = prime$humanright)
confusionMatrix(data = prime$primeprimeleft,reference = prime$humanleft)
confusionMatrix(data = prime$primeprimeright,reference = prime$humanright)

confusionMatrix(data = prime$left,reference = prime$humanleft)
confusionMatrix(data = prime$right,reference = prime$humanright)
