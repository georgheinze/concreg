cindex<-function(object, confint=FALSE, level=0.95) {
  OC<-exp(coef(object))
  cindex<-OC/(1+OC)
  if(confint) {
    ci<-confint(object, level=level, what="cindex")
    cindex<-rbind(cindex,ci)
  }
  return(cindex)
}