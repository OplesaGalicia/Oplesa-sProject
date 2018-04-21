rdnStr <- function(n)
{
  str<-""
  
    str<-intToUtf8(sample(65:122,n,replace = TRUE))  
    return(str)
    
}

genera_pob <- function(tam,n)
{
  c<- rep("",tam)
  for (i in 1:tam) 
    c[i]<-rdnStr(n)
    return(c)
  
}

fitness<-function(cad,cadModelo)
{
  suma=0
  for (i in 1:nchar(cadModelo)) {
    suma=suma+abs(utf8ToInt(substr(cadModelo,i,i))-utf8ToInt(substr(cad,i,i)))
  }
  return(suma)
}
#cadModelo es la cadena a la que me debo acercar
evalPob<-function(pob,cadModelo)
{
  n=nchar(cadModelo)
  fpob=rep(0,length(pob))
  for (i in 1:length(pob)) {
    fpob[i]=fitness(pob[i],cadModelo)
  }
  df = data.frame(pob,fpob,stringsAsFactors = FALSE)
  df = setNames(df,c("pob","fitness"))
  df = df[order(df$fitness),] #se ordena con base a fitness para que los de calif mas alta vaya narriba
  return(df)
}


cruza<-function(padre,madre)
{
  hijo1=paste(substr(padre,1,nchar(padre)%/%2),substr(madre,nchar(madre)%/%2+1,nchar(madre)),sep = "")
  hijo2=paste(substr(madre,1,nchar(madre)%/%2),substr(padre,nchar(padre)%/%2+1,nchar(padre)),sep = "")
  return (c(hijo1,hijo2))
}

muta<-function(str)
{
  r = sample(1:nchar(str),1)
  e = paste(substr(str,1,r-1),intToUtf8(sample(65:122,1)), substring(str,r+1), sep = "")
  return (e)
}


genetico<-function(tam_pob,probCruce,probMut,nMaxIter,cadena)
{
  pob = genera_pob(tam_pob,nchar(cadena))
  epob = evalPob(pob,cadena)
  epob= epob$pob
  i=0
  
  while(i<nMaxIter && epob[1]!=cadena) #mientras pueda seguir iterando e la epob aun no sea igual a la cadena deseada
  {
    npob=rep("",tam_pob)
    npob[1]=epob[1]
    npob[2]=epob[2]
    for (ind in 1:((tam_pob-2)/2)) {
      #se hace la cruza
      j=sample(1:(tam_pob-2),1)
      k=sample(1:(tam_pob-2),1) #se puso el -2 para no volver a cojer los 2 mejores
      if(runif(1)<probCruce)
      {
        hijos=cruza(epob[j], epob[k])
        hijo1=hijos[1]
        hijo2=hijos[2]
      }else
      {
        hijo1=epob[j]
        hijo2=epob[k]
      }
      #ahora la mutacion
      if(runif(1)<probMut)
        hijo1=muta(hijo1) #se quito el parametro de nchar(cadena)
      if(runif(1)<probMut)
        hijo2=muta(hijo2)
      npob[ind*2+1]=hijo1
      npob[ind*2+2]=hijo2
    }
    epob=evalPob(npob,cadena)
    print(epob$pob)
    epob=epob$pob
    i=i+1
  }
  return(i)
}

