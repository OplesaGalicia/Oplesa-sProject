install.packages("Rcmdr", dependencies=TRUE)
installed.packages("tcltk")

library(tcltk2)
library(gWidgets)


#abre un csv con un open file dialog y lo guarda en csv_data que esta en un
 # global environment
getcsv <- function() {
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {CSV Files} {.csv} } { {All Files} * }"))
  if (name == "")
    return(data.frame()) # Return an empty data frame if no file was selected
  data <- read.csv(name)
  assign("csv_data", data, envir = .GlobalEnv)
  cat("The imported data are in csv_data\n")
 
  
 
}

verData <- function()
{
 
   
   #funciones para predecir
   
   
   
   library(caret)
   
   
   
   setwd("C:\\Users\\Propietario\\OneDrive\\Documentos")
   #irisdata <- read.csv("iris.data.csv", header=T)
   irisdata <- csv_data
   colnames(irisdata) <- c("sepal.length","sepal.width","petal.length","petal.width","species")
   str(irisdata)
   View(irisdata)
   
   
   
   #spliteamos el dataset en 2, el 80% para entrenar los modelos y el 20%
   #para testear el modelo
   #split the data
   sample = createDataPartition(irisdata$species, p=0.80, list=FALSE)
   guardaIrisTOtrain = sample
   #create training data     entrena con el 80% o sea 120
   iris_train <- irisdata[sample,]
   str(iris_train)
   
   #creamos ahora el test data
   iris_test <- irisdata[-sample,]
   str(iris_test)
   # intento xd as.numeric(levels(iris_test))[iris_test]
   
   #SUMMARIZE DATA
   percentage <- prop.table(table(iris_train$species))*100
   cbind(Freq = table(iris_train$species), Percentage= percentage)
   #con esto podemos ver que cada clase tiene el mismo numero de instancias
   # 40 equivalente al  33% del dataset (por flor)
   
   summary(iris_train)
   
   #VISUALIZACION de data
   # split the input attributes x and the output attribute(or class) y.
   x <- iris_train[,1:4]
   y <- iris_train[,5]  #guarda los nombres de las 120
   
   #box plots, input variables are numeric
   #par(mfrow=c(1,4))
   #for (i in 1:4) {
   #  boxplot(x[i], main=names(iris_train)[i])
   #}  ESTO ME SACABA UN ERROR: adding class "factor" to an invalid object
   
   
   library(ggplot2)
   qplot(y, xlab='species') #grafica las muestras de cada especie de iris
   qplot(x[,1],xlab = 'sepal.width') #grafica todas las muestras de anchura sepalosa :)
   
   ### todo esto no jala  
   library(caret)
   featurePlot(x=x[1], y=y)
   featurePlot(x=x, y=y, plot='box', auto.key=list(columns=3))
   
   featurePlot(x=x, y=y, 
               plot='density', 
               scales = list(x = list(relation='free'),
                             y = list(relation='free')),
               auto.key=list(columns=3))
   #hasta aca
   
   
   
   
   #packages e1071 MASS  iban aca
   library(caret)
   control <- trainControl(method='cv', number=10)
   metric <- 'Accuracy'
   
   # Linear Discriminant Analysis (LDA)
   #set.seed(101)
   #fit.lda <- train(species~., data=iris_train, method='lda', 
   #                trControl=control, metric=metric)
   # NO JALO BIEN CON EL LDA
   
   
   
   
   #metodo de KNN k nearest node   ESTE NO LO JALA
   #set.seed(101)
   #fit.knn <- train(Species~., data=iris_train, method='knn', 
   #                trControl=control, metric=metric)
   
   # Support Vector Machines (SVM) with a radial kernel
   set.seed(101)
   fit.svm <- train(species~., data=iris_train, method='svmRadial', 
                    trControl=control, metric=metric)
   
   iris_prediction <- predict(fit.svm, iris_test)
   
   #caret confMat(data=> predicted classes, reference => factor of classes to be used as the true results)
   ConfMatrix <- confusionMatrix(iris_prediction, iris_test$species)
   ConfMatrix
   
   CM <- table(iris_test[,5], iris_prediction) 
   #contiene la col de iris test con los nombres, la iris_prediction con nombres y las Freq
   View(CM)
   
   
   bypass = ConfMatrix$byClass
   str(bypass)
   bypass[31:33]  #guarda los accuracy
   BalancedAccuracy <- bypass[31:33]
   View(bypass[31:33])
   
   #funciones para predecir
   
   
   #tkmessageBox(title="titulito", message = paste0("hola",CM,"."))
   #View(csv_data)
   #win4<- tktoplevel()
   #win4$env$butDlg <- tk2button(win4,text="launch")
   #tkpack(win4$env$butDlg,padx=60,pady=50)
   
   
   
   
   
   mat1 <-matrix(c("Setosa","Versicolor","Virginica","Setosa","Versicolor","Virginica",
                   "Setosa","Versicolor","Virginica",
                   "setosa","setosa","setosa","Versicolor","Versicolor","Versicolor",
                   "Virginica","Virginica","Virginica",
                   CM),ncol=3)
   
   tclTable <- tclArray()
   for(i in 1:nrow(mat1))
     for(j in 1:ncol(mat1))
       tclTable[[i-1, j-1]] <- strsplit(mat1[i,j], " " , fixed = TRUE)[[1]]
   
   
   win5 <-tktoplevel()
   tktitle(win5) <- "Predicciones y Fitness"
   win5$env$table1 <- tk2table(win5, variable=tclTable, rows=9,cols=3,titlerows=0,selectmode="extended", colwidth= 25, background="white")
  # tkgrid(tk2label(win5, text= BalancedAccuracy[1] ), padx=60, pady=c(5,35))
   tkpack(win5$env$table1,fill="both", expand = TRUE)
   
   
   #balance accur mensajes
   AccuracySetosa <- cat("Setosa  : " , BalancedAccuracy[1] )
   AccuracySetosa0 <- cat("Versicolor  : " , BalancedAccuracy[2] )
   AccuracySetosa2 <- cat("Setosa  : " , BalancedAccuracy[3] )
   AccuracySetosa2
   
   winPoisson <- tktoplevel()
   tktitle(winPoisson) <- "Selecciona n errores a predecir"
   tkgrid(tk2label(winPoisson, text= paste("Porcentaje Exito Setosa",BalancedAccuracy[1]) ), padx=60, pady=c(5,20))
   tkgrid(tk2label(winPoisson, text= paste("Porcentaje Exito Versicolor",BalancedAccuracy[2]) ), padx=60, pady=c(5,20))
   tkgrid(tk2label(winPoisson, text= paste("Porcentaje Exito Virginica",BalancedAccuracy[3]) ), padx=60, pady=c(5,20))
   tkgrid(tk2label(winPoisson, text="Cantidad de futuros errores a calcular por POISSON:", justify="left"), padx=10,pady= c(15,5), sticky="w")
   #input para los errores a predecir
   name <- tclVar("3")
   winPoisson$env$entName <-tk2entry(winPoisson, width = "25", textvariable = name)
   tkgrid(winPoisson$env$entName, padx=60, pady=c(5,20))
   tkbind(winPoisson$env$entName, "<Return>")
   winPoisson$env$butCalcular <- tk2button(winPoisson, text="Calcular", width=-6)
   tkgrid(winPoisson$env$butCalcular, padx=10, pady= c(5,15))
   
   
   
   
}

win1 <- tktoplevel()
tktitle(win1) <- "Menu Principal"
win1$env$menu <- tk2menu(win1)
tkconfigure(win1, menu = win1$env$menu)
#win1$env$menuFile <- tk2menu(win1$env$menu, tearoff=FALSE)
win1$env$butSelect <- tk2button(win1, text = "Select CSV File", command = getcsv)
#tkadd(win1$env$menu,"cascade", label="file", menu=win1$env$butSelect)
win1$env$butSelect2 <- tk2button(win1, text = "ver data", command = verData)
tkpack(win1$env$butSelect, padx = 90, pady=30)
tkpack(win1$env$butSelect2, padx=90, pady=10)










# tkgrid(tk2label(win1,text ="alojo mora"))
#win1$env$butOK <- tk2button(win1, text = "ok", width= -6)

 











win1 <- tktoplevel()
name <- tclVar("Anonymous")
win1$env$entName <-tk2entry(win1, width = "25", textvariable = name) 
tkgrid(tk2label(win1, text = "Please enter your first name:", justify = "left"), padx = 10, pady = c(15, 5), sticky = "w") 
tkgrid(win1$env$entName, padx = 10, pady = c(0, 15))
onOK <- function() 
  { nameVal <- tclvalue(name) 
  tkdestroy(win1) 
  msg <- paste("You have a nice name,", nameVal) 
  tkmessageBox(message = msg) } 
win1$env$butOK <-tk2button(win1, text = "OK", width = -6, command = onOK)
tkgrid(win1$env$butOK, padx = 10, pady = c(5, 15)) 
tkbind(win1$env$entName, "<Return>", onOK)








win2 <- tktoplevel() 
#labelText <- tclVar("This is a text label") 
labelText <- csv_data
win2$env$label <- tk2label(win2, textvariable = labelText) 
tkgrid(win2$env$label)



#al presionar el boton OK, ejecuta la funcion
pressedOK <- function()
  tkmessageBox(message = "You pressed OK!")

win1 <- tktoplevel()	# Create a new Tk window
tktitle(win1) <-"tk ventana 1"
win1$env$butOK <- tk2button(win1, text = "OK", width = -6, command = pressedOK)
tkgrid(win1$env$butOK, padx = 150, pady = 30)		# Place the button on the window
#tkdestroy(win1)  #destruye





