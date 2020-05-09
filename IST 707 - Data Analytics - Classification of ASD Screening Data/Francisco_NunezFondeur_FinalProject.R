##############################################
# Name: Francisco Nunez-Fondeur
# Description: IST 707 - Syracuse University
# Date: 05/16/2019 - Final Project - Autism AQ-10
#
# -------------------------------------------



#----- Installs ----#

#Install c50
if (!require(C50)) install.packages('c50')

#Install caret
if (!require(caret)) install.packages('caret')

#countrycode
if (!require(countrycode)) install.packages('countrycode')

#Install class
if (!require(class)) install.packages('class')  

#Install dplyr
if (!require(dplyr)) install.packages('dplyr')

#Install e1071
if (!require(e1071)) install.packages('e1071')

#farff
if (!require(farff)) install.packages('farff')

#gmodels
if (!require(gmodels)) install.packages('gmodels')

#Install kernlab
if (!require(kernlab)) install.packages('kernlab')

#SQLDF
if (!require(sqldf)) install.packages('sqldf')

#----- Step 1: Load the data sets-----#

##Load Data 
#The data was from the UCI Machine Learning Repository
#https://archive.ics.uci.edu/ml/datasets/Autistic+Spectrum+Disorder+Screening+Data+for+Children++#
#https://archive.ics.uci.edu/ml/datasets/Autistic+Spectrum+Disorder+Screening+Data+for+Adolescent+++
#https://archive.ics.uci.edu/ml/datasets/Autism+Screening+Adult

#rawdata
filepath = 'F:/School/Syracuse University/MS Data Sciences/School Work/IST/IST 707/Project/Data/Austism-Child-Data.arff'
Autism_Child_Data <- readARFF("F:/School/Syracuse University/MS Data Sciences/School Work/IST/IST 707/Project/Data/Autism-Child-Data.arff")
Autism_Adult_Data <- readARFF("F:/School/Syracuse University/MS Data Sciences/School Work/IST/IST 707/Project/Data/Autism-Adult-Data.arff")
Autism_Adolescent_Data <- readARFF("F:/School/Syracuse University/MS Data Sciences/School Work/IST/IST 707/Project/Data/Autism-Adolescent-Data.arff")

#sets for editing
AutChild <- Autism_Child_Data
AutTeen <- Autism_Adolescent_Data
AutAdult <- Autism_Adult_Data

#----- Reorganizing Data -----#

#I will reoganize my set columns to match the attribute information document provided
AutChild <- AutChild[,c(21,11:15,20,16,17,19,1:10,18)]
AutTeen <- AutTeen[,c(21,11:15,20,16,17,19,1:10,18)]
AutAdult <- AutAdult[,c(21,11:15,20,16,17,19,1:10,18)]

#Cleansing errors
  #AutAdult
  AutAdult$ethnicity <- as.character(AutAdult$ethnicity)
  AutAdult$ethnicity[is.na(AutAdult$ethnicity)] <- "Unknown"
  AutAdult$ethnicity <- as.factor(AutAdult$ethnicity)
  AutAdult$relation <- as.character(AutAdult$relation)
  AutAdult$relation[is.na(AutAdult$relation)] <- "Unknown"
  AutAdult$relation <- as.factor(AutAdult$relation)
  
  #AutChild
  AutChild$ethnicity <- as.character(AutChild$ethnicity)
  AutChild$ethnicity[is.na(AutChild$ethnicity)] <- "Unknown"
  AutChild$ethnicity <- as.factor(AutChild$ethnicity)
  AutChild$relation <- as.character(AutChild$relation)
  AutChild$relation[is.na(AutChild$relation)] <- "Unknown"
  AutChild$relation <- as.factor(AutChild$relation)
  AutChild$relation <- gsub("self", "Self", AutChild$relation)

  #AutTeen
  AutTeen$ethnicity <- as.character(AutTeen$ethnicity)
  AutTeen$ethnicity[is.na(AutTeen$ethnicity)] <- "Unknown"
  AutTeen$ethnicity <- as.factor(AutTeen$ethnicity)
  AutTeen$relation <- as.character(AutTeen$relation)
  AutTeen$relation[is.na(AutTeen$relation)] <- "Unknown"
  AutTeen$relation <- as.factor(AutTeen$relation)
  AutTeen$age_desc <- as.factor("12-17 years")
  
  #Replace NAs in age column with mean value
  AutAdult$age[is.na(AutAdult$age)] <- mean(AutAdult$age, na.rm=TRUE)
  AutChild$age[is.na(AutChild$age)] <- mean(AutChild$age, na.rm=TRUE)
  AutTeen$age[is.na(AutTeen$age)] <- mean(AutTeen$age, na.rm=TRUE)

  #change col classification
  cols = c(11:21);    
  AutAdult[,cols] = apply(AutAdult[,cols], 2, function(x) as.numeric(as.character(x)));
  AutChild[,cols] = apply(AutChild[,cols], 2, function(x) as.numeric(as.character(x)));
  AutTeen[,cols] = apply(AutTeen[,cols], 2, function(x) as.numeric(as.character(x)));

  #combining the three sets into one total set
  AutTotal <- rbind(AutChild,AutTeen,AutAdult)
  colnames(AutTotal)[1] <- "ClassASD"
  colnames(AutTotal)[6] <- "AutismInFamily"
  colnames(AutTotal)[8] <- "country"
  colnames(AutTotal)[9] <- "used_app"
  summary(AutTotal)
  str(AutTotal)
  
  #----- Look At ethnicity-----# 
  List <- NA
  List <- sqldf("select 
                          ethnicity
                          from AutTotal
                          group by ethnicity")
  # I ran List here and observed 12 ethnicities
  
  #----- Look At countries-----# 
  List <- NA
  List <- sqldf("select 
                          country
                          from AutTotal
                          group by country")
  # I ran List here and observed 89 countries
  
  #Merge Duplicate Classifications
  #AutTotal$gender <- gsub("m", "Gender-M", AutTotal$gender)
  #AutTotal$gender <- gsub("f", "Gender-F", AutTotal$gender)
  AutTotal$ethnicity <- gsub("Hispanic", "Latino", AutTotal$ethnicity)
  AutTotal$ethnicity <- gsub("Hispanic", "Latino", AutTotal$ethnicity)
  AutTotal$ethnicity <- gsub("others", "Others", AutTotal$ethnicity)
  AutTotal$ethnicity <- gsub("Others", "Other-Ethnicity", AutTotal$ethnicity)
  AutTotal$ethnicity <- gsub("Unknown", "Unknown-Ethnicity", AutTotal$ethnicity)
  AutTotal$jundice <- gsub("yes", "JundiceY", AutTotal$jundice)
  AutTotal$jundice <- gsub("no", "JundiceN", AutTotal$jundice)
  AutTotal$AutismInFamily <- gsub("yes", "FamAutismY", AutTotal$AutismInFamily)
  AutTotal$AutismInFamily <- gsub("no", "FamAutismN", AutTotal$AutismInFamily)
  AutTotal$relation <- gsub("Health care professional", "RelationHealthcarePro", AutTotal$relation)
  AutTotal$relation <- gsub("Others", "RelationOther", AutTotal$relation)
  AutTotal$relation <- gsub("Parent", "RelationParent", AutTotal$relation)
  AutTotal$relation <- gsub("Relative", "RelationRelative", AutTotal$relation)
  AutTotal$relation <- gsub("Self", "RelationSelf", AutTotal$relation)
  AutTotal$relation <- gsub("Unknown", "RelationUnkown", AutTotal$relation)
  AutTotal$country <- gsub("Viet Nam", "Vietnam", AutTotal$country)
  AutTotal$used_app <- gsub("yes", "UsedAppY", AutTotal$used_app)
  AutTotal$used_app <- gsub("no", "UsedAppN", AutTotal$used_app)
  AutTotal$age_desc <- gsub("4-11 years", "4to11", AutTotal$age_desc)
  AutTotal$age_desc <- gsub("12-17 years", "12to17", AutTotal$age_desc)
  AutTotal$age_desc <- gsub("18 and more", "18Plus", AutTotal$age_desc)
  
  #New Continent Column created
  AutTotal$continent <- countrycode(sourcevar = AutTotal[, "country"], origin = "country.name", destination = "continent")
  AutTotal[15, 22] <- "Europe"
  AutTotal[196, 22] <- "Americas"
  AutTotal <- AutTotal[,c(1,3,5:7,9:21)]
  
  #----- Creating Training and Test Sets FOR ONLY Questionaire-----#
          #Model shows 100% accuracy in decision tree.
          #Predicting factor is a score >6 (results)
          Total1 <- AutTotal[,c(1, 8:18)] 
          #create sets using caret library
          inTraining <- createDataPartition(Total1$ClassASD, times = 1, p = .66, list = FALSE)
          trainAut <- Total1[ inTraining,]
          trainAut$ClassASD <- droplevels( trainAut)$ClassASD
          testAut  <- Total1[-inTraining,]
          testAut$ClassASD <- droplevels(testAut)$ClassASD
          
          #building Model using c50 Algorithim (c50 library)
          dt_model <- C5.0(trainAut[-1], trainAut$ClassASD)
          dt_model
          
          summary(dt_model) 
          
          #evaluating performance
          pred_model <- predict(dt_model, testAut)
          CrossTable(testAut$ClassASD, pred_model, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual diagnosis', 'predicted diagnosis') )
  
  
  #Create Expanded  Set for more DM
  #help from https://www.r-statistics.com/tag/transpose/
  AutExp <- AutTotal
                      #----- Step 1: Gender-----# 
                      List <- NA
                      List <- sqldf("select 
                                          Gender
                                          from AutExp
                                          group by Gender")
                      List1 <- List
                      Column <- NA
                      Column <- t(List1)
                      colnames(Column) <- Column[1,]
                      NewDF <- Column
                      #Part2
                      B <- NA
                      B <- matrix(NA, nrow = (length(AutExp$ClassASD)), ncol = (length(List$gender)))
                      B <- as.data.frame (B)
                      colnames(B) <- Column
                      colnames(B) <- Column[1,]
                      
                      #Combine
              NewAutDF <- cbind(AutExp,B)
                     #----- Step 2: Jundice-----# 
                      List <- NA
                      List <- sqldf("select 
                                                          jundice
                                                          from AutExp
                                                          group by jundice")
                      List1 <- List
                      Column <- NA
                      Column <- t(List1)
                      colnames(Column) <- Column[1,]
                      NewDF <- Column
                      #Part2
                      B <- NA
                      B <- matrix(NA, nrow = (length(AutExp$ClassASD)), ncol = (length(List$jundice)))
                      B <- as.data.frame (B)
                      colnames(B) <- Column
                      colnames(B) <- Column[1,]
                      #Combine
              NewAutDF <- cbind(NewAutDF,B)
                      #----- Step 3: AutismInFamily -----# 
                      List <- NA
                      List <- sqldf("select 
                                                                  AutismInFamily 
                                                                  from AutExp
                                                                  group by AutismInFamily")
                      List1 <- List
                      Column <- NA
                      Column <- t(List1)
                      colnames(Column) <- Column[1,]
                      NewDF <- Column
                      #Part2
                      B <- NA
                      B <- matrix(NA, nrow = (length(AutExp$ClassASD)), ncol = (length(List$AutismInFamily )))
                      B <- as.data.frame (B)
                      colnames(B) <- Column
                      colnames(B) <- Column[1,]
                      #Part3
              NewAutDF <- cbind(NewAutDF,B)        
                      #----- Step 4: Relation -----# 
                      List <- NA
                      List <- sqldf("select 
                                                    relation
                                                    from AutExp
                                                    group by relation")
                      List1 <- List
                      Column <- NA
                      Column <- t(List1)
                      colnames(Column) <- Column[1,]
                      NewDF <- Column
                      #Part2
                      B <- NA
                      B <- matrix(NA, nrow = (length(AutExp$ClassASD)), ncol = (length(List$relation)))
                      B <- as.data.frame (B)
                      colnames(B) <- Column
                      colnames(B) <- Column[1,]
                      #Part3
            NewAutDF <- cbind(NewAutDF,B)  
                      #----- Step 5: Used App-----# 
                      List <- NA
                      List <- sqldf("select 
                                                                    used_app
                                                                    from AutExp
                                                                    group by used_app")
                      List1 <- List
                      Column <- NA
                      Column <- t(List1)
                      colnames(Column) <- Column[1,]
                      NewDF <- Column
                      #Part2
                      B <- NA
                      B <- matrix(NA, nrow = (length(AutExp$ClassASD)), ncol = (length(List$used_app)))
                      B <- as.data.frame (B)
                      colnames(B) <- Column
                      colnames(B) <- Column[1,]
                      #Part3
              NewAutDF <- cbind(NewAutDF,B)  
                    #----- Step 5: Age Desc-----# 
                    List <- NA
                    List <- sqldf("select 
                                                                          age_desc
                                                                          from AutExp
                                                                          group by age_desc")
                    List1 <- List
                    Column <- NA
                    Column <- t(List1)
                    colnames(Column) <- Column[1,]
                    NewDF <- Column
                    #Part2
                    B <- NA
                    B <- matrix(NA, nrow = (length(AutExp$ClassASD)), ncol = (length(List$age_desc)))
                    B <- as.data.frame (B)
                    colnames(B) <- Column
                    colnames(B) <- Column[1,]
                    #Part3
              NewAutDF <- cbind(NewAutDF,B)  
  AutExp2 <- NewAutDF
  
  #2 Fill in Rows for DM
  #From https://www.listendata.com/2017/03/if-else-in-r.html
  #mydata$x4 = ifelse(mydata$x2>150,1,0)
          colnames(AutExp2)

          AutExp2[,19] = ifelse((AutExp2[,2])==(colnames(AutExp2[19])),1,0)
          AutExp2[,20] = ifelse((AutExp2[,2])==(colnames(AutExp2[20])),1,0)
          AutExp2[,21] = ifelse((AutExp2[,3])==(colnames(AutExp2[21])),1,0)
          AutExp2[,22] = ifelse((AutExp2[,3])==(colnames(AutExp2[22])),1,0)
          AutExp2[,23] = ifelse((AutExp2[,4])==(colnames(AutExp2[23])),1,0)
          AutExp2[,24] = ifelse((AutExp2[,4])==(colnames(AutExp2[24])),1,0)
          AutExp2[,25] = ifelse((AutExp2[,5])==(colnames(AutExp2[25])),1,0)
          AutExp2[,26] = ifelse((AutExp2[,5])==(colnames(AutExp2[26])),1,0)
          AutExp2[,27] = ifelse((AutExp2[,5])==(colnames(AutExp2[27])),1,0)
          AutExp2[,28] = ifelse((AutExp2[,5])==(colnames(AutExp2[28])),1,0)
          AutExp2[,29] = ifelse((AutExp2[,5])==(colnames(AutExp2[29])),1,0)
          AutExp2[,30] = ifelse((AutExp2[,5])==(colnames(AutExp2[30])),1,0)
          AutExp2[,31] = ifelse((AutExp2[,6])==(colnames(AutExp2[31])),1,0)
          AutExp2[,32] = ifelse((AutExp2[,6])==(colnames(AutExp2[32])),1,0)
          AutExp2[,33] = ifelse((AutExp2[,7])==(colnames(AutExp2[33])),1,0)
          AutExp2[,34] = ifelse((AutExp2[,7])==(colnames(AutExp2[34])),1,0)
          AutExp2[,35] = ifelse((AutExp2[,7])==(colnames(AutExp2[35])),1,0)
          
#----- Creating Traing and Test Sets FOR ALL FACTORS (No quetionaire)-----#
        #Total2 <- AutExp2[,c(1, 2, 11:137)] 
        #Total2 <- AutExp2[,c(1, 2, 22:53)] 
        Total2 <- AutExp2[,c(1,19:35)]
        
        #create sets using caret library
        inTraining <- createDataPartition(Total2$ClassASD, times = 1, p = .66, list = FALSE)
        trainAut <- Total2[ inTraining,]
        trainAut$ClassASD <- droplevels( trainAut)$ClassASD
        testAut  <- Total2[-inTraining,]
        testAut$ClassASD <- droplevels(testAut)$ClassASD
        
        #building Model using c50 Algorithim (c50 library)
        dt_model <- C5.0(trainAut[-1], trainAut$ClassASD)
        dt_model #use summary(dt_model) to get the % accuracy and tree
        
        summary(dt_model) 
        
        #evaluating performance
        pred_model <- predict(dt_model, testAut)
        CrossTable(testAut$ClassASD, pred_model, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual diagnosis', 'predicted diagnosis') )
        
        #improving model performance
        dt_boost10 <- C5.0(trainAut[-1], trainAut$ClassASD, trials= 10)
        dt_boost10 #use summary(dt_boost10) to get the % accuracy and tree
        summary(dt_boost10)
        
        #evaluating boosted performance
        pred_model_boost  <- predict(dt_boost10, testAut)
        CrossTable(testAut$ClassASD, pred_model_boost, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual diagnosis', 'predicted diagnosis') )

 #----- Use Naïve Bayes Model-----# 
        #build the classifiers
        Autclassifier<- naiveBayes(trainAut, trainAut$ClassASD)
        Autclassifier
        
        #evaluate model classfier
        Auttestpredicter <- predict(Autclassifier, testAut)
      
        #compare predictions to true values
        CrossTable(testAut$ClassASD, Auttestpredicter, prop.chisq = FALSE, prop.t = FALSE, dnn = c('actual diagnosis', 'predicted diagnosis') )
      
        #create confusion matrix
        cMatrix <- table(Auttestpredicter, testAut$ClassASD)
        plot(cMatrix)
        confusionMatrix(cMatrix)
        
        #build improved model
        Autclassifier2<- naiveBayes(trainAut, trainAut$ClassASD, laplace = 3)
        Autclassifier2
        
        #evaluate improved model
        Auttestpredicter2 <- predict(Autclassifier2, testAut)
        
        #compare improved predictions to true values
        CrossTable(testAut$ClassASD, Auttestpredicter2, prop.chisq = FALSE, prop.t = FALSE, dnn = c('actual diagnosis', 'predicted diagnosis') )
 
#----- Use k-NN Model-----# 
        #model training
        trainAutlabel <- trainAut[,1]
        testAutlabel <- testAut [,1]
        knnAutSpread <- knn(train=trainAut[,-1], testAut [,-1], cl=trainAutlabel, k=7)
        summary(knnAutSpread)
        CrossTable(x=testAutlabel, y=knnAutSpread, prop.chisq = FALSE)
        
        #improved performance using z-score standardization
        #new model training
        Total2z <- as.data.frame(scale(Total2[,-1]))
        Total2z_train <- Total2z[1:733,]
        Total2z_test <- Total2z[734:1100,]
        Total2z_train_label <- Total2[1:733,1]
        Total2z_test_label <- Total2[734:1100,1]
        
        #new performance
        knnAutPrediction <- knn(train=Total2z_train, Total2z_test, cl=Total2z_train_label, k=11)
        summary(knnAutPrediction)
        CrossTable(x=Total2z_test_label, y=knnAutPrediction, prop.chisq = FALSE)   
        
#----- Use SVM Model-----#      
        #build the model classifier
        ksvmmodelclassifier <- ksvm(ClassASD ~ ., data=trainAut, kernel ="vanilladot")
        ksvmmodelclassifier
        
        #evaluate performance
        ksmvmodelpredictions  <- predict(ksvmmodelclassifier, testAut)
        CrossTable(testAut$ClassASD, ksmvmodelpredictions, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual number', 'predicted number') )
        #model predicted 236 / 373 (63.27%)
        
        #improved performance
        ksvmmodelclassifierrbf <- ksvm(ClassASD ~ ., data=trainAut, kernel ="rbfdot")
        ksvmmodelclassifierrbf
        ksmvmodelpredictionsrbf  <- predict(ksvmmodelclassifierrbf, testAut)
        CrossTable(testAut$ClassASD, ksmvmodelpredictionsrbf, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual number', 'predicted number') )
        #model predicted 247/ 373 (66.21%) 