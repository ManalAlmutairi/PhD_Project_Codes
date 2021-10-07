# ============================================

# The implementation of PRISM algorithm in R
# Manal Khalaf ALmutairi

# ===========================================


# install necessary packegs

# install.packages(c ("RWeka"), dependencies = TRUE )
# install.packages( "xlsx")

#install.packages("keys")
#install.packages("hash")
#install.packages("hashmap")

# loading libraries
# library(RWeka)
#library (hash)
#library (hashmap)

start.time <- proc.time()

library(data.table)
library (caTools)
library (plyr)
library (xlsx)
library (ggplot2)
library(stats)
library (MASS)
library( caret)
library (ROCR)
library(discretization)
#library(dprep)
#library(dplyr)


# import the dataset file using URL file

#myData <-  read.table ("data/iris.data", header = FALSE)   # iris dataset

#myData <-  read.table ("data/transfusion.data", header = FALSE, sep = ",")   # blood transfusion dataset

#fileName <- "data/iris.data"        # " " #

#fileName <- "data/seeds.data"      # " " # 

#fileName <- "data/wine.data"        # , #

#fileName <- "data/transfusion.data"    # , #

#fileName <- "data/banknote.data"     # , #

#fileName <- "data/ecoli.data"         # " " # 

#fileName <- "data/yeast.data"

#fileName <- "data/page-blocks.data"         # " " #

#fileName <- "data/Data_User_Modeling.xlsx"

fileName <- "data/BreastTissue.xlsx"

#fileName <- "data/glass.data"



#fileName <- "data/shuttle.data"

#fileName <- "data/lenses.data"

#fileName <- "data/magic04.data"

#fileName <- "data/skin-segmentation.data"

#fileName <- "data/tae.data"                 # Mixed attributes   not working 

#fileName <- "data/diagnosis.data"             # Mixed attributes     not working 

#fileName <-  "data/abalone.data"     # 29 classes   <<< changed to be 3 categories 

#fileName <- "data/cmc.data"

#fileName <- "data/breast-cancer-wisconsin.data"

#fileName <- "data/fertility_Diagnosis.data"      #  this dataset is categorical despite having a numeric attributes' values

#fileName <- "data/haberman.data"

if ( fileName == "data/BreastTissue.xlsx") {
   sheetNo <- 2
   
   
} else if (fileName == "data/Data_User_Modeling.xlsx" ) {
   
   sheetNo <- 1
}

 myData <- read.xlsx( fileName, sheetNo ,  as.data.frame=TRUE, header=TRUE, stringsAsFactors = FALSE)


#myData <- read.table(fileName ,header = FALSE, stringsAsFactors = FALSE)     # Dataset that use space sperator
 
 
#myData <-  read.table (fileName , header = FALSE, sep = "," , stringsAsFactors = FALSE)   # dataset that use comma Separator
 


# to add headers to the columns



if (fileName == "data/lenses.data")  {
   myData <- myData [,-1]
   myData <- data.frame((lapply(myData, as.character)))
   names (myData) <- c ("age", "specRx", "astig", "tears", "class")
   
} else if (fileName == "data/wine.data") {
   names ( myData) <- c ("class", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium",
                         "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins",
                         "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")        
   
} else if (fileName == "data/iris.data" | fileName == "data/iris_short.data") {
   names (myData) <- c ("sepal length", "sepal width", "petal length", "petal width", "class")
   
} else if  (fileName == "data/transfusion.data") {
   names (myData) <- c ("Recency", "Frequency", "Monetary", "Time", "class")
   
} else if (fileName == "data/seeds.data") {
   names (myData) <- c ("Area A", "perimeter P", "Compactness C", "length of kernel", "width of kernel",
                        "asy mmetry coefficient", "length of kernel groove", "class")     # seeds dataset
   
   
} else if (fileName == "data/banknote.data") {
   names (myData) <- c ( "variance of Wavelet", "skewness", "curtosis", "entropy", "class" )
   
} else if (fileName == "data/page-blocks.data") {
   names (myData) <- c ( "height", "lenght", "area", "eccen", "p_black", "p_and", "mean_tr", 
                         "blackpix", "blackand", "wb_trans" , "class"  )
   
} else if (fileName == "data/magic04.data") {
   names (myData) <- c ( "fLength" ,"fWidth", "fSize", "fConc", "fConc1", "fAsym", "fM3Long", 
                         "fM3Trans", "fAlpha", "fDist", "class" )
   
} else if (fileName == "data/yeast.data") { 
   names ( myData) <- c ( "Sequence Name", "mcg" , "gvh", "alm", "mit" , "erl" , "pox", "vac", "nuc", "class")
   
} else if (fileName ==  "data/skin segmentation.data") {
   names ( myData) <- c ( "B" , "G", "R", "class" )
   
} else if (fileName == "data/tae.data") {
   
   names (myData) <- c ( "TA speaker", "Course instructor", "Course","semester", "Class size", "Class attribute")
   
}else if (fileName == "data/diagnosis.data")  {
   
   names (myData) <- c ( "Temperature", "nausea", "Lumbar pain", "Lumbar pain", 
                         "Micturition pains", "Burning of urethra", "bladder inflammation","Nephritis of renal pelvis origin" )
   
} else if ( fileName == "data/abalone.data")  {
   names (myData)  <- c ( "Sex", "Length", "Diameter", "Height", "Whole", "Shucked", "Viscera", "Shell", "Rings")   
   
   
} else if ( fileName == "data/cmc.data") {
   
   names  (myData) <- c ( "Wife's age", "Wife's education", "Husband's education", "No of children", "Wife's religion", "Wife's now working?" , "Husband's occupation",
                          "Living index", "Media exposure", "Contraceptive used"   )
   
} else if ( fileName == "data/ecoli.data") {
   
   names (myData) <- c ( "Sequence Name", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2", "class")
   
} else if ( fileName == "data/breast-cancer-wisconsin.data") {
   names ( myData) <- c ("code number", "Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", 
                         "Marginal Adhesion", "Single Epithelial Cell Size", "Bare Nuclei", "Normal Nucleoli", "Mitoses", "class")
   
} else if (fileName == "data/glass.data") {
   names(myData) <- c ( "Id", "RI", "Na", "Mg", "Al","Si", "K", "Ca", "Ba", "Fe", "class")    
}

# remove the first column if it only contained ids  
if (colnames(myData[1]) == "Id") myData <- myData [-1]


# splitting data into tarining subset (70%) and testing subset (30%) 

# set.seed(2373)
# train <- sample (nrow(myData), 0.7 * nrow(myData))
# trainData <- (myData [train, ])
# testData <- (myData [-train, ])
# table (trainData$class)
# table (testData$class)


# .Random.seed[1:6]
#  seed is an integer vector, containing the random number generator (RNG) state for random number generation in R

seedNo <- 10    
set.seed(seedNo)       

if (fileName == "data/BreastTissue.xlsx") { 
   myData [, -1] <-  round (myData [,-1], 2)
}

# delete the first column in this dataset which contains ids (unpredictive attribute)
if (fileName == "data/ecoli.data"  | fileName == "data/yeast.data") 
{ 
   myData <- myData[ , -1]  }

if (fileName == "data/Data_User_Modeling.xlsx"){
   
   trainData <- read.xlsx( "data/Data_User_Modeling.xlsx", 1 ,  colIndex=1:6, as.data.frame=TRUE, header=TRUE,stringsAsFactors = FALSE )
   testData <- read.xlsx( "data/Data_User_Modeling.xlsx", 2 ,  colIndex=1:6, as.data.frame=TRUE, header=TRUE, stringsAsFactors = FALSE )
  
   # combine train and test data in order to use the  
   myData <- rbind(trainData, testData)
   
} 

# normalise attributes values to be between 0 and 1 
f.normalisation <- function( x )  {
   normalisedValues <- (x-min(x))/(max(x)-min(x))
   return(normalisedValues)
   
}
tempNormData <- myData
# get the number of  classes in the datastet  (extra info)
numOfClasses <- length(unique(myData$class))   

numofCol <- ncol(myData)

# determine which column is the class column
classCol <-  which(colnames(myData) == "class")

# Normalise data set values in case of negative values 
if (fileName == "data/magic04.data") {
   #Normalize Data
   for ( normaliseThis in 1 : numofCol ) {
      
      if (normaliseThis  != classCol) 
      tempNormData [normaliseThis] <- f.normalisation ( tempNormData [normaliseThis])
      
   }
   myData <-  tempNormData
   
}

# in some datasets class column come first so it shouldn't be counted as a predictive column such as in wine datset

if (classCol == numofCol ) { allPredictAttr <- (numofCol - 1)
} else  {allPredictAttr <- numofCol  }

# first attribute in yeast dataset and wine dataset is not a predictive attribute
if ( fileName == "data/wine.data" |  fileName == "data/BreastTissue.xlsx" ){ 
   
   # Reorder the dataframe cloumns to move the class column from first to last (this step is only done in this version of PRISM algorithm)
   tempData <- myData [ , c (2 : allPredictAttr, classCol)]
   myData <- tempData
   allPredictAttr <- (numofCol - 1)
   predictAttr <-  1 
} else  predictAttr <- 1

# find out whether attributes variables are considered as a factor or continuous?
checkForNumAtrr <- sapply(myData, is.numeric)
dataContainsNumericAttr<- which (checkForNumAtrr)

# preprocessing step: convert the numeric data into categorical using Chimerge algorithm
#numToCat<- chiM(myData , alpha = 0.05)
numToCat <- modChi2(myData, alp = 0.5)
#numToCat_2 <- chi2(myData , alp = 0.1, del = 0.05 )

tempData <- numToCat$Disc.data

# convert the numerical discretised data into factors to be treated as categorical variable in PRISM algorithm
myData <- numToCat$Disc.data
for ( discretAttr in predictAttr:allPredictAttr) {

   myData [[discretAttr]] <- sapply(myData [[discretAttr]], as.factor)

}

# to reduce the overfitting that may occured in the rules 

limitedTermsUsed <- FALSE
maxTerms <- 3
stoppingCriteriaUsed <- FALSE
minimumRuleAccuracy <- 0.00

# This datasets is already sampeled (first 3133 used for training) and (final 1044 used for testing)
if (fileName == "data/abalone.data") {
   trainData <-  myData [1:3133,]
   testData <- myData [3134:4177,]
   
   # this data set is already splitted previously into train data and test sets.
} else if (fileName == "data/Data_User_Modeling.xlsx"){
   # After discrritise the complete data set (myData) , herer we resplit myData back into train and test sets 
   trainData <- myData [1:258 , ]
   testData <- myData [259:403 , ]
   
}  else {
   
   # Another way of splitting using a split ratio according to the class
   sample <- sample.split(myData$class, SplitRatio = .70)
   trainData <- subset(myData, sample == TRUE)
   testData <- subset(myData , sample == FALSE)
   
} 

if  (fileName != "data/Data_User_Modeling.xlsx") {
write.xlsx(myData, "data/myData.xlsx" )
write.xlsx( trainData, "data/myTrainData.xlsx")
write.xlsx( testData, "data/myTestData.xlsx")

}

debugMe <- FALSE
debugPredFun <- FALSE

# ========Function to check if data contains classes other than the given class   ==========
f.classesOtherThanGivenClass <- function( D, inputClass) {
   
   # contains other than the input classs
   containsOtherClasses <- FALSE
   
   print (c("before loop", containsOtherClasses))
   
   # go through each instance in the datasetD
   for ( h in D$class ) {
      
      if( h != inputClass){
         containsOtherClasses <- TRUE
         cat ('Does Dataset contain classes other than ', inputClass, ' ? '  , containsOtherClasses)
         break()
      }
      
   }

   print (c( "after loop",containsOtherClasses))
   
   return (containsOtherClasses) }  

# define original dataset according to the psydocode 
originalData <- trainData


# define untoched data: this dataset will not be changed at any point.
unTouchedData <- trainData

# intializing the type of rule term
ruleTermType <- "0"     

# create a data frame which contains the highest probabilities for each class
ruleTermsWithBestProbability <- data.frame (matrix(nrow = 1, ncol = 6))
names (ruleTermsWithBestProbability) <- c ("Attribute", "Type" ,"Value", "Greater_Equal" ,"Class","Probability")

# define the rules library which contains all the rules for all the classes as a set of data frames
rulesLibrary.R  <- list () 
N <- 1

# create a data frame which contains only data that covered by the induced rule ( R)
dataCoveredByR <- data.frame(matrix(nrow=1,ncol= ncol(originalData)))
names (dataCoveredByR) <- names (originalData)

# to distinguish between different types of attributes :
#  0 = no operator  ( categorical attribute)
#  1 = Smaller than side  ( Numerical attribute)
#  2 = Greater or Equal side   ( Numerical attribute)

operator <- c ( 0, 1 , 2)

# This function is to calculate the probabilitiy according to the current att-val pair combanation

f.calculateRuleTermProbability <-  function (dataD, thisClass, thisAttr, thisAttrVal, thisOperator) {
   #browser()
   ## Compute attribute-value pair frequency at current class
   refinedData <- dataD [dataD[["class"]] == thisClass , ]
  
   
   # Categorical attributet
   if (thisOperator == 0) {
      refinedData <- refinedData[which(as.factor(refinedData[,thisAttr]) == thisAttrVal),]
      
   }else {
      # numerical attribute (smaller side)
      if (thisOperator == 1) {
         refinedData <- refinedData[which((refinedData[,thisAttr]) < thisAttrVal),]
         
      } else {
         # numerical attribute (Greater or equal side)
         if (thisOperator == 2) {
            refinedData <- refinedData[which((refinedData[,thisAttr]) >= thisAttrVal),]
            
         }
      }
      
      
   }
   
   thisRuleTermFrequency <- nrow(refinedData)
   
   ## compute total frequency of current attrbiute-value for the whole dataset
   
   if (thisOperator == 0) {
      
      refinedAttr <- dataD[dataD[[thisAttr]] == thisAttrVal ,]
      thisTotalAttrFrequency <- nrow(refinedAttr)
      
   } else {
      if (thisOperator == 1) {
         refinedAttr <- dataD[dataD[[thisAttr]] < thisAttrVal ,]
         thisTotalAttrFrequency <- nrow(refinedAttr)
         
      }else {
         if (thisOperator == 2) {
            refinedAttr <- dataD[dataD[[thisAttr]] >= thisAttrVal ,]
            thisTotalAttrFrequency <- nrow(refinedAttr)
            
         }
         
      }
      
   }
   
   
   # browser( text = "STOP")
   
   ## Calculate the probablities of occurance, P(class | attrVal), for each possible rule term
   thisRuleTermProbability <- thisRuleTermFrequency / thisTotalAttrFrequency
   
   #thisRuleTermProbability [is.finite(thisRuleTermProbability)] <- 0
   thisRuleTermProbability [is.nan(thisRuleTermProbability)] <- 0
   
   # browser( text =  "STOP")
   
   # @@@ ---- Display The results for current attributes ---- @@@ \
   currentAttrResultTable <- data.frame(matrix(data =  NA, nrow=1,ncol= 7 ))
   #currentAttrResultTable <- matrix( data = NA, nrow = 1 , ncol = 7)
   
   cat ('\n', 'current Attribute  = ', thisAttr, '\n\n')
   names(currentAttrResultTable) <- c("Type", "name", "value", "Operator" ,"Attr Freq", "RT Freq" ,"RT probab")
   
   if (thisOperator == 0 ) {
      currentAttrResultTable [1] <- "Categorical"
      currentAttrResultTable [4] <- NA
   } else {
      if (thisOperator == 1)   {
         currentAttrResultTable [1] <- "Numerical"
         currentAttrResultTable [4] <- "<"
      } else {
         if (thisOperator == 2) {
            currentAttrResultTable [1] <- "Numerical"
            currentAttrResultTable [4] <- ">="
         }
      }
      
   }
   
   currentAttrResultTable[2] <- colnames(dataD[thisAttr])
   currentAttrResultTable[3] <- thisAttrVal
   currentAttrResultTable[5] <- thisTotalAttrFrequency
   currentAttrResultTable[6] <- thisRuleTermFrequency
   currentAttrResultTable[7] <- thisRuleTermProbability
   
   
   print (currentAttrResultTable)
   
   ## display the subset that covered current rule
   cat ('\n', thisRuleTermFrequency ,' rows of the training set covered by current rule term : \n')
   #print (refinedData)
   
   # @@@ ----------------------------- @@@ \
   
   # browser( text = "STOP")
   return (
      list (
         "outRuleTermFrequency" = thisRuleTermFrequency,
         "outTotalAttrFrequency" = thisTotalAttrFrequency,
         "outRuleTermProbability" = thisRuleTermProbability ,
         "outRefinedData" = refinedData) )
   
}   # End of function:  f.calculateRuleTermProbability


# This function is to update the ruleTermProbabilities with a given new input ruleTerm to be added to
f.updateRuleTermProbabilities <- function ( thisRuleTermToAdd,  allRuleTermProbab) {
   
   if (is.na(allRuleTermProbab[1,1]))  {
      allRuleTermProbab [1,] <- thisRuleTermToAdd
      
   } else {
      allRuleTermProbab <- rbind (unique(allRuleTermProbab), thisRuleTermToAdd)
      
   }
   # browser( text =  "STOP")
   
   return (allRuleTermProbab)
   
}  # End of function : f.updateRuleTermProbabilities

#### 
# this function checks if the attributes values are identical
f_checkIdenticalValues <- function  (x) {
   if (length (unique(x)) == 1L) { 
      warning( "'density probabilities of current attribute values are identical")
      return ( TRUE)
      
   }else { return (FALSE)}
   
}



#  /*** the following fucntion: ***/
# - updating the classified instances data frame with a given new prediction /

f.updatePredictionsTable <- function ( exmp , predClass, dfClassified) {
   
   # add additional column to the data set which contains the predicted class voted by the Rules
   aPredictionToAdd <- cbind( exmp , 'Predicted Class'= predClass )
   
   cat ('\n predicted class = ', predClass, '\n')
   
   # create a data frame contains all classified instances
   if (is.na (dfClassified[1,1]) ) {       
      dfClassified  <- aPredictionToAdd 
   } else {
      dfClassified <- rbind(dfClassified, aPredictionToAdd) 
      
   }
   return (dfClassified)
   
}      # End of function:  f.updatePredictionsTable  



# /***** The following function is checking the accuracy of each prediction seperatly.
# - increment the corrected prediction counter and display the results
# - the results here does not effect the classifier accuracy   ******/

f.checkPredictions <- function (predClass, realClass ) {
   isCorrect <- FALSE
   if ( predClass == realClass ) {
      isCorrect <- TRUE
      
      cat ( '\n CORRECT Prediction ......  \n ')
      
   } else {
      cat (' \n WRONG Prediction !!! ... \n ')
      
   }
   return(isCorrect)
}    # End of function : f.checkPrediction


####  . . .  . . . . . . . . .  ..   End of all the Functions Section  . . . . . ..  .. . . ###

#browser()

#  measuring the execusion time for a comparison reason.
# start clock



# Loop through each class 

for (i in levels(as.factor(originalData$class))) {     # loop1   i is the class id
   
   keepGoing <- TRUE
   while (keepGoing)  {
      
      # Create D based on original dataset
      datasetD <- originalData
      
      # display which class now
      
      cat ('\n****************\n', 'Hello Class:  ', i, '*************** \n')
      
      #  create a vector to collect all the probabilities of the given class
      RuleTermProbabForClass <- c ()
      
      
      # create a new vector to collect the list of used attribute for the current class
      usedAttribute <- c()     
      
      # create a data frame which contains all the selected rule terms 
      listOfRuleTerms <- data.frame(matrix(nrow=1,ncol=6))
      names(listOfRuleTerms) <- c("Type" ,"Attribute", "Greater_Equal" , "Value", "Class","Probability")
      
      noMoreAtt <- FALSE
      
      while ( f.classesOtherThanGivenClass (datasetD, i) == TRUE ) {    
         
         # create a data frame to collect all the probabilities for the current class
         ruleTermProbabilities <- data.frame (matrix(nrow = 1, ncol = 7))
         names (ruleTermProbabilities) <- c ( "Type" , "Attribute", "Greater_Equal", "Value", "Class", "Frequency", "Probability")
         
         
         #  loop through all the predictive attributes, class attribute not counted,
         
          #browser( )
         for (attr in predictAttr:allPredictAttr) {      # loop2   
            
            
            
            # check whether this attribute is already used to induce a rule term for the current class
            if (colnames(datasetD[attr]) %in%  usedAttribute)   {      
               
               
               cat ('\n **** Checking used attribute list ..... \n')
               print (c( "current attribute is : ", colnames(datasetD[attr]))) 
               print (c( "used Attributes:", usedAttribute))
               cat ( '\n This attribute will be skipped ! \n ')
               
               
               # if there is no more attributes to spilt on and the training dataset still has 
               # classes other than the target class 
               
               if (length (usedAttribute) == allPredictAttr) {
                  noMoreAtt <- TRUE
                  break
                  
               } else (next)
            }
            
            
            # a vector to store each column in the data frame separately 
            currentAttr <- datasetD[,attr]
           
            
            # The following branch of if statment exectues when the type of an atrtribute is Numerical (continuos)
            if ( is.numeric(currentAttr)) {
               
               
               
               
               myFlag <- TRUE
               
               maxRuleTermProbability <- 0
               currentAttrValue <- 0
               
               # Sort the current attribute in an ascending order 
               currentAttr <- unique(currentAttr)
               sortedCurrentAttr <- sort(currentAttr, decreasing = FALSE)
               
               for ( attrVal in sortedCurrentAttr) {
                 # browser()
                  maxProbChange <- FALSE
                  signGreaterEqual <- "none"
                  
                  # set the operator to 1 which refers to the smaller side part of the rule term for the continous attributes
                  operator <- 1     
                  calculateRuleTermProbability <- f.calculateRuleTermProbability ( dataD = datasetD, 
                                                                                   thisClass = i, 
                                                                                   thisAttr = attr,
                                                                                   thisAttrVal = attrVal, 
                                                                                   thisOperator = operator) 
                  
                  
                  aRuleTermProbabilitySmallerSide <- calculateRuleTermProbability$outRuleTermProbability
                  
                  
                  if (aRuleTermProbabilitySmallerSide >= maxRuleTermProbability) {
                     
                     aRuleTermFrequency <- calculateRuleTermProbability$outRuleTermFrequency
                     
                     totalAttrFrequency <- calculateRuleTermProbability$outTotalAttrFrequency
                     
                     maxRuleTermProbability <- aRuleTermProbabilitySmallerSide
                     
                     refinedDataset <- calculateRuleTermProbability$outRefinedData
                     
                     maxProbChange <- TRUE
                     signGreaterEqual <- FALSE
                     
                  }
                  
                  # set the operator to 1 which refers to the Greater_equal side part of the rule term for the continous attributes
                  operator <- 2     
                  calculateRuleTermProbability <- f.calculateRuleTermProbability ( dataD = datasetD, 
                                                                                   thisClass = i, 
                                                                                   thisAttr = attr,
                                                                                   thisAttrVal = attrVal, 
                                                                                   thisOperator = operator) 
                  
                  
                  aRuleTermProbabilityGreaterSide <- calculateRuleTermProbability$outRuleTermProbability
                  
                  
                  # browser( text = "STOP")
                  
                  # Since that the attribute values are sorted descendingly, the RT probability is checked here 
                  # looping through the rest of the current attribute values will be stoped if the rule term probabilty decreased 
                  if (aRuleTermProbabilityGreaterSide >= maxRuleTermProbability) {
                     
                     aRuleTermFrequency <- calculateRuleTermProbability$outRuleTermFrequency
                     
                     totalAttrFrequency <- calculateRuleTermProbability$outTotalAttrFrequency
                     
                     maxRuleTermProbability <- aRuleTermProbabilityGreaterSide
                     
                     refinedDataset <- calculateRuleTermProbability$outRefinedData
                     
                     maxProbChange <- TRUE
                     signGreaterEqual <- TRUE
                     
                  }
                  
                  # the following if-statement is true when both sides of the numerical attributes are lower the maximum probability
                  if (!maxProbChange) {
                     cat ( 'The conditional probability for current attr-val is lower than the last maximum probability. Therefore, NO further values need to be tested on this attribute! ')
                     break ()
                     
                  }
                  
                  if (signGreaterEqual) {
                     print ( " ***** Greater_Equal side has the highest probability :")
                     aRuleTerm <- c ( colnames(datasetD[attr]) , ">=", attrVal)
                     
                  } else {
                     print (" ****** Smaller side has the highest probability :")
                     aRuleTerm <-  c ( colnames(datasetD[attr]), "<", attrVal)
                     
                  }
                  
                  cat ('\n maxRuleTermProbability = ', maxRuleTermProbability)
                  aRuleTermProbability <- maxRuleTermProbability
                  
                  if (aRuleTermProbability == 0 )  { 
                     print ( " This rule does not cover any instance; so it will be discarded !")
                     
                     next }
                  
                  
                  # update the ruleTermProbabilities array
                  aRuleTermProbToAdd <-  c("continous", colnames(datasetD[attr]), signGreaterEqual, attrVal,  
                                           i, aRuleTermFrequency, aRuleTermProbability)
                  
                  cat ('\n \n  New rule term is added to the ruleTermProbabilities list : \n', aRuleTermProbToAdd)
                  
                  
                  ##### HERE ADD THE STOPPING CRITIREA AND fILTERING CRITERIA ######
                  #################################################################
                  
                  
                  if ( aRuleTermProbability < minimumRuleAccuracy ) {
                     stoppingCriteriaUsed <- TRUE
                     #browser()
                     next()
                  }
                  
                  
                  ruleTermProbabilities <- f.updateRuleTermProbabilities ( aRuleTermProbToAdd, ruleTermProbabilities)
                  
                  
                  # collect all the resulted probabilities in a separate vector to find the rule term with highest probability in the next step 
                  RuleTermProbabForClass = c (RuleTermProbabForClass, aRuleTermProbability)
                  
                  #print (refinedDataset)
                  
                  
                  
               }
               
               # The following else statment exectues in case of Categorical attributes
            } else {
               
               operator <- 0
               signGreaterEqual <- "none"
               
               #levels(as.factor(currentAttr))
               
               for (attrVal in unique(currentAttr) ) {    # loop3
                  
                  
                  
                  calculateRuleTermProbability <- f.calculateRuleTermProbability ( dataD = datasetD, 
                                                                                   thisClass = i, 
                                                                                   thisAttr = attr,
                                                                                   thisAttrVal = attrVal, 
                                                                                   thisOperator = operator)
                  # assign the previous called function output to the global variables
                  
                  aRuleTerm <- c ( colnames(datasetD[attr]), "=", attrVal)
                  
                  aRuleTermFrequency <- calculateRuleTermProbability$outRuleTermFrequency 
                  
                  totalAttrFrequency <- calculateRuleTermProbability$outTotalAttrFrequency
                  
                  aRuleTermProbability <- calculateRuleTermProbability$outRuleTermProbability
                  if ( aRuleTermProbability <= 0)  { 
                     #browser(" value below zero : skipped ... ")
                     next()  }
                  
                  refinedDataset <- calculateRuleTermProbability$outRefinedData
                  
                  # update the ruleTermProbabilities array 
                  aRuleTermProbToAdd <- c("categorical", colnames(datasetD[attr]) , signGreaterEqual, attrVal, i, aRuleTermFrequency, aRuleTermProbability )
                  
                  ruleTermProbabilities <- f.updateRuleTermProbabilities ( aRuleTermProbToAdd, ruleTermProbabilities)
                  
                  ##   collect all the probablities in a vector in order to decide in the next step which is the maximum 
                  RuleTermProbabForClass = c (RuleTermProbabForClass, aRuleTermProbability)
                  
                 # browser()
               }   # end loop3
               
               
            }     # end of else statement part
            
            # result table moved here 
            
            # # @@@ ---- Display The final results  ---- @@@ \
            # resultTable <- data.frame(matrix(data =  NA, nrow=1,ncol= 5 ))
            # 
            # #resultTable <- matrix( data = NA, nrow = 1 , ncol = 7)
            # 
            # cat ('\n', 'current Attribute  = ', attr, '\n\n')
            # names(resultTable) <- c("Type", "Rule_Term", "RT_Frequency" ,"RT_Probabilty", "Attribute_Frequency")
            # resultTable$`Rule_Term`<- list(aRuleTerm )
            # 
            # resultTable$Type <- "continous"
            # 
            # resultTable$`RT_Frequency` <- aRuleTermFrequency
            # resultTable$`RT_Probabilty` <- aRuleTermProbability
            # resultTable$`Attribute_Frequency` <- totalAttrFrequency
            # 
            # print (resultTable)   
            
            # browser()
            
         }     # end loop2
         
         if (is.na( ruleTermProbabilities [1,1]))  {
            
            break
            
         }
         
         print ("<<<< ruleTermProbabilities table >>>> \n")
         print (ruleTermProbabilities)
         # browser( text = "STOP")
         
         # select the rule term that maximise the probability of the current class
         # highestProbability <- max(RuleTermProbabForClass)
         
         highestProbability <- max (as.numeric( ruleTermProbabilities[,7]))
         cat ('\n the highest Probability = ', highestProbability,'\n' )
         
         
         # select the rule term with highest probability 
         
         #browser()
         rowToAdd <- ruleTermProbabilities[which(ruleTermProbabilities[,7]  == max(ruleTermProbabilities[,7])),]
         
         
         # To be sure that the only one of the similar probabilities is chosen which means no duplication
         # Select the att-val pair which has the highest frequency in the set of instances being considered
         if (nrow(rowToAdd) > 1) {
            
            highestFrequency <- max(as.numeric (rowToAdd$Frequency))
            rowToAdd <- rowToAdd [which(as.numeric(rowToAdd [ , 6]) ==  highestFrequency) ,]
            #rowToAdd <- rowToAdd [which(rowToAdd[,5] == max(rowToAdd[,5])) ,]
            if (nrow (rowToAdd) > 1) {
               rowToAdd <- rowToAdd[!duplicated(rowToAdd$Frequency),]
            }
            
         } 
         
         # Checking the previous highest probability.If it has a higher frequency that the highest probability. then consider it.
         sortedByFrequency <- ruleTermProbabilities [order (as.numeric (ruleTermProbabilities$Frequency) , as.numeric( ruleTermProbabilities$Probability), decreasing = TRUE) ,]
         
         
         if ( as.numeric( sortedByFrequency[1,]$Probability) != highestProbability ) {
            
            diffCoverage <- as.numeric (rowToAdd[1,]$Probability) - as.numeric (sortedByFrequency[1,]$Probability)
            
            if (diffCoverage != 0 & diffCoverage <= 0.06) { 
               
               rowToAdd <- sortedByFrequency[1,]
               
            }
            
         } 
         
         # select the highest probability that has the highest frequency
         bestProbabElement <- row.names(rowToAdd)
         # ruleTermProbabilities [bestProbabElement,]
         bestAttribute <-  ruleTermProbabilities [bestProbabElement,2]
         bestAttrValue <- ruleTermProbabilities [bestProbabElement,4] 
         
         
         print (paste ("best attribute is" , bestAttribute) )
         
         print (paste ("best value for this attribute = ", bestAttrValue))
         
         #  remove column No 5, the frequency. After finding the best probability, it won't be needed any more
         rowToAdd <- rowToAdd [,-6]
         
         
         # rowToAdd <- rowToAdd[!duplicated(rowToAdd$Probability),]
         
         cat ('the row to be added to ruleTermsWithBestProbabilities array is: \n')
         print (rowToAdd)
         
         
         # collect all the rule terms with the best probability among the 
         if  (is.na(ruleTermsWithBestProbability[1,1])) {
            ruleTermsWithBestProbability[1,]<- rowToAdd
         } else {
            ruleTermsWithBestProbability <- rbind(ruleTermsWithBestProbability,rowToAdd)
         } 
         cat ('\n --- rule Terms With Best Probability : \n')
         # print (ruleTermsWithBestProbability)
         
         # add the attribute with best probability value to the usedAttribute list
         
         usedAttribute <- c (usedAttribute, rowToAdd$Attribute) 
         cat ('\n used attributes so far are : \n', usedAttribute)
         
         # add selected rule term to the current rule list
         if (is.na (listOfRuleTerms[1,1]) ) {
            listOfRuleTerms [1, ] <- rowToAdd
         } else {
            listOfRuleTerms <- rbind(listOfRuleTerms, rowToAdd)
         }
         
         #browser( text =  " check subset S creation ...")
         # create subset S containing all the instances covered by the best rule terms
         
         
         if ( rowToAdd$Type == "categorical" ) {
            dataCoveredByBestProbability <-  datasetD [which(datasetD [ bestAttribute] ==  bestAttrValue),]
         } else {
            if (rowToAdd$Type == "continous") {
               
               if ( rowToAdd$Greater_Equal ) {
                  dataCoveredByBestProbability <- 
                     datasetD [which(datasetD[ bestAttribute] >= as.numeric (bestAttrValue)) ,]
               } else {
                  dataCoveredByBestProbability <-
                     datasetD [which(datasetD[ bestAttribute] < as.numeric (bestAttrValue)) ,]
               }
               
            }
         }
         
         
         cat ('\n Instances covered by best selected rule Term ( dataset S ): ', 
              nrow(dataCoveredByBestProbability) ,' instances \n' )
         
         print (dataCoveredByBestProbability)
         
         
         # When dataset S does not cover any instances, means that the last Rule-term add nothing to the rule
         # Soultion:
         #     - Remove the last rule-term from the list. 
         #     - Check the majority class of the subset S.
         #         If it is similar to the target class, the rule will be considered to be completed.  
         #         Then, assign subset S to DatasetD
         #         If the majority class is different from the target class, discard the whole rule and remove subset S from the original data
         
         
         noInstCoveredRuleTerm <- FALSE
         
         if (nrow (dataCoveredByBestProbability) == 0 ) {
            cat ( 'Last rules-term will be removed from the list  ')
            #browser( text = "Deleting last rule-term")
            noInstCoveredRuleTerm <- TRUE
            aTermToBeRemoved <- nrow(listOfRuleTerms) 
            listOfRuleTerms <- listOfRuleTerms [ - aTermToBeRemoved, ]
            break()
            
            
         } 
         
         if (debugMe) browser( text = "stop before assigning S to D")
         
         # assign the subset S to the datasetD
         datasetD <- dataCoveredByBestProbability
         
         # find all instances that covered by rule R
         if (is.na (dataCoveredByR[1,1]) ) {
            dataCoveredByR <- dataCoveredByBestProbability
         } else {
            dataCoveredByR <- unique (rbind(dataCoveredByR, dataCoveredByBestProbability))
         }
         
         # To control the number of rule terms per Rule and it is decided by the user (be default the maximum number of rule-terms = 4 )
         reachMaxTermsNum <- FALSE
         if (nrow (listOfRuleTerms) > maxTerms & limitedTermsUsed)  {
            #browser()
            reachMaxTermsNum <- TRUE
            break
         }
         # browser( text = "STOP")  
      } # end of while Loop
      
      # ========================
      
      if ( nrow(listOfRuleTerms ) > 0 & !is.na(listOfRuleTerms[1,1])) {
         
         cat ('\n list of Rule Terms: \n')
         print (listOfRuleTerms)
         
         # to here, we got the complete induced rule R which is a conjunction of all rules in listOfRuleTerms data frame.
         cat ('\n *-*-*-*- Induced Rule : *-*-*-*\n \n')
         cat ('IF ')
         for (r in 1:nrow(listOfRuleTerms)){
            
            if (listOfRuleTerms[r,1] == "categorical") {
               cat ( listOfRuleTerms[r,2], '= ',listOfRuleTerms[r,4])
               
            } else { 
               if (listOfRuleTerms[r,3] == TRUE ) {
                  cat ( listOfRuleTerms[r,2], '>= ',listOfRuleTerms[r,4])
               }
               else {
                  cat ( listOfRuleTerms[r,2], '< ',listOfRuleTerms[r,4])
                  
               }
            }
            
            if (r != nrow (listOfRuleTerms) ){
               cat (' and ')
            } else {
               cat (' THEN class = ', listOfRuleTerms[r,5])
            }
            
            
         }
         cat ('\n *-*-*-*-*-*-*-*-*-*--* \n')
         
         #if (debugMe ) browser(text = "Stop, rule has been induced")
         # Add the current list of rules terms (the induced Rules) to the global Rules library
         
         if (nrow(listOfRuleTerms) > 0 )    {
            moreRules <- TRUE
         } else {
            moreRules <- FALSE
         }
         if  ( moreRules) {
            
            rulesLibrary.R [[N]] <- listOfRuleTerms
            N <- N + 1
            
         }
         
         #  browser( text = "New Rule is generated")
         
         # Probalem:  having no more attributes to use to genertae more rule-term in order to build a pure rule.
         # solution >> check the majority class in the current set of the data, 
         # If it is similar to the target class >> keep the rule
         # If it is different, delete the rule  
         
         # first: find the majority class for the target instances
         if (noMoreAtt | noInstCoveredRuleTerm | reachMaxTermsNum) { 
            
            cat (' \n WORNING: DatasetD contains classes other than the target class. As there are no more attributes to spit on!
              or the rule induction was terminated. Majority vote will be tested Now...\n')
            
            #browser( text =  "Checking the majority class")
            
           # findMajorityVoteClass <- c()
            
            
         #    for  (findMajClass in 1:nrow(datasetD)) {
               
          #     findMajorityVoteClass  <- c ( findMajorityVoteClass, datasetD$class[findMajClass] )
               
               
               
           # }
            
            majorityClass <-  names (which.max(table(datasetD$class))) 
            
            
            
            #majorityClass <- levels(as.factor(originalData$class))[as.numeric(majorityClass)]
            if (majorityClass  == i ) {
               
               cat ( 'Majority vote: ', majorityClass, ' | ', ' target class: ', i  )
               
               instancesMatchingTargetClass <- datasetD [datasetD$class == majorityClass, ]
               covPos <-  nrow (instancesMatchingTargetClass)
               
               cat ( '\n', covPos , ' instances out of ', nrow(datasetD), ' are positivly covered by the rule \n')
               
               # consider the vote and remove the current datasetD instances form the original data including the instances from other classes 
               # All the instances in the current datasetD will consider to be belonged to the target class including those from different classes 
               
               refinedOriginalData <- datasetD
               
               ######################################## <<<<<<<<<<<<<<<<<<<<<<<<<<<<
               
            }  else { cat ( '\n datasetD contains instances labled with classes different form the target one and there is no more attributes to spilt on. \n',
                            'Also, the majority vote did not match the target class. So, more action has to be taken to solve this problem' )
               cat ( ' currrent solution : discard the rule and the datasetD that has this clash')
               #browser( text =  "Last induced rule to be removed and the datasetD will be removed form the original data")
               
               aRuleToBeRemoved <- length(rulesLibrary.R)
               
               print ( c("the following rule will be removed from the rules Library: ", aRuleToBeRemoved))
               rulesLibrary.R <- rulesLibrary.R [ -aRuleToBeRemoved]
               N <- N-1
               
               refinedOriginalData <- datasetD [datasetD$class == i , ]  
            }
            
            
         }else {
            
            # (Removing Function ): find the subset of data that covered by the current rule R in the original dataset in order to remove it
            # make use of listOfRuleTerms data frame     # ====<<<<  DONE correctly >>>-====
            
            #browser( text =  "removing all instances that covered by the current rule")
            
            for (eachRuleTerm in 1:nrow(listOfRuleTerms)) {
               for (eachOriginalCol in predictAttr :allPredictAttr) {
                  if (listOfRuleTerms[eachRuleTerm,"Attribute"] == colnames(originalData[eachOriginalCol]) ) {
                     
                     if (eachRuleTerm == 1) {
                        refinedOriginalData <- originalData [ originalData$class == i , ]
                     } 
                    
                     
                     if (listOfRuleTerms$Type[ eachRuleTerm] == "categorical") {
                        refinedOriginalData <- 
                           refinedOriginalData [which(as.factor(refinedOriginalData[ , eachOriginalCol]) 
                                                      == listOfRuleTerms$Value [eachRuleTerm ]), ]
                        
                        
                     } else {
                        
                        if (listOfRuleTerms$Greater_Equal [eachRuleTerm ] == TRUE ) {
                           refinedOriginalData <- 
                              refinedOriginalData [which((refinedOriginalData[ , eachOriginalCol]) 
                                                         >= as.numeric(listOfRuleTerms$Value [eachRuleTerm ])), ] 
                           
                        } else {
                           refinedOriginalData <- 
                              refinedOriginalData [which((refinedOriginalData[ , eachOriginalCol]) 
                                                         < as.numeric(listOfRuleTerms$Value [eachRuleTerm])), ]
                        }
                        
                     }
                     
                     # }  
                     
                  }
                  
               }  
               
            }
         }
         cat ('\n data covered by the Rule: \n')
         print (refinedOriginalData)
         
         #print (c( "Data Not Covered by Rules",dataNotcoveredByRule))
         
         # remove the selected rows from the original dataset
         rowsToRemove <-  row.names (refinedOriginalData)
         cat ( ' rows numbers to be removed from the original data set = ', rowsToRemove, '\n')
         
         print (c("No of Col before removing the rows above = ", nrow (originalData)))
         
         # browser( text= "check before the removing process")
         
         dataNotCoveredByRule <- originalData[ !(row.names(originalData) %in% rowsToRemove),  ]
         
         
         cat ('\n the original data after removing instances that covered by the rule term R: \n')
         
         originalData <- dataNotCoveredByRule
         print (c("No of Col after the removing process = ", nrow (originalData)))
         
         #browser( text= "check after the removing process")
         # print (originalData)
         
         # check that all instances from current class have been removed from original dataset
         
      }else break()
      
      if (!(i %in% originalData$class))
         keepGoing <- FALSE
      
   }    # end of while loop 1 (keepGoing)
   
   
   # reset the original dataset
   cat ('\n dataset now is re-setted back to the completed data: \n')
   originalData <- unTouchedData
   
   
}   # end of for Loop1

# Display the final results ( the complete set of Rules)
cat ('\n RULES LIBRARY \n,', '\n =============== \n')
print (rulesLibrary.R)
rulesNumber <- length (rulesLibrary.R)


cat ('\n *-*-*-*- Induced Rules : *-*-*-*\n \n')

totalNumOfRuleTerms <- 0

for ( ruleIndx in 1: rulesNumber) {
   cat ('IF ')
   aRuleToPrint <- rulesLibrary.R[[ruleIndx]]
   
   totalNumOfRuleTerms <- totalNumOfRuleTerms + nrow(aRuleToPrint)
   
   for ( trm in 1:nrow(aRuleToPrint)) {
      if (aRuleToPrint$Type[trm] == "categorical") {
         cat ( aRuleToPrint[trm,2], '= ',aRuleToPrint[r,4])
         
      } else if (aRuleToPrint$Type[trm] == "continous" ) {
         if (aRuleToPrint[trm,3] == TRUE ) {
            cat ( aRuleToPrint[trm,2], '>= ',aRuleToPrint[trm,4])
         }
         else {
            cat ( aRuleToPrint[trm,2], '< ',aRuleToPrint[trm,4])
            
         }
         
      } 
      
      if (trm != nrow (aRuleToPrint) ){
         cat (' AND ')
      } else {
         cat (' THEN class = ', aRuleToPrint[trm,5], '\n')
         
      }
      
      
   }
   
   
}

#browser( text = "Prediction Part will be started soon ...")

# @@@@@@@  ------------------------------------------ @@@@@@@%

# @@@@@@@               Prediction FUNCTION           @@@@@@@ 

# @@@@@@@  ------------------------------------------ @@@@@@@%



#determine the majority class within the the training data to predict the unclassified instance

PredictedClassByMajority <-  names (which.max(table(trainData$class)))


# create a data frame which contains all the classified instances (Test Data) 
numTestExamples <- nrow(testData)

factorsLevels <- levels(as.factor(testData$class))

allClassifiedInstances <- data.frame(matrix(nrow=1,ncol= ncol(testData) +2))     
names(allClassifiedInstances) <- c( names(testData), "Predicted Class")   


classifiedInstancesByRules <-  data.frame ( matrix( nrow = 1, ncol =  ncol(testData) + 1))
names (classifiedInstancesByRules) <- c( names(testData), "Predicted Class")

# counters initialisation
totalExmpCorrectlyClassified <- 0
truePositiveExamples <- 0
falsePositiveExamples <- 0

NumOfExmpCoveredByRule <- 0
unclassifiedInstances <- 0

correctlyClassifiedByMajority <- 0
wronglyClassifiedByMajority <- 0

# create a list of rules that are used to classifiy the instances in order to calculate the accuracy of each one
usedRulesList <- data.frame((matrix(nrow=1,ncol= 4)))   # 4th method to create the used Rules list

# assign the names to the headers of the data frame
names(usedRulesList) <- c("Rules","Used Times", "Correctly used", "Rules Accuracy")    

# first column in the data frame is a list of rules
usedRulesList$Rules <- as.list (usedRulesList$Rules)    

# second column in the data frame is a numeric variable
usedRulesList$`Used Times` <-  as.numeric(usedRulesList$`Used Times`) 
usedRulesList$`Used Times` <- 0   

usedRulesList$`Correctly used` <- as.numeric(usedRulesList$`Correctly used`)
usedRulesList$`Correctly used` <- 0  

usedRulesList$`Rules Accuracy` <- as.numeric(usedRulesList$`Rules Accuracy`)
usedRulesList$`Rules Accuracy` <- 0

usedIndx <- 0


# loop throught each instance in the test data
for   ( anExample in 1:numTestExamples)  {     # for 1
   
   
   currentInstance <- testData [ anExample,]
   cat (' \n  Hi  ....loop No: ', anExample)
   cat ( ' \n instance ', anExample, ': \n')
   print ( currentInstance)
   
   rulescoveredInstance <- list()
   ruleCovIns <- 0
   
   
   # check if there are any Rules that cover the instance.
   for  (ruleNo in 1: rulesNumber) {     # for 2
      aRule <- rulesLibrary.R [[ruleNo]]
      aRuleNumTerms <- nrow(aRule)
      aRuleClass <- aRule$Class[1]
      noOfTimesRuleUsed <- 0
      matchedTerms <- 0
      
      cat ( '\n checking Rule : ', ruleNo, ' : \n')
     #print (aRule)
      
     # cat ('\n it has ', aRuleNumTerms , ' terms \n')
      
      #if (debugPredFun) browser( text= " ... debugging")
      
      # loop throught all the terms of the current Rule
      for ( aTerm in  1:aRuleNumTerms)  {    # for 3
         termAttributeName <- aRule$Attribute[aTerm]
         termAttributeValue <- aRule$Value[aTerm]
         # 
         # cat ('\n Term No: ', aTerm, '\n Attribute Name: ', termAttributeName, 
         #      '\n Attribute value: ', termAttributeValue, '\n')
         
         
         if (aRuleNumTerms >1) {
            cat ( '\n checking Rule term No : ', aTerm, ' : \n')
            #print (aRule[aTerm,])
            
            #if (debugPredFun) browser( text= " start debugging")
         }
         
         
         if ( is.numeric (currentInstance [[termAttributeName]] )) {
            numericExample <- TRUE
            
            
         } else {
            numericExample <- FALSE
         }  
         if  (numericExample ) {
            termAttributeValue <- as.numeric(termAttributeValue)
            # browser(text = "STOP")
            if (aRule$Greater_Equal[aTerm]) {
               if ( currentInstance [[termAttributeName]] >= termAttributeValue)  {
                  matchedTerms <- matchedTerms + 1
                  # cat ('\n Term No:  ', aTerm , '  is matching \n')
                  
               }
            } else if ( currentInstance [[termAttributeName]] < termAttributeValue)  {
               matchedTerms <- matchedTerms + 1
               
               #cat ('\n Term No:  ', aTerm , '  is matching \n')
            }
            
            # the following else statment will excuate when it is categorical attribute      
         } else if  ( currentInstance [[termAttributeName]] == termAttributeValue)  {
            matchedTerms <- matchedTerms + 1
            
            # cat ('\n Term No:  ', aTerm , '  is matching \n')
            
         }
         
         
      }   # for 3 
      
      # cat ( '\n ', matchedTerms, ' term(s) out of ', aRuleNumTerms, ' term(s) is/are matching \n')
      #  collect all the rules that have been found covered the current instance in a list
      
      if (matchedTerms == aRuleNumTerms) {
         
         rulescoveredInstance [[ruleCovIns + 1]] <- aRule
         
        # cat ( ' \n The instance : \n')
        # print ( currentInstance) 
         cat ('\n ***  Instance is Covered by the Rule *** \n')
         
         break ()
         
      } else {
         
         cat ('\n *** Instance is NOT covered by the Rule *** \n \n ')
         
      }
      
   }   #  for 2  
   
   cat ('\n Current Instance :')
   print (currentInstance)
   
   cat ('\n  List of  Rules covered this instance : \n ')
   print ( rulescoveredInstance)
   
   if (debugPredFun) browser( text= "  Debugging ...")
   
   #  according to the original PRISM algorithm, if there is more than one rules covered the current instance, choose the first matching one
   numOfRuleCoveredInstance <-  length(rulescoveredInstance)
   
   # the following condition will be TRUE if there is one or more rules covered the instances
   if ( numOfRuleCoveredInstance > 0 ) {    
      
      cat (' \n the first matching rule occured in the previous list will be chosen \n')
      firstMatchedRule <- rulescoveredInstance [1]
      predictedClass <- firstMatchedRule[[1]]$Class[1]
      NumOfExmpCoveredByRule <- NumOfExmpCoveredByRule + 1
      
      # create usedRuleList to update the number of the times that the Rule used  
      
      if (is.na (usedRulesList$Rules[1]))  {
         
         usedRulesList$Rules[1] <- firstMatchedRule
         usedRulesList$`Used Times`[1] <- 1
         usedIndx <- usedIndx + 1
         foundIt <- FALSE
         
      } else {
        
         # looping through the used Rules list to find if a given rule is already added to the rules list or not?
         sizeUsedRulesList <- nrow(usedRulesList)
         
         foundIt <- FALSE
         for (ruleExistIndx in 1:sizeUsedRulesList ){
            aRuleUsed <- usedRulesList$Rules[[ruleExistIndx]]
            
            
            #check if both rules have same rule terms number; if not, no need to enter the following loop
            if (nrow(aRuleUsed) == nrow(as.data.frame(firstMatchedRule))) {
               sameRule <- TRUE
               for ( ii in 1:dim (aRuleUsed)[1]) {
                  for (jj in 1:dim(aRuleUsed)[2]) {
                     if ( aRuleUsed [ii,jj] != firstMatchedRule[[1]][ii,jj] ) {
                        sameRule <- FALSE
                        break()
                     }
                     
                  }
                  if ( ! sameRule  ) {
                     break()
                  }
               } 
            } else {
               sameRule <- FALSE
            }
            
            
            if ( sameRule ) {
               foundIt <- TRUE
               break
               
            }
            
            
         }
         
         if (foundIt) {
          
            cat ( '\n The matching rule is already used ', 
                  usedRulesList$`Used Times`[ruleExistIndx], ' times so far \n' )
            
            usedRulesList$`Used Times`[ruleExistIndx] <- usedRulesList$`Used Times`[ruleExistIndx] + 1
            
            
         } else {
          
            
            cat ( '\n The matching rule is used for the first time \n')
            usedRuleToAdd <- as.data.frame( cbind('Rules' = firstMatchedRule, 'Used Times'= 1,
                                                  "Correctly used" = 0, "Rules Accuracy" = 0))
            usedRuleToAdd$`Used Times` <- as.numeric(usedRuleToAdd$`Used Times`)
            usedRuleToAdd$`Correctly used` <- as.numeric(usedRuleToAdd$`Correctly used`)
            usedRuleToAdd$`Rules Accuracy` <- as.numeric( usedRuleToAdd$`Rules Accuracy`)
            usedRulesList <- rbind(usedRulesList,usedRuleToAdd)  
            usedIndx <- usedIndx + 1      
            
         }
         
      }
      
      
      
      # add current prediction to the general classified instances table 
      # (classified by rules +  classifed usong majority class strategy )  
      allClassifiedInstances <-  f.updatePredictionsTable (exmp =  testData[anExample,] , 
                                                     predClass = predictedClass , 
                                                     dfClassified =  allClassifiedInstances )
      
      allClassifiedInstances$class <- factor(allClassifiedInstances$class, 
                                             levels = factorsLevels)
      
      allClassifiedInstances$`Predicted Class` <- factor(allClassifiedInstances$`Predicted Class`, 
                                                         levels = factorsLevels)
      
      
      
      # add the same current prediction to the table of classified instances by rules
      # (exluding the examples that classiffed using majority class strategy )  
      classifiedInstancesByRules <- f.updatePredictionsTable (exmp =  testData[anExample,] , 
                                                              predClass = predictedClass , 
                                                              dfClassified =  classifiedInstancesByRules )
      
      classifiedInstancesByRules$class <- factor(classifiedInstancesByRules$class, 
                                                 levels = factorsLevels )
      
      
      classifiedInstancesByRules$`Predicted Class` <- factor(classifiedInstancesByRules$`Predicted Class`, 
                                                             levels =factorsLevels)
      
      
      if  (foundIt ) {
         thisRuleIndx <-  ruleExistIndx
      } else {
         thisRuleIndx <- usedIndx
         
      }
      
      
      
      # / **** The following function:
      # / - updating the UsedRulesList attributes and calculate the Rules Accuracy 
      # / ****
      # f.updateUsedRulesList <- function (usedRules, i, numCorrectIns ) {
      #    numCorrectIns <- numCorrectIns + 1
      #    usedRules["Correctly used"][i] <- usedRules["Correctly used"][i] + 1
      #    usedRules["Rules Accuracy"][i] <- 
      #       ( usedRules["Correctly used"][i] / usedRules["Used Times"] [i] ) * 100
      #    
      #    return( list ( usedRules, numCorrectIns))
      # }
      
      
      # call 'f.checkPredictions' function to check whether the predicted class is correct or not
      validatePrediction  <-  f.checkPredictions ( predClass =  predictedClass, 
                                                   realClass = currentInstance$class )
      
      
      if  (validatePrediction == TRUE )  {
         
         # browser( text = "Debug: Correct prediction, update usedRulesList attributes  ...")
         
         totalExmpCorrectlyClassified <- totalExmpCorrectlyClassified + 1
         truePositiveExamples <- truePositiveExamples + 1
         usedRulesList$`Correctly used`[thisRuleIndx] <- usedRulesList$`Correctly used`[thisRuleIndx] + 1
         usedRulesList$`Rules Accuracy`[thisRuleIndx] <-
            (usedRulesList$`Correctly used`[thisRuleIndx] / usedRulesList$`Used Times`[thisRuleIndx])* 100
         
         cat ( '\n the rule is used correctly ',usedRulesList$`Correctly used`[thisRuleIndx] , ' time(s) \n')
         
         # usedRulesFuncOutput <- f.updateUsedRulesList ( usedRules = usedRulesList,
         #                                                i = indx ,
         #                                                numCorrectIns = truePositiveExamples)
         # usedRulesList <- usedRulesFuncOutput[[1]]
         # truePositiveExamples <- usedRulesFuncOutput [[2]]
         
      } else {
         
         # browser( text = "Debug: Wrong Prediction:  
         #          only Accuracy attribute need to be updated in the UsedRulesList ...")
         
         
         falsePositiveExamples  <- falsePositiveExamples + 1
         usedRulesList$`Rules Accuracy`[thisRuleIndx] <- 
            (usedRulesList$`Correctly used`[thisRuleIndx] / usedRulesList$`Used Times`[thisRuleIndx])* 100 
      } 
      
      
      #  else part is excuting  when there is no Rule covered the current instance (unclassified Examples)
   } else {     
      
      if (debugPredFun) {
         browser( text = "There is no rule covered the current instance  ...")
      } 
      
      unclassifiedInstances <- unclassifiedInstances + 1 
      
      # # The majority class among the predicted instances 
      # majorityClass <- names((which.max(table(allClassifiedInstances$`Predicted Class`))))   
      
      
      # Calling function to update the classified instances DF with a new given prediction for the current instance 
      # -  using a majority class prediction technique 
      allClassifiedInstances <-  f.updatePredictionsTable (exmp =  testData[anExample,] , 
                                                     predClass = PredictedClassByMajority , 
                                                     dfClassified = allClassifiedInstances )
      
      allClassifiedInstances$class <- factor(allClassifiedInstances$class, 
                                             levels = factorsLevels)
      
      allClassifiedInstances$`Predicted Class` <- factor(allClassifiedInstances$`Predicted Class`, 
                                                         levels = factorsLevels)
      
      validatePrediction <- f.checkPredictions ( predClass =  PredictedClassByMajority, 
                                                 realClass = currentInstance$class ) 
      
      if  (validatePrediction == TRUE )  {
         correctlyClassifiedByMajority <- correctlyClassifiedByMajority + 1
         totalExmpCorrectlyClassified <- totalExmpCorrectlyClassified + 1
      }   else {
         wronglyClassifiedByMajority <- wronglyClassifiedByMajority + 1
         # browser( text =  " prediction using majority class is wrong !!")
         
      }
   }
   
   # browser( text= " Debug: current instance analysis is finished ...   ")
   
}    # for 1


# Calculate the total Accuracy for the classifier
totalClassifiedInstances <- nrow(allClassifiedInstances)
wrongPredictions <-0
correctPredictions <- 0

for (inst in 1:totalClassifiedInstances) {
   if ( allClassifiedInstances[inst,]$class != allClassifiedInstances[inst,]$`Predicted Class`){
      wrongPredictions <- wrongPredictions + 1
      
   } else {
      correctPredictions <- correctPredictions + 1
      
   }
   
}


# Precision, recall and F1 score are useful when the class labels are not uniformly distributed (e.g. most instances belong to one class).
# In such cases, accuracy could be misleading as one could predict the dominant class most of the time and still achieve a relatively high 
# overall accuracy but very low precision or recall for other classes.


allConfusionMat <- confusionMatrix( allClassifiedInstances$`Predicted Class`,
                                    allClassifiedInstances$class )


byRulesconfusionMat <- confusionMatrix ( classifiedInstancesByRules$`Predicted Class`,
                                         classifiedInstancesByRules$class)

if ( numOfClasses > 2 ) {
   
   # Calculate Recall value for all the classes (sensitivity value in the confusion matrix) 
   
   byRules_Recall <- mean (byRulesconfusionMat$byClass[,1], na.rm = TRUE)
   
   # calculate precision value for all the classes ( pos pred value in the confusion matrix)
   byRules_Precision  <- mean (byRulesconfusionMat$byClass[,3], na.rm = TRUE)
   
}  else {
   
   
   byRules_Recall <- byRulesconfusionMat$byClass[[1]]
   byRules_Precision  <- byRulesconfusionMat$byClass[[3]]
   
}

# calculate F1 Score which is defined as the harmonic mean (or a weighted average) of precision and recall.
byRules_F1.score <- 2 *(( byRules_Recall * byRules_Precision) / (byRules_Recall + byRules_Precision))


globalAccuracy  <- (correctPredictions / totalClassifiedInstances) 

# stop clock
print (  "execution time :" )
print (proc.time() - start.time) 


cat ('\n ############# PRISM Final Results ############# \n \n ', '  Dataset: ', fileName, ' (', seedNo , ') \n'   )


cat ( '\n Train Data: ', nrow(trainData)  )
cat (' \n Test Data: ', totalClassifiedInstances)

cat (' \n Stopping Critiera is used?', stoppingCriteriaUsed , ' (Minimum Rule Accuracy = ', minimumRuleAccuracy *100, '%) \n' )
cat ( '\n Limited Terms/Rule are used?', limitedTermsUsed , ' ', maxTerms + 1, ' Terms/Rule')
cat ( ' \n *****************************************************************')

cat ( '\n Number of Rules: ', length(rulesLibrary.R))
cat ( '\n average of terms/rule: ', totalNumOfRuleTerms / rulesNumber)

cat (' \n Number of all examples covered by the Rules = ', NumOfExmpCoveredByRule )
cat (' \n Number of positive examples covered by the rules (Completence) = ', truePositiveExamples)
cat (' \n Number of negative examples covered by the rules = ', falsePositiveExamples, '\n')

cat ( '\n Number of unclassifeied examples (Majority Class strategy) = ', unclassifiedInstances)
cat (' \n Correctly classifeied by majority labeling = ', correctlyClassifiedByMajority)
cat (' \n Wrongly classifeied by majority labeling = ', wronglyClassifiedByMajority)


cat ('\n \n ', (unclassifiedInstances / totalClassifiedInstances ) , 
     ' of the examples are not covered by the rules (classified using Majority Class)')

cat ('\n ', (NumOfExmpCoveredByRule / totalClassifiedInstances )  , 
     ' of the examples are covered by the Rules')

cat('\n \n Correct Predictions =  ', correctPredictions )
cat('\n Wrong Predictions =  ', wrongPredictions , '\n' )


cat ('\n Examples covered by the rules (coverage)= ', 
     ((truePositiveExamples + falsePositiveExamples) / totalClassifiedInstances) *100 , '%')

cat (' \n False positive examples (Consistency ) = ' , 
     (falsePositiveExamples / NumOfExmpCoveredByRule) *100, '%' )

cat ( '\n Covered positive examples among all positive examples (Recall)= ', 
      (truePositiveExamples / correctPredictions)* 100 , '%' )

cat ('\n Covered positive examples among all the examples (Support) =', 
     (truePositiveExamples/ totalClassifiedInstances) *100, '%')


cat ( ' \n \n  ********** Results computed from the confusion matrix  ************ '  )

cat ( '\n Recall  = ', byRules_Recall )

cat ( ' \n Precision  = ' , byRules_Precision )

cat ( ' \n F1 Score value  = ', byRules_F1.score )

cat ( ' \n *****************************************************************')

cat('\n General Accuracy =  ', globalAccuracy )

cat ('\n \n Tentative Accuracy = ', (truePositiveExamples / NumOfExmpCoveredByRule)  )

 

cat ( ' \n ---------------------------------------------------------------------------------------')


#  End of Coding

# 
# class(temp$Rules$Rule2)   # list
# temp$Rules$Rule2[[1]]$Class == temp$Rules$Rule1[[1]]$Class

