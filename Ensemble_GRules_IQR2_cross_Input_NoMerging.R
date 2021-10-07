# ==========  This code is exactly as same as Ensemble_GRules_IQR2_cross.R  =============-
# ========             ONLY differ in taking input parameters from user   ==========-
#
#                           Manal Khalaf ALmutairi
#       
# =====================================================================================-


# Set the working directory
# setwd("/Users/manal/Desktop/R projects/PhD_Project_Codes")


getwd()
######## no normalisation needed in this code ********

# install necessary packegs

# install.packages(c ("RWeka"), dependencies = TRUE )
# install.packages( "xlsx")

#install.packages("keys")
#install.packages("hash")
#install.packages("hashmap")

# install.packages("stringr")
4
# loading libraries
# library(RWeka)
#library (hash)
#library (hashmap)

# install.packages("MLmetrics")
rm (list=ls())
print ( "Working Environment is cleared .. ")

options (show.error.locations = TRUE)

options ( digits = 4)

# install.packages("dplyr")
# library(data.table)
# library (caTools)
# # library (plyr)
# library (xlsx)
# library (ggplot2)
# library (stats)
# library (MASS)
# library( caret)
# library (ROCR)
# library(discretization)
# library(rgl)
# 
# #library(dprep)
# library(dplyr)
# 
# library(h2o)
# 
# library(tibble)
# 
# library(tidyselect)
# 
# library(RSQLite)
# 
# library(nortest)
# 
# library(fBasics)
# 
# library(graphics)
# 
# library(vcd)
# 
# library(moments)
# 
# library( e1071)
# 
# library(FSelector)
# 
# library(ggcorrplot)


Sys.setenv( JAVA_HOME = "")
library(rJava)
library(stringr)

library(dplyr)
library(h2o)
library(devtools)
library(caret)
library(tibble)
library(moments)
library(DBI)
library(RSQLite)
library(tidyselect)
library(rlist)
library (xlsx)



library(caTools)

library(XLConnect)

library(DescTools)

library(rlang)

library(stats)

library(shiny)

#library(MLmetrics)


######## Take Input from user ########

#  display datasets list to the user to select one
DatasetsList <- read.xlsx( "Datasets_List.xlsx", 1 ,  as.data.frame=TRUE, header=TRUE, stringsAsFactors = FALSE)
cat(  c( paste('\n       Datasets List      ', ' ========================= \n', sep = '\n') ))
print(DatasetsList)
fileNameInput <- readline( prompt = "Select Dataset: ")

# validate user input 
correctFileInput <- 1:24

while( as.integer (fileNameInput) %in% correctFileInput == FALSE )  {
      fileNameInput <- readline( prompt = "Please select correct dataset number: ")
      
}

cat(  c( paste('\n       Algorithms List ', ' ==========================\n', sep = '\n') ))
cat ( ' 1.  G-Rules-IQR   (single)\n\n 2.  ReG-Rules  (Ensemble)\n\n 3.  CRC (Consolidated Ensemble)  \n\n ')

modelAlgorithmInput <- readline( prompt =  "Select the algorithm : ")

# validate user input 
correctAlgoInput <- 1:3

while( as.integer (modelAlgorithmInput) %in% correctAlgoInput == FALSE )  {
   modelAlgorithmInput <- readline( prompt = "Please select correct algorithm number: ")
   
}

if ( modelAlgorithmInput == "2" | modelAlgorithmInput == "3") {
   
   # User input of the number of base classifiers that will be used to build the ensemble and validate the input
   repeat {
      numBaseClassifiersInput <- as.numeric (readline ( prompt =  
                                             "Enter the number of base classifiers for the ensemble model: "))
      if ( !is.na (numBaseClassifiersInput)) break
      
   }
   
}
cat(  c( paste('\n        Evaluation Method', ' ============================ \n', sep = '\n') ))
cat ( ' 1.  Separate Training and Testing sets \n\n 2.  Cross Validation Method \n\n ')

EvalMethodInput <- readline( prompt =  "Enter your choice number : ")

correctEvalInput <- 1:2
while( as.integer (EvalMethodInput) %in% correctEvalInput == FALSE )  {
   EvalMethodInput <- readline( prompt = "Please select correct number of the evaluation method: ")
   
}

######### End of taking input from user

##########        Parameters and Pre-processing step  #########

if (modelAlgorithmInput == "1") {       # G-Rules-IQR
   buildEnsembleModel <- FALSE
   finalEnsembleResults <- FALSE    # This flag should be always False and it will be change within the code (at the base classifiers combining stage)
   
} else {
   buildEnsembleModel <- TRUE
   finalEnsembleResults <- FALSE    # This flag should be always False and it will be change within the code (at the base classifiers combining stage)
   
   numBaseClassifiers <- numBaseClassifiersInput
   
   rankingModels <- TRUE  # This flag variable has to be true to apply the ranking method
   
   if  ( modelAlgorithmInput == "2") {       # ReG-Rules
      applyConsolidation <- FALSE # This flag variable has to be true to apply the consolidation method
      
   } else if (modelAlgorithmInput == "3" ) {        # Consolidated ReG-Rules
      applyConsolidation <- TRUE   # This flag variable has to be true to apply the consolidation method
      
   }
} 

if ( EvalMethodInput == "1" ) {      # Train & Test sets
   usingCrossValidation <- FALSE
   
} else {              # cross validation
   usingCrossValidation <- TRUE
   foldsNum <- 5
   
}

TransformToNormality <- TRUE        # [[ permanent values  ]]
# to reduce the overfitting that may occured in the rules   
limitedTermsUsed <- FALSE      # [[ Default value ]]
maxTerms <- 4                  # [[ Default value ]]
stoppingCriteriaUsed <- FALSE   # always False, No Change , it depends on the rule-term pobability     [[ permanent value  ]]
minimumRuleAccuracy <- 0.00     #[[ permanent values  ]]
checkNextHigherProbability <- TRUE    # always TRUE     [[ permanent values  ]]

toKeepTheRule <- 0.00
rulesDeletion <- FALSE # This for large datasets only to remove overfitted rules that cover less than 10 inctances 


start.time <- Sys.time()

print ( " Run time started at : ")
print (start.time)

################  Loading Data files to R    ################ 

print ( "Preprocessing Dataset in progress ...")

print ( "Loading data file....")

if (fileNameInput == "1") {
   
   fileName <- "data/iris.data"        # " " #
   
} else if (fileNameInput == "2" ) {
   
   fileName <- "data/seeds.data"      # " " #
   
} else if (fileNameInput == "3" ) {
   fileName <- "data/wine.data"        # , #
   
} else if (fileNameInput == "4" ) {
   fileName <- "data/transfusion.data"     # , #
   
   
} else if (fileNameInput == "5" ) {
   fileName <- "data/banknote.data"     # , #
   
   
} else if (fileNameInput == "6" ) {
   fileName <- "data/ecoli.data"     # " " #
   
} else if (fileNameInput == "7" ) {
   fileName <- "data/yeast.data"      # " " #
   
} else if (fileNameInput == "8" ) {
   limitedTermsUsed <- TRUE
   maxTerms <- 4
   fileName <- "data/page-blocks.data"      # " " #
   
} else if (fileNameInput == "9" ) {
   fileName <- "data/Data_User_Modeling.xls"
   
} else if (fileNameInput == "10" ) {
   fileName <- "data/BreastTissue.xlsx"
   
} else if (fileNameInput == "11" ) {
   fileName <- "data/glass.data"       # , #
   
} else if (fileNameInput == "12" ) {
   fileName <- "data/HTRU_2.data"
   
} else if (fileNameInput == "13" ) {
   limitedTermsUsed <- TRUE
   maxTerms <- 4
   fileName <- "data/magic04.data"
   
} else if (fileNameInput == "14" ) {
   limitedTermsUsed <- TRUE
   maxTerms <- 4
   fileName <- "data/winequality-white.data"
   
} else if (fileNameInput == "15" ) {
   fileName <- "data/breast-cancer-wisconsin.data"
   
} else if (fileNameInput == "16" ) {
   fileName <- "data/post-operative.data"    #   this dataset is mixed have missing values ( not clean data)
   
} else if (fileNameInput == "17" ) {
   fileName <- "data/wifi_localization.data"    # how wifi signal strengths can be used to determine one of the indoor locations.
   
   
} else if (fileNameInput == "18" ) {
   limitedTermsUsed <- TRUE
   maxTerms <- 4
   fileName <- "data/Indian_Liver_Patient.data"
   
} else if (fileNameInput == "19" ) {
   fileName <- "data/sonar.data"
   
} else if (fileNameInput == "20" ) {
   limitedTermsUsed <- TRUE
   maxTerms <- 4
   fileName <- "data/leaf.txt"
   
} else if (fileNameInput == "21" ) {
   limitedTermsUsed <- TRUE
   maxTerms <- 3
   fileName <- "data/log2.csv"
   
} else if (fileNameInput == "22" ) {
   limitedTermsUsed <- TRUE
   maxTerms <- 3
   fileName <- "data/bank_full.csv"
   
} else if (fileNameInput == "23" ) {
   limitedTermsUsed <- TRUE
   maxTerms <- 5    
   fileName <- "data/avila.data"
   
} else if (fileNameInput == "24" ) {
   fileName <- "data/shuttle.data"
   
} 
   
print (fileName)

# fileName <- "data/letter_recognition.data"

# fileName <- "data/winequality-red.data"

# fileName <- "data/EEG_Eye_State.data"   #   create extremely overfitted rules

# fileName <- "data/covtype.data"

# fileName <- "data/Chess.data"        # This data set might be considered a (categorical dataset) despite containig numeric attribute values

# fileName <- "data/breast-cancer-wisconsin.data"

# fileName <- "data/skin_segmentation.data"   # this is a large data!! R can not read it ; SparkR should be used

# fileName <- "data/parkinsons.data"

# fileName <- "data/personActivity.txt"

# fileName <- "data/adult.data"

#fileName <- "data/lenses.data"

#fileName <- "data/tae.data"                 # Mixed attributes   not working 

#fileName <- "data/diagnosis.data"             # Mixed attributes     not working 

# fileName <-  "data/abalone.data"     # 29 classes   <<< changed to be 3 categories

#fileName <- "data/cmc.data"

#fileName <- "data/fertility_Diagnosis.data"      #  this dataset is categorical despite having a numeric attributes' values

#fileName <- "data/haberman.data"

# myData<- read.csv(fileName, header = FALSE, sep = "," )

#myData <- read.csv ("data/winequality-red.csv", header = TRUE, sep = ";")

if ( fileName == "data/BreastTissue.xlsx") {
   sheetNo <- 2
   
   
} else if (fileName == "data/Data_User_Modeling.xls" ) {
   
   trainSheetNo <- 2
   testSheetNo <- 3
}


#  seed is an integer vector, containing the random number generator (RNG) state for random number generation in R

if (fileName  == "data/seeds.data")    {
   seedNo <- 90
} else if ( fileName == "data/yeast.data")  {
   seedNo <- 700
} else if ( fileName == "data/transfusion.data" ) {
   seedNo <- 400
} else if ( fileName == "data/post-operative.data") {
   seedNo <- 20
} else if (fileName == "data/parkinsons.data") {
   seedNo <- 90
} else if (fileName == "data/leaf.txt") {
   seedNo <- 10
} else {
   seedNo <- 10
   
}

#boundLimit <- 3

set.seed(seedNo)



if ( fileName == "data/iris.data") {
   myDataColNames <- c ("sepal_length", "sepal_width", "petal_length", "petal_width", "class")
   delimitCol <- ""
   headerRow <- FALSE
   
} else if ( fileName == "data/seeds.data")  {    # space 
   myDataColNames <- c ("Area_A", "perimeter_P", "Compactness_C", "length_of_kernel", "width_of_kernel",
                        "asymmetry_coefficient", "length_of_kernel_groove", "class")
   delimitCol <- ""
   headerRow <- FALSE
   
   
} else if (fileName == "data/wine.data" ) {
   myDataColNames <- c  ("class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium",
                         "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins",
                         "Color_intensity", "Hue", "OD280/OD315_of_diluted wines", "Proline")
   delimitCol <- ","
   headerRow <- FALSE
   
   
   
} else if (fileName == "data/transfusion.data") {
   myDataColNames <- c ("Recency", "Frequency", "Monetary", "Time", "class")
   delimitCol <- ","
   headerRow <- TRUE
   
   
   
}  else if (fileName == "data/banknote.data") {
   myDataColNames <- c ( "variance_of_Wavelet", "skewness", "curtosis", "entropy", "class" )
   delimitCol <- ","
   headerRow <- FALSE
   
   
}   else if ( fileName == "data/ecoli.data") {
   myDataColNames <- c (  "Sequence_Name", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2", "class")
   delimitCol <- ""
   headerRow <- FALSE
   
   
} else if (fileName == "data/yeast.data") { 
   myDataColNames <- c ( "Sequence_Name", "mcg", "gvh", "alm", "mit" , "erl" , "pox", "vac", "nuc", "class")
   delimitCol <- ""
   headerRow <- FALSE
   
   
} else if (fileName == "data/page-blocks.data") {
   myDataColNames <- c ( "height", "lenght", "area", "eccen", "p_black", "p_and", "mean_tr", 
                         "blackpix", "blackand", "wb_trans" , "class"  )
   delimitCol <- ""    # This file is downloded in a compressed version ! 
   headerRow <- FALSE
   
   
} else if (fileName ==  "data/Data_User_Modeling.xls") {
   
   myDataColNames <- c ("STG" , "SCG",  "STR",   "LPR", "PEG", "class")
   
   headerRow <- TRUE
   
} else if ( fileName == "data/BreastTissue.xlsx") {
   myDataColNames <- c ( "Class", "I0", "PA500", "HFS", "DA" , "AREA", "A_DA", "MAX_IP", "DR" ,"P"  )
   
   
} else if (fileName == "data/glass.data" ) { 
   myDataColNames <-  c ( "Id", "RI", "Na", "Mg", "Al","Si", "K", "Ca", "Ba", "Fe", "class")
   delimitCol <- ","
   headerRow <- FALSE
   
   
} else if  (fileName == "data/HTRU_2.data") {
   myDataColNames <- c("IP_mean", "IP_sd", "IP_kurtosis", "IP_Skewness", "DM-SNR_mean", "DM-SNR_sd", "DM-SNR_kurtosis", "DM-SNR_Skewness", "class")
   delimitCol <- ","
   headerRow <- FALSE
   
   
} else if  ( fileName == "data/magic04.data" ) {
   myDataColNames <- c ( "fLength" ,"fWidth", "fSize", "fConc", "fConc1", "fAsym", "fM3Long", 
                         "fM3Trans", "fAlpha", "fDist", "class" )
   delimitCol <- ","
   headerRow <- FALSE
   
   
} else if (fileName ==  "data/winequality-white.data" ) {
   myDataColNames <- c( "fixed_acidity","volatile_acidity","citric_acid" ,"residual_sugar",
                        "chlorides","free_sulfur_dioxide","total_sulfur_dioxide","density","pH",
                        "sulphates","alcohol" ,"class")
   delimitCol <- ";"
   headerRow <- TRUE
   
   
} else if (fileName == "data/letter_recognition.data") {
   myDataColNames <- c ( "class", "x-box", "y-box", "width", "high ", "onpix", "x-bar", "y-bar", "x2bar", 
                         "y2bar", "xybar", "x2ybr", "xy2br", "x-ege", "xegvy", "y-ege", "yegvx"  )
   delimitCol <- ","
   headerRow <- FALSE
   
   
} else if ( fileName == "data/breast-cancer-wisconsin.data") {     ###  >>> fix the way how to add the headers to the dataset
   myDataColNames <- c ("code_number", "Clump_Thickness", "Uniformity_of_Cell_Size", "Uniformity_of-Cell_Shape", 
                        "Marginal_Adhesion", "Single_Epithelial_Cell_Size", "Bare_Nuclei", "Bland_Chromatin", "Normal_Nucleoli", "Mitoses", "class")
   
   delimitCol <- ","
   headerRow <- FALSE
   
}  else if (fileName == "data/post-operative.data" ) {     ###  >>> fix the way how to add the headers to the dataset
   myDataColNames <- c ("L-CORE", "L-SURF", "L-O2", "L-BP", "SURF-STBL", "CORE-STBL", "BP-STBL",
                        "COMFORT", "class")
   delimitCol <- ","
   headerRow <- FALSE
   
   
} else if (fileName == "data/EEG_Eye_State.data" ) {
   myDataColNames <- c ( "AF3", "F7", "F3", "FC5", "T7", "P7", "O1", "O2", "P8", "T8","FC6",
                         "F4","F8", "AF4", "class" )
   delimitCol <- ","
   headerRow <- FALSE
   
   
} else if (fileName == "data/covtype.data") {
   myDataColNames <- c ("Elevation", "Aspect", "Slope" , "Horizontal_Distance_To_Hydrology",
                        "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways", "Hillshade_9am",
                        "Hillshade_Noon", "Hillshade_3pm", "Horizontal_Distance_To_Fire_Points", 
                        paste("Wilderness_Area_",1:4, sep = "" ) , paste("Soil_Type_", 1:40, sep = "")
                        , "class")
   delimitCol <- ","
   headerRow <- FALSE
   
   # The rest not used yet !   
} else if (fileName ==  "data/skin_segmentation.data") {
   myDataColNames <- c ( "B" , "G", "R", "class" )
   delimitCol <- ""
   headerRow <- FALSE
   
   
} else if  ( fileName == "data/wifi_localization.data" ) {
   myDataColNames <- c ( "WS1" , "WS2", "WS3" , "WS4" , "WS5" , "WS6" , "WS7", "class")
   delimitCol <- ""
   headerRow <- FALSE
   
   
} else if (fileName == "data/Indian_Liver_Patient.data")  {
   
   myDataColNames <- c ( "Age", "Gender", "TB", "DB", "Alkphos", "Sgpt", "Sgot", "TP", "ALB", "A.G_Ratio", "class")
   delimitCol <- ","
   headerRow <- FALSE
   
   
} else if (fileName == "data/tae.data") {
   
   myDataColNames <- c ( "TA_speaker", "Course_instructor", "Course","semester", "Class_size", "Class_attribute")
   
}else if (fileName == "data/diagnosis.data")  {
   
   myDataColNames <- c ( "Temperature", "nausea", "Lumbar_pain", "Lumbar_pain", 
                         "Micturition_pains", "Burning_of_urethra", "bladder_inflammation","Nephritis_of_renal_pelvis_origin" )
   
} else if ( fileName == "data/abalone.data")  {
   myDataColNames  <- c ( "Sex", "Length", "Diameter", "Height", "Whole", "Shucked", "Viscera", "Shell", "class")   
   
   
}else if ( fileName == "data/cmc.data") {
   
   myDataColNames <- c ( "Wife's_age", "Wife's_education", "Husband's_education", "No_of_children", "Wife's_religion", "Wife's_now_working?" , "Husband's_occupation",
                         "Living_index", "Media_exposure", "Contraceptive-used"   )
   
}    else if ( fileName == "data/fertility_Diagnosis.data") {
   myDataColNames <- c ( "Season", "Age", "Childish-diseases", "Accident", "Surgical_intervention",
                         "High_fevers", "alcohol_consumption", "Smoking", "sitting_hours", "class")
   
} else if ( fileName == "data/haberman.data") {
   myDataColNames <- c ( "Age", "operation_year", "positive_axillary_nodes", "class")
   
}    else if ( fileName == "data/Chess.data") {
   myDataColNames <- c( "White_King_file", "White_King_rank", "White_Rook_file", "White_Rook_rank", "Black_King_file", "Black_King_rank", "class" )
   delimitCol <- ","
   headerRow <- FALSE
   
   
   
}  else if (fileName == "data/avila.data") {
   myDataColNames <- c ( "intercolumnar_distance", "upper_margin", "lower_margin", "exploitation", "row_number",
                         "modular_ratio", "interlinear_spacing", "weight", "peak_number", "modular_ratio/interlinear_spacing" ,"class" )
   fileNameTrain <- "data/avila/avila-tr.data"
   fileNameTest <- "data/avila/avila-ts.data"
   delimitCol <- ","
   headerRow <- FALSE
   
   
} else if (fileName == "data/sonar.data") { 
   delimitCol <- ","
   headerRow <- TRUE

} else if (fileName == "data/parkinsons.data") {
   delimitCol <- ","
   headerRow <- TRUE
   
} else if ( fileName == "data/leaf.txt") {
   myDataColNames <- c("class" ,"Number" ,"Eccentricity", "Aspect_Ratio" , "Elongation" , "Solidity", "Stochastic_Convexity",
                       "Isoperimetric_Factor" , "Maximal_Indentation_Depth", "Lobedness", "Average Intensity", "Average_Contrast",
                       "Smoothness" ,"Third_moment", "Uniformity" , "Entropy")
   delimitCol <- ","
   headerRow <- FALSE
   
}  else if ( fileName == "data/personActivity.txt" ) {
   myDataColNames <- c( "Sequence_Name" , "Tag_identificator", "timestamp" , "date", "X", "Y", "Z", "class"  )
   delimitCol <- ","
   headerRow <- FALSE
   
   
} else if ( fileName == "data/adult.data") {
   myDataColNames <- c( "age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship" ,
                        "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "class" )
   fileNameTrain <-  "data/adult_1.data"
   fileNameTest <- "data/adult_2.txt"
   delimitCol <- ","
   headerRow <- FALSE
   
}   else if (fileName == "data/bank_full.csv") {
   # myDataColNames <- c ("age", "job", "marital", "education", "default", "balance", "housing", 
   #                      "loan", "contact", "day", "month", "duration" , "campaign", "pdays", "previous",
   #                      "poutcome", "class" )
   delimitCol <- ";"
   headerRow <- TRUE
   
   
} else if ( fileName == "data/log2.csv") {
   myDataColNames <- c ("Source_Port", "Destination_Port","NAT_Source_Port", "NAT_Destination_Port" , "class",
                     "Bytes" , "Bytes_Sent", "Bytes_Received", "Packets", "Elapsed_time" , "pkts_sent", "pkts_received" )
   delimitCol <- ","
   headerRow <- TRUE
   
}  else if ( fileName == "data/shuttle.data") {
   myDataColNames <- c("A1" , "A2" , "A3" , "A4" , "A5" , "A6" , "A7", "A8" ,"A9" , "class")
   fileNameTrain <- "data/shuttle_Tr.txt"
   fileNameTest <- "data/shuttle_test.txt"
   delimitCol <- ""
   headerRow <- FALSE
}


#   Reading Data set into R

print ( "Reading dataset ..."  )

if (fileName == "data/Data_User_Modeling.xls"){
   
   trainData <- read.xlsx( "data/Data_User_Modeling.xls", trainSheetNo ,  colIndex=1:6, as.data.frame=TRUE, header=TRUE,stringsAsFactors = FALSE )
   testData <- read.xlsx( "data/Data_User_Modeling.xls", testSheetNo ,  colIndex=1:6, as.data.frame=TRUE, header=TRUE, stringsAsFactors = FALSE )
   
   names(trainData) <- c ("STG" , "SCG",  "STR",   "LPR", "PEG", "class")
   names(testData) <- c ("STG" , "SCG",  "STR",   "LPR", "PEG", "class")
   
   # combine train and test data in order to use the
   myData <- rbind(trainData, testData) 
   
} else if ( fileName == "data/BreastTissue.xlsx") {
   myData <- read.xlsx( fileName, sheetNo ,  as.data.frame=TRUE, header=TRUE, stringsAsFactors = FALSE)
   names(myData) <- c ( "class", "I0", "PA500", "HFS", "DA" , "AREA", "A_DA", "MAX_IP", "DR" ,"P" )
   
} else if ( fileName == "data/avila.data" | fileName == "data/adult.data" | fileName == "data/shuttle.data" ) {
   trainData <- read.table( file = fileNameTrain , header = headerRow , sep = delimitCol ,col.names = myDataColNames , stringsAsFactors = FALSE)
   testData <- read.table( file = fileNameTest , header = headerRow , sep = delimitCol ,col.names = myDataColNames , stringsAsFactors = FALSE)
   avilaTrainSize <-  nrow(trainData)
   avilaTestSize <- nrow(testData)
   
   # clean adult dataset 
   tempTrainData <- replace(trainData, trainData == " ?" | trainData == "?" | trainData == "? " , NA)
   tempTrainData <- na.omit (tempTrainData)
   trainData <- tempTrainData
   
   tempTestData <- replace(testData, testData == " ?" | testData == "?" | testData == "? " , NA)
   tempTestData <-  na.omit (tempTestData)
   testData <- tempTestData
   rm(tempTrainData, tempTestData)
   
   myData <- rbind(trainData, testData, make.row.names = FALSE)
   
   # Clean class levels of adult dataset
   
   if (fileName == "data/adult.data" ) { 
      
      myData$class <- as.character( myData$class)
      myData$class[myData$class == " <=50K." ]  <- "<=50K"
      myData$class[myData$class == " <=50K" ]  <-  "<=50K"
      myData$class[myData$class == " >50K" ] <-  ">50K"
      myData$class[myData$class == " >50K." ] <- ">50K"
      
      myData$class <- as.factor(myData$class)
      }
  
   
} else if (fileName == "data/sonar.data"){
   myData <- read.table (fileName , header = headerRow, sep = delimitCol,  stringsAsFactors = FALSE)
   names(myData)[names(myData) == "R"] <- "class"
   myDataColNames <- names(myData)
   
} else if (fileName == "data/parkinsons.data") {
   myData <- read.table (fileName , header = headerRow, sep = delimitCol,  stringsAsFactors = FALSE)
   names(myData)[names(myData) == "status"] <- "class"
   myDataColNames <- names(myData)
   
} else if ( fileName == "data/bank_full.csv" ) {
   myData <- read.table (fileName , header = headerRow, sep = delimitCol,  stringsAsFactors = FALSE)
   names(myData)[names(myData) == "y"] <- "class"
   myDataColNames <- names(myData)
   
} else  {
   myData <- read.table (fileName , header = headerRow, sep = delimitCol, col.names = myDataColNames,  stringsAsFactors = FALSE)
   
}


if (fileName == "data/breast-cancer-wisconsin.data"  ) {
   myData$Bare_Nuclei <- as.numeric(myData$Bare_Nuclei)
   
} else if (fileName == "data/post-operative.data") myData$COMFORT <- as.numeric(myData$COMFORT )




# compute the size of file before read it to avoid crashig with R's limited memory
file.info("data/letter_recognition.data")$size
file.info("data/skin_segmentation.data")$size

# Find whether the dataset inculdes ids as a column? if not, id will be add (later) to the dataset as a sequtional unique attribute    
idExist <- FALSE
if (  any ( grepl("\\bID\\b" , myDataColNames, ignore.case = TRUE))) {
   idCol <- which (grepl("\\bID\\b" , myDataColNames, ignore.case = TRUE)) 
   if ( length(idCol) == 1 ) {
      myDataColNames [idCol] <- "id"
      idExist <- TRUE
   } else ( print("There are more than one ID attributes in the data set! please Check!"))
   
}



# in some datasets class column come first so it shouldn't be counted as a predictive column such as in wine datset
colNum <- ncol(myData)

# determine which column is the class column
classCol <-  which(colnames(myData) == "class")

# reorder the columns to ensure that the class column is the last one
if (classCol != colNum) {
   myData.temp <- myData %>% select(-class , class)
   myData <- myData.temp 
   classCol <- colNum
}

# colToBeDiscarded <- c ( "ID" ,"id" , "Id", "iD",  "Sequence_Name" , "sequence_name", "Code_Number", "code_number", "Sequence Name")
colToBeDiscarded <- c ( which (grepl("\\bsequence_name\\b" , colnames(myData), ignore.case = TRUE)),
                        which (grepl("\\bCode_Number\\b" , colnames(myData), ignore.case = TRUE)) ,
                        which (grepl("\\bID\\b" , colnames(myData), ignore.case = TRUE)) ,
                        which (grepl("\\bname\\b" , colnames(myData), ignore.case = TRUE)) ,
                        which (grepl("\\bnumber\\b" , colnames(myData), ignore.case = TRUE)))
colToBeDiscarded <-  colnames(myData [colToBeDiscarded])
anyColumnDiscarded <- FALSE
if ( any (colnames(myData)  %in% colToBeDiscarded ) )  {
   anyColumnDiscarded <- TRUE
   unpredictiveAttr <- which ( colnames (myData ) %in% colToBeDiscarded)
   
   predictiveAttr <- colnames(myData)[ - c(unpredictiveAttr , classCol)]
}   else  predictiveAttr <- colnames( myData ) [-classCol]



if ( fileName == "data/covtype.data")  predictiveAttr <-  row.names(selectedFeature)


#  In case of personActivity dataset, date and time columns have to be removed from the list of predictiveAttr
if ( fileName == "data/personActivity.txt" ) {
   
   refinedPredictiveAttr <-  c( which(predictiveAttr == "timestamp") , which(predictiveAttr == "date" ))
   predictiveAttr <- predictiveAttr [ - refinedPredictiveAttr]
   
}

if ( fileName == "data/adult.data") {
   refinedPredictiveAttr <-   which(predictiveAttr == "education") 
   predictiveAttr <- predictiveAttr [ - refinedPredictiveAttr]
   
}

if ( fileName == "data/bank_full.csv") {
   
   predictiveAttr <- predictiveAttr [ - c(9:12 , 14:15 )]
   
   
}


### Cleaning Datasets ####

# **NEW**: CLEANING DATASET **: Dealing with the duplication in class column (of character type) where the training 
# dataset class levels are not matching the testing dataset class levels because of case sensitivity and using underscore
# instead of space ; this problem occured in User_Modelling Data set (Very Low / very_low)
# solution : lower case all the letters and replace the spaces by underscores

if ( fileName == "data/Data_User_Modeling.xls") {
   
   # The following step is to replace any punctuations such as (* , ; :) by underscore(_) >>> currently no need for it 
   # myData$class <- str_replace_all(myData$class, "[[:punct:]]","_")  
   
   # This is to replace spaces by underscore
   myData$class <- tolower(str_replace_all ( myData$class , "\\s+","_"))
   
}
# get the class levels and the number of  classes in the datastet  
classLevels <- levels(as.factor(myData$class))
numOfClasses <- length(classLevels)   


# ** CLEANING DATASET ** :dealing with missing values (none class labels) if any
missingValues <- FALSE
if ( any (myData == "?" | myData == " ?" | myData == "? " | is.na(myData)))  {
   missingValues <- TRUE
}

if (missingValues) {

      tempData <- replace(myData, myData == " ?" | myData == "?" | myData == "? " , NA)
      #any(is.na(tempData))
      myData <- tempData
      
      instWithMissingVal <- c()
      notClassificationAttr <- c()
      # find the column names with missing values
      colNamesWithMissingVal <- names(which(sapply(myData, anyNA)))
      
      
      # replace missing values (NA) in an attribute with the highest frequency value of that attribute 
      for ( M in 1:length (colNamesWithMissingVal) ){
         instWithMissingVal[M] <- sum (is.na(myData[colNamesWithMissingVal[M]])) 
         if (instWithMissingVal [M] > 0.25 * nrow(myData[colNamesWithMissingVal[M]])) {
            notClassificationAttr [M] <- colNamesWithMissingVal[M]
         }  else {
            attrValTableFrequency <- table(myData[colNamesWithMissingVal[M]])
            mostFrequentVal <- names (which.max(attrValTableFrequency) ) 
            
            #  checking the type of the current missing colum; whether it  is numeric or categorical ?
            # So: if the type of attribute values is numeric, the selected value should be convert to numeric first before applying to the missing places in the column
            # other wise the selected value will be treated as categorical  
            if ( is.numeric( myData[[colNamesWithMissingVal[M]]] )) {
               mostFrequentVal <- as.numeric(mostFrequentVal)
            }
            
            toReplace.na <- which (is.na(myData[colNamesWithMissingVal[M]]))
            myData[toReplace.na, colNamesWithMissingVal[M]] <- mostFrequentVal
         }
      }
 
}


#### opposite of %in% function ######
`%notin%` <- Negate(`%in%`)

#####ckecks for the empty variables ( integer(0) ####
is.integer0 <- function(x)
{
   is.integer(x) && length(x) == 0L
}


# find out whether class attribute are considered as a factor or continuous?
# extract numerical (continuous) attributes from data to be discretised later 

myData [ , classCol] <- as.factor(myData [, classCol])

# **NEW *** clean the values of the class attributes factors to insure that there is no duplication in the levels (due to spellings or underscores)
levels(myData$class) 

checkForNumAtrr <- sapply(myData, is.numeric)
numericCols <- which (checkForNumAtrr)

# *** NEW *** specify the numerical columns by their names instead of their positions in the columns list
numericColNames <-  colnames(myData[numericCols])   
summary(myData)


numericAttrTbl <- myData %>% select_if (is.numeric  ) 
if ( "id" %in% colnames(numericAttrTbl))  numericAttrTbl <-  select(numericAttrTbl , - "id" ) 
if ("class" %in% colnames(numericAttrTbl))  numericAttrTbl <- select(numericAttrTbl , - "class")
numColNames <- colnames(numericAttrTbl)

categAttrTble <- myData %>% select_if(is.character)
if ( "id" %in% colnames(numericAttrTbl))  numericAttrTbl <-  select(numericAttrTbl , - "id" ) 
if ("class" %in% colnames(categAttrTble))  categAttrTble <- select(categAttrTble , - "class")
categColNames <- colnames(categAttrTble) 

# @@@@@@@  ------------------------------------------ @@@@@@@%

# ########        Parameters and Pre-processing step  #########

# @@@@@@@  ------------------------------------------ @@@@@@@%

# usingCrossValidation <- FALSE
# foldsNum <- 5
# 
# TransformToNormality <- TRUE
# 
# 
# ##### Reduce Overfitting #########
# #--------------------------------#
# 
# # to reduce the overfitting that may occured in the rules 
# limitedTermsUsed <- FALSE
# maxTerms <- 4
# stoppingCriteriaUsed <- FALSE   # always False, No Change , it depends on the rule-term pobability
# minimumRuleAccuracy <- 0.00
# checkNextHigherProbability <- TRUE    # always TRUE
# 
# toKeepTheRule <- 0.00
# rulesDeletion <- FALSE # This for large datasets only to remove overfitted rules that cover less than 10 inctances 
# 
# # Ensemble Parameters ###
# # ------------------- ###
# 
# buildEnsembleModel <- FALSE
# finalEnsembleResults <- FALSE    # This flag should be always False and it will be change within the code (at the base classifiers combining stage)
# 
# numBaseClassifiers <- 100
# 
# rankingModels <- FALSE  # This flag variable has to be true to apply the ranking method
# 
# applyConsolidation <- FALSE # This flag variable has to be true to apply the consolidation method




if ( TransformToNormality ) {
   
   
   print ( "Pre-Processing Step: Normality Testing for continuous attributes is in progress ... ")
   
   
   # This function is checking the normality condition for the data, either complete data  or samples ; 
   # if sampleIndex variable > 0 , means data has to be sampled before checking the normality. otherwise no need for sampling 
   f.checkNormality <- function ( sampleIndex , dataToBeChecked) {
      #browser()
      if ( sampleIndex > 0) {
         set.seed (sampleIndex * 3000 )
         currentDataToBeChecked <- sample ( dataToBeChecked , 2000)
      } else {
         currentDataToBeChecked <- dataToBeChecked
      }
      currentNormTest <- jarque.test(currentDataToBeChecked)
      currentSkewness <- skewness(currentDataToBeChecked)
      currentKurtosis <- kurtosis(currentDataToBeChecked) 
      
      return (
         list (
            "test_Normality" = currentNormTest,
            "data_Skewness" = currentSkewness,
            "data_Kurtosis" = currentKurtosis 
         )
      )
   }
   
   
   
   # check if there are any numeric values in the data frame before sarting the transforming to the normality process
   if ( length(numColNames) > 0  )  {
      # Before checking the normality distribuation, we should make sure that there is no class has less than 3 instances
      # these classes with less than 3 instances have to be removed from normality test. Then have to be merge again with dataset
      
      classCounts <- table(myData$class)           ## c of counts at each class
      classesCoveredLessThan3Inst <- any(as.vector(classCounts ) < 3)
      if ( classesCoveredLessThan3Inst ) {
         lessThan3Inst <- which (as.vector(classCounts) < 3)            ##To get the numerical count values 
         classLessThan3Inst <- names (classCounts [lessThan3Inst])     ## To get the classes names with less than 3 instances 
         
         # Update the class levels 
         usedClassLevels <- classLevels [ !classLevels %in% classLessThan3Inst] 
         classLevels <- usedClassLevels 
         
         
         # remove the rows with classes cover less than 3 instances temporarly form the data set
         rowsToBeRemovedTemp <- myData %>% filter ( class %in% classLessThan3Inst)
         
         
      }
   } # else print ("There are no numeric attributes in this dataset")
   
   #**** for very large data sets, normality testing becomes less important. *****
   
   # Loop through each class, test the distribution of its continuous attributes 
   # transform their distribution into normal distribution when needed.
   
   
   # list of attributes that have been transformed 
   transformedColIndx <- 1
   transformedCol <- list()
   
   # The following loop goes through all the columns in the data frame! The continuous attribites will be approxiamte to the normality if needed. Some classes will be skipped if it is occured in (classLessThan3Inst) list
   for (j in predictiveAttr ) { 
      
      # check if the current the attribute is numerical or categorical 
      if (j %in% numericColNames)    { 
         
         # to make sure that the current attribute values are not exact identical .
         containesIdenticalVal <- data.frame (matrix(nrow = 1, ncol = 2))
         names (containesIdenticalVal) <- c ("Attribut name", "Class Name" )
         
         # Class lables that previoulsy removed from the data frame because of covering less than 3 instances, should be excluded from class levels
         for ( i in classLevels)  {
            # if ( i == "deny" & j == "NAT_Source_Port")  browser()
            # This list will include another list ( nested lists)
            sampleTest.BeforeTrans <- list()
            sampleTest.AfterTrans <- list()
            dataNormal <- FALSE
            needToTransform <- TRUE
            identicalchecker <- FALSE      # ( NEW & need to be checked ) this line was outside current loop , check if it's a correct place or not?
            
            if (classesCoveredLessThan3Inst) {
               if ( i %in% classLessThan3Inst) {
                  next()
               }
            } 
            # STEP 1: check wheather the values are identical or not. If not, compute the p.value for the column at the current class before the approximation process
            rowsToCheckNorm <- myData [ myData$class == i, j]     # j = predictiveAttr  ,  i = classLevels
            
            if (length(unique (rowsToCheckNorm)) == 1)  {
               
               identicalchecker <- TRUE
               print ("Identical atrribute values; No need to transformation!")
              
               next()
               
               #¢¢¢ NEW: The following lines of the code deleted on 19May19 and replaced by the above lines
               # attrNameToAdd  <- c ( j , i)
               # if (is.na (containesIdenticalVal[1,1]) ) {
               #    containesIdenticalVal [1,1 ] <- j
               #    containesIdenticalVal [1,2] <- i
               # } else {
               #    containesIdenticalVal <- rbind(containesIdenticalVal, attrNameToAdd)
               # }
               # cat ( 'All the values of attribute ', j ,  'at class ', i , 'are identical' ,'\n')
               
            } else {
               if (length (rowsToCheckNorm )  < 2000)  {
                  testNorm.BeforeTransform <-  f.checkNormality (0 ,  rowsToCheckNorm)
                  
                  if (testNorm.BeforeTransform$test_Normality$p.value > 0.05 ) {
                     dataNormal <- TRUE 
                  }
               } else {
                  PassedTestBeforeTrans <- 0
                  for (s in 1:5) {
                     normalityTest_sample <- f.checkNormality ( s, rowsToCheckNorm)
                     # normalityTest <- as.matrix (normalityTest_sample)
                     sampleTest.BeforeTrans[[s]] <- normalityTest_sample
                     if ( normalityTest_sample$test_Normality$p.value > 0.05 | is.na (normalityTest_sample$test_Normality$p.value )) PassedTestBeforeTrans <- PassedTestBeforeTrans + 1
                     
                  }
              
                  if (PassedTestBeforeTrans >= 3) {
                     dataNormal <- TRUE 
                  }  
               }
            } 
            # STEP 2:  if there is a need for approximation to normality, apply (log10 formula) to the current column values. 
            # Then compute the p.values of the transformed column values 
            
            if ( dataNormal == FALSE & identicalchecker == FALSE   ) {
               needToTransform <- TRUE
               # If a data attribute has larger than 2000 observations, shapiro test can not be used test it! 
               # probelm solved by doing the test on 5 differnt samples of that attribute. 
               # if 3 of these samples passed the test, hypothesis of normality can be rejected
               
               
               # cat (' \n  %- *-*-*-*-*-*-*-*-*_*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-* -% \n')
               # cat ( 'Normality Test (before transforming ): \n Attribute: ', j , ' Class: ', i , '\n')
               # print (testNorm.BeforeTransform) 
               
       
               # if ( testNorm.BeforeTransform$p.value < 0.05) { 
               
               # cat('p.value = ',testNorm.BeforeTransform$p.value, ' for attribute ' , j , ' of class : ', i, ' less than 0.05 \n')
               # 
               # cat ( 'Skewness  = ', Skew.BeforeTransform, '\n')
               # cat ( 'Kurtosis  = ', Kurt.BeforeTransform, '\n')
   
               minAttrVal <- min (rowsToCheckNorm)
               if ( minAttrVal <= 0 )  { 
                  log10Transform <- log10 ( 1 +  rowsToCheckNorm - minAttrVal)
               } else {
                  log10Transform <- log10 ( 1+ rowsToCheckNorm )
               }
               
               # Normality testing after approximating the current column values to the normal distribution .
               if (length(rowsToCheckNorm) < 2000) {
                  
                  testNorm.AfterTransform <-  f.checkNormality (0 ,  log10Transform)
                  
                  if (testNorm.AfterTransform$test_Normality$p.value < testNorm.BeforeTransform$test_Normality$p.value ) {
                     needToTransform <- FALSE  }
                  
               } else {
                  PassedTestAfterTrans <- 0
                  for (s in 1:5) {
                     normalityTest_sample <- f.checkNormality ( s, log10Transform)
                     
                     sampleTest.AfterTrans[[s]] <- normalityTest_sample
                     if ( normalityTest_sample$test_Normality$p.value > 0.05 | is.na (normalityTest_sample$test_Normality$p.value )) PassedTestAfterTrans <- PassedTestAfterTrans + 1
                     
                  }
                  if (PassedTestAfterTrans < PassedTestBeforeTrans) {
                     needToTransform <- FALSE 
                  } 
               }       # end of else statement  (instances numbers > 2000)
               
               if (  needToTransform) {
         
                  #    print ( "attribute values have been transformed to a normal distribution ")
                  transformedCol [[transformedColIndx ]] <- list (  "Transformed_Col" = j, "transformed_class" = i )
                  
                  transformedColIndx <- transformedColIndx + 1
                  if (  minAttrVal <= 0 )  {
                     
                     myData[ myData$class == i , j ] <- 
                        log10 (1 + myData[myData$class == i , j ] - minAttrVal  )  
                     
                  }  else {
                     
                     myData[ myData$class == i , j ] <- 
                        log10 (1+ myData[myData$class == i , j ]  )  
                     
                  }
                  
                  
               }  # if (  needToTransform) {
               
               
            }     # if (dataNormal == FALSE)

         }     # for (classLevels)
         
      }     # if (j %in% numColNames) 
      else {
         cat ( '\n', j, 'is not a numeric attribute')
      }
   }        #  for loop ( columns)
   
   print ("Noramlity Testing is completed ... ")
   
}
 #### Train Then Test partitioning   ====

print ("Partitioning dataset into training and testing sets...  ")

if ( ! usingCrossValidation ) { 
   
   # This datasets is already sampling (first 3133 used for training) and (final 1044 used for testing)
   if (fileName == "data/abalone.data") {
      trainData <-  myData [1:3133,]
      testData <- myData [3134:4177,]
      
   } else if (fileName == "data/Data_User_Modeling.xls"){
      # here we resplit myData back into train and test sets 
      trainData <- myData [1:258 , ]
      testData <- myData [259:403 , ]
      
   #} else if (fileName == "data/avila.data") {
      #trainData <- myData[1:10430 , ]
      #testData <- myData [10431:20867 ,  ]
      
      
   } else if (fileName == "data/covtype.data") {
      trainData <-  myData [1:15120,]
      testData <- myData [15121:581012 , ]
      
   # } else if( fileName == "data/adult.data" ) {
   #    
   #    trainData <- myData[1:30162 , ]
   #    testData <- myData [30163:45222 ,  ]
      
   } else if (fileName == "data/shuttle.data") {
      trainData <- myData [ 1: 43500 ,]
      testData <- myData [43501 : 58000 , ]
      
      
   } else  {
      
      # Another way of splitting using a split ratio according to the class
      sample <- sample.split(myData$class, SplitRatio = .70)
      trainData <- subset(myData, sample == TRUE)
      testData <- subset(myData , sample == FALSE)
      
   }  
   
   
} else {

      ### #### Cross Validation  ########
   
   #1-  Randomly shuffle the data
   myData <- myData [sample (nrow(myData)) , ]
   
   #2- Create 5 equally size folds
   folds <- cut(seq(1, nrow(myData)) , breaks = foldsNum , labels = FALSE)
   
}


# if  (fileName != "data/Data_User_Modeling.xls") {
#    write.xlsx(myData, "data/myData.xls" )
#    write.xlsx( trainData, "data/myTrainData.xls")
#    write.xlsx( testData, "data/myTestData.xls")
#    
# }


# get the number of  classes in the datastet  (extra info)
# numOfClasses <- length(unique(trainData$class))   


# # define untoched data: this dataset will not be changed at any point. (no need to this step )
# unTouchedData <- trainData


# create a data frame which contains the highest probabilities for each class
ruleTermsWithBestProbability <- data.frame (matrix(nrow = 1, ncol = 6))
names (ruleTermsWithBestProbability) <- c ("Type", "Value_1","Attribute", "Value_2" , "Class",  "Probability")


# @@@@@@@  ------------------------------------------ @@@@@@@%
#########             Functions Section              #########
# @@@@@@@  ------------------------------------------ @@@@@@@%

f.partitionMyTrainData <- function (bsTrainData , baseCl) {

   set.seed(15000 * baseCl)
   shuffledBaseData <- bsTrainData [sample (nrow (bsTrainData), replace = FALSE) , ]
   
   # ------------------------------------ #  ways of sampling
   # trainIndex <- createDataPartition( shuffledBaseData$class ,
   #                                    p = 0.632 ,
   #                                    list = FALSE ,
   #                                    times = 1)
   
   # BC_train <- shuffledBaseData [  trainIndex , ]
   # BC_valid <- shuffledBaseData [- trainIndex , ]
   
   
   # trainDataSize <- round(nrow( shuffledBaseData) * 0.632)
   # valiDataSize <- round(nrow(shuffledBaseData) * 0.368)
   # # 
   # 
   #    
   # BC_train  <- shuffledBaseData[sample(nrow(shuffledBaseData), trainDataSize , replace = TRUE) , ]
   # 
   # 
   # BC_valid <- shuffledBaseData[ sample(nrow(shuffledBaseData), valiDataSize , replace = TRUE) ,]
   
   # trainDataSize <- round(nrow( shuffledBaseData) * 0.632)
   
   # valiDataSize <- round(nrow(shuffledBaseData) * 0.368)
   
   # ------------------------------------- # 
   
   trainDataSize <- nrow(bsTrainData)
   
   trainIdx <- sample ( 1: nrow(shuffledBaseData) , size = trainDataSize , replace = TRUE )
   BC_train <- shuffledBaseData [ trainIdx , ]
   
   BC_valid <- shuffledBaseData [-trainIdx ,]
   
   
   # BC_valid <- dataForTestSampling[ sample(nrow(dataForTestSampling), valiDataSize , replace = FALSE) ,]
   
   # cat('\n row names selected for training: ', rownames(BC_train))
   
   # cat('\n row names selected for validation: ', rownames(BC_valid))
   
   # excludedRows <- which(rownames( dataForTestSampling) %notin% rownames(BC_valid))
   
   
   # cat ('\n row names not selected in training neither in validation datasets: ', rownames(dataForTestSampling [excludedRows ,]) )
   
   # cat ( '\n Train Data rows for classifier #',baseCl ,' = ', round (nrow(BC_train )) , '\n')
   
   
   return ( list ( "BCtrainData" = BC_train , 
                   "BCvalidData" = BC_valid ,
                   "trainSize" = round (nrow(BC_train )) ,
                   "validSize" = round (nrow(BC_valid )) 
   )
   )
   
}


f.initialRuleTermProbabilityTbl <- function () {

   RT_probabilityTbl <- data.frame (matrix(data = NA, nrow = 1, ncol = 8), stringsAsFactors = FALSE)
   names (RT_probabilityTbl) <- c ("Type", "Rule_Term.X" ,"Rule_Term.attribute", "Rule_Term.Y", "Class", "RT_Frequency" , "Attr_Frequency", "Probability" )
   RT_probabilityTbl$Type <- as.character(NA)
   RT_probabilityTbl$Rule_Term.X <- as.double(NA)
   RT_probabilityTbl$Rule_Term.attribute <- as.character(NA)
   RT_probabilityTbl$Rule_Term.Y <- as.double(NA)
   RT_probabilityTbl$Class <- as.character( NA)
   RT_probabilityTbl$RT_Frequency <- as.double(NA)
   RT_probabilityTbl$Attr_Frequency <- as.double(NA)
   RT_probabilityTbl$Probability <- as.double(NA)
   
   return(RT_probabilityTbl)
   
}

# ======== Function to check if data contains classes other than the given class  
f.classesOtherThanGivenClass <- function( D, inputClass) {
   
   # contains other than the input classs
   containsOtherClasses <- FALSE
   
   
   # go through each instance in the datasetD
   for ( h in D$class ) {
      
      if( h != inputClass){
         containsOtherClasses <- TRUE
         # cat ('Does Dataset contain classes other than ', inputClass, ' ? '  , containsOtherClasses)
         break()
      }
      
   }
   
   return (containsOtherClasses) }  


# This function is to calculate the probabilitiy according to the current att-val pair combanation

f.calculateRuleTermProbability <-  function (dataD, thisClass, thisRuleTerm, thisAttrType , reGenerateRT) {

   # find Instancese that covered by the current given class
   thisAttr <- thisRuleTerm[["attribute"]] 
   thisX <- thisRuleTerm[["x"]]
   thisY <- thisRuleTerm[["y"]]
   
   ## Compute attribute-value pair frequency at current class
   # refinedData <- dataD %>% dplyr::filter(class == thisClass)
   refinedData <- dataD [dataD [["class"]] == thisClass , ]
   
   
   # Categorical attributet
   if (thisAttrType == "categorical") {
      refinedData <- refinedData[refinedData[thisAttr] == thisY ,]
      
      # numerical attribute (smaller side)
   }else if (thisAttrType == "continuous") {       
      
      refinedData <- refinedData[which((refinedData[,thisAttr]) > thisX),]
      refinedData <- refinedData[which((refinedData[,thisAttr]) <=  thisY),]
      
      # filtering data using dplyr functions to speed up the processing; the following step SHOULD produce the
      # same results as the above step! 
      # refinedData <- refinedData %>% dplyr::filter ( (!!as.name(thisAttr)) > thisX)
      # refinedData <- refinedData %>% dplyr::filter ( (!!as.name(thisAttr)) <= thisY)
      
   }
   
   thisRuleTermFrequency <- nrow(refinedData)
   
   ## calculate the attribute-value pair frequency (probability) for the whole dataset (without specifying a class)
   
   # refinedAttr <- dataD %>% dplyr::select(thisAttr) 
   
   ## compute total frequency of current attrbiute-value for the whole dataset
   
   if (thisAttrType == "categorical") {
      
      # refinedAttr <- refinedAttr[refinedAttr[thisAttr] == thisY ,]    # it's not working and be replaced by the following
      refinedAttr <- dataD[dataD[[thisAttr]] == thisY ,]
      thisTotalAttrFrequency <- nrow(refinedAttr)
      
   } else if (thisAttrType == "continuous") {
      refinedAttr <- dataD[dataD[thisAttr] > thisX ,]
      refinedAttr <- refinedAttr[refinedAttr[thisAttr] <= thisY ,] 
      
      
      # Equalivent step to the above  
      # refinedAttr <- refinedAttr %>% dplyr::filter ( (!!as.name(thisAttr)) > thisX)
      # refinedAttr <- refinedAttr %>% dplyr::filter ( (!!as.name(thisAttr)) <= thisY)
      
      thisTotalAttrFrequency <- nrow(refinedAttr)
      
   }
   
   ## Calculate the probablities of occurance, P(class | attrVal), for each possible rule term
   thisRuleTermProbability <- thisRuleTermFrequency / thisTotalAttrFrequency
   
   #thisRuleTermProbability [is.finite(thisRuleTermProbability)] <- 0
   thisRuleTermProbability [is.nan(thisRuleTermProbability)] <- 0
   
   # @@@ ----------------------------- @@@ \

   if (  reGenerateRT == TRUE ) {
      return (
         list (
            "Rule_Term" = thisRuleTerm,
            "outRuleTermFrequency" = thisRuleTermFrequency,
            "outRuleTermProbability" = thisRuleTermProbability ,
            "outTotalAttrFrequency" = thisTotalAttrFrequency,
            "outRefinedData" = refinedData , 
            "outRefinedAttr" = refinedAttr) )
      
   } else {
      return (
         list (
            "Rule_Term" = thisRuleTerm,
            "outRuleTermFrequency" = thisRuleTermFrequency,
            "outRuleTermProbability" = thisRuleTermProbability ,
            "outTotalAttrFrequency" = thisTotalAttrFrequency,
            "outRefinedData" = refinedData) )
      
   }
   
   
}   # End of function:  f.calculateRuleTermProbability


# This function is to update the ruleTermProbabilities with a given new input ruleTerm to be added to
f.updateRuleTermProbabilities <- function ( thisAttrType, thisClass, thisRuleTermToAdd,  allRuleTermProbab, thisRuleTermProbab) {
   
   # fill a data frame to be added to another one
   thisRuleTermToAdd$Type <- thisAttrType
   thisRuleTermToAdd$Rule_Term.X <- thisRuleTermProbab$Rule_Term$x
   thisRuleTermToAdd$Rule_Term.attribute <- thisRuleTermProbab$Rule_Term$attribute
   thisRuleTermToAdd$Rule_Term.Y <- thisRuleTermProbab$Rule_Term$y
   thisRuleTermToAdd$Class <- thisClass
   thisRuleTermToAdd$RT_Frequency <- thisRuleTermProbab$outRuleTermFrequency
   thisRuleTermToAdd$Attr_Frequency <-thisRuleTermProbab$outTotalAttrFrequency
   thisRuleTermToAdd$Probability <- thisRuleTermProbab$outRuleTermProbability
   
   
   if (is.na(allRuleTermProbab[1,1]))  {
      allRuleTermProbab [1,] <- thisRuleTermToAdd
      
   } else {
      indx <- nrow(allRuleTermProbab) + 1
      allRuleTermProbab [indx, ] <- thisRuleTermToAdd
      
   }
   
   return (allRuleTermProbab)
   
}  # End of function : f.updateRuleTermProbabilities

#### 
# this function checks if the attributes values are identical
f_checkIdenticalValues <- function  (x) {
   if (length (unique(x)) == 1L) { 
      # print ( "density probabilities of current attribute values are identical")
      return ( TRUE)
      
   }else { return (FALSE)}
   
}

# standard normal distribution of the first quartile is - 0.67 and standard normal distribution the third quartile is 0.67
f.find_XY_Bounds <- function( this_attrSd, this_indexStart, this_indexEnd)  {
   
   x <- this_attrSd * ( -0.67) + this_indexStart
   y <- this_attrSd * (0.67) + this_indexEnd
   
   return(
      list( "X" = x ,
            "Y" = y
      )
   )
   
}

#  /*** the following fucntion: ***/
# - updating the classified instances data frame with a given new prediction /

f.updatePredictionsTable <- function ( exmp , predClass, dfClassified) {
   
   # add additional column to the data set which contains the predicted class voted by the Rules
   aPredictionToAdd <- cbind( exmp , 'Predicted Class'= predClass )
   
   # cat ('\n predicted class = ', predClass, '\n')
   
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
      
   } 
   return(isCorrect)
}    # End of function : f.checkPrediction


###### End of global Functions Section ####-


# @@@@@@@  ------------------------------------------ @@@@@@@%
########        Functions used in Prediction stage     #######
# @@@@@@@  ------------------------------------------ @@@@@@@%


f.initialClassifiedInstancesTable <- function(testDataFactors) {
   classifiedInstancesTbl <- data.frame(matrix(nrow=1,ncol= 5), stringsAsFactors = FALSE)
   names(classifiedInstancesTbl) <- c("id", "class", "Predicted_Class", "Classification_Type", "Rule_ID"  )
   classifiedInstancesTbl$id <- as.numeric(NA)
   classifiedInstancesTbl$class <- factor(classifiedInstancesTbl$class, 
                                          levels = testDataFactors)
   classifiedInstancesTbl$Predicted_Class <- factor(classifiedInstancesTbl$Predicted_Class, 
                                                    levels = testDataFactors)
   classifiedInstancesTbl$Classification_Type <- as.character(NA)
   classifiedInstancesTbl$Rule_ID <- as.numeric(NA)
   
   return(classifiedInstancesTbl)
   
} # END of f.initialClassifiedInstancesTable function

f.updatePredictionsTable <- function( thisClassifiedInstanceToAdd , thisClassifiedInstancesTbl ) {
   
   if (is.na(thisClassifiedInstancesTbl [1,1])) {
      thisClassifiedInstancesTbl[1,] <- thisClassifiedInstanceToAdd
   } else {
      # indx <- nrow(thisClassifiedInstancesTbl) + 1
      # thisClassifiedInstancesTbl [indx ,] <- thisClassifiedInstanceToAdd
      thisClassifiedInstancesTbl <- rbind(thisClassifiedInstancesTbl , thisClassifiedInstanceToAdd)
   }
   return( thisClassifiedInstancesTbl)
   
}    # END of f.updatePredictionsTable function


# This function to order the rules according to their probabilities values
f.orderRulesByProbability <- function (unOrderedRules) {

   rulesNames <- 1:length(unOrderedRules)
   
   names(unOrderedRules) <- rulesNames
   
   # mapRulesByProbability <- list.map(unOrderedRules , last(Probability))
   
   mapRulesByFrequency <- list.map(unOrderedRules , last(RT_Frequency))
   
   
   #moreThanOneFrequency <- allFrequencies [ which (allFrequencies !=1 )]
   
   
   #orderedRulesNames  <- order (-moreThanOneFrequency)
   
   orderedRulesNames  <- order (-unlist(mapRulesByFrequency ,  use.names =  T))
   
   orderedRules <- list()
   previousFrequency <- 0
   previousProbability <- 0
   for ( con in 1: length(orderedRulesNames)) { 
      ####################################
      
      # a new rule to be appended to the new list
      newOrderedRule <- unOrderedRules [ orderedRulesNames[con]]
      currentFrequency <-  last (newOrderedRule[[1]]$RT_Frequency)
      currentProbability <- last (newOrderedRule [[1]]$Probability)
      
      # compare the frequency and probabilty values of the new rule with the old one that already appended in the previous alteration
      if ( (previousFrequency == currentFrequency) & (previousProbability < currentProbability ) ) {
         
            # replace the position of the current to be before the last appended rule 
            previousOrderedRules <- unOrderedRules [ orderedRulesNames[con - 1]]
            
            orderedRules [con -1] <- newOrderedRule
            # rename the new replaced rules as the last appended name still exist
            names (orderedRules) [ names (orderedRules) == names(previousOrderedRules) ] <- names(newOrderedRule)

            orderedRules <- append( orderedRules , previousOrderedRules )
            previousFrequency <- last ( orderedRules[[con]]$RT_Frequency)
            previousProbability <- last( orderedRules [[con]]$Probability)
        
      } else {
        
         orderedRules <- append( orderedRules , unOrderedRules [ orderedRulesNames[con]])
         previousFrequency <- last ( orderedRules[[con]]$RT_Frequency)
         previousProbability <- last( orderedRules [[con]]$Probability)
         
      }
      
     
   }   
 
   
   return(orderedRules)  
   
}



# @@@@@@@  ------------------------------------------ @@@@@@@%
#########             Classification Stage           ######### 
# @@@@@@@  ------------------------------------------ @@@@@@@%


# Building a base G-rule-IQR classifier

GRulesIQR.classification <- function( trainD ) {
   
   cat ('\n')
   
   print ( "Classification Stage ....")

   
   # define original dataset according to the psydocode 
   originalData <- trainD
   
   classLevels <-  levels(as.factor(originalData$class))
   
   #=========   #=========   #=========   xxxxxx under invistagation  xxxxx #=========   #=========
   
   # This part to double check the class levels because in a rare situation the previous step have levels not in the originalData classes 
   refinedClassLevels <- unique(as.character( originalData$class))
   refinedClassLevels <- levels (as.factor(refinedClassLevels))
   
   if ( any (classLevels %in% refinedClassLevels == FALSE )) {
      
      classTORemovePosition <- which (classLevels %in% refinedClassLevels  == FALSE)
      refinedClassLevels <- classLevels [ - classTORemovePosition]
      classLevels <- refinedClassLevels
   }
   #=========   #=========   #=========   xxxxxx under invistagation xxxxx #=========   #=========
   
   # define the rules library which contains all the rules for all the classes as a set of data frames
   rulesLibrary.R  <- list () 
   N <- 1
   
   # Loop through each class 
   
   for (eachCl in classLevels) {     # loop1   eachCl is the class id
      
      keepGoing <- TRUE
      
      while (keepGoing)  {       #    Until all insatnces of current class are removed from originalData set
         
         # Create D based on original dataset
         datasetD <- originalData
         
         #  create a vector to collect all the probabilities of the given class
         ruleTermProbabForClass <- c ()
         
         # create a new vector to collect the list of used attribute for the current class
         usedAttribute <- c()     
         
         # create a data frame which contains all the selected rule terms 
         listOfRuleTerms <- f.initialRuleTermProbabilityTbl()

         # Remove the (Attr_Frequency) column forom the created listOfRuleTerms
         listOfRuleTerms <- listOfRuleTerms %>% select(-Attr_Frequency)
         
         # listOfRuleTerms <- data.frame(matrix(nrow=1,ncol=6))
         # names(listOfRuleTerms) <- c("Type", "Value_1","Attribute", "Value_2" , "Class",  "Probability")
         
         noMoreAtt <- FALSE
         
         while ( f.classesOtherThanGivenClass (datasetD, eachCl) == TRUE ) {    
            
            
            # create a matrix to collect all the probabilities for the current class
            ruleTermProbabilities <- f.initialRuleTermProbabilityTbl( )
            
            #  loop through all the predictive attributes, class attribute is not counted,
            for ( eachAttr in predictiveAttr )  {
               # check whether this attribute is already used to induce a rule term for the current class
               # if (colnames(datasetD[eachAttr]) %in%  usedAttribute)   {      
               if (eachAttr %in%  usedAttribute)   {
                  
                  # if there is no more attributes to spilt on and the training dataset still has 
                  # classes other than the target class 
                  
                  # if (length (usedAttribute) == allPredictAttr) {
                  if (length (usedAttribute) == length(predictiveAttr)) {
                     noMoreAtt <- TRUE
                     break
                     
                  } else next()
               }     # if (eachAttr %in% usedAttribute)
               
               # a small dataframe to store each column in the dataset each time separately 
             
               # currentAttr <- datasetD %>% dplyr::filter(class == eachCl) %>% select(id, eachAttr)
               currentAttr <- datasetD [datasetD$class == eachCl ,  ] %>% select( eachAttr)
               
               # The following branch of if-statment exectues when the type of an atrtribute is Numerical (continuos)
               if ( eachAttr %in% numColNames) {
                  
                  # sort the column in desceding order and compute the mena and the standard deviation 
                  currentAttrValues <- pull (currentAttr) %>% sort(., decreasing = FALSE) 
                  
                  # currentAttr <- arrange_(currentAttr , eachAttr)
                  # currentAttrValues <-  pull (currentAttr , eachAttr)
                  
                  attrType  <- "continuous"
                  
                  # calculate the mean , variance and standard deviation of continous attribute for the current class (eachCl)
                  attrMean <- mean (currentAttrValues) 
                  attrSd <- sd ( currentAttrValues)
                  attrvariance  <- var (currentAttrValues)
                  
                  # solving the problem of having only one instance in the train data
                  
                  trainDataOneInstance <- FALSE
                  if (length (unique(currentAttrValues)) == 1 ) trainDataOneInstance <- TRUE
                  
                  next_x <- 1
                  next_y <- 1
                  x <- NA
                  y <- NA
                  
                  if ( ! trainDataOneInstance) {
                    
                     # remove the duplicated values ( unique values) for simplyfing and also to easily find next lower and greater values
                     currentAttrGPDD <-  unique(currentAttr[eachAttr]) 
                     
                     # reorder the current column values in increasing order. The results in a form of a data table
                     currentAttrGPDD <- currentAttrGPDD %>% arrange_( eachAttr)
                     
                     # Reorder the data and produce the results in a form of voctor
                     # currentAttrGPDD <- currentAttrGPDD[order(currentAttrGPDD[eachAttr]) , ]
                     
                     # Compute the GPDD values for the current attribute instances
                     currentAttrGPDD <-  currentAttrGPDD %>% 
                        mutate( GPDD = dnorm(  currentAttrGPDD [[eachAttr]] , attrMean , attrSd))
                     # print (currentAttrGPDD)
                     
                     # # The following line adds another column to the data table without changing the order of the rows
                     # It keeps the row names and can be replaced by the above 
                     # currentAttrGPDD <- cbind(currentAttrGPDD , GPDD = dnorm(  currentAttrGPDD [[eachAttr]] , attrMean , attrSd))
                     
                     # select the row/s with highest GPDD value , (hint: both next lines produce same results! however using the data table function
                     # instead of dplyr keeps the original order of the rows while the other recounted the lines)
                     
                     highestDensityRows <- currentAttrGPDD [currentAttrGPDD$GPDD == max (currentAttrGPDD$GPDD) ,  ] 
                     # highestDensityRows <- dplyr::filter(currentAttrGPDD,  GPDD == max(GPDD))
                     
                     highestDensityRowsLength <-  nrow (highestDensityRows) 
                     
                     # in case of having more than one instance with highest density value, double check if the GPDD values are exactly the same 
                     # even though the diferences in the instances values (rare)!
                     if ( highestDensityRowsLength > 1 ) { 
                        
                        allDensityIdentical <- f_checkIdenticalValues ( highestDensityRows$GPDD)
                        
                     }
                     # this step produce the same results as the one after; however it would be very useful if the GPDD column added using cbind function not dplyr function 
                     # highestDensityIndex <- as.integer (row.names(highestDensityRows) )      
                     
                     highestDensityIndex <- which ( currentAttrGPDD[[eachAttr]] == highestDensityRows[[eachAttr]] ) 
                     
                     # cat("highest density row number: ", highestDensityIndex)
                     # print (highestDensityRows)
                     
                     # Compute the first and the third quartiles using z-score (0.67) under the curve 
                     # The following is a default status when we a GPDD value refere to one attribute value. 
                     # however; sometimes this is not the case
                     
                     if (highestDensityRowsLength == 1)   {   
                        
                        indexStart <-  highestDensityIndex
                        indexEnd <- highestDensityIndex
                        
                     } else if (highestDensityRowsLength > 1)  {
                        
                        if ( allDensityIdentical )  {
                           indexStart <- highestDensityIndex[1]
                           indexEnd <- highestDensityIndex [highestDensityRowsLength]
                           
                        } else {
                           indexStart <- highestDensityIndex[1]
                           indexEnd <- highestDensityIndex [1] + ( highestDensityRowsLength -1)
                           
                        }
                     }
                    
                     upper_Lower_Bounds <- f.find_XY_Bounds( attrSd , currentAttrGPDD [[eachAttr]][indexStart],  currentAttrGPDD [[eachAttr]][indexEnd]) 
                     x <- upper_Lower_Bounds$X
                     y <- upper_Lower_Bounds$Y
                     
                     # if ( ! trainDataOneInstance )   END 
                     
                  }   else  {  # if trainDataOneInstance 
                     
                     x <- unique (currentAttr [[eachAttr]]) - 0.001
                     y <- unique (currentAttr [[eachAttr]]) + 0.001
                     
                  }
                  
                  # Create a RuleTerm  RT
                  aRuleTerm <- list ( "x" = x , "attribute" = eachAttr  , "y" = y )
                  # print (aRuleTerm)
                  
                  
                  
                  aRuleTermProbability <- f.calculateRuleTermProbability ( dataD = datasetD,
                                                                           thisClass = eachCl,
                                                                           thisRuleTerm = aRuleTerm,
                                                                           thisAttrType = attrType ,
                                                                           reGenerateRT = FALSE)
                  
                  
                  if (aRuleTermProbability$outRuleTermProbability == 0 )  { 
                     # print ( " This rule does not cover any instance; so it will be discarded !")
                     
                     next }
                  
                  if ( aRuleTermProbability$outRuleTermProbability < minimumRuleAccuracy ) {
                     stoppingCriteriaUsed <- TRUE
                  
                     next()
                  }
                  
                  # iniallise a row to be added to the ruleTermProbablities tabel
                  aRuleTermProbToAdd  <- f.initialRuleTermProbabilityTbl()
                  
                  # Add the results into RuleTermsProbabilities
                  ruleTermProbabilities <- f.updateRuleTermProbabilities ( attrType, eachCl, aRuleTermProbToAdd, ruleTermProbabilities , aRuleTermProbability)
                  
                  # The following else statment exectues in case of Categorical attributes
                  # } else if (is.character (currentAttr[[eachAttr]])) { 
                  
               } else if (eachAttr %in% categColNames) { 
                  attrType <- "categorical"
                  categLevels <- levels (as.factor(currentAttr[[eachAttr]]))
                  for (attrVal in categLevels) {    # loop3
                     aRuleTerm <- list( "x" = NA , "attribute" = eachAttr  , "y" = attrVal) 
                     # print (aRuleTerm)
                     
                     # Calculate RT probability for the current attribute ( categorical)
                     aRuleTermProbability <- f.calculateRuleTermProbability ( dataD = datasetD,
                                                                              thisClass =  eachCl,
                                                                              thisRuleTerm = aRuleTerm,
                                                                              thisAttrType = attrType,
                                                                              reGenerateRT = FALSE)
                     # print (aRuleTermProbability)
                     
                     # iniallise a row to be added to the ruleTermProbablities tabel
                     aRuleTermProbToAdd  <- f.initialRuleTermProbabilityTbl()
                     
                     # Add the results into RuleTermsProbabilities
                     ruleTermProbabilities <- f.updateRuleTermProbabilities ( attrType, eachCl, aRuleTermProbToAdd, ruleTermProbabilities , aRuleTermProbability)
                     
                  }
                  
               }   #   else if (eachAttr %in% categColNames)  END
               
               
            }     # for ( eachAttr in predictiveAttr )
           
            if (is.na( ruleTermProbabilities [1,1]))  {
             
               break ()
               
            }
            highestProbability <-  max(as.numeric(ruleTermProbabilities$Probability))
            
            # select the rule term that maximise the probability of the current class (Best Rule Term), then add it to the listOfRuleTerm 
            # rowToAdd2 <- dplyr::filter( ruleTermProbabilities , Probability == max(Probability) )
            rowToAdd <- ruleTermProbabilities [which(as.numeric (ruleTermProbabilities$Probability) ==  highestProbability) , ]
            
            # tie-breaking rules:  select the terms with the highest frequency
            if ( nrow ( rowToAdd) > 1 ) {
             
               rowToAdd <- rowToAdd [which((rowToAdd$RT_Frequency) ==  max (rowToAdd$RT_Frequency)) ,]
               
               # similar above but without considering the row names
               # rowToAdd2 <- dplyr::filter(rowToAdd2 , RT_Frequency == max(RT_Frequency) )      
               if (nrow(rowToAdd) > 1 ) 
                  rowToAdd <- rowToAdd [!duplicated(rowToAdd$RT_Frequency) , ]  # another tie-breaking step
               
            }
            
            # sortedByFrequency <- ruleTermProbabilities [order (as.numeric (ruleTermProbabilities$RT_Frequency), decreasing = TRUE) ,]
            if ( checkNextHigherProbability )  {
               highestFrequency <- max(as.numeric(ruleTermProbabilities$RT_Frequency))
               RT_highestFrequency <- ruleTermProbabilities[which(as.numeric (ruleTermProbabilities$RT_Frequency) == highestFrequency) , ]
               if ( RT_highestFrequency[1,]$Probability != highestProbability ) {
                  diffCoverage <- as.numeric(rowToAdd$Probability) - as.numeric(RT_highestFrequency[1,]$Probability)
                  if (diffCoverage != 0 & diffCoverage <= 0.06) {
                   
                     rowToAdd <- RT_highestFrequency[1,]
                  }
               } 
            }    
            # After selecting the best rule term among ruleTermsProbablities; there is no need to keep (Attr_Frequency) column
            rowToAdd <- rowToAdd %>% dplyr::select(-Attr_Frequency)
            row.names(rowToAdd)
            
            # Add selected rule term to the current listOfRuleTerms
            if (is.na (listOfRuleTerms[1,1]) ) {
               listOfRuleTerms [1, ] <- rowToAdd[1,]
            } else {
               listOfRuleTerms <- rbind (listOfRuleTerms, rowToAdd)
            }
            # Update usedAttributes list
            usedAttribute <- c ( usedAttribute , rowToAdd$Rule_Term.attribute)
            
            # Create Subset S contains instances that covered by the selected Rule Term
            bestAttribute <- rowToAdd$Rule_Term.attribute
            if ( rowToAdd$Type == "categorical") {
               bestAttrValue <- rowToAdd$Rule_Term.Y
               
               dataCoveredByBestProbability <- datasetD [which(datasetD[bestAttribute] == bestAttrValue) , ]
               
            }else if ( rowToAdd$Type == "continuous") {
               bestRT.X <- as.numeric(rowToAdd$Rule_Term.X) 
               bestRT.Y <- as.numeric(rowToAdd$Rule_Term.Y)
               
               dataCoveredByBestProbability <-
                  datasetD [which(datasetD [ bestAttribute] > bestRT.X ) ,]
               
               dataCoveredByBestProbability <-                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                  dataCoveredByBestProbability [which(dataCoveredByBestProbability
                                                      [bestAttribute] <= bestRT.Y) ,]
               
            }
            
            # print ("Instances covered by best rule term in datadetD :")
            # print (dataCoveredByBestProbability)
            
            # Remove RT from the listOfRuleTerms if there S is empty 
            # If S is empty, this means that the last Rule-term add nothing to the rule and have to be discarded
            # Soultion:
            # - Remove the last rule-term from the listOfRuleTerms. 
            # - Check the majority class of the subset S:
            #   If it is similar to the target class, the rule will be considered to be completed.  
            #   Then, assign subset S to DatasetD
            #   If the majority class is different from the target class, discard the whole rule and remove subset S from the original data
            
            noInstCoveredRuleTerm <- FALSE
            
            if (nrow (dataCoveredByBestProbability) == 0  | !any (dataCoveredByBestProbability$class == eachCl) ) {
               #   cat ( 'Last rules-term will be removed from the list  ')
               
               noInstCoveredRuleTerm <- TRUE
               aTermToBeRemoved <- nrow(listOfRuleTerms) 
               listOfRuleTerms <- listOfRuleTerms [ - aTermToBeRemoved, ]
               break()
               
            } 
            
            # Assign the subset S to the datasetD
            
            datasetD <- dataCoveredByBestProbability
            
            # To control the number of rule terms per Rule and it is decided by the user (be default the maximum number of rule-terms = 4 )
            reachMaxTermsNum <- FALSE
            if (nrow (listOfRuleTerms) >= maxTerms & limitedTermsUsed)  {
              
               reachMaxTermsNum <- TRUE
               break
            }
         }      #  while (classOtherThangivenClass)  END
         
      
         
         if ( !(is.na (listOfRuleTerms [1,1])))  {
            
            
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
            
             cat('\n Rule number ' , N -1 , ' just generated .. \n')
            # print (listOfRuleTerms)
           
            # Problem:  having no more attributes to use to genertae more rule-term in order to build a pure rule.
            # solution >> check the majority class in the current set of the data, 
            # If it is similar to the target class >> keep the rule
            # If it is different, delete the rule 
            
            if (noMoreAtt | noInstCoveredRuleTerm | reachMaxTermsNum) { 
               
               #  cat (' \n WORNING: DatasetD contains classes other than the target class. As there are no more attributes to spit on!
               #  or the rule induction was terminated. Majority vote will be tested Now...\n')
               
               # first: find the majority class for the target instances
               majorityClass <-  names (which.max(table(datasetD$class))) 
               
               # # second: find the size of current datasetD for the target class eachCl
               # currentDatasetDSize <- datasetD %>% dplyr::filter(class == eachCl) %>% nrow()
               # currentOriginalDataSize <- originalData %>% dplyr::filter(class == eachCl) %>% nrow()
               
               # second: find the size of current datasetD for the target class eachCl
               currentDatasetDSize <- nrow ( datasetD [ datasetD$class == eachCl,])
               currentOriginalDataSize <- nrow (originalData [ originalData$class == eachCl , ])
               
               # The current rule will be kept if the majority class is equal to the target class 
               # and the size of the datasetD for the target class should contains at least 60% from the datasetD table
               
               if (majorityClass  == eachCl &
                   #nrow(datasetD) > currentOriginalDataSize * 0.1  &
                   currentDatasetDSize > nrow(datasetD) * toKeepTheRule) {   # the current rule  will be kept
                  
                  
                  #  cat ( 'Majority vote: ', majorityClass, ' | ', ' target class: ', eachCl  )
                  
                  #  cat ( '\n the Rule will be kept and all the instances in the clash set (datasetD) will be removed from the original data')
                  
                  # instancesMatchingTargetClass <- datasetD %>% dplyr::filter(class == majorityClass) %>% nrow()    # CovPos
                  
                  instancesMatchingTargetClass <- datasetD [datasetD$class == majorityClass, ]
                  covPos <-  nrow (instancesMatchingTargetClass)
                  
                  # cat ( '\n', covPos , ' instances out of ', nrow(datasetD), ' are positivly covered by the rule \n')
                  
                  # because the rule will be kept, all the instances in the clash set should be removed from the original training data
                  # in other words, assign the current datasetD to the original data set.
                  
                  refinedOriginalData <- datasetD
                  
                  # According to a paper titled (J-measure Based Hybrid Pruning for Complexity Reduction in Classification Rules) 
                  # page 435:
                  # if the rule is discarded, the all instances that match the target class should be deleted form the training
                  # set before the start of the next rule induction.
                  # If the rule is kept, then all instances in the clash set should be deleted from the training data.
                  
               }  else {
                  # cat ( '\n datasetD contains instances from different classes other than the terget one eachCl and there is no more attributes to use  \n',
                  #      'Also, the majority class is not similar to class eachCl .' )
                  # cat ( ' currrent solution : discard the rule and remove the instances that match the target class from the original data')
                  
                
                  aRuleToBeRemoved <- length(rulesLibrary.R)
                  
                  cat ( ' \n' ,  aRuleToBeRemoved , ' will be removed from the RulesLibrary')
                   # print ( c("the following rule will be removed from the rules Library: ", aRuleToBeRemoved))
                  rulesLibrary.R <- rulesLibrary.R [ -aRuleToBeRemoved]
                  N <- N-1
                  refinedOriginalData <- datasetD [datasetD$class == eachCl , ]   
                  
                  # refinedOriginalData <- datasetD [datasetD$class == eachCl , ]       
               }
               #   if (noMoreAtt | noInstCoveredRuleTerm | reachMaxTermsNum)   END
               
            } else {
               
               for (eachRuleTerm in 1:nrow(listOfRuleTerms))   {
                  aTermAttribute <- listOfRuleTerms$Rule_Term.attribute[eachRuleTerm]
                  
                  if (eachRuleTerm == 1 )   {
                     refinedOriginalData <- originalData [ originalData$class == eachCl , ]
                  }
                  if ( listOfRuleTerms[eachRuleTerm ,]$Type == "continuous") {
                     refinedOriginalData <- refinedOriginalData [which((refinedOriginalData[ , aTermAttribute]) 
                                                                       > as.numeric (listOfRuleTerms$Rule_Term.X[eachRuleTerm])), ] 
                     
                     refinedOriginalData <- 
                        refinedOriginalData [which((refinedOriginalData[ , aTermAttribute]) 
                                                   <= as.numeric (listOfRuleTerms$Rule_Term.Y[eachRuleTerm])), ] 
                  } else if ( listOfRuleTerms[eachRuleTerm , ]$Type == "categorical") {
                     refinedOriginalData <- 
                        refinedOriginalData [which(as.factor(refinedOriginalData[ , aTermAttribute]) 
                                                   == listOfRuleTerms$Rule_Term.Y [eachRuleTerm ]), ]
                  }
               } 
               
            } 
           
            
            # remove the selected rows from the original dataset
            
            rowsToRemove <-  row.names (refinedOriginalData)
            
            
            #  cat ( ' rows numbers to be removed from the original data set = ', rowsToRemove, '\n')
            
            # print (c("No of Col before removing the rows above = ", nrow (originalData)))
            
            dataNotCoveredByRule <- originalData[ !(row.names(originalData) %in% rowsToRemove),  ]
            
            #   cat ('\n the original data after removing instances that covered by the rule term R: \n')
            
            originalData <- dataNotCoveredByRule
            
            #  print (c("No of Col after the removing process = ", nrow (originalData)))
            
            # print (originalData)
            
            
            # check that all instances from current class have been removed from original dataset
         }else break()
         
         if (!(eachCl %in% originalData$class)) { 

           
            keepGoing <- FALSE
         }
        
         originalDataLeft <-  which(originalData$class == eachCl)
         cat ( '\n', length (originalDataLeft) , ' instances of class (', eachCl , ') are left \n')
         
      }    # end of while loop 1 (keepGoing)
      
      
      # reset the original dataset
      #   cat ('\n dataset now is re-setted back to the completed data: \n')
      originalData <- trainD
      
     
   }   # end of for Loop1
   
   
   return(rulesLibrary.R)
   
}     # End of GRules.IQR function 


f.displayRules <- function ( rulesLib.R) {
   
   
   # Display the final results ( the complete set of Rules)
   #cat ('\n RULES LIBRARY \n,', '\n =============== \n')
   #print (rulesLib.R)
   rulesNumber <- length (rulesLib.R)
   
   cat ('\n', rulesNumber , 'rules have been generated ')
   
   #cat ('\n *-*-*-*- Induced Rules : *-*-*-*\n \n')
   
   totalNumOfRuleTerms <- 0
   
   for ( ruleIndx in 1: rulesNumber) {
      #cat (ruleIndx , ') ' ,'IF ')
      aRuleToPrint <- rulesLib.R[[ruleIndx]]
      
      totalNumOfRuleTerms <- totalNumOfRuleTerms + nrow(aRuleToPrint)
      
      # for ( trm in 1:nrow(aRuleToPrint)) {
      #    
      #    if (aRuleToPrint$Type[trm] == "continuous" ) {
      #       cat ( aRuleToPrint$Rule_Term.X[trm] , ' < ', aRuleToPrint$Rule_Term.attribute[trm], ' <= ',aRuleToPrint$Rule_Term.Y[trm])
      #       
      #    } else if (aRuleToPrint$Type[trm] == "categorical"  ) {
      #       
      #       cat (  aRuleToPrint$Rule_Term.attribute[trm], ' = ',aRuleToPrint$Rule_Term.Y[trm])
      #       
      #    }
      #    
      #    if (trm != nrow (aRuleToPrint) ){
      #       cat (' AND ')
      #    } else {
      #       cat (' THEN class = ', aRuleToPrint$Class[trm], '\n')
      #       
      #    }
      # }
   }    # for ( ruleIndx in 1: rulesNumber)  
   return( "average_Rule_Length" = totalNumOfRuleTerms / rulesNumber)
   
}    # END of displayRules function   


# @@@@@@@  ------------------------------------------ @@@@@@@%
# @@@@@@@               Prediction Section               @@@@@@@% 
# @@@@@@@  ------------------------------------------ @@@@@@@%


GRulesIQR.prediction <- function (trainD , testD , rulesLib.R)    {
  
   cat ( '\n')
   print ( "Prediction Stage ...")
  
   #determine the majority class within the the training data to predict the unclassified instance
   
   PredictedClassByMajority <-  names (which.max(table(trainD$class)))

   # create a data frame which contains all the classified instances (Test Data) 
   
   numTestExamples <- nrow(testD)
   testDataRowNames <- row.names(testD)
   
   factorsLevels <- levels(as.factor(testD$class))
   
   # Create another spark file of the TestData in order to be updated in the validation part.
   
   allClassifiedInstances <- f.initialClassifiedInstancesTable( factorsLevels)
   
   
   # prepare the column of the class and the predicted class in the previous table to be able to save factors not a string
   # allClassifiedInstances$class <- factor(allClassifiedInstances$class, 
   #                                        levels = factorsLevels)
   # 
   # allClassifiedInstances$Predicted_Class <- factor(allClassifiedInstances$Predicted_Class,  
   #                                                  levels = factorsLevels )
   
   
   # counters initialisation
   classifiedByRules <- 0
   correctlyClassifiedByRules <- 0        # (computed manually)    = True Positive Examples
   wronglyClassifiedByRules <- 0        #  (computed manually)  = False Positive Examples 
   
   unClassifiedInstances <- 0
   
   correctlyClassifiedByMajority <- 0
   wronglyClassifiedByMajority <- 0
   totalExmpCorrectlyClassified <- 0
   
   
   # create a list of rules that are used to classifiy the instances in order to calculate the accuracy of each one
   usedRulesList <- data.frame((matrix(nrow=1,ncol= 4)))   # 4th method to create the used Rules list
   
   # assign the names to the headers of the data frame
   names(usedRulesList) <- c("Rules", "Rule_ID", "Used_Times", "Correctly_used" )   
   
   # first column in the data frame is a list of rules
   usedRulesList$Rules <- as.list(usedRulesList$Rules)
   
   # second column in the data frame is a numeric variable
   usedRulesList$Rule_ID <-  as.numeric(usedRulesList$Rule_ID) 
   usedRulesList$Rule_ID <- 0   
   
   usedRulesList$Used_Times <-  as.numeric (usedRulesList$Used_Times) 
   usedRulesList$Used_Times <- 0
   
   usedRulesList$Correctly_used <- as.numeric(usedRulesList$Correctly_used)
   usedRulesList$Correctly_used <- 0  
   
   usedIndx <- 0
   
   rulesNumber <- length(rulesLib.R)
   
   if ( is.null(names(rulesLib.R))) {
      
      names(rulesLib.R) <- 1: rulesNumber
      

   } 
   rulesLib.RNames <- names(rulesLib.R) 
      

   
   # loop throught each instance in the test data
   for   ( anExample in testDataRowNames)  {     # for 1

      currentInstance <- testD [ anExample,]
      
      rulescoveredCurrentInstance <- list ("Rule_ID" = 0 , "Rule" = list (NA))
      ruleCovIns <- 0
      
      
      # check if there are any Rules that cover the instance.
    #  for  (ruleID in 1: rulesNumber) {     # for 2
      
      for  (ruleID in rulesLib.RNames) {     # for 2
         
         aRule <- rulesLib.R [[as.name (ruleID)]]
         aRuleNumTerms <- nrow(aRule)
         aRuleClass <- aRule$Class[1]
         noOfTimesRuleUsed <- 0
         matchedTerms <- 0
         
         # loop throught all the terms of the current Rule
         for ( aTerm in  1:aRuleNumTerms)  {    # for 3
            
            # termAttributeName <- aRule$Attribute[aTerm]
            attrValToBeCheck <- dplyr::select(currentInstance , aRule$Rule_Term.attribute[aTerm]) %>% pull()
            if ( is.numeric( attrValToBeCheck )) {
               if ( aRule$Type[aTerm] == "continuous") 
                  if (attrValToBeCheck > as.numeric(aRule$Rule_Term.X[aTerm]) & 
                      attrValToBeCheck <= as.numeric(aRule$Rule_Term.Y[aTerm]) ){
                     matchedTerms <- matchedTerms +1 
                  } else break()
               
            } else {
               
               if ( aRule$Type[aTerm] == "categorical") {
                  if (attrValToBeCheck == aRule$Rule_Term.Y[aTerm] )  {
                     matchedTerms <- matchedTerms +1
                  } else break() 
                  
                  
               }
            }
            
         }     # for 3
         
         
         #cat ( '\n ', matchedTerms, ' term(s) out of ', aRuleNumTerms, ' term(s) is/are matching \n')
         #  collect all the rules that have been found covered the current instance in a list
         
         if (matchedTerms == aRuleNumTerms) {
          
            ruleCovIns <- ruleCovIns + 1 
            
            rulescoveredCurrentInstance$Rule_ID[[ruleCovIns]] <- ruleID
            rulescoveredCurrentInstance$Rule[[ruleCovIns]] <- aRule
            
            
            break()
            
         } 
         
      }   #  for 2  
      
      
       cat ('\n Current Example : ', anExample ,'\n' )
       
       if (usingCrossValidation)  cat ('CV: ', cv , ' , ' )
       if ( buildEnsembleModel  ) {    
          cat ('BC: ', BC) 
         if (bestClassifiersSelected) cat (' , TopBC: ', topBC) 
       }  
       
       
       #print (currentInstance)
      # 
      # cat ('\n  List of  Rules covered this instance : \n ')
      # print ( rulescoveredCurrentInstance)
      
      instanceCoveredByRules <- TRUE    # Default value
      
      if ( rulescoveredCurrentInstance$Rule_ID[[1]] == 0 ) instanceCoveredByRules <- FALSE
      
      #  In G-Prism, if there is more than one rule covered the current instance, choose the the one that has the highest frequency
      numOfRuleCoveredInstance <-  length(rulescoveredCurrentInstance)
      
      # the following condition will be TRUE if there is one or more rules covered the instances
      if ( instanceCoveredByRules ) {    
         
         #    cat (' \n the first matching rule occured in the previous list will be chosen \n')
         firstMatchedRule <- rulescoveredCurrentInstance$Rule[[1]]
         firstMatchedRuleID <- rulescoveredCurrentInstance$Rule_ID[1]
         predictedClass <- firstMatchedRule$Class[1]
         classifiedByRules <- classifiedByRules + 1
         
         # firstMatchedRule <- rulescoveredCurrentInstance [1]
         # predictedClass <- firstMatchedRule[[1]]$Class[1]
         # classifiedByRules <- classifiedByRules + 1
         
         # create usedRuleList to update the number of the times that the Rule used  
         
         if (is.na (usedRulesList$Rules[1]))  {
            
            
            
            usedRulesList$Rules[1][[1]] <- firstMatchedRule
            usedRulesList$Rule_ID[1] <- firstMatchedRuleID
            usedRulesList$Used_Times[1] <- 1
            foundIt <- FALSE
            usedIndx <- usedIndx + 1
            thisRuleIndx <- usedIndx  # this counter used to update the remaining Rule details later
            
         } else {
            # looping through the used Rules list to find if a given rule is already added to the rules list or not?
            sizeUsedRulesList <- nrow(usedRulesList)
            
            foundIt <- FALSE
            for (ruleExistIndx in 1:sizeUsedRulesList ){
               aRuleUsed <- usedRulesList$Rules[[ruleExistIndx]]
               
               if ( isTRUE(all.equal(aRuleUsed, firstMatchedRule)) ){
                  foundIt <- TRUE
                  break ()
                  
               } 
               
            }    # for (ruleExistIndx)
            
            if (foundIt) {
               #   cat ( '\n The matching rule is already used ', 
               #        usedRulesList$`Used Times`[ruleExistIndx], ' times so far \n' )
               
               usedRulesList$Used_Times[ruleExistIndx] <- usedRulesList$Used_Times[ruleExistIndx] + 1
               
               thisRuleIndx <- ruleExistIndx # this counter used to update the remaining Rule details later
               
            } else {
               
               
               #    cat ( '\n The matching rule is used for the first time \n')
               
               usedRuleToAdd <- as.data.frame( cbind('Rules' = rulescoveredCurrentInstance$Rule[1], 'Rule_ID' = firstMatchedRuleID ,'Used_Times'= 1,
                                                     "Correctly_used" = 0))
               usedRuleToAdd$Used_Times <- as.numeric(usedRuleToAdd$Used_Times)
               usedRuleToAdd$Correctly_used <- as.numeric(usedRuleToAdd$Correctly_used)
               
               usedRulesList <- rbind(usedRulesList,usedRuleToAdd)  
               usedIndx <- usedIndx + 1 
               thisRuleIndx <- usedIndx
               
            }
            
         }

         validatePrediction <- f.checkPredictions ( predClass = predictedClass , 
                                                    realClass = currentInstance$class   )
         
         if ( validatePrediction == TRUE) {
            usedRulesList$Correctly_used[thisRuleIndx] <- usedRulesList$Correctly_used[thisRuleIndx] + 1
            correctlyClassifiedByRules <- correctlyClassifiedByRules + 1
            totalExmpCorrectlyClassified <- totalExmpCorrectlyClassified + 1
            
         } else wronglyClassifiedByRules <- wronglyClassifiedByRules + 1
         
         
         classifiedInstanceToAdd <- f.initialClassifiedInstancesTable(factorsLevels)
         classifiedInstanceToAdd$id <- anExample
         classifiedInstanceToAdd$class <- currentInstance$class
         classifiedInstanceToAdd$Predicted_Class <- predictedClass
         classifiedInstanceToAdd$Classification_Type <- "Rules"
         classifiedInstanceToAdd$Rule_ID <- firstMatchedRuleID
         # classifiedInstanceToAdd <- as.data.frame(cbind ("id" = anExample, "class" = currentInstance$class ,
         #                                                 "Predicted_Class" = predictedClass , "Classification_Type" = "Rules",
         #                                                 "Rule_ID" = firstMatchedRuleID ))
         # 
         allClassifiedInstances <- f.updatePredictionsTable (classifiedInstanceToAdd, allClassifiedInstances)
         
         
         #  else part is excuting when there is no Rule covered the current instance (unclassified Examples)
      } else {     

         unClassifiedInstances <- unClassifiedInstances + 1 
         
         classifiedInstanceToAdd <- f.initialClassifiedInstancesTable(factorsLevels)
         classifiedInstanceToAdd$id <- anExample
         classifiedInstanceToAdd$class <- currentInstance$class
         classifiedInstanceToAdd$Predicted_Class <- PredictedClassByMajority
         classifiedInstanceToAdd$Classification_Type <- "Majority Class"
         classifiedInstanceToAdd$Rule_ID <- NA
         # classifiedInstanceToAdd <- as.data.frame(cbind ("id" = anExample, "class" = currentInstance$class ,
         #                                                 "Predicted_Class" = PredictedClassByMajority , "Classification_Type" = "Majority Class",
         #                                                 "Rule_ID" = NA ))
         
         allClassifiedInstances <- f.updatePredictionsTable (classifiedInstanceToAdd, allClassifiedInstances)
         
         
         validatePrediction <- f.checkPredictions ( predClass = PredictedClassByMajority , 
                                                    realClass = currentInstance$class   )
         if ( validatePrediction == TRUE)  {
            
            correctlyClassifiedByMajority <- correctlyClassifiedByMajority + 1
            totalExmpCorrectlyClassified <- totalExmpCorrectlyClassified + 1
            
         } else  wronglyClassifiedByMajority <- wronglyClassifiedByMajority + 1
         
         
      }
      
     
   }    # for 1

   print ( "Predictions for Testing dataset is completed .. ")
   predictionCounters <- list ( "total_Exam_Correct_Classified" = totalExmpCorrectlyClassified ,
                                "classified_by_Rules" = classifiedByRules ,
                                "correctly_classified_byRules" = correctlyClassifiedByRules ,
                                "wrongly_classified_byRules" = wronglyClassifiedByRules ,
                                "correctly_classified_byMajority" = correctlyClassifiedByMajority ,
                                "wrongly_classified_byMajority" = wronglyClassifiedByMajority ,
                                "unclassified_Exam" = unClassifiedInstances )
   return ( list( 
      "used_Rules_List"=  usedRulesList , 
      "all_Classified_Instancese" = allClassifiedInstances, 
      "prediction_Counters" =  predictionCounters
   ) )
   
   
   
}      # END of GRulesIQR.prediction function



# @@@@@@@  ------------------------------------------ @@@@@@@%
# @@@@@@@               Prediction Results               @@@@@@@% 
# @@@@@@@  ------------------------------------------ @@@@@@@%

f.GenerateConfusionMatrix <- function (usedRulesList, allClassifiedInstances, testD )  {
   
  
   # Compute the accuracy of each used Rules. 
   
   usedRulesList <- usedRulesList %>% mutate( Rules_Accuracy = Correctly_used / Used_Times  )
   print ("Computing the accuracy of all used Rules is completed ...")
   
   print ("Generating Confusion Matrcies in progress ...")
   
   # Calculate the total Accuracy for the classifier
   totalClassifiedInstances <- nrow(allClassifiedInstances)
   wrongPredictions <-0
   correctPredictions <- 0
   
   factorsLevels <- levels(as.factor(testD$class))
   
   # INFO NOTE: Columns values used in confusion matrix should be factors .
   
   allClassifiedInstances$Predicted_Class <- factor(allClassifiedInstances$Predicted_Class, levels = factorsLevels)
   allClassifiedInstances$class <- factor(allClassifiedInstances$class , levels =  factorsLevels)
   
   # Precision, recall and F1 score are useful when the class labels are not uniformly distributed (e.g. most instances belong to one class).
   # In such cases, accuracy could be misleading as one could predict the dominant class most of the time and still achieve a relatively high 
   # overall accuracy but very low precision or recall for other classes.
 
   allConfusionMat <- confusionMatrix( allClassifiedInstances$Predicted_Class,
                                       allClassifiedInstances$class,  mode = "everything"  )
   
   # allConfusionMat$byClass[(is.na(allConfusionMat$byClass))] <- 0
   
   
   # confusion matrix for the predictions that based on Rules only 
   byRulesClassifiedInstances <- allClassifiedInstances %>% dplyr::filter(Classification_Type == "Rules") 
   byRulesConfusionMat <-  confusionMatrix ( byRulesClassifiedInstances$Predicted_Class, byRulesClassifiedInstances$class,  mode = "everything")
   
   # byRulesConfusionMat$byClass[(is.na(byRulesConfusionMat$byClass))] <- 0
   
   
   
   print ("Generating Confusion Matrcies is completed  ...")
   
   return(list ("Global_Confusion_Matrix" = allConfusionMat , 
                "ByRules_Confusion_Matrix" = byRulesConfusionMat ,
                "used_Rules_List" = usedRulesList [ , 2:5]))
   
}   # End of f.GenerateConfusionMatrix function 

f.displayResults <- function ( buildEM , finalResults,  trainDataSize, testDataSize , rulesLibrarySize,  
                               averageRuleLength ,  predictionCounters , allClassifiedInstances,
                               allConfusionMat , byRulesConfusionMat , baseClNo , applyConso)  {
   

   # Important Note: some of the following calculations are calculated using two different ways! (as a double check to my final results )
  
   totalExmpCorrectlyClassified <- predictionCounters$total_Exam_Correct_Classified
   classifiedByRules <- predictionCounters$classified_by_Rules
   correctlyClassifiedByRules <-  predictionCounters$correctly_classified_byRules
   wronglyClassifiedByRules <- predictionCounters$wrongly_classified_byRules
   correctlyClassifiedByMajority <-  predictionCounters$correctly_classified_byMajority
   wronglyClassifiedByMajority <- predictionCounters$wrongly_classified_byMajority
   unClassifiedInstances <- predictionCounters$unclassified_Exam
   
   if (numOfClasses  > 2)  {
      
      allConfusionMat$byClass <- as.data.frame( allConfusionMat$byClass)
      byRulesConfusionMat$byClass <- as.data.frame( byRulesConfusionMat$byClass)
      all_Average_Value <- array()
      byRules_Average_Value <- array()
      
      for ( conMat in 1:length(allConfusionMat$byClass) ) {
         all_Average_Value [conMat] <- mean (allConfusionMat$byClass[ ,conMat] ,  na.rm = TRUE)
         byRules_Average_Value [conMat] <- mean(byRulesConfusionMat$byClass [ , conMat] , na.rm = TRUE)
         
      }
      allConfusionMat$byClass <- rbind( allConfusionMat$byClass , all_Average_Value)
      byRulesConfusionMat$byClass <- rbind(byRulesConfusionMat$byClass , byRules_Average_Value)
      rownames(allConfusionMat$byClass)[nrow(allConfusionMat$byClass)] <-   "Average_Value"
      rownames(byRulesConfusionMat$byClass)[nrow(byRulesConfusionMat$byClass)] <-   "Average_Value"
      
      
      # Extract Recall and Precision values for all the classes from the confusion matrix
      # Note that sensistivity = Recall and Pos Pred Value = Precision 
      
      all_Recall <-  allConfusionMat$byClass["Average_Value",]$Recall
      all_Precision <- allConfusionMat$byClass["Average_Value",]$Precision 
      all_F1score <- allConfusionMat$byClass["Average_Value",]$F1
      
      
      byRules_Recall <- byRulesConfusionMat$byClass["Average_Value",]$Recall  # Sensitivity = Recall
      byRules_Precision  <- byRulesConfusionMat$byClass["Average_Value",]$Precision     #Pos Pred Value = Precision
      byRules_F1score <- byRulesConfusionMat$byClass["Average_Value",]$F1
      
      
      
   }  else {
      all_Recall <- allConfusionMat$byClass [[6]]      
      all_Precision <- allConfusionMat$byClass [[5]]
      all_F1score <-  allConfusionMat$byClass [[7]] 
      
      byRules_Recall <- byRulesConfusionMat$byClass[[1]]       #  Sensitivity = Recall
      byRules_Precision  <- byRulesConfusionMat$byClass[[3]]     #   Pos Pred Value = Precision
      byRules_F1score <- byRulesConfusionMat$byClass [[7]]
      
      
   }
   
   
   # Important Note: some of the following calculations are calculated using two different ways for validation purposes!
   
   # calculate F1 Score which is defined as the harmonic mean (or a weighted average) of precision and recall.
   1 / ((1/byRules_Recall + 1/byRules_Precision) / 2)
   
   
   totalClassifiedInstances <- nrow ( allClassifiedInstances)
   
   # NOTE: ( correctlyClassifiedByRules)  is an incremental counter calculated within the loop of prediction strage.
   # The alternative way is counting the corrected predictions directly from byRulesClassificationTable
   
   byRulesClassificationCorrect <- allClassifiedInstances %>% dplyr::filter(Classification_Type == "Rules") %>% 
      dplyr::filter(Predicted_Class == class) %>% nrow()
   
   # NOTE: ( totalExmpCorrectlyClassified) is an incremental counter calculated within the loop of prediction stage.
   # The alternative way is counting the corrected predictions directly from allClassificationTable.
   correctPredictions <- allClassifiedInstances %>% dplyr::filter ( Predicted_Class == class ) %>% nrow()
   wrongPredictions <- allClassifiedInstances %>% dplyr::filter ( Predicted_Class != class ) %>% nrow()
   
   
   globalAccuracy <- totalExmpCorrectlyClassified / totalClassifiedInstances 
   TentativeAccuracy <- correctlyClassifiedByRules / classifiedByRules
   AbstainRate <- unClassifiedInstances / totalClassifiedInstances
   # stop clock
   end.time <- Sys.time()
   cat('\n Start time: ',  as.character(start.time))
   cat('\n End time: ',  as.character(end.time), '\n \n')
   time.taken <- round ( end.time - start.time , 2)
   #print (  "execution time :")
   print ( time.taken)
   
   
   cat ( ' \n Dataset: ', fileName, ' (', seedNo , ') \n'   )
   
   if ( buildEM & ! finalResults) {
      
      cat ( '\n ####### ReG-Rules Classifier Results with ' ,numBaseClassifiers ,
            ' base classifiers ####### \n' , 
            ' \n @@@@@@@@@@@@@@  Base Classifier No: ', baseClNo, '@@@@@@@@@@@@@@' ) 
      
   } else if (finalResults & ! applyConso ) {
      cat ( '\n ####### ReG-Rules Classifier Results with ' ,numBaseClassifiers ,
            ' base classifiers ####### \n' ,
            ' \n @@@@@@@@@@@@@@  Final Results @@@@@@@@@@@@@@ \n' ) 
      
      
   } else if ( finalResults & applyConso ) {
      
      cat ( '\n ####### CRC Classifier Results with ' ,numBaseClassifiers ,
            ' base classifiers ####### \n' ,
            ' \n @@@@@@@@@@@@@@  Final Results @@@@@@@@@@@@@@ \n' ) 
      
   } else{
      
      
      cat   ('\n ########################## Single G-Rules-IQR Classifier Results ######################## \n \n ') 
   }
   
   cat ( '\n ', (if_else(TransformToNormality == FALSE  , ' Without approximating to normal distribution \n\n', 'With approximating to normal distribution \n \n')))
   
   cat ( '\n Train Data: ', trainDataSize )
   cat (' \n' , (if_else(buildEM & !finalResults, 'Validation Data' , 'Test Data')) , totalClassifiedInstances , testDataSize)
   #cat ( '\n bounds limit: ', boundLimit , '\n')
   
   if ( toKeepTheRule > 0) {
      cat('\n NOTICE: a rule that is decided by majority label strategy should cover at least 60% of dataset \n')
   }
   
   cat (' \n Stopping criteria is used?', (if_else(stoppingCriteriaUsed , "Yes" , "No")) , '|  (Minimum Rule Accuracy = ', minimumRuleAccuracy *100, '%) \n' )
   cat ( '\n Limited Terms/Rule are used?', (if_else(limitedTermsUsed , "Yes" , "No" ))  , '| ', maxTerms , ' Terms/Rule') 
   cat ( '\n Rules with less coverage are removed to reduce overfitting in large datasets?', (if_else(rulesDeletion , "Yes" , "No" )) )
   cat ( '\n *****************************************************************')
   
   if ( finalResults & ! applyConso ) {
      cat ( '\n Average number of Rules within the top classifiers before merging: '  ,  rulesLibrarySize$rules_Avg_Num , '\n')
      cat ('\n Average number of Rules within the top classifiers after merging: ', rulesLibrarySize$merged_Rules_Avg_Num , '\n')
      
      
   } else if ( finalResults & applyConso ) {
      cat ( '\n Number of consolidated rules: ', rulesLibrarySize)
      cat ( '\n Number of rules without consolidation: ' , sum (sapply (topBC.RulesBank$BC_Rules , length ) ) )
      
      
   }  else cat ( '\n Number of rules: ', rulesLibrarySize)
   
   cat ( '\n average of terms/rule: ', averageRuleLength)
   
   cat (' \n Total number of examples covered by Rules =  ', classifiedByRules )
   cat (' \n Number of examples correctly covered by Rules (True Positive) = \n               ', correctlyClassifiedByRules ,
        ' (manually) ' , byRulesClassificationCorrect , ' (using the by_Rules_Classification_Table)')
   cat (' \n Number of examples wrongly covered by Rules (False Positive) =', wronglyClassifiedByRules, '\n')
   
   cat ( '\n Number of unclassified examples (Majority Class strategy) = ', unClassifiedInstances)
   cat (' \n Correctly classified by majority labelling = ', correctlyClassifiedByMajority)
   cat (' \n Wrongly classified by majority labelling = ', wronglyClassifiedByMajority)
   
   cat ('\n \n  Abstaining Rate = ', AbstainRate )
   
   
   # cat ('\n ', (classifiedByRules / totalClassifiedInstances ) , 
   #      ' of the examples are covered by the Rules')      #  similar to Rules Coverage 
   
   cat('\n \n Correct Predictions =  ', totalExmpCorrectlyClassified , ' (incremental counter) ' 
       , correctPredictions , ' (using all_Classification_Table)' )
   cat('\n Wrong Predictions =  ', wrongPredictions , '\n' )
   
   cat ('\n (Rules Coverage) = ', 
        (classifiedByRules / totalClassifiedInstances) *100 , '%')
   
   # cat (' \n False positive examples examples (Consistency ) = ' , 
   #      (falsePositiveExamples / classifiedByRules) *100 )
   # 
   # recall_measure <- truePositiveExamples / correctPredictions
   # cat ( '\n Covered positive examples among all positive examples (Recall)= ', 
   #       (truePositiveExamples / correctPredictions ) * 100)
   # 
   # cat ('\n Covered positive examples among all the examples (Support) =', 
   #      (truePositiveExamples/ totalClassifiedInstances) *100)
   
   
   #cat ( ' \n\n general precision = ',  truePositiveExamples / (truePositiveExamples + falsePositiveExamples))
   
   cat ( ' \n \n  ********** Results Computed from the Confusion Matrix  ************ '  )
   
   cat ( ' \n General Recall  = ', all_Recall ,  '   |   Rules Recall = ' , byRules_Recall)
   
   cat ( ' \n General Precision  = ' , all_Precision, '   |   Rules Precision = ',  byRules_Precision )
   
   cat ( ' \n General F1 Score value  = ', all_F1score , '   |   Rules F1 Score = ', byRules_F1score )
   
   cat ( ' \n *****************************************************************')
   
   cat('\n General Accuracy (%) = ', globalAccuracy, ' (from incremental Counter)\n                        ' , 
       allConfusionMat$overall[[1]] , '  ( from the all confusion matrix )' )
   
   cat('\n \n Tentative Accuracy = ', TentativeAccuracy, ' ( from incremental Counter)\n                     ',
       byRulesConfusionMat$overall[[1]], ' ( from Rules confusion matrix )')
   
   cat ( ' \n ---------------------------------------------------------------------------------------')
   
   return( list (  "Abstaining_Rate" = AbstainRate ,
                   "General_Accuracy" = globalAccuracy ,
                   "Tentative_Accuray" = TentativeAccuracy ,
                   "Rules_Recall" = byRules_Recall ,
                   "Rules_Precision" = byRules_Precision ,
                   "Rules_F1Score" =  byRules_F1score
                   
   ))
}   # End of f.displayResults function 


# Ranking Models Function

f.RankingModels <- function(  EnsemMod , totalBCNum) {

   thisBestBaseClassifiers <-  EnsemMod [order ( - EnsemMod[ , 'Tentative_Accuracy'] , 
                                                 - EnsemMod [ , 'Average_Correctly_used' ] ,
                                                   EnsemMod [ , 'Abstain_Rate' ] ) ,]
   
   
   # thisBestBaseClassifiers <-  EnsemMod [order ( - EnsemMod[ , 'Tentative_Accuracy'] , - EnsemMod [ , 'General_Accuracy'] , EnsemMod [ , 'Abstain_Rate'] ,
   #                         EnsemMod [ , 'Number_of_Rules'] , EnsemMod [ , 'Average_Rule_Length'] , - EnsemMod [ , 'Number_of_UsedRules' ] ),]
   
   # choose the top best base classifiers ( 10% if Base classifiers more than 100 and 20% if BC less than 100)
   if ( totalBCNum < 100 ) {
      thisTopBestClassifiers <- thisBestBaseClassifiers [1:round (50 * nrow(thisBestBaseClassifiers) /100) ,]
      
   } else  thisTopBestClassifiers <- thisBestBaseClassifiers [1:round (20 * nrow(thisBestBaseClassifiers) /100) ,]
   
   return( thisTopBestClassifiers) 
   
   
}


# Reduce overfitting by removing rules with very little coverage 

f.removeOverfittedRules <- function (rulesLib.R) {
   
   rulesNumber <- length (rulesLib.R)
   
   rulesToRemoved <- c()
   
   totalNumOfRuleTerms <- 0
   
   for ( ruleIndx in 1: rulesNumber) {
      
      aRuleToPrint <- rulesLib.R[[ruleIndx]]
      
      RuleCoverage <- aRuleToPrint$RT_Frequency[nrow(aRuleToPrint)]
      
      if ( RuleCoverage < 20 ) {
         rulesToRemoved <- c( rulesToRemoved , ruleIndx)
      }
   }
   
   newRulesLib.R <- rulesLib.R [ - rulesToRemoved]
   return( newRulesLib.R )
   
}

#------------------------------------------------------------#

########################## MAIN BODY #########################

#------------------------------------------------------------#
cv <- 1

if ( usingCrossValidation) { 
   
   # cross validation strating here
    
   #Segement  data by fold using the which() function 
   testIndexes <- which(folds== cv , arr.ind=TRUE)
   testData <- myData [ testIndexes , ]
   trainData <- myData [ - testIndexes , ] 
   
   
   # Generate table to collect cross validation results
   crossValidationResults <- data.frame(matrix(data = NA , nrow = foldsNum + 1 , ncol = 8 ) , stringsAsFactors = FALSE)
   names (crossValidationResults) <- c ( "Folds" , "Number_of_Rules" , "Abstaining_Rate" , "Recall" ,"Precision", "F1Score",
                                "General_Accuracy" , "Tentative_Accuracy" )
   rownames (crossValidationResults)[foldsNum+1] <- "Average"
   
   cat ( '\n Cross validation fold : ', cv , '\n')
   
}


repeat{
   
   
if ( ! buildEnsembleModel ) {
   
####--------------------------- Single Stand alone classifier ---------------------------
   
   rulesLibrary.R <- GRulesIQR.classification (trainD =  trainData)
   
  ####################  Reduce overfitting by removing rules with very little coverage   
  #### this part never will be used for now #####
  if ( rulesDeletion  )  {
     beforeDeletionRulesNum <- length (rulesLibrary.R)
     newRulesLibrary.R <-  f.removeOverfittedRules (rulesLibrary.R)
     afterDeletionRulesNum <- length ( newRulesLibrary.R)
     rulesLibrary.R <- newRulesLibrary.R
     
  }
  
  ##################################   
  
   
   averageRuleLength <-  f.displayRules (rulesLib.R = rulesLibrary.R)
   
   validateCurrentClassifier <- GRulesIQR.prediction(  trainD =  trainData , 
                                                       testD =  testData ,
                                                       rulesLib.R = rulesLibrary.R)
   
   
   ConfusionMatrices <-  f.GenerateConfusionMatrix ( usedRulesList = validateCurrentClassifier$used_Rules_List ,
                                                     allClassifiedInstances = validateCurrentClassifier$`all_Classified_Instancese`,
                                                     testD = testData)
   
   
   
   currentPredictionResults <- f.displayResults ( buildEnsembleModel , finalEnsembleResults , trainDataSize = nrow(trainData) ,testDataSize =  nrow(testData),
                                                  rulesLibrarySize = length(rulesLibrary.R) , averageRuleLength = averageRuleLength,
                                                  predictionCounters = validateCurrentClassifier$prediction_Counters ,
                                                  allClassifiedInstances = validateCurrentClassifier$`all_Classified_Instancese`,
                                                  allConfusionMat = ConfusionMatrices$Global_Confusion_Matrix ,
                                                  byRulesConfusionMat = ConfusionMatrices$ByRules_Confusion_Matrix , 
                                                  baseClNo = BC , FALSE)
   
   
   # Highlight If statment if you want to apply merging to the single stand alone classifier  (G-Rules-IQR)
   if ( usingCrossValidation  ) { 
      
     
      crossValidationResults$Folds[cv] <- cv
      crossValidationResults$Number_of_Rules [cv] <- length(rulesLibrary.R)
      crossValidationResults$Abstaining_Rate[cv] <- currentPredictionResults$Abstaining_Rate
      crossValidationResults$Recall[cv] <- currentPredictionResults$Rules_Recall
      crossValidationResults$Precision[cv] <- currentPredictionResults$Rules_Precision
      crossValidationResults$F1Score[cv] <- currentPredictionResults$Rules_F1Score
      crossValidationResults$General_Accuracy[cv] <- currentPredictionResults$General_Accuracy
      crossValidationResults$Tentative_Accuracy[cv]<-currentPredictionResults$Tentative_Accuray
   
      
      if ( cv < foldsNum ) {
         cv <- cv + 1
         
         #Segement  data by fold using the which() function 
         testIndexes <- which(folds== cv , arr.ind=TRUE)
         testData <- myData [ testIndexes , ]
         trainData <- myData [ - testIndexes , ] 
         cat ( '\n Cross validation fold : ', cv , '\n')
         next()
         
      }else {
         cat ( '\n' , foldsNum ,  'folds cross validation results of G-Rules-IQR classifier for ', fileName , ' dataset \n \n' )
         crossValidationResults[ foldsNum + 1 ,'Folds'] <- 0 
         crossValidationResults[ foldsNum + 1 ,'Number_of_Rules'] <- mean(crossValidationResults[1:foldsNum,'Number_of_Rules']) 
         crossValidationResults[ foldsNum + 1 ,'Abstaining_Rate'] <- mean(crossValidationResults[1:foldsNum,'Abstaining_Rate']) 
         crossValidationResults[ foldsNum + 1 ,'Recall'] <- mean(crossValidationResults[1:foldsNum,'Recall']) 
         crossValidationResults[ foldsNum + 1 ,'Precision'] <- mean(crossValidationResults[1:foldsNum,'Precision']) 
         crossValidationResults[ foldsNum + 1 ,'F1Score'] <- mean(crossValidationResults[1:foldsNum,'F1Score']) 
         crossValidationResults[ foldsNum + 1 ,'General_Accuracy'] <- mean(crossValidationResults[1:foldsNum,'General_Accuracy']) 
         crossValidationResults[ foldsNum + 1 ,'Tentative_Accuracy'] <- mean(crossValidationResults[1:foldsNum,'Tentative_Accuracy'])
         
         print (crossValidationResults)
          
         break
      }
     
     
   } else  {
      
      stop()    # stop here to avoid merging rules for the single based classifier  ( produce G-Rules-IQR results)
      
   }
   
   stop()
  
   
   ####### Merging rules in single based rules : this part never excute until previous lines with stop and break functions are removed ####---
   
   
   rulesLibrarySizes <- sapply(rulesLibrary.R , nrow)
   
   # To save time, this step is targeting rules with single terms only and replacing the one before 
   rulesLibrarySingleTerm <- which ( rulesLibrarySizes == 1 )
   rulesLibraryMultiTerms <- which ( rulesLibrarySizes > 1)
   
   reListOfRulesTerms <- list( data.frame())
   
   mergedRulesNum <- c ()
   replacedRulesMultiTermsExist <- FALSE
   
   all_ReplacedRulesIDs <- data.frame(matrix ( data = NA , nrow = 1 , ncol = 2) ,
                                      stringsAsFactors = FALSE)
   names(all_ReplacedRulesIDs) <- c ("overlapRuleId_1" , "overlapRuleId_2" )
  
   
  
   ####### copied from merging ensemble  ###### ---
   
   # filteredTrainData <- trainData
   
   for (eachRuleMultiTerm in 1: length(rulesLibrary.R) ) { 
      
      if ( ! is.na ( categColNames [1] ))  break()
      
      if ( eachRuleMultiTerm %in% mergedRulesNum ) next()
      
      currentRuleMulti <- rulesLibrary.R [[eachRuleMultiTerm]]
      currentRuleMulti <- cbind(currentRuleMulti , RuleId = eachRuleMultiTerm)
      
      currentAttrNames <- currentRuleMulti$Rule_Term.attribute
      currentAttrClass <- currentRuleMulti$Class[1]
      
      CurrentRuleSize  <- nrow(currentRuleMulti)
      
      # the following initialisation is exactly same as replecedRanges table ! (think about use a function here)
      mergeCurrentTermValues <- data.frame(matrix(data = NA , nrow = 1 , ncol = 6 ) ,
                                           stringsAsFactors = FALSE)
      names(mergeCurrentTermValues) <- c ( "Rule_Term.X", "Rule_Term.attribute" , "Rule_Term.Y", "Class" , 
                                           "RT_Frequency" , "Probability" )
      mergeCurrentTermValues$Rule_Term.X <- as.numeric(mergeCurrentTermValues$Rule_Term.X )
      mergeCurrentTermValues$Rule_Term.attribute <- as.character(mergeCurrentTermValues$Rule_Term.attribute)
      mergeCurrentTermValues$Rule_Term.Y <- as.numeric(mergeCurrentTermValues$Rule_Term.Y)
      mergeCurrentTermValues$Class <- as.character(mergeCurrentTermValues$Class)
      # mergeCurrentTermValues$Discarded_Rules <- as.list (mergeCurrentTermValues$Discarded_Rules)
      mergeCurrentTermValues$RT_Frequency <- as.numeric(mergeCurrentTermValues$RT_Frequency)
      mergeCurrentTermValues$Probability <- as.numeric(mergeCurrentTermValues$Probability)
      
      overlappedRulesFound <- FALSE
  
      
      if ( all (currentRuleMulti$Type == "continuous")  & 
           length (rulesLibrarySizes[which (rulesLibrarySizes == CurrentRuleSize)]) > 1)  {
         rulesNumbersToBeChecked <- which(rulesLibrarySizes == CurrentRuleSize)  
         
         contRulesToBeMerged <- list(data.frame())
         ruleSeq <- 0
         
         currentPosition <- which(rulesNumbersToBeChecked == eachRuleMultiTerm)
         refinedRulesNumbersToBeChecked  <- rulesNumbersToBeChecked [ - (1: currentPosition)]
         
         # in case of having no rules left to compare with, i.e. it is the last rule in the rulesLibrary.R  
         if ( is.integer0( refinedRulesNumbersToBeChecked )) next()
         
         for ( anotherRuleNumber in refinedRulesNumbersToBeChecked ) {
            if ( anotherRuleNumber %in% mergedRulesNum )  next()
            ruleToCompare <- rulesLibrary.R [[anotherRuleNumber]]
            ruleToCompare <- cbind(ruleToCompare , RuleId = anotherRuleNumber)
            
            # if (isTRUE( all.equal( currentAttrNames , ruleToCompare$Rule_Term.attribute) ) &
            #     ( currentAttrClass == ruleToCompare$Class[1])) {
            
            if ( all (currentAttrNames %in% ruleToCompare$Rule_Term.attribute ) == TRUE &
                 ( currentAttrClass == ruleToCompare$Class[[1]])) {
               
               # A rule with same attribute names for the target class has been found
               replacedRulesMultiTermsExist <- TRUE
               
               if (is_empty(contRulesToBeMerged[[1]]) ) {
                  ruleSeq <- 1
                  
                  contRulesToBeMerged [[ruleSeq]]  <- currentRuleMulti
                  
               } 
               ruleSeq <- ruleSeq + 1
               
               contRulesToBeMerged [[ruleSeq]]  <- ruleToCompare
               
            }
         }       #    for ( anotherRuleNumber in  refinedRulesNumbersToBeChecked )
         if (is_empty(contRulesToBeMerged[[1]]) ) {
            # if ( replacedRulesMultiTermsExist == FALSE ) {
            
            #cat ('\n No Rules in the current libaray sharing same boundries (same attribute names) with the current rule No: ', eachRuleMultiTerm, '\n')
            
            
         } else  {    # current rule have overlapped rule-terms that can be merged together.
            # important note: all the terms in the current rule have to be overlapped in order to generate a new rule !
            # partially overlapping is not accepted 
            
            #  create a table of all the rules ids that appeared in contRulesToBeMerged table in order to track them within the following replacedRanges 
            #  table to insure a rules are overlapped in all its terms.
            
            
            ranges_XY  <- data.frame(matrix(data = NA , nrow = 1 , ncol = 4 ) ,
                                     stringsAsFactors = FALSE)
            names(ranges_XY) <- c ( "Attribute_Name" ,"RuleId", "Rule_Term.x" , "Rule_Term.Y" )
            
            
            replacedRanges <- data.frame(matrix(data = NA , nrow = 1 , ncol = 8 ) ,
                                         stringsAsFactors = FALSE)
            names(replacedRanges) <- c ( "Rule_Term.X", "Rule_Term.attribute" , "Rule_Term.Y", "Class" , 
                                         "RuleId_1" , "RuleId_2" , "RT_Frequency" , "Probability" )
            replacedRanges$Rule_Term.X <- as.numeric(replacedRanges$Rule_Term.X )
            replacedRanges$Rule_Term.attribute <- as.character(replacedRanges$Rule_Term.attribute)
            replacedRanges$Rule_Term.Y <- as.numeric(replacedRanges$Rule_Term.Y)
            replacedRanges$Class <- as.character(replacedRanges$Class)
            replacedRanges$RuleId_1 <- as.numeric(replacedRanges$RuleId_1 )
            replacedRanges$RuleId_2 <- as.numeric(replacedRanges$RuleId_2)
            replacedRanges$RT_Frequency <- as.numeric(replacedRanges$RT_Frequency)
            replacedRanges$Probability <- as.numeric(replacedRanges$Probability)
            
            overlappedRulesID <- c ()
            rulesRemovedFromMerging <- c ()
            
            for ( mrgAtt in currentAttrNames) {
               for ( mrgR in 1 : length(contRulesToBeMerged)) {
                  currentTermToMerge <- as.data.frame(contRulesToBeMerged[mrgR])
                  
                  if ( currentTermToMerge$RuleId [1] %in% rulesRemovedFromMerging ) {
                    # cat ( '\n ', currentTermToMerge$RuleId [1] , ' are already eliminated from testing')
                     next()
                  }
                  
                  
                  if (is.na (ranges_XY [1,1])) {
                     myRange <- 1
                  } else {
                     
                     myRange <- myRange + 1
                  }
                  ranges_XY [myRange, 1] <- mrgAtt
                  ranges_XY [myRange, 2] <- currentTermToMerge$RuleId [1]
                  ranges_XY [myRange, 3] <- currentTermToMerge [ which(currentTermToMerge$Rule_Term.attribute == mrgAtt) ,][[2]]
                  ranges_XY [myRange, 4] <- currentTermToMerge [ which(currentTermToMerge$Rule_Term.attribute == mrgAtt) ,][[4]]
                  thisRange <-  c ( ranges_XY [myRange, 3] , ranges_XY [myRange, 4] )
                  
                  if ( mrgR > 1 ) {
                     
                     # in case of having more than two overlapped ranges, previous range will be copied from the replaced ranges instead of ranges_XY
                     
                     previousRange <-  c ( ranges_XY [myRange - 1, 3] , ranges_XY [myRange - 1, 4])
                     
                     if ( thisRange %overlaps% previousRange ) {
                        
                        if (is.na (replacedRanges [1,1])) {
                           repRange <- 1
                        } else {
                           
                           repRange <- repRange + 1
                        }
                        
                        replacedRanges [repRange , 1] <- min( thisRange , previousRange)
                        replacedRanges [repRange , 2] <-    mrgAtt
                        replacedRanges [repRange , 3] <-  max(thisRange , previousRange)
                        replacedRanges [repRange , 4] <- currentAttrClass
                        replacedRanges [repRange , 5] <- ranges_XY [myRange - 1 , 2]
                        replacedRanges [repRange , 6] <- ranges_XY [myRange , 2]
                        
                        overlappedRulesID <- c ( overlappedRulesID , ranges_XY [myRange - 1 , 2] , ranges_XY [myRange , 2] )
                        
                        if ( is.na (all_ReplacedRulesIDs [1,1]) ) {
                           allIDs <- 1
                        } else {
                           allIDs <- allIDs + 1
                        }
                        

                        all_ReplacedRulesIDs[allIDs , 1] <- ranges_XY [myRange - 1 , 2]
                        all_ReplacedRulesIDs[allIDs , 2] <- ranges_XY [myRange , 2] 
              
                         
                     }   else  {  
                        
                        #cat ( '\n Rule No :' , currentTermToMerge$RuleId [1] , ' will be removed from current merging of Rules ' )
                        
                        # in case of failing the overlapping test for the current rule term , all the rule will be discarded from merging process
                        rulesRemovedFromMerging <- c (  rulesRemovedFromMerging , currentTermToMerge$RuleId [1])
                        
                        # ThisRange value did not passed the overlapping test with the previous range ; so it will be removed from range_XY table before the 
                        # next iteration to avoid wrongly considered as a previous range then.!
                        ranges_XY <- ranges_XY [ - myRange , ]
                        myRange <- myRange - 1   # decrease the counter of the table because the last element have been removed 
                        
                     }
                     
                  }     #    if ( mrgR > 1 )
                  
                  
               }    #    for ( mrgR in 1 : length(contRulesToBeMerged)) 
               
            }     #     for ( mrgAtt in currentAttrNames)
            
            
            # rules are considered to be replaced only if all the ranges in replaced ranges table are overlapped each other .
            # if ( length (attributesList)  == length(currentAttrNames) ) {
            
            attributesList <- unique(replacedRanges$Rule_Term.attribute) 
            
            # if ( isTRUE( all.equal( currentAttrNames , attributesList ) ) ) {
            
            if ( all (currentAttrNames %in% attributesList ) == TRUE) {
               overlappedRulesFound <- TRUE
               
               sizeReplacedRanges <- nrow (replacedRanges)
               
               # a new rule have been generated using the replaced terms.
               
               
               for ( overlappedTermName in  1: length( attributesList) ) { 
                  byName  <- replacedRanges [ which (replacedRanges$Rule_Term.attribute == attributesList[overlappedTermName] ), ] 
                  
                  mergeCurrentTermValues[ overlappedTermName, 1] <- min(byName$Rule_Term.X)
                  mergeCurrentTermValues[ overlappedTermName, 2] <- byName$Rule_Term.attribute[1]
                  mergeCurrentTermValues[ overlappedTermName, 3] <- max(byName$Rule_Term.Y)
                  mergeCurrentTermValues[ overlappedTermName, 4] <- byName$Class[1]
                  # mergeCurrentTermValues[ overlappedTermName, 5] <- byName$RuleId_1
                  # mergeCurrentTermValues[ overlappedTermName, 6] <- byName$RuleId_2
                  mergeCurrentTermValues[ overlappedTermName, 5] <- NA
                  mergeCurrentTermValues[ overlappedTermName, 6] <- NA
                  
                  
                  reGenerate_aRuleTerm <- list( "x" = mergeCurrentTermValues[ overlappedTermName, 1] , 
                                                "attribute" = mergeCurrentTermValues[ overlappedTermName, 2], 
                                                "y" = mergeCurrentTermValues[ overlappedTermName, 3] ) 
                  
              
                  # compute the frequncy and the probability of each term in the new rule.
                  
                  if ( length(currentAttrNames)  > 1 ) {
                     
                     if (overlappedTermName == 1) {
                        reCalculateRuleTermProb <- f.calculateRuleTermProbability (dataD =  trainData ,
                                                                                   thisClass =  mergeCurrentTermValues[ overlappedTermName, 4],
                                                                                   thisRuleTerm = reGenerate_aRuleTerm ,
                                                                                   thisAttrType =  "continuous",
                                                                                   reGenerateRT = TRUE)
                        
                     } else  {  # means that this is not the first term in the the current rule. dataset that produced form the 
                        # previous step is going to be reused to test the next term in our current new generated rule
                        reCalculateRuleTermProb <- f.calculateRuleTermProbability (dataD =  reCalculateRuleTermProb$outRefinedAttr ,
                                                                                   thisClass =  mergeCurrentTermValues[ overlappedTermName, 4],
                                                                                   thisRuleTerm = reGenerate_aRuleTerm ,
                                                                                   thisAttrType =  "continuous",
                                                                                   reGenerateRT = TRUE)
                        
                     }    # overlappedTerm > 1
                     
                     
                  } else {
                     reCalculateRuleTermProb <- f.calculateRuleTermProbability (dataD =  trainData ,
                                                                                thisClass =  mergeCurrentTermValues[ overlappedTermName, 4],
                                                                                thisRuleTerm = reGenerate_aRuleTerm ,
                                                                                thisAttrType =  "continuous",
                                                                                reGenerateRT = TRUE)
                  }
                  # Update the RT_Frequency and Probabolity columns in the list of the rules according to the above recalculation step
                  mergeCurrentTermValues[ overlappedTermName, 5] <- reCalculateRuleTermProb$outRuleTermFrequency
                  mergeCurrentTermValues[ overlappedTermName, 6] <- reCalculateRuleTermProb$outRuleTermProbability
                  
                  
               }     #    for ( overlappedTerm in  1: attributesLength ) 
               
               
            }  else   overlappedRulesFound <- FALSE   # if ( attributesLength  == length(currentAttrNames) ) 
            
            
         }     # else if ( replacedRulesMultiTermsExist == TRUE )
         
      } else next()     #   not enough similarities in the rule-terms of the current rule to be checked for merging process
      
      # if ( is.na (replacedRanges [1,1]))  { 
      if ( overlappedRulesFound  == FALSE )  { 
         # cat ( '\n Rules list in classifier No ' ,topClassifierNum , ' with multiple terms does exist but no matching found for current rule No ' , eachRuleMultiTerm , ' so no merging process happened here \n')
         replacedRulesMultiTermsExist <- FALSE
         
      }
      
      if ( replacedRulesMultiTermsExist == FALSE) {
         
         next()
         # same as in lines starting in 3240
         # no action will happen here !  go to next iteration and move the current rules library to the rules bank without any modifications
      }
      
      # before going to the next multi-Terms-rule wintin the current rulesLibrary.R , use reListOfRulesTerms table to copy all the overlapped ranges 
      # which successfully passed the investigations above and reproduced a new merged rule   
      
      if ( overlappedRulesFound == TRUE )  {
         if (  is_empty (reListOfRulesTerms [[1]] ) ) {
            reLsTerms <- 1
            
         } else reLsTerms <- reLsTerms +  1
         
         reListOfRulesTerms [[ reLsTerms ]] <-  mergeCurrentTermValues
         
         # create a mergedRules list to avoid retest the merged rules more than once.
         
         mergedRulesNum <- c ( mergedRulesNum , unique (overlappedRulesID) )
         
      }

      # toremove <-  row.names(reCalculateRuleTermProb$outRefinedData)
      # 
      # coveredData <- filteredTrainData[toremove ,]
      # 
      # classFilter <- filteredTrainData [ filteredTrainData$class == mergeCurrentTermValues$Class ,] 
      # ruleFilter <- classFilter [ which (rownames(classFilter) %in% rownames(coveredData) == FALSE),]
      # 
      # filteredTrainData <- ruleFilter
      
   }    #  for (eachRuleMultiTerm in length(rulesLibrary.R) )
   
   
   # make reListOfRulesTerms in a structure of the original rulesLibrary.R
   
   
   replacedRulesSize <- length (reListOfRulesTerms)
   
   for ( lis in 1: replacedRulesSize )   {
      
      currentReListOfRulesTerms <- reListOfRulesTerms[[lis]]
      
      # currentReListOfRulesTerms <- currentReListOfRulesTerms [-5]
      currentReListOfRulesTerms <- mutate ( currentReListOfRulesTerms ,  Type = "continuous" )
      currentReListOfRulesTerms <- currentReListOfRulesTerms %>% select( Type , everything())
      
      
      reListOfRulesTerms[[lis]] <- currentReListOfRulesTerms
      
   }
   
   
   # create a new rulesLibrary for the current topBC which includes the merged rules .
   mergedRules <- list()
   for ( newRule in 1: length(reListOfRulesTerms)) {
      mergedRules [[newRule]] <- reListOfRulesTerms [[newRule]]
      
   }   
   
   mergedRules <- append(mergedRules , rulesLibrary.R [ - mergedRulesNum] , after = replacedRulesSize)
   
   
   averageMergeRuleLength <-  f.displayRules (rulesLib.R = mergedRules)
   
   # Test the new merged rules before considering any changes to rulesBank list
   reValidateCurrentClassifier <-  GRulesIQR.prediction ( trainD = trainData,
                                                      testD =  testData , 
                                                      rulesLib.R = mergedRules)
   
   
   reCalculateUsedRulesListSize <- nrow(reValidateCurrentClassifier$used_Rules_List)
   reCalculateAverageCorrectlyUsedRules <- sum (reValidateCurrentClassifier$used_Rules_List$Correctly_used) / reCalculateUsedRulesListSize
   
   reConfusionMatrices  <- f.GenerateConfusionMatrix ( usedRulesList = reValidateCurrentClassifier$used_Rules_List ,
                                                       allClassifiedInstances = reValidateCurrentClassifier$all_Classified_Instancese ,
                                                       testD = testData )
   
   
   
   reCurrentPredictionResults <- f.displayResults ( buildEM = buildEnsembleModel , finalEnsembleResults ,trainDataSize = nrow(trainData) , 
                                                      testDataSize = nrow(testData), rulesLibrarySize = length(mergedRules), 
                                                      averageRuleLength = averageMergeRuleLength, 
                                                      predictionCounters = reValidateCurrentClassifier$prediction_Counters ,
                                                      allClassifiedInstances  = reValidateCurrentClassifier$all_Classified_Instancese ,
                                                      allConfusionMat = reConfusionMatrices$Global_Confusion_Matrix ,
                                                      byRulesConfusionMat = reConfusionMatrices$ByRules_Confusion_Matrix , 
                                                      baseClNo = 0 , applyConso = applyConsolidation)
   
   
   # To insure that the current merging of rules haven't degrade the quality of the classifiers

   RulesAreMerged <- FALSE
   if ( round (reCurrentPredictionResults$Tentative_Accuray , digits = 2) >=
        round (currentPredictionResults$Tentative_Accuray , digits = 2) ) {
      if ( round ( reCurrentPredictionResults$General_Accuracy , digits = 2) >=
           round (currentPredictionResults$General_Accuracy , digits = 2)) {
         if ( reCurrentPredictionResults$Abstaining_Rate <= currentPredictionResults$Abstaining_Rate) {
            
            RulesAreMerged <- TRUE
            
         }
      }
   }
   
   ####### copied from merging ensemble ####### ---
   
   
   allnum <- 1:length(rulesLibrary.R)
  
  
   #print ("new rules:")
   #print (reListOfRulesTerms)
  
   
   cat('Merged Rules: ' , mergedRulesNum ,  
       '\n \n Rules not merged:' , allnum [-mergedRulesNum] ,  '\n') 
   #print(all_ReplacedRulesIDs)
   
   
   if ( usingCrossValidation) { 
      
      cv <- cv + 1
      
      #Segement  data by fold using the which() function 
      testIndexes <- which(folds== cv , arr.ind=TRUE)
      testData <- myData [ testIndexes , ]
      trainData <- myData [ - testIndexes , ] 
      next()
   } else  {
      stop()    # stop here to avoid merging rules for the single based classifier  ( produce G-Rules-IQR results)
      
   }
  
   #stop()        # move this stop function to be before the merging part of the single based to produce G-Rules-IQR results 
   
  
  
} else {
   
####------------------ Generate the Ensemble Model  ------------------
  
   
   # Generate Ensemble Model Data frame 
   EnsembleModel <- data.frame(matrix(data = NA , nrow = numBaseClassifiers , ncol = 8 ) , stringsAsFactors = FALSE)
   names (EnsembleModel) <- c ( "Base_Classifier_No" , "Number_of_Rules" , "Average_Rule_Length" , "Number_of_UsedRules" ,"Average_Correctly_used", "Abstain_Rate",
                                "General_Accuracy" , "Tentative_Accuracy")
   
   RulesBank <- list(   )
   
   usedRulesBank <- list( data.frame())
   confusionMatBank <- list( data.frame())
   
   baseTrainData <- trainData
   
   PredictedClassByMajority <-  names (which.max(table(trainData$class)))
   
   bestClassifiersSelected <- FALSE
   
   
   for ( BC in 1:numBaseClassifiers)   {
      
      
      partitionedData <- f.partitionMyTrainData ( bsTrainData = baseTrainData , baseCl = BC )
      trainDataSize <- partitionedData$trainSize
      valiDataSize <- partitionedData$validSize
      
      
      # trainDataSize <- round(nrow( baseTrainData) * 0.632)
      # valiDataSize <- round(nrow(baseTrainData) * 0.368)
      # set.seed(100 * BC)
      # BC_trainData  <- baseTrainData[sample(nrow(baseTrainData), trainDataSize , replace = TRUE) , ]
      # BC_valiData <- baseTrainData[ sample(nrow(baseTrainData), valiDataSize , replace = TRUE) ,]
      # cat ( 'Train Data rows for classifier #',BC ,' = ', nrow(BC_trainData ) , '\n')
      # BC_trainData
      
      BC_trainData <-  partitionedData$BCtrainData 
      BC_valiData <- partitionedData$BCvalidData
      
      rulesLibrary.R  <- GRulesIQR.classification (BC_trainData)
      
      ####################  Reduce overfitting by removing rules with very little coverage 
      
      if ( rulesDeletion  )  {
         beforeDeletionRulesNum <- length (rulesLibrary.R)
         newRulesLibrary.R <-  f.removeOverfittedRules (rulesLibrary.R)
         afterDeletionRulesNum <- length ( newRulesLibrary.R)
         rulesLibrary.R <- newRulesLibrary.R
         
      }
      
      ##################################  
      
      averageRuleLength <-  f.displayRules (rulesLib.R = rulesLibrary.R)
      
      # RulesPool$Classifier_No [BC] <- BC
      RulesBank[[BC]] <- rulesLibrary.R
      
      
      
      cat('Validation data for classifier #', BC , ' = ', nrow(BC_valiData))
      # BC_valiData
      
      validateCurrentClassifier <- GRulesIQR.prediction(  trainD =  BC_trainData , 
                                                          testD =  BC_valiData ,
                                                          rulesLib.R = rulesLibrary.R )

            # The number of correctly used times for each rules in the classifier which was collected during the validation step reflects the quality of that classifier!
      # So the average of these correctly used times will be calculated for future useful step.
      currentUsedRulesListSize <- nrow(validateCurrentClassifier$used_Rules_List)
      averageCurrentCorrectlyUsedRules <- sum (validateCurrentClassifier$used_Rules_List$Correctly_used) / currentUsedRulesListSize
      
      ConfusionMatrices <-  f.GenerateConfusionMatrix ( usedRulesList = validateCurrentClassifier$used_Rules_List ,
                                                        allClassifiedInstances = validateCurrentClassifier$all_Classified_Instancese,
                                                        testD = BC_valiData)
     
      # To keep the list of used rules updated, a bank of all the indiviual BC used rules will be created (please note that the results within this bank will be changed later after merging some rules)
      usedRulesBank [[BC]] <- ConfusionMatrices$used_Rules_List
      
      # To keep (statistics by class) part in the confusion matrix for each base classifier.  
      confusionMatBank [[BC]] <- ConfusionMatrices$ByRules_Confusion_Matrix$byClass
       
      currentPredictionResults <- f.displayResults ( buildEnsembleModel , finalEnsembleResults , trainDataSize = trainDataSize , testDataSize = valiDataSize,
                                                     rulesLibrarySize = length(rulesLibrary.R) , averageRuleLength = averageRuleLength,
                                                     predictionCounters = validateCurrentClassifier$prediction_Counters ,
                                                     allClassifiedInstances = validateCurrentClassifier$all_Classified_Instancese,
                                                     allConfusionMat = ConfusionMatrices$Global_Confusion_Matrix ,
                                                     byRulesConfusionMat = ConfusionMatrices$ByRules_Confusion_Matrix, 
                                                     baseClNo = BC , applyConso = applyConsolidation)
      
      
      EnsembleModel$Base_Classifier_No [BC] <- BC
      EnsembleModel$Number_of_Rules[BC] <- length(rulesLibrary.R)
      EnsembleModel$Average_Rule_Length [BC] <- averageRuleLength
      EnsembleModel$Number_of_UsedRules [BC] <- currentUsedRulesListSize
      EnsembleModel$Average_Correctly_used [BC] <- averageCurrentCorrectlyUsedRules
      EnsembleModel$Abstain_Rate [BC] <- currentPredictionResults$Abstaining_Rate
      EnsembleModel$General_Accuracy [BC] <- currentPredictionResults$General_Accuracy
      EnsembleModel$Tentative_Accuracy [BC] <- currentPredictionResults$Tentative_Accuray
      
   }     #  for ( BC in 1:numBaseClassifiers)
   
   

#####--------- Models selection: 1) Ranking Based Strategy  2) Random Based strategy ------------  
# This part is implemented to show the differences between Ranking and no Ranking strategies in the evaluation chapter/section
   
   if ( rankingModels == TRUE ) {

      # 1) Ranking Based models selection; reorder the base classifiers table.
      topBestClassifiers <- f.RankingModels ( EnsemMod =  EnsembleModel ,
                                              totalBCNum = numBaseClassifiers )
   } else {
      # 2) Random based models selection ; no ordering is applied to the ensemble table
    #  topBestClassifiers <- EnsembleModel
      topBestClassifiers <-  EnsembleModel [1:round (20 * nrow(EnsembleModel) /100) ,]
      
   }
   
   topBestClassifiersSize <- nrow(topBestClassifiers)
   
}


#--------- Merging Rules for the top 20 classifiers in the Ensemble model for continuous attributes  ---------------


# merging the rules libararies of each selected top classifiers to improve the function of these BC
# individually before integrate them as a part of the ensemble model prediction system

bestBCNumbers <- as.numeric (topBestClassifiers$Base_Classifier_No[1:topBestClassifiersSize])


topBestClassifiersAfterMerging <- data.frame(matrix(data = NA , nrow = topBestClassifiersSize , ncol = 8 ) , row.names = bestBCNumbers,  stringsAsFactors = FALSE)
names (topBestClassifiersAfterMerging) <- c ( "Base_Classifier_No", "Number_of_Rules", "Average_Rule_Length", "Number_of_UsedRules" , 
                                              "Average_Correctly_used" , "Abstain_Rate", "General_Accuracy", "Tentative_Accuracy")


# create a separate rule bank library to collect all the rules that have been merged individually for each separate top BC 
topBC.RulesBank <- data.frame(matrix(data = NA , nrow = topBestClassifiersSize , ncol = 2  ), stringsAsFactors = FALSE)
names(topBC.RulesBank) <- c ( "Base_Classifier_No", "BC_Rules")
topBC.RulesBank$Base_Classifier_No <- as.numeric (topBC.RulesBank$Base_Classifier_No)
topBC.RulesBank$BC_Rules <- as.list (topBC.RulesBank$BC_Rules)


###### NEW IF CONDITION ADDED ON JUNE 2021 To TEST CONSOLIDATION WITHOUT THE LOCAL RULE MERGING METHOD  #######
applyLocaRuleMerging <- TRUE   

if ( applyLocaRuleMerging  & applyConsolidation == FALSE) {
    
   usedMergedRulesBank <- list( data.frame())
   
   topBestClassifiersWithoutMerging <- c()
   #topNum <- 0
   
   bestClassifiersSelected <- TRUE
   
   # Search each BC in the top list to test it's generted rules after applying the Merging stratgy which will be explained further later 
   for ( topBC in 1:topBestClassifiersSize) {    # <<<<<<<< think about adding 1:bestBCNumbers and allow thr 
      
      
      topClassifierNum <- bestBCNumbers [topBC]
      
      currentRulesLibrary <-  RulesBank[[topClassifierNum]]
      
      # bestBC_PrunedRules <- currentRulesLibrary     
      # topNum <- topNum + 1
      # topBC.RulesBank$Base_Classifier_No [topNum] <- topBC
      # topBC.RulesBank$BC_Rules[[topNum]] <- currentRulesLibrary
      
      # First aspect in the Merging strategy is chosing the rules with one term .
      
      
      rulesLibrarySizes <- sapply(currentRulesLibrary , nrow)
      
      # To save time, this step is targeting rules with single terms only and replacing the one before 
      rulesLibrarySingleTerm <- which ( rulesLibrarySizes == 1 )
      rulesLibraryMultiTerms <- which ( rulesLibrarySizes > 1)
      
      # before repartition the data set ; check the size of the rules 
      if ( length (rulesLibrarySingleTerm ) >= 2 | length (rulesLibraryMultiTerms) >= 2) {
         
         
         rePartitionDataCurrentCl <- f.partitionMyTrainData (bsTrainData =  baseTrainData , 
                                                             baseCl = topClassifierNum )
         
         BC_trainData <- rePartitionDataCurrentCl$BCtrainData
         BC_valiData <- rePartitionDataCurrentCl$BCvalidData
         
      }
      
      
      reListOfRulesTerms <- list( data.frame())
      
      mergedRulesNum <- c ()
      replacedRulesMultiTermsExist <- FALSE
      
      for (eachRuleMultiTerm in 1: length(currentRulesLibrary) ) { 
         
         if ( ! is.na ( categColNames [1] ))  break()
         
         if ( eachRuleMultiTerm %in% mergedRulesNum ) next()
         
         currentRuleMulti <- currentRulesLibrary [[eachRuleMultiTerm]]
         currentRuleMulti <- cbind(currentRuleMulti , RuleId = eachRuleMultiTerm)
         
         currentAttrNames <- currentRuleMulti$Rule_Term.attribute
         currentAttrClass <- currentRuleMulti$Class[1]
         
         CurrentRuleSize  <- nrow(currentRuleMulti)
         
         # the following initialisation is exactly same as replecedRanges table ! (think about use a function here)
         mergeCurrentTermValues <- data.frame(matrix(data = NA , nrow = 1 , ncol = 6 ) ,
                                              stringsAsFactors = FALSE)
         names(mergeCurrentTermValues) <- c ( "Rule_Term.X", "Rule_Term.attribute" , "Rule_Term.Y", "Class" , 
                                              "RT_Frequency" , "Probability" )
         mergeCurrentTermValues$Rule_Term.X <- as.numeric(mergeCurrentTermValues$Rule_Term.X )
         mergeCurrentTermValues$Rule_Term.attribute <- as.character(mergeCurrentTermValues$Rule_Term.attribute)
         mergeCurrentTermValues$Rule_Term.Y <- as.numeric(mergeCurrentTermValues$Rule_Term.Y)
         mergeCurrentTermValues$Class <- as.character(mergeCurrentTermValues$Class)
         # mergeCurrentTermValues$Discarded_Rules <- as.list (mergeCurrentTermValues$Discarded_Rules)
         mergeCurrentTermValues$RT_Frequency <- as.numeric(mergeCurrentTermValues$RT_Frequency)
         mergeCurrentTermValues$Probability <- as.numeric(mergeCurrentTermValues$Probability)
         
         overlappedRulesFound <- FALSE
         
         
         
         if ( all (currentRuleMulti$Type == "continuous")  & 
              length (rulesLibrarySizes[which (rulesLibrarySizes == CurrentRuleSize)]) > 1)  {
            rulesNumbersToBeChecked <- which(rulesLibrarySizes == CurrentRuleSize)  
            
            contRulesToBeMerged <- list(data.frame())
            ruleSeq <- 0
            
            currentPosition <- which(rulesNumbersToBeChecked == eachRuleMultiTerm)
            refinedRulesNumbersToBeChecked  <- rulesNumbersToBeChecked [ - (1: currentPosition)]
            
            # in case of having no rules left to compare with, i.e. it is the last rule in the currentRulesLibrary  
            if ( is.integer0( refinedRulesNumbersToBeChecked )) next()
            
            for ( anotherRuleNumber in refinedRulesNumbersToBeChecked ) {
               if ( anotherRuleNumber %in% mergedRulesNum )  next()
               ruleToCompare <- currentRulesLibrary [[anotherRuleNumber]]
               ruleToCompare <- cbind(ruleToCompare , RuleId = anotherRuleNumber)
               
               # if (isTRUE( all.equal( currentAttrNames , ruleToCompare$Rule_Term.attribute) ) &
               #     ( currentAttrClass == ruleToCompare$Class[1])) {
               
               if ( all (currentAttrNames %in% ruleToCompare$Rule_Term.attribute ) == TRUE &
                    ( currentAttrClass == ruleToCompare$Class[[1]])) {
                  
                  # A rule with same attribute names for the target class has been found
                  replacedRulesMultiTermsExist <- TRUE
                  
                  if (is_empty(contRulesToBeMerged[[1]]) ) {
                     ruleSeq <- 1
                     
                     contRulesToBeMerged [[ruleSeq]]  <- currentRuleMulti
                     
                  } 
                  ruleSeq <- ruleSeq + 1
                  
                  contRulesToBeMerged [[ruleSeq]]  <- ruleToCompare
                  
               }
            }       #    for ( anotherRuleNumber in  refinedRulesNumbersToBeChecked )
            if (is_empty(contRulesToBeMerged[[1]]) ) {
               # if ( replacedRulesMultiTermsExist == FALSE ) {
               
               cat ('\n No Rules in the current libaray sharing same boundries (same attribute names) with the current rule No: ', eachRuleMultiTerm, '\n')
               
               
            } else  {    # current rule have overlapped rule-terms that can be merged together.
               # important note: all the terms in the current rule have to be overlapped in order to generate a new rule !
               # partially overlapping is not accepted 
               
               #  create a table of all the rules ids that appeared in contRulesToBeMerged table in order to track them within the following replacedRanges 
               #  table to insure a rules are overlapped in all its terms.
               
               
               ranges_XY  <- data.frame(matrix(data = NA , nrow = 1 , ncol = 4 ) ,
                                        stringsAsFactors = FALSE)
               names(ranges_XY) <- c ( "Attribute_Name" ,"RuleId", "Rule_Term.x" , "Rule_Term.Y" )
               
               
               replacedRanges <- data.frame(matrix(data = NA , nrow = 1 , ncol = 8 ) ,
                                            stringsAsFactors = FALSE)
               names(replacedRanges) <- c ( "Rule_Term.X", "Rule_Term.attribute" , "Rule_Term.Y", "Class" , 
                                            "RuleId_1" , "RuleId_2" , "RT_Frequency" , "Probability" )
               replacedRanges$Rule_Term.X <- as.numeric(replacedRanges$Rule_Term.X )
               replacedRanges$Rule_Term.attribute <- as.character(replacedRanges$Rule_Term.attribute)
               replacedRanges$Rule_Term.Y <- as.numeric(replacedRanges$Rule_Term.Y)
               replacedRanges$Class <- as.character(replacedRanges$Class)
               replacedRanges$RuleId_1 <- as.numeric(replacedRanges$RuleId_1 )
               replacedRanges$RuleId_2 <- as.numeric(replacedRanges$RuleId_2)
               replacedRanges$RT_Frequency <- as.numeric(replacedRanges$RT_Frequency)
               replacedRanges$Probability <- as.numeric(replacedRanges$Probability)
               
               overlappedRulesID <- c ()
               rulesRemovedFromMerging <- c ()
               
               for ( mrgAtt in currentAttrNames) {
                  for ( mrgR in 1 : length(contRulesToBeMerged)) {
                     currentTermToMerge <- as.data.frame(contRulesToBeMerged[mrgR])
                     
                     if ( currentTermToMerge$RuleId [1] %in% rulesRemovedFromMerging ) {
                        cat ( '\n ', currentTermToMerge$RuleId [1] , ' are already eliminated from testing')
                        next()
                     }
                     
                     
                     if (is.na (ranges_XY [1,1])) {
                        myRange <- 1
                     } else {
                        
                        myRange <- myRange + 1
                     }
                     ranges_XY [myRange, 1] <- mrgAtt
                     ranges_XY [myRange, 2] <- currentTermToMerge$RuleId [1]
                     ranges_XY [myRange, 3] <- currentTermToMerge [ which(currentTermToMerge$Rule_Term.attribute == mrgAtt) ,][[2]]
                     ranges_XY [myRange, 4] <- currentTermToMerge [ which(currentTermToMerge$Rule_Term.attribute == mrgAtt) ,][[4]]
                     thisRange <-  c ( ranges_XY [myRange, 3] , ranges_XY [myRange, 4] )
                     
                     if ( mrgR > 1 ) {
                        
                        # in case of having more than two overlapped ranges, previous range will be copied from the replaced ranges instead of ranges_XY
                        
                        previousRange <-  c ( ranges_XY [myRange - 1, 3] , ranges_XY [myRange - 1, 4])
                        
                        if ( thisRange %overlaps% previousRange ) {
                           
                           if (is.na (replacedRanges [1,1])) {
                              repRange <- 1
                           } else {
                              
                              repRange <- repRange + 1
                           }
                           
                           replacedRanges [repRange , 1] <- min( thisRange , previousRange)
                           replacedRanges [repRange , 2] <-    mrgAtt
                           replacedRanges [repRange , 3] <-  max(thisRange , previousRange)
                           replacedRanges [repRange , 4] <- currentAttrClass
                           replacedRanges [repRange , 5] <- ranges_XY [myRange - 1 , 2]
                           replacedRanges [repRange , 6] <- ranges_XY [myRange , 2]
                           
                           overlappedRulesID <- c ( overlappedRulesID , ranges_XY [myRange - 1 , 2] , ranges_XY [myRange , 2] )
                           
                        }   else  {  
                           
                           cat ( '\n Rule No :' , currentTermToMerge$RuleId [1] , ' will be removed from current merging of Rules ' )
                           
                           # in case of failing the overlapping test for the current rule term , all the rule will be discarded from merging process
                           rulesRemovedFromMerging <- c (  rulesRemovedFromMerging , currentTermToMerge$RuleId [1])
                           
                           # ThisRange value did not passed the overlapping test with the previous range ; so it will be removed from range_XY table before the 
                           # next iteration to avoid wrongly considered as a previous range then.!
                           ranges_XY <- ranges_XY [ - myRange , ]
                           myRange <- myRange - 1   # decrease the counter of the table because the last element have been removed 
                           
                        }
                        
                     }     #    if ( mrgR > 1 )
                     
                     
                  }    #    for ( mrgR in 1 : length(contRulesToBeMerged)) 
                  
               }     #     for ( mrgAtt in currentAttrNames)
               
               
               # rules are considered to be replaced only if all the ranges in replaced ranges table are overlapped each other .
               # if ( length (attributesList)  == length(currentAttrNames) ) {
               
               attributesList <- unique(replacedRanges$Rule_Term.attribute) 
               
               # if ( isTRUE( all.equal( currentAttrNames , attributesList ) ) ) {
               
               if ( all (currentAttrNames %in% attributesList ) == TRUE) {
                  overlappedRulesFound <- TRUE
                  
                  sizeReplacedRanges <- nrow (replacedRanges)
                  
                  # a new rule have been generated using the replaced terms.
                  
                  
                  for ( overlappedTermName in  1: length( attributesList) ) { 
                     byName  <- replacedRanges [ which (replacedRanges$Rule_Term.attribute == attributesList[overlappedTermName] ), ] 
                     
                     mergeCurrentTermValues[ overlappedTermName, 1] <- min(byName$Rule_Term.X)
                     mergeCurrentTermValues[ overlappedTermName, 2] <- byName$Rule_Term.attribute[1]
                     mergeCurrentTermValues[ overlappedTermName, 3] <- max(byName$Rule_Term.Y)
                     mergeCurrentTermValues[ overlappedTermName, 4] <- byName$Class[1]
                     # mergeCurrentTermValues[ overlappedTermName, 5] <- byName$RuleId_1
                     # mergeCurrentTermValues[ overlappedTermName, 6] <- byName$RuleId_2
                     mergeCurrentTermValues[ overlappedTermName, 5] <- NA
                     mergeCurrentTermValues[ overlappedTermName, 6] <- NA
                     
                     
                     reGenerate_aRuleTerm <- list( "x" = mergeCurrentTermValues[ overlappedTermName, 1] , 
                                                   "attribute" = mergeCurrentTermValues[ overlappedTermName, 2], 
                                                   "y" = mergeCurrentTermValues[ overlappedTermName, 3] ) 
                     
                     # compute the frequncy and the probability of each term in the new rule.
                     
                     if ( length(currentAttrNames)  > 1 ) {
                        
                        if (overlappedTermName == 1) {
                           reCalculateRuleTermProb <- f.calculateRuleTermProbability (dataD =  BC_trainData ,
                                                                                      thisClass =  mergeCurrentTermValues[ overlappedTermName, 4],
                                                                                      thisRuleTerm = reGenerate_aRuleTerm ,
                                                                                      thisAttrType =  "continuous",
                                                                                      reGenerateRT = TRUE)
                           
                        } else  {  # means that this is not the first term in the the current rule. dataset that produced form the 
                           # previous step is going to be reused to test the next term in our current new generated rule
                           reCalculateRuleTermProb <- f.calculateRuleTermProbability (dataD =  reCalculateRuleTermProb$outRefinedAttr ,
                                                                                      thisClass =  mergeCurrentTermValues[ overlappedTermName, 4],
                                                                                      thisRuleTerm = reGenerate_aRuleTerm ,
                                                                                      thisAttrType =  "continuous",
                                                                                      reGenerateRT = TRUE)
                           
                        }    # overlappedTerm > 1
                        
                        
                     } else {
                        reCalculateRuleTermProb <- f.calculateRuleTermProbability (dataD =  BC_trainData ,
                                                                                   thisClass =  mergeCurrentTermValues[ overlappedTermName, 4],
                                                                                   thisRuleTerm = reGenerate_aRuleTerm ,
                                                                                   thisAttrType =  "continuous",
                                                                                   reGenerateRT = TRUE)
                     }
                     # Update the RT_Frequency and Probabolity columns in the list of the rules according to the above recalculation step
                     mergeCurrentTermValues[ overlappedTermName, 5] <- reCalculateRuleTermProb$outRuleTermFrequency
                     mergeCurrentTermValues[ overlappedTermName, 6] <- reCalculateRuleTermProb$outRuleTermProbability
                     
                     
                  }     #    for ( overlappedTerm in  1: attributesLength ) 
                  
                  
               }  else   overlappedRulesFound <- FALSE   # if ( attributesLength  == length(currentAttrNames) ) 
               
               
            }     # else if ( replacedRulesMultiTermsExist == TRUE )
            
         } else next()     #   not enough similarities in the rule-terms of the current rule to be checked for merging process
         
         # if ( is.na (replacedRanges [1,1]))  { 
         if ( overlappedRulesFound  == FALSE )  { 
            # cat ( '\n Rules list in classifier No ' ,topClassifierNum , ' with multiple terms does exist but no matching found for current rule No ' , eachRuleMultiTerm , ' so no merging process happened here \n')
            replacedRulesMultiTermsExist <- FALSE
            
         }
         
         if ( replacedRulesMultiTermsExist == FALSE) {
            
            next()
            # same as in lines starting in 3240
            # no action will happen here !  go to next iteration and move the current rules library to the rules bank without any modifications
         }
         
         # before going to the next multi-Terms-rule wintin the current rulesLibrary.R , use reListOfRulesTerms table to copy all the overlapped ranges 
         # which successfully passed the investigations above and reproduced a new merged rule   
         
         if ( overlappedRulesFound == TRUE )  {
            if (  is_empty (reListOfRulesTerms [[1]] ) ) {
               reLsTerms <- 1
               
            } else reLsTerms <- reLsTerms +  1
            
            reListOfRulesTerms [[ reLsTerms ]] <-  mergeCurrentTermValues
            
            # create a mergedRules list to avoid retest the merged rules more than once.
            
            mergedRulesNum <- c ( mergedRulesNum , unique (overlappedRulesID) )
            
         }
         
         
      }    #  for (eachRuleMultiTerm in length(currentRulesLibrary) )
      
      if ( is_empty(reListOfRulesTerms [[1]])  == TRUE ) {
         
         
         cat ( ' \n No merging process happened for the current rules library of classifier No  ', topClassifierNum , '\n')
         
         # topNum <- topNum + 1
         topBC.RulesBank$Base_Classifier_No [topBC] <- topClassifierNum
         topBC.RulesBank$BC_Rules[[topBC]] <- currentRulesLibrary
         
         topBestClassifiersAfterMerging [topBC , ] <- topBestClassifiers[which(topBestClassifiers$Base_Classifier_No == topClassifierNum) ,] 
         topBestClassifiersWithoutMerging <- c( topBestClassifiersWithoutMerging , topClassifierNum )
         
         
         # Update usedMergedRulesLis by copying usedRulesList from usedRulesBank without any modifications
         
         aMergedUsedRulesListToAdd <- list(usedRulesBank[[topClassifierNum]])
         names(aMergedUsedRulesListToAdd) <- topClassifierNum  
         
         if ( topBC == 1 )  { 
            usedMergedRulesBank <- aMergedUsedRulesListToAdd 
         }else {
            usedMergedRulesBank <- append(usedMergedRulesBank , aMergedUsedRulesListToAdd )
         }
         
         next()     # Go to the next topBC classifier       
      } 
      
      # make reListOfRulesTerms in a structure of the original rulesLibrary.R
      
      
      replacedRulesSize <- length (reListOfRulesTerms)
      
      for ( lis in 1: replacedRulesSize )   {
         
         currentReListOfRulesTerms <- reListOfRulesTerms[[lis]]
         
         # currentReListOfRulesTerms <- currentReListOfRulesTerms [-5]
         currentReListOfRulesTerms <- mutate ( currentReListOfRulesTerms ,  Type = "continuous" )
         currentReListOfRulesTerms <- currentReListOfRulesTerms %>% select( Type , everything())
         
         
         reListOfRulesTerms[[lis]] <- currentReListOfRulesTerms
         
      }
      
      
      # create a new rulesLibrary for the current topBC which includes the merged rules .
      tempMergedRules <- list()
      for ( newRule in 1: length(reListOfRulesTerms)) {
         tempMergedRules [[newRule]] <- reListOfRulesTerms [[newRule]]
         
      }   
      
      tempMergedRules <- append(tempMergedRules , currentRulesLibrary [ - mergedRulesNum] , after = replacedRulesSize)
      
      
      averageRuleLength <-  f.displayRules (rulesLib.R = tempMergedRules)
      
      # Test the new merged rules before considering any changes to rulesBank list
      reValidateCurrentBestBC <-  GRulesIQR.prediction ( trainD = BC_trainData,
                                                         testD =  BC_valiData , 
                                                         rulesLib.R = tempMergedRules)
      
      
      reCalculateUsedRulesListSize <- nrow(reValidateCurrentBestBC$used_Rules_List)
      reCalculateAverageCorrectlyUsedRules <- sum (reValidateCurrentBestBC$used_Rules_List$Correctly_used) / reCalculateUsedRulesListSize
      
      reConfusionMatrices  <- f.GenerateConfusionMatrix ( usedRulesList = reValidateCurrentBestBC$used_Rules_List ,
                                                          allClassifiedInstances = reValidateCurrentBestBC$all_Classified_Instancese ,
                                                          testD = BC_valiData )
      
      
      
      reCurrentBCPredictionResults <- f.displayResults ( buildEM = buildEnsembleModel , finalEnsembleResults ,trainDataSize = trainDataSize , 
                                                         testDataSize = valiDataSize, rulesLibrarySize = length(tempMergedRules), 
                                                         averageRuleLength = averageRuleLength, 
                                                         predictionCounters = reValidateCurrentBestBC$prediction_Counters ,
                                                         allClassifiedInstances  = reValidateCurrentBestBC$all_Classified_Instancese ,
                                                         allConfusionMat = reConfusionMatrices$Global_Confusion_Matrix ,
                                                         byRulesConfusionMat = reConfusionMatrices$ByRules_Confusion_Matrix , 
                                                         baseClNo = topClassifierNum, applyConso = applyConsolidation)
      
      
      # To insure that the current merging of rules haven't degrade the quality of the classifiers
      
      RulesAreMerged <- FALSE
      if ( round (reCurrentBCPredictionResults$Tentative_Accuray , digits = 2) >=
           round (topBestClassifiers [which(topBestClassifiers$Base_Classifier_No == topClassifierNum),]$Tentative_Accuracy , digits = 2) ) {
         if ( round ( reCurrentBCPredictionResults$General_Accuracy , digits = 2) >=
              round (topBestClassifiers [which(topBestClassifiers$Base_Classifier_No == topClassifierNum),]$General_Accuracy , digits = 2)) {
            if ( reCurrentBCPredictionResults$Abstaining_Rate <= topBestClassifiers [which(topBestClassifiers$Base_Classifier_No == topClassifierNum),]$Abstain_Rate) {
               
               RulesAreMerged <- TRUE
               
            }
         }
      }
      ### IMPORTANT NOTE:  usedMergedRulesBank list can be accessed using the name of the TopBC not by its postition in the topBestClassifiers table
      # e.g. usedMergedRulesBank$`2` equalivant  to usedMergedRulesBank[[3]] but not same as usedMergedRulesBank[[2]]
      #  This is different in case of usedRulesBank where only the positions applied to the list to access the base classifiers 
      
      if ( RulesAreMerged ) {
         topBC.RulesBank$Base_Classifier_No [topBC] <- topClassifierNum      
         
         topBC.RulesBank$BC_Rules[[topBC]] <- tempMergedRules
         
         topBestClassifiersAfterMerging$Base_Classifier_No[topBC] <-  topClassifierNum
         topBestClassifiersAfterMerging$Number_of_Rules[topBC] <- length(tempMergedRules)
         topBestClassifiersAfterMerging$Average_Rule_Length[topBC] <- averageRuleLength
         topBestClassifiersAfterMerging$Number_of_UsedRules [topBC] <- reCalculateUsedRulesListSize
         topBestClassifiersAfterMerging$Average_Correctly_used [topBC] <- reCalculateAverageCorrectlyUsedRules
         topBestClassifiersAfterMerging$Abstain_Rate[topBC] <- reCurrentBCPredictionResults$Abstaining_Rate
         topBestClassifiersAfterMerging$General_Accuracy [topBC] <- reCurrentBCPredictionResults$General_Accuracy
         topBestClassifiersAfterMerging$Tentative_Accuracy[topBC] <- reCurrentBCPredictionResults$Tentative_Accuray
         
         # Update usedMergedRulesLis with the new results of usedRulesList after completing the merging process 
         aMergedUsedRulesListToAdd <- list(reConfusionMatrices$used_Rules_List)
         names(aMergedUsedRulesListToAdd) <- topClassifierNum  
         
         if ( topBC == 1 )  { 
            usedMergedRulesBank <- aMergedUsedRulesListToAdd 
         }else {
            usedMergedRulesBank <- append(usedMergedRulesBank , aMergedUsedRulesListToAdd )
         }
         
      } else {
         
         cat ( '\n topBC No: ', topClassifierNum , ' No rules merging occured \n')
         
         topBestClassifiersWithoutMerging <- c (topBestClassifiersWithoutMerging , topClassifierNum)
         # topNum <- topNum + 1
         topBC.RulesBank$Base_Classifier_No [topBC] <- topClassifierNum
         topBC.RulesBank$BC_Rules[[topBC]] <- currentRulesLibrary
         
         topBestClassifiersAfterMerging [topBC , ] <- topBestClassifiers[which(topBestClassifiers$Base_Classifier_No == topClassifierNum) ,] 
         
         # Update usedMergedRulesLis by copying usedRulesList from usedRulesBank without any modifications
         
         aMergedUsedRulesListToAdd <- list(usedRulesBank[[topClassifierNum]])
         names(aMergedUsedRulesListToAdd) <- topClassifierNum  
         
         if ( topBC == 1 )  { 
            usedMergedRulesBank <- aMergedUsedRulesListToAdd 
         }else {
            usedMergedRulesBank <- append(usedMergedRulesBank , aMergedUsedRulesListToAdd )
         }
         
      }
      
      
      
   }    #   for ( topBC in bestBCNumbers)
   
   
   if ( ! is.na (topBestClassifiersAfterMerging [1,1]) ) { 
      
      # Compute averege values for the best selected base classifiers
      
      comparisons_Summary <- data.frame(matrix( data = NA, nrow = 2, ncol = 6 ), 
                                        row.names = c("Average results of before merging the rules" , 
                                                      "Average results after merging the rules" ), 
                                        stringsAsFactors = FALSE)
      names (comparisons_Summary) <- names ( topBestClassifiers [c (-1 ,-3) ])
      
      comparisons_Summary [1,] <- topBestClassifiers["Average" , c (-1, -3)]
      comparisons_Summary [2,]$Number_of_Rules <- round (mean(topBestClassifiersAfterMerging$Number_of_Rules) )
      comparisons_Summary [2,]$Number_of_UsedRules <- round(mean(topBestClassifiersAfterMerging$Number_of_UsedRules))
      comparisons_Summary [2,]$Average_Correctly_used <- round( mean (topBestClassifiersAfterMerging$Average_Correctly_used))
      comparisons_Summary [2,]$Abstain_Rate <- mean(topBestClassifiersAfterMerging$Abstain_Rate)
      comparisons_Summary [2,]$General_Accuracy <- mean(topBestClassifiersAfterMerging$General_Accuracy)
      comparisons_Summary [2,]$Tentative_Accuracy <- mean(topBestClassifiersAfterMerging$Tentative_Accuracy)
      
   } else {
      
      cat ( '\n\n NO CHANGES APPLIED TO THE TOP BEST BASE CLASSIFIERS RULES LISTS, hence, the final Rules-bank lists still the same' )
      #print (topBestClassifiers)
   }
   
   ###### NEW ELSE CONDITION ADDED ON JUNE 2021 To TEST CONSOLIDATION WITHOUT THE LOCAL RULE MERGING METHOD  #######
   
}  else  if (applyConsolidation == TRUE )    {
    
   #Fill the topBC.RulesBank list 
   for ( topBC in 1:topBestClassifiersSize) { 
      
      topClassifierNum <- bestBCNumbers [topBC]
      
      currentRulesLibrary <-  RulesBank[[topClassifierNum]]
      topBC.RulesBank$Base_Classifier_No [topBC] <- topClassifierNum
      topBC.RulesBank$BC_Rules[[topBC]] <- currentRulesLibrary
      
   }
   
   
}


#######-------------   Consolidation Section -----------------------
if ( applyConsolidation == TRUE)   {

   
   consolidatedRules <- list(data.frame())
   consolidR <- 1
   
   for (eachLabel in classLevels ){    # loop 1  filter by class
      byClassRules <- list() 
      classIndex <- 1
      # categorise rules by class labels
      for (bestBC in bestBCNumbers )  {    #  loop 2   search all top rules sets
         
         bestBC.Position <- which (topBC.RulesBank$Base_Classifier_No == bestBC )
         
         aSet <- topBC.RulesBank$BC_Rules[[bestBC.Position]]    # extract a set
         
         
        
         aSetSize <-  length(aSet)
         
         for ( classExtract in 1 : aSetSize ) {    # loop 3  extract target class from the current rules set
            myRuleByClass <- aSet[[classExtract]]
            myRuleByClass <- cbind(myRuleByClass , TopBC_Num = bestBC)
            # filter 
            if (( myRuleByClass$Class[[1]] == eachLabel)  & (last(myRuleByClass$RT_Frequency ) != 1)) {
               byClassRules [[classIndex]] <- myRuleByClass
               classIndex <- classIndex + 1
               
            }
            
            
         }     # loop 3   a target class are extracted from all top rules sets
         
      }   #  loop 2     # search all top rules sets    
      # categorise byClassRules list by attributes 
      # Good command  to access a specific item in a list using its name that has been assigned previously
      
      # byClassRules[ which(list.names(byClassRules) == 6 )]
      
      byClassRuleSize <-  length(byClassRules)
      
      
      # add names to the list in order to can find each rule separatley  
      byClassRulesNames <- (1:byClassRuleSize)     
      names (byClassRules) <-  byClassRulesNames
      byClassRules

      # rulesNamesToRemove <- c()    #  rules already checked for the similarites
      
      
      #To remove the rules that already merged from the following alterations in loop 4
      rulesIDs <- c()
      for ( attrExtract in byClassRulesNames ) {    # loop 4  extract target attribute form a rules list created from loop 3
         attrExtract
         # to avoid process the same merged rules 
         if ( attrExtract %in% rulesIDs ) {
          
            next()
         }
         
         byAttRules <- list()
         AttIndex <- 1
         # target rule from the rules list generated from loop 3
         myRuleByAtt <- byClassRules [[which(names(byClassRules) == attrExtract)]]
         myRuleByAtt <- cbind(myRuleByAtt , RuleID = attrExtract)
         # target attributes to be alocated
         targetAttributes <- myRuleByAtt$Rule_Term.attribute
       
         
         # create a list to collect all the rules that sharing the same attributes 
         byAttRules [[AttIndex]] <- myRuleByAtt
         AttIndex <- AttIndex + 1
         
         
         # eliminate the rules that had been merged from the next loop (if any!) otherwise use the original byClassRulesNames
         
         if (!(is.null (rulesIDs))) {
            refinedByClassRulesNames <- byClassRulesNames [ - rulesIDs]
            
         } else {
            refinedByClassRulesNames <- byClassRulesNames
         }
         
         
         # determine the position of current rule (under invistigating) within the updated list of byClassRulesNames
         curentPositionByclassRulesList <- which(refinedByClassRulesNames == attrExtract)
         
         # remove all the rules that have been processed form the beginig of the list to the current position
         refinedByClassRulesNames <-  refinedByClassRulesNames [ - (1:curentPositionByclassRulesList) ]

      
         for (similarExtract in refinedByClassRulesNames) {    # loop 5
            
            # extract another rule to be checked 
            aRuleToCheck <-  byClassRules [[which(names(byClassRules) == similarExtract)]]
            
            aRuleToCheck <- cbind(aRuleToCheck ,RuleID = similarExtract )
            
            if ( nrow(aRuleToCheck) != nrow(myRuleByAtt) )    {
               
               next()
               
            }
            
            # extract a rule's atttribute to be compared with the target attribute.
            attrToCheck <- aRuleToCheck$Rule_Term.attribute
            
            if ( isTRUE(all.equal(sort(attrToCheck), sort(targetAttributes))) ) {     # sort uses to sort the attributes before checking for similarities between attributes that have different orders in two different rules
               
               # a rule with similar attributes have been found , then rules will be stored in byAttRules list
               byAttRules [[AttIndex]] <- aRuleToCheck
               AttIndex <- AttIndex + 1
               
               # # rules will be removed form next alterations of loop 4
               
            }  
            
         }   # loop 5
         
         
         # All rules in byAttRules list share the same class and have the same attributes
         # assign names to the byAttRules list in order to can find each rule separatley later
         byAttRulesNames <- (1:length(byAttRules))     
         names (byAttRules) <-  byAttRulesNames
         

         # Check the size of byAttRules list; if it less than 2, this means there is no other rules to consolidate with
         if (length (byAttRules) < 1 ) {
            next()
         } else if (length (byAttRules) == 1 ) {
  
            consolidatedRules [[consolidR]] <- byAttRules [[1]][ , -(8:9)]
            rulesIDs <- c( rulesIDs, attrExtract)
            next()
         }
         
         eachRulesize <-  sapply (byAttRules,nrow)
         removedRules <- c()
         
         
         rulesMightBeOverlapped <- byAttRulesNames
         
         # This loop go through all the rules in byAttRules lists
         for ( byAttR in rulesMightBeOverlapped) {   # loop 6
            byAttR
            
          
            # In the first alteration of this loop, next condition will be false.
            # however removedRules value will be updated at the end of this loop (#6) according to the the output of loop 7 by 
            # assigning overlappedTermsID value to removedRules ; 
            
            if ( byAttR %in% removedRules) {

               next()
            }
            
            currentAttRule <- byAttRules [[byAttR]]
            attRuleSize <- nrow(currentAttRule)
            
            # Apply the merging algorithm for current target attribute of current class  
            
            
            consoReplacedRanges <-  data.frame(matrix(data = NA , nrow = 1 , ncol = 7 ) ,
                                               stringsAsFactors = FALSE)
            names (consoReplacedRanges) <- c( "Type" ,"Rule_Term.X", "Rule_Term.attribute" , "Rule_Term.Y", "Class" , 
                                              "RT_Frequency" , "Probability"  )
            consoReplacedRanges$Type <- as.character (consoReplacedRanges$Type)
            consoReplacedRanges$Rule_Term.X <- as.numeric(consoReplacedRanges$Rule_Term.X)
            consoReplacedRanges$Rule_Term.attribute <- as.character(consoReplacedRanges$Rule_Term.attribute)
            consoReplacedRanges$Rule_Term.Y <- as.numeric(consoReplacedRanges$Rule_Term.Y)
            consoReplacedRanges$Class <- as.character(consoReplacedRanges$Class)
            consoReplacedRanges$RT_Frequency <- as.numeric(consoReplacedRanges$RT_Frequency)
            consoReplacedRanges$Probability <- as.numeric(consoReplacedRanges$Probability)

            overlappedAttRules <- TRUE   
            
            # eliminate the rules that had been merged from the next loop (if any!) otherwise use the original byClassRulesNames
            
            if (!(is.null (removedRules))) {
               refinedRulesToMatching <- rulesMightBeOverlapped [ - removedRules]
               
            } else {
               refinedRulesToMatching <- rulesMightBeOverlapped
            }
            # check if there are any rules in rulesMightBeOverlapped names that already merged in previous alterations in this loop 
            # if yes, it must be removed from refinedRulesMatching values  
            
            currentAttRulePosition <- which(refinedRulesToMatching == byAttR)
            refinedRulesToMatching <- refinedRulesToMatching [ - (1 : currentAttRulePosition)]
            
            # This loop go through all terms of the selected rule (currentAttRules)
            overlappedTermsID <- c()
            
            # To avoid entring the next two loops with no more rules to compare with 
            
            if ( length(refinedRulesToMatching) != 0)  {
               
               for (byAttRsize in 1: attRuleSize) {     # loop 7
                  currentTerm <- currentAttRule [byAttRsize ,]
                  currentTermName <- currentTerm$Rule_Term.attribute
                  
                  # T First run of this loop 7, the following condition will be False. 
                  # It Will be True when the currentTerm overlapped with terms in other rules in  previous alteration. 
                  # Thus, the next loop (#8) will search byAttRules list using these overlapped ids only
                  if (!(is.null (overlappedTermsID))) {
                     refinedRulesToMatching <- overlappedTermsID
      
                     overlappedTermsID <- c()
                  }
                  overlappedTermsList <- list()
                  overTrm <- 1
                  
                  # This loop go through all rules in ByAttRules to find rules that overlapped with currentTerm which is part of currentAttRule
                  for (nextByAttRules in refinedRulesToMatching) {    # loop 8
                     
                     newAttRuleToCompare <- byAttRules [[nextByAttRules]]
                     
                     # To avoid having similar rules with different order of attributes, find the position of the current term within the newAttRuleToCompare; 
                     newTermPosition <-  which(newAttRuleToCompare$Rule_Term.attribute == currentTermName)
                     
                     if (newAttRuleToCompare$Rule_Term.attribute[newTermPosition] == currentTermName) {
                        
                        # categorical attributes consolidation
                        if ( currentTermName %in% categColNames) {
                           currentAttRule_XY <- currentTerm$Rule_Term.Y
                           newAttRuleToCompare_XY <- newAttRuleToCompare$Rule_Term.Y[newTermPosition]
                           
                           
                        } else  if (currentTermName %in% numericColNames){
                           currentAttRule_XY <- c( as.numeric (currentTerm$Rule_Term.X) , as.numeric (currentTerm$Rule_Term.Y) )
                           newAttRuleToCompare_XY <- c ( as.numeric(newAttRuleToCompare$Rule_Term.X[newTermPosition]) , as.numeric(newAttRuleToCompare$Rule_Term.Y[newTermPosition]) )
                           
                        }
                       
                        matchingValueExist <- FALSE
                        
                        if (currentTermName %in% categColNames ) {
                           
                           if (currentAttRule_XY ==  newAttRuleToCompare_XY)  {
                              matchingValueExist <- TRUE
                           }
                           
                        } else if (currentTermName %in% numericColNames ) {
                           if (currentAttRule_XY %overlaps% newAttRuleToCompare_XY ) {
                              matchingValueExist <- TRUE
                           }
                           
                        } 
                        if (matchingValueExist ) {
                        
                        #if (currentAttRule_XY %overlaps% newAttRuleToCompare_XY ) {
                           
                           if ( length(overlappedTermsList ) == 0) {
                              # add currentAttRule to the list  
                              overlappedTermsList [[overTrm]] <- currentAttRule 
                              overTrm <- overTrm + 1
                              
                              # In case of more than one term in that rules to be checked; make sure that this is the last term before adding byAttR to removedRules values
                              if (byAttRsize == attRuleSize) {
                                 
                                 
                                 removedRules <- c( removedRules , byAttR)
                                 
                              }
                              
                           }
                           overlappedTermsList [[overTrm]] <- newAttRuleToCompare
                           overTrm <- overTrm + 1
                           overlappedTermsID <- c (overlappedTermsID , nextByAttRules)
                           
                           if (byAttRsize == attRuleSize) {
                              
                              removedRules <- c( removedRules , nextByAttRules)
                           }
                           
                        }
                     }
                     
                  }      # loop 8
                  
                  overlappedTermsList
                  
                  # in case of one of the currentAttRule terms fail to find any overlapped terms within newAttRuleToCompare values
                  # this will lead to store this current rule directly in consolidated rules list without merging 
                  
                  if ( length(overlappedTermsList ) == 0) {
                     overlappedAttRules <- FALSE
                     
                     break()
                     
                  } 
                  # No Overlapping found
                  
               }  #loop 7
            } else {
               overlappedAttRules <- FALSE  # only one rule in byAttRules; no more rules to compare with
              
               
            }  
            
            
            # loop 7 output: list of all rules extracted from byAttRules list that overlapped currentAttRule that is extracted from loop 6
            if ( overlappedAttRules ) {
               
               # Merge loop 7 output and currentAttRule.
               
               Trm <- 1
               
               repeat{
                  
                  firstOverlapTerm <- overlappedTermsList [[1]][Trm,]
                  
                  sizeOfFirstOverlapRule <- nrow (overlappedTermsList [[1]] )
                  # create a table to collect all the ranges of the same attributes and same class lables in order 
                  consoRanges_XY <- data.frame(matrix(data = NA , nrow = 1 , ncol = 6 ) ,
                                               stringsAsFactors = FALSE)
                  names(consoRanges_XY) <- c ("Attribute_Name" ,"RuleId", "Rule_Term.x" , "Rule_Term.Y" , "RT_Frequency" , "Probability")
                  
                  if (is.na (consoRanges_XY [1,1])) {
                     myConsoRange <- 1
                  } else {
                     
                     myConsoRange <- myConsoRange + 1
                  }
                  
                  consoRanges_XY [myConsoRange, 1] <- firstOverlapTerm$Rule_Term.attribute
                  consoRanges_XY [myConsoRange, 2] <- firstOverlapTerm$RuleID
                  consoRanges_XY [myConsoRange, 3] <- firstOverlapTerm$Rule_Term.X
                  consoRanges_XY [myConsoRange, 4] <- firstOverlapTerm$Rule_Term.Y
                  consoRanges_XY [myConsoRange, 5] <- firstOverlapTerm$RT_Frequency
                  consoRanges_XY [myConsoRange, 6] <- firstOverlapTerm$Probability
                  
                  # To avoid updating the rulesIDs values more than one 
                  if ( Trm == sizeOfFirstOverlapRule )  {
                     rulesIDs <- c(rulesIDs, firstOverlapTerm$RuleID)
                     
                  }
                  
                  
                  for ( overlapRange in  2 : length(overlappedTermsList)) { 
                     
                     currentOverlapRule <- overlappedTermsList [[overlapRange]]
                     TrmName <-  which (currentOverlapRule$Rule_Term.attribute == firstOverlapTerm$Rule_Term.attribute )
                     currentOverlapRule [TrmName,] 
                     
                     if (is.na (consoRanges_XY [1,1])) {
                        myConsoRange <- 1
                     } else {
                        
                        myConsoRange <- myConsoRange + 1
                     }
                     
                     consoRanges_XY [myConsoRange, 1] <- currentOverlapRule$Rule_Term.attribute[TrmName]
                     consoRanges_XY [myConsoRange, 2] <- currentOverlapRule$RuleID [TrmName]
                     if ( firstOverlapTerm$Rule_Term.attribute %in% categColNames) {
                        consoRanges_XY [myConsoRange, 3] <- currentOverlapRule$Rule_Term.X[TrmName]
                        consoRanges_XY [myConsoRange, 4] <- currentOverlapRule$Rule_Term.Y[TrmName]
                        
                     } else  if ( firstOverlapTerm$Rule_Term.attribute %in% numericColNames) {
                        consoRanges_XY [myConsoRange, 3] <- as.numeric (currentOverlapRule$Rule_Term.X[TrmName])
                        consoRanges_XY [myConsoRange, 4] <- as.numeric (currentOverlapRule$Rule_Term.Y[TrmName])
                        
                     }
                     consoRanges_XY [myConsoRange, 5] <- currentOverlapRule$RT_Frequency[TrmName]
                     consoRanges_XY [myConsoRange, 6] <- currentOverlapRule$Probability[TrmName]
                     
                     # To avoid updating the rulesIDs values more than one 
                     if ( Trm == sizeOfFirstOverlapRule ) {
                        rulesIDs <- c(rulesIDs, currentOverlapRule$RuleID [TrmName])
                     }
                     
                     
                  }
                  
                  # # fill consoReplacedRanges table with the replaced x and y
                  if ( is.na(consoReplacedRanges [1,1])) {
                     consoRepRange <- 1
                  } else {
                     
                     consoRepRange <- consoRepRange + 1
                     
                  }
                  if ( consoRanges_XY$Attribute_Name[1] %in% categColNames) {
                     consoReplacedRanges [consoRepRange , 1] <- "categorical"
                     consoReplacedRanges [consoRepRange , 2] <- NA
                     consoReplacedRanges [consoRepRange , 3] <- consoRanges_XY$Attribute_Name[1]
                     consoReplacedRanges [consoRepRange , 4] <- consoRanges_XY$Rule_Term.Y[1]
                     consoReplacedRanges [consoRepRange , 5] <- eachLabel
                     consoReplacedRanges [consoRepRange , 6] <- round( mean(consoRanges_XY$RT_Frequency))
                     consoReplacedRanges [consoRepRange , 7] <- mean(consoRanges_XY$Probability)
                     
                  } else  if ( consoRanges_XY$Attribute_Name[1] %in% numericColNames) {
                     consoReplacedRanges [consoRepRange , 1] <- "continuous"
                     consoReplacedRanges [consoRepRange , 2] <- min(consoRanges_XY$Rule_Term.x)[1]
                     consoReplacedRanges [consoRepRange , 3] <- consoRanges_XY$Attribute_Name[1]
                     consoReplacedRanges [consoRepRange , 4] <- max(consoRanges_XY$Rule_Term.Y)[1]
                     consoReplacedRanges [consoRepRange , 5] <- eachLabel
                     consoReplacedRanges [consoRepRange , 6] <- round( mean(consoRanges_XY$RT_Frequency))
                     consoReplacedRanges [consoRepRange , 7] <- mean(consoRanges_XY$Probability)
                     
                  }
                 
                  
                  Trm <- Trm + 1
                  if (Trm >  sizeOfFirstOverlapRule) {
                     break
                  }
               }     # repeat
               
               # # create new rule from the new list of terms in consoReplacedRanges table and then store it into consolidatedRules  
               
               consoRanges_XY
               
               consolidatedRules [[ consolidR]] <- consoReplacedRanges
               consolidR <- consolidR + 1
               
            } else {
               # The rule will be added directly to the consolidatedRules list without applying any merging
               
               consolidatedRules [[ consolidR]] <- currentAttRule [ , - (8:9)]
               consolidR <- consolidR + 1
               removedRules <- c( removedRules , byAttR )
               rulesIDs <- c(rulesIDs , currentAttRule$RuleID [1])
            }
            
         }     # loop 6
         
      }  # loop 4
      
   }    # loop 1
   
   
}        # if ( applyConsolidation == TRUE)




# @@@@@@@  ------------------------------------------ @@@@@@@%
#########             Testing Stage           #########- 
# @@@@@@@  ------------------------------------------ @@@@@@@%


####---------------- Testing consolidated ReG-Rules classifier  ----------------- 

if ( applyConsolidation == TRUE) {

   finalEnsembleResults <- TRUE

   # Reorder the consolidated rules according to the probability before using them in the testing stage
   orderedConsolidatedRules <- f.orderRulesByProbability (consolidatedRules)
   
  
   averageRuleLength <-  f.displayRules (rulesLib.R = orderedConsolidatedRules)
   
   validateCurrentClassifier <- GRulesIQR.prediction(  trainD =  trainData , 
                                                       testD =  testData ,
                                                       rulesLib.R = orderedConsolidatedRules )
   ConfusionMatrices <-  f.GenerateConfusionMatrix ( usedRulesList = validateCurrentClassifier$used_Rules_List ,
                                                     allClassifiedInstances = validateCurrentClassifier$`all_Classified_Instancese`,
                                                     testD = testData)
   
   currentPredictionResults <- f.displayResults ( buildEnsembleModel , finalEnsembleResults , trainDataSize = nrow(trainData) ,testDataSize =  nrow(testData),
                                                  rulesLibrarySize = length(orderedConsolidatedRules) , averageRuleLength = averageRuleLength,
                                                  predictionCounters = validateCurrentClassifier$prediction_Counters ,
                                                  allClassifiedInstances = validateCurrentClassifier$`all_Classified_Instancese`,
                                                  allConfusionMat = ConfusionMatrices$Global_Confusion_Matrix ,
                                                  byRulesConfusionMat = ConfusionMatrices$ByRules_Confusion_Matrix , 
                                                  baseClNo = NA, applyConso = applyConsolidation)
   
  # stop()   
   
} else  {       #   applyConsolidation == FALSE
  
###---------------- Testing ReG-Rules classifier without consolidation -----------

# Initialise counters
classifiedByRules_Ensemble <- 0
correctlyClassifiedByRules_Ensemble <- 0        # (computed manually)    = True Positive Examples
wronglyClassifiedByRules_Ensemble <- 0        #  (computed manually)  = False Positive Examples 

unClassifiedInstances_Ensemble <- 0

correctlyClassifiedByMajority_Ensemble <- 0
wronglyClassifiedByMajority_Ensemble <- 0
totalExmpCorrectlyClassified_Ensemble <- 0

# Start the testing stage using voting system of the ensemble model 

numTestExamples <- nrow(testData)
testDataRowNames <- row.names(testData)

factorsLevels <- levels(as.factor(testData$class))

allClassifiedInstances_Ensemble <- f.initialClassifiedInstancesTable( factorsLevels)
allClassifiedInstances_Ensemble <- allClassifiedInstances_Ensemble[ - 5]

# This list will be used to investigate the committee decision across the test data.   (TEMPORARY)
weightedAvgProbBank <- list (data.frame())    


for   ( anExample in testDataRowNames)  {     # for 1      
   
   
   ExampleNo <- 0
   
   classifiers_Committee <- data.frame(matrix ( nrow = topBestClassifiersSize , ncol = 7),row.names = bestBCNumbers,
                                       stringsAsFactors = FALSE) 
   names(classifiers_Committee) <- c ("Classifier_No" , "Rule_ID" , "Used_Times" , "Correctly_used", 
                                      "Tentative_Accuracy", "Vote", "Classification_Type")
   
   currentInstance <- testData [ anExample,]
   instanceCoveredByAnyBCRules <- FALSE
   for ( BBCNo in bestBCNumbers) {
      
      RulesBankIndx <- which(topBC.RulesBank$Base_Classifier_No == BBCNo )
      
      BBC_Rules <- topBC.RulesBank$BC_Rules[[RulesBankIndx]]   
      
      rulesNumber <- length(BBC_Rules)
      
      ### Reorder the rules according to their quality ####  <<<<<<  THIS SHOULD BE MOVED OUTSIDE THE OUTER LOOP
      orderedAsCorrectlyUsedTimes <-  usedMergedRulesBank[[as.name(BBCNo)]] [ order(-usedMergedRulesBank[[as.name(BBCNo)]] [ , 'Correctly_used']) ,] 
      orderedCurrentRules_IDs <- unlist(orderedAsCorrectlyUsedTimes$Rule_ID , use.names = F)
      
      newOrder.temp <- 1:rulesNumber
      rulesID.LessQuality <- newOrder.temp [ - as.numeric(orderedCurrentRules_IDs)]
      
      orderedCurrentRules_IDs <- append(as.numeric(orderedCurrentRules_IDs) , rulesID.LessQuality)
      
      # check if there are any Rules that cover the instance.
      for  (ruleID in orderedCurrentRules_IDs) {     # for 2
         
         instanceCoveredByCurrentRules <- FALSE   # to be Checked
         
         aRule <- BBC_Rules [[ruleID]]
         aRuleNumTerms <- nrow(aRule)
         aRuleClass <- aRule$Class[1]
         noOfTimesRuleUsed <- 0
         matchedTerms <- 0
         
         # loop throught all the terms of the current Rule
         for ( aTerm in  1:aRuleNumTerms)  {    # for 3
            
            # termAttributeName <- aRule$Attribute[aTerm]
            attrValToBeCheck <- dplyr::select(currentInstance , aRule$Rule_Term.attribute[aTerm]) %>% pull()
            if ( is.numeric( attrValToBeCheck )) {
               if ( aRule$Type[aTerm] == "continuous") 
                  if (attrValToBeCheck > as.numeric(aRule$Rule_Term.X[aTerm]) & 
                      attrValToBeCheck <= as.numeric(aRule$Rule_Term.Y[aTerm]) ){
                     matchedTerms <- matchedTerms +1 
                  } else break()
               
            } else {
               
               if ( aRule$Type[aTerm] == "categorical") {
                  if (attrValToBeCheck == aRule$Rule_Term.Y[aTerm] )  {
                     matchedTerms <- matchedTerms +1
                  } else break() 
               }
            }
            
         }     # for 3
         
         if (matchedTerms == aRuleNumTerms) {
            
            instanceCoveredByCurrentRules <- TRUE     # to be Checked
            
            break()
            
         }
         
      }   #  for 2  
      
      if (  instanceCoveredByCurrentRules ) {
         
         # this flag indicates that current instance is covered by (at least) one BBC rules 
         instanceCoveredByAnyBCRules <- TRUE
         currentUsedRulesList <- usedMergedRulesBank [[as.name (BBCNo)]]
         
         ExampleNo <- ExampleNo + 1 
         classifiers_Committee$Classifier_No[ExampleNo] <- BBCNo
         BBC_Index <- which(topBestClassifiersAfterMerging$Base_Classifier_No == BBCNo) 
         classifiers_Committee$Rule_ID[ExampleNo] <- ruleID
         
         if ( ruleID %in% currentUsedRulesList$Rule_ID )  {
            UsedRule <- currentUsedRulesList[which(currentUsedRulesList$Rule_ID == ruleID) ,]
            classifiers_Committee$Used_Times[ExampleNo] <- UsedRule$Used_Times
            classifiers_Committee$Correctly_used[ExampleNo] <- UsedRule$Correctly_used
            
         } else {
            classifiers_Committee$Used_Times[ExampleNo]  <- 0
            classifiers_Committee$Correctly_used[ExampleNo]  <- 0
            
         }
         
         classifiers_Committee$Tentative_Accuracy[ExampleNo] <-  topBestClassifiersAfterMerging$Tentative_Accuracy[BBC_Index] 
         
         classifiers_Committee$Vote[ExampleNo] <- aRuleClass
         classifiers_Committee$Classification_Type[ExampleNo] <- "Rules"
         
         cat ( '\n current Instance: ', anExample , ' is classified by Rule No: ' ,ruleID , ' in Classifier No: ', BBCNo, '\n'  )
         
      } else if (! instanceCoveredByCurrentRules) {
         
         # cat ( '\n\n All the rules list in the base classifier No ', BBCNo, ' did not cover the current dataset instance No ', anExample ,
         #       '\n the predicted class is founded using majority labeling approach \n')
         ExampleNo <- ExampleNo + 1 
         classifiers_Committee$Classifier_No[ExampleNo] <- BBCNo
         BBC_Index <- which(topBestClassifiersAfterMerging$Base_Classifier_No == BBCNo) 
         classifiers_Committee$Rule_ID[ExampleNo] <- NA
         classifiers_Committee$Used_Times[ExampleNo] <- NA
         classifiers_Committee$Correctly_used[ExampleNo] <- NA
         classifiers_Committee$Tentative_Accuracy[ExampleNo] <- topBestClassifiersAfterMerging$Tentative_Accuracy[BBC_Index]
         classifiers_Committee$Vote[ExampleNo] <- PredictedClassByMajority
         classifiers_Committee$Classification_Type[ExampleNo] <- "Majority Class"
         
      }
      
   }      #    for ( BBCNo in bestBCNumbers)
   
   if (instanceCoveredByAnyBCRules) { 
      
      classifiedByRules_Ensemble <- classifiedByRules_Ensemble + 1 
      
      #  Evaluate the classifiers_Committee results to decide the best vote.  
      
      allClassifier_Committee <- classifiers_Committee
      
      if ( any(classifiers_Committee$Classification_Type ==  "Majority Class")) {
         classifiersWithZeroWgh  <- which (classifiers_Committee$Classification_Type == "Majority Class")
         classifiers_Committee <-  classifiers_Committee [ -classifiersWithZeroWgh, ]
      }
   
      votingLevels <-  levels (as.factor(classifiers_Committee$Vote))
      
      if ( length (votingLevels) > 1 ) {
    
         weighted_Average_Probab <- data.frame(matrix( data = NA , nrow = length(votingLevels) , ncol = 5),  stringsAsFactors = FALSE)
         names(weighted_Average_Probab) <- c ( "Predicted_Class", "Vote_Frequency" ,"Total_Rule_Used_Times" , "Total_Correctly_Used" , 
                                               "Total_Tentative_Acc"  )
         
         # total number of correctly used column in the whole classifiers_committee table
         all_Correctly_Used <- sum ( classifiers_Committee$Correctly_used , na.rm = TRUE)
         voteIndx <- 0
         for ( eachVote in votingLevels ) {
            # indx <- which(row.names(weighted_Average_Probab) == eachVote )
            voteIndx <- voteIndx + 1 
            weighted_Average_Probab$Predicted_Class [voteIndx] <- eachVote
            # currentVoteFrequency <-  classifiers_Committee %>% filter(Vote == eachVote) %>% nrow()
            currentVoteFrequency <- nrow (classifiers_Committee[ which(classifiers_Committee$Vote == eachVote),])
            weighted_Average_Probab$Vote_Frequency[voteIndx] <- currentVoteFrequency
            
            weighted_Average_Probab$Total_Rule_Used_Times[voteIndx] <- 
               classifiers_Committee %>% filter(Vote == eachVote) %>% 
               select(Used_Times) %>% sum( . , na.rm = TRUE)
            
            
            weighted_Average_Probab$Total_Correctly_Used[voteIndx] <- 
               classifiers_Committee %>% filter(Vote == eachVote) %>% 
               select(Correctly_used) %>% sum( . , na.rm = TRUE)
            
            
            
            totalVoteTenAcc <- classifiers_Committee %>% filter(Vote == eachVote) %>% select(Tentative_Accuracy) %>% sum() 
            
            weighted_Average_Probab$Total_Tentative_Acc [voteIndx] <-  totalVoteTenAcc 
            
            
         }
         # compute the correctly used probability for each vote considering all the correctly used values in the classifiers_committee table  
         # Important Note: check first for Nan values (Total_Correctly_Used = 0 and all_Correctly_Used = 0) ; in such cases no multipications are required
         # and zero  will be directly assigned to Correctly_Used_Probability 
         
         if ( all_Correctly_Used != 0 ) {
            weighted_Average_Probab <-  weighted_Average_Probab %>% mutate(Correctly_Used_Probability = Total_Correctly_Used / all_Correctly_Used )
            
         } else {

            weighted_Average_Probab <-  weighted_Average_Probab %>% mutate(Correctly_Used_Probability = 0 )
            
         }
         
         
         ######### Weighted Average Probability Bank ####### 
         # A list used to collect all the weighted Average probabilities tables in order to test the classifier committee decision across the test data 
         
         aWeightedAvgToAdd <- list(weighted_Average_Probab)
         names(aWeightedAvgToAdd) <- anExample  
         
         if ( ExampleNo == 1 )  { 
            
            weightedAvgProbBank <- aWeightedAvgToAdd 
         }else {
            weightedAvgProbBank <- append(weightedAvgProbBank , aWeightedAvgToAdd )
         }
        
         ####
         ####### Weighing methods still under investigations ####
         ##### 1) using Tentative accuracy as the first criterion 
         
         # classifiersCommitteeDecision <- weighted_Average_Probab [which (weighted_Average_Probab$Total_Tentative_Acc == max(weighted_Average_Probab$Total_Tentative_Acc)), ]
         # 
         # if ( nrow(classifiersCommitteeDecision) > 1)  {
         #    classifiersCommitteeDecision  <- classifiersCommitteeDecision [which( classifiersCommitteeDecision$Correctly_Used_Probability == 
         #                                                                             max(classifiersCommitteeDecision$Correctly_Used_Probability) ) ,] 
         #    if ( nrow(classifiersCommitteeDecision ) > 1) {
         #       classifiersCommitteeDecision <- classifiersCommitteeDecision [ which( classifiersCommitteeDecision$Vote_Frequency == 
         #                                                                                max(classifiersCommitteeDecision$Vote_Frequency) ) , ] 
         #       
         #       if ( nrow(classifiersCommitteeDecision ) > 1 )  
         #          classifiersCommitteeDecision <- classifiersCommitteeDecision [1 , ]
         #    }
         #    
         # } 
         
         ##### 2) using CUR as the first criterion
         classifiersCommitteeDecision <- weighted_Average_Probab [which (weighted_Average_Probab$Correctly_Used_Probability == max(weighted_Average_Probab$Correctly_Used_Probability)), ]
         
         if ( nrow(classifiersCommitteeDecision) > 1)  {
            classifiersCommitteeDecision  <- classifiersCommitteeDecision [which( classifiersCommitteeDecision$Total_Tentative_Acc == 
                                                                                     max(classifiersCommitteeDecision$Total_Tentative_Acc) ) ,] 
            if ( nrow(classifiersCommitteeDecision ) > 1) {
               classifiersCommitteeDecision <- classifiersCommitteeDecision [ which( classifiersCommitteeDecision$Vote_Frequency == 
                                                                                        max(classifiersCommitteeDecision$Vote_Frequency) ) , ] 
               
               if ( nrow(classifiersCommitteeDecision ) > 1 )  
                  classifiersCommitteeDecision <- classifiersCommitteeDecision [1 , ]
            }
            
         } 
         
         predictedClass <- classifiersCommitteeDecision$Predicted_Class  
         
         # if ( length (votingLevels) > 1 )    END
         
      } else    predictedClass <- votingLevels
    
      validatePrediction_Ensemble <- f.checkPredictions ( predClass = predictedClass , 
                                                          realClass = currentInstance$class   )
      
      if ( validatePrediction_Ensemble == TRUE ) {
         correctlyClassifiedByRules_Ensemble <- correctlyClassifiedByRules_Ensemble + 1
         totalExmpCorrectlyClassified_Ensemble <- totalExmpCorrectlyClassified_Ensemble + 1 
         
      } else wronglyClassifiedByRules_Ensemble <- wronglyClassifiedByRules_Ensemble + 1
      
      classifiedInstanceToAdd_Ensemble  <- f.initialClassifiedInstancesTable( factorsLevels )
      classifiedInstanceToAdd_Ensemble <- classifiedInstanceToAdd_Ensemble [-5]
      
      classifiedInstanceToAdd_Ensemble$id <- anExample
      classifiedInstanceToAdd_Ensemble$class <- currentInstance$class
      classifiedInstanceToAdd_Ensemble$Predicted_Class <- predictedClass
      classifiedInstanceToAdd_Ensemble$Classification_Type <- "Rules"
      
      allClassifiedInstances_Ensemble <- f.updatePredictionsTable (classifiedInstanceToAdd_Ensemble , allClassifiedInstances_Ensemble)
      
      #  if (instanceCoveredByAnyBCRules)    END
   } else {     
      
      unClassifiedInstances_Ensemble <- unClassifiedInstances_Ensemble + 1 
      
      classifiedInstanceToAdd_Ensemble <- f.initialClassifiedInstancesTable(factorsLevels)
      classifiedInstanceToAdd_Ensemble <- classifiedInstanceToAdd_Ensemble [-5]
      
      classifiedInstanceToAdd_Ensemble$id <- anExample
      classifiedInstanceToAdd_Ensemble$class <- currentInstance$class
      classifiedInstanceToAdd_Ensemble$Predicted_Class <- PredictedClassByMajority
      classifiedInstanceToAdd_Ensemble$Classification_Type <- "Majority Class"
      
      
      allClassifiedInstances_Ensemble <- f.updatePredictionsTable (classifiedInstanceToAdd_Ensemble , allClassifiedInstances_Ensemble)
      
      
      validatePrediction_Ensemble <- f.checkPredictions ( predClass = PredictedClassByMajority , 
                                                          realClass = currentInstance$class   )
      if ( validatePrediction_Ensemble == TRUE)  {
         
         correctlyClassifiedByMajority_Ensemble <- correctlyClassifiedByMajority_Ensemble + 1
         totalExmpCorrectlyClassified_Ensemble <- totalExmpCorrectlyClassified_Ensemble + 1
         
      } else  wronglyClassifiedByMajority_Ensemble <- wronglyClassifiedByMajority_Ensemble + 1
      
   }
   
}     #     for   ( anExample in testDataRowNames)     for # 1

allClassifiedInstances_Ensemble$Predicted_Class <- factor(allClassifiedInstances_Ensemble$Predicted_Class, levels = factorsLevels)
allClassifiedInstances_Ensemble$class <- factor(allClassifiedInstances_Ensemble$class , levels =  factorsLevels)

# Precision, recall and F1 score are useful when the class labels are not uniformly distributed (e.g. most instances belong to one class).
# In such cases, accuracy could be misleading as one could predict the dominant class most of the time and still achieve a relatively high 
# overall accuracy but very low precision or recall for other classes.


allConfusionMat_Ensemble <- confusionMatrix( allClassifiedInstances_Ensemble$Predicted_Class,
                                             allClassifiedInstances_Ensemble$class,  mode = "everything"  )

# allConfusionMat_Ensemble$byClass[(is.na(allConfusionMat_Ensemble$byClass))] <- 0


# confusion matrix for the predictions that based on Rules only 
byRulesClassifiedInstances_Ensemble <- allClassifiedInstances_Ensemble %>% dplyr::filter(Classification_Type == "Rules") 
byRulesConfusionMat_Ensemble <-  confusionMatrix ( byRulesClassifiedInstances_Ensemble$Predicted_Class, 
                                                   byRulesClassifiedInstances_Ensemble$class,  mode = "everything")

# byRulesConfusionMat_Ensemble$byClass[(is.na(byRulesConfusionMat_Ensemble$byClass))] <- 0

length (which (byRulesClassifiedInstances_Ensemble$class == byRulesClassifiedInstances_Ensemble$Predicted_Class))

print ("Generating Confusion Matrcies is completed  ...")

# Display the results for Ensemble Model
finalEnsembleResults <- TRUE

# Compute the average of the number of rules libraries of the top best classifiers.
mergedRulesAvgNum <- sum (topBestClassifiersAfterMerging$Number_of_Rules) / topBestClassifiersSize

rulesAvgNum <- sum ( topBestClassifiers$Number_of_Rules) / topBestClassifiersSize   

TopRulesNumber <- list ( 'rules_Avg_Num' =  rulesAvgNum , 'merged_Rules_Avg_Num' = mergedRulesAvgNum)

topRulesAvgLength <- sum ( topBestClassifiers$Average_Rule_Length) / topBestClassifiersSize  


EnsemblePredictionCounters <- list ( "total_Exam_Correct_Classified" = totalExmpCorrectlyClassified_Ensemble ,
                                     "classified_by_Rules" = classifiedByRules_Ensemble ,
                                     "correctly_classified_byRules" = correctlyClassifiedByRules_Ensemble ,
                                     "wrongly_classified_byRules" = wronglyClassifiedByRules_Ensemble ,
                                     "correctly_classified_byMajority" = correctlyClassifiedByMajority_Ensemble ,
                                     "wrongly_classified_byMajority" = wronglyClassifiedByMajority_Ensemble ,
                                     "unclassified_Exam" = unClassifiedInstances_Ensemble )

EnsemblePredictionResults <- f.displayResults ( buildEnsembleModel , finalEnsembleResults , trainDataSize = nrow(trainData) ,testDataSize =  nrow(testData),
                                                rulesLibrarySize = TopRulesNumber , averageRuleLength = topRulesAvgLength,
                                                predictionCounters = EnsemblePredictionCounters ,
                                                allClassifiedInstances = allClassifiedInstances_Ensemble ,
                                                allConfusionMat = allConfusionMat_Ensemble ,
                                                byRulesConfusionMat = byRulesConfusionMat_Ensemble , 
                                                baseClNo = NA , applyConso = applyConsolidation)






}   #  end else part (applyConsolidation == FALSE)

if ( usingCrossValidation ) { 
   
   if (applyConsolidation == FALSE ) {
      
      crossValidationResults$Folds[cv] <- cv
      crossValidationResults$Number_of_Rules [cv] <- mergedRulesAvgNum
      crossValidationResults$Abstaining_Rate[cv] <- EnsemblePredictionResults$Abstaining_Rate
      crossValidationResults$Recall[cv] <- EnsemblePredictionResults$Rules_Recall
      crossValidationResults$Precision[cv] <- EnsemblePredictionResults$Rules_Precision
      crossValidationResults$F1Score[cv] <- EnsemblePredictionResults$Rules_F1Score
      crossValidationResults$General_Accuracy[cv] <- EnsemblePredictionResults$General_Accuracy
      crossValidationResults$Tentative_Accuracy[cv]<-EnsemblePredictionResults$Tentative_Accuray
      
   } else {
      
      crossValidationResults$Folds[cv] <- cv
      crossValidationResults$Number_of_Rules [cv] <- length(orderedConsolidatedRules)
      crossValidationResults$Abstaining_Rate[cv] <- currentPredictionResults$Abstaining_Rate
      crossValidationResults$Recall[cv] <- currentPredictionResults$Rules_Recall
      crossValidationResults$Precision[cv] <- currentPredictionResults$Rules_Precision
      crossValidationResults$F1Score[cv] <- currentPredictionResults$Rules_F1Score
      crossValidationResults$General_Accuracy[cv] <- currentPredictionResults$General_Accuracy
      crossValidationResults$Tentative_Accuracy[cv]<-currentPredictionResults$Tentative_Accuray
      
   }
   
   
   
   
   if ( cv < foldsNum ) {
      cv <- cv + 1
      
      #Segement  data by fold using the which() function 
      testIndexes <- which(folds== cv , arr.ind=TRUE)
      testData <- myData [ testIndexes , ]
      trainData <- myData [ - testIndexes , ] 
      cat ( '\n Cross validation fold : ', cv , '\n')
      finalEnsembleResults <- FALSE
      next()
      
   }else {
      cat ( '\n' , foldsNum ,  'folds cross validation results of the ensembel (ReG-Rules) classifier for ', fileName , ' dataset \n \n' )
      crossValidationResults[ foldsNum + 1 ,'Folds'] <- 0 
      crossValidationResults[ foldsNum + 1 ,'Number_of_Rules'] <- mean(crossValidationResults[1:foldsNum,'Number_of_Rules']) 
      crossValidationResults[ foldsNum + 1 ,'Abstaining_Rate'] <- mean(crossValidationResults[1:foldsNum,'Abstaining_Rate']) 
      crossValidationResults[ foldsNum + 1 ,'Recall'] <- mean(crossValidationResults[1:foldsNum,'Recall']) 
      crossValidationResults[ foldsNum + 1 ,'Precision'] <- mean(crossValidationResults[1:foldsNum,'Precision']) 
      crossValidationResults[ foldsNum + 1 ,'F1Score'] <- mean(crossValidationResults[1:foldsNum,'F1Score']) 
      crossValidationResults[ foldsNum + 1 ,'General_Accuracy'] <- mean(crossValidationResults[1:foldsNum,'General_Accuracy']) 
      crossValidationResults[ foldsNum + 1 ,'Tentative_Accuracy'] <- mean(crossValidationResults[1:foldsNum,'Tentative_Accuracy'])
      
      print (crossValidationResults)
      
      break
   }


if ( usingCrossValidation & cv < 5) { 
   
   # collect the folds results in one table
   
   cv <- cv + 1
   
   #Segement  data by fold using the which() function 
   testIndexes <- which(folds== cv , arr.ind=TRUE)
   testData <- myData [ testIndexes , ]
   trainData <- myData [ - testIndexes , ] 
   
} else {
   break }
} else  stop()



}     # End of repeat function 

#### Final results using Cross validation will show here


#  End of Coding








#  
############ Visualisation ###### 
# writing the results in excel file for visualisation purposes

# if  ( file.exists ("myworkbook.xlsx")) {
#    
#    unlink("myworkbook.xlsx", recursive = TRUE) 
#    
# } else {
#    
#    write.xlsx(EnsembleModel, file = "myworkbook.xlsx", sheetName = "EnsembleModel", col.names = TRUE, 
#               row.names = TRUE, append = FALSE)
#    
#    
#    write.xlsx(topBestClassifiers, file = "myworkbook.xlsx", sheetName = "topBC",  col.names = TRUE, 
#               row.names = TRUE, append = TRUE)
#    
#    write.xlsx(topBestClassifiersAfterMerging, file = "myworkbook.xlsx", sheetName = "afterMergingTopBC",  
#               col.names = TRUE, row.names = TRUE, append = TRUE)
#    
# }


# confusionMatBank[[1]][[]]
# 
# library(gplots)
# library(PRROC)
# confusionClasses <- nrow(ConfusionMatrices$ByRules_Confusion_Matrix$byClass)
# sen1 <- sum(ConfusionMatrices$ByRules_Confusion_Matrix$byClass[,1]) / confusionClasses
# spec1 <-  1- (sum(ConfusionMatrices$ByRules_Confusion_Matrix$byClass[,2]) / confusionClasses)
# roc <- roc.curve ( scores.class0 = as.numeric( specAverage) , scores.class1 = as.numeric(senAverage), curve = TRUE)
# plot(roc)
# allConfusionMat_Ensemble$byClass[(is.na(allConfusionMat_Ensemble$byClass))] <- 0
# # create vectors of tpr and fpr scores to use them in the ROC curve plotting.
# senAverage <- c ()
# specAverage <- c()
# for ( rc in 1 : numBaseClassifiers ) {
#  confusionMatBank[ (is.na(confusionMatBank[[3]]))]  <- 0
#  senAverage  <- c ( senAverage ,  sum(confusionMatBank[[rc]][,1], na.rm = TRUE) / confusionClasses)
#  specAverage <- c ( specAverage , 1 - ( sum(confusionMatBank[[rc]][,2], na.rm = TRUE) / confusionClasses))
#  
#    
# }
#plot(senAverage, specAverage)
