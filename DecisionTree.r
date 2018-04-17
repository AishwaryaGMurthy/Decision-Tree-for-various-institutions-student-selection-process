setwd("/Users/aishwaryamurthy/Downloads/itu/ai/Signature Assignment")
Train_Data<-read.csv("Training Data.csv",header = TRUE)
Test_Data<-read.csv("Test Data.csv",header = TRUE)

#Train_Data[,-which(names(Train_Data)=="Timestamp") && which(names(Train_Data)=="Name") && which(names(Train_Data)=="UniversityApplied") && which(names(Train_Data)=="Date.of.Application") && which(names(Train_Data)=="Date.of.Decision") && which(names(Train_Data)=="IELTS") && which(names(Train_Data)=="Under.Graduate.Aggregate") && which(names(Train_Data)=="Scale") && which(names(Train_Data)=="Undergrad.University")]
# We have to ignore the following columns as they won't help much in our algorithm, those attributes are:
# Timestamp, Name, UniversityApplied, Date.of.Application, Date.of.Decision,Major
# IELTS - because most of the IELTS value is NA, and colleges seem to consider TOEFL & IELTS, so we can ignore IELTS and consider only TOEFL
# Under.Graduate.Aggregate, Scale & Undergrad.University - because this value is substituted by Percentage attribute
Train_Data <- Train_Data[,c(-1,-2,-3,-4,-6,-7,-12,-15,-16,-17)]
#So, currently the attributes of Train Data are displayed below
names(Train_Data)
# To make the attributes of Test Data and Training data to be the same, we omit GRE_Quants, Work.Ex & International Papers
#Eventhough, they seem to be great attributes to predict the selection process
Train_Data <- Train_Data[c(-3,-6,-7)]
#Omit the NA values
Train_Data <- na.omit(Train_Data)
Test_Data <- na.omit(Test_Data)
#Change the Result in ternms of 0s & 1s - Accept: 1, Reject:0
Train_Data$Result = factor(Train_Data$Result,
                             levels = c('Accept', 'Reject'),
                             labels = c(1, 0))

# Same steps are repeated for Test Data
# We ignore IELTS in test data
Test_Data <- Test_Data[,c(-5)]
#Change the Result in ternms of 0s & 1s - Accept: 1, Reject:0
Test_Data$Result = factor(Test_Data$Result,
                           levels = c('Accept', 'Reject'),
                           labels = c(1, 0))

#Convert the Result - a numeric attribute to a set of class to find the subset of the data based on the class 'Result' - Accept or Reject
Train_Data$Result<-as.factor(Train_Data$Result)
Classes<-levels(Train_Data$Result) # We have classes 0 - Reject & 1 - Accept

checkifSameClass <- function(Res){
  if(length(unique(Res))==1){
    #cat(sprintf("All Data belong to class %d",unique(Res)$Result)) 
    return (unique(Res))
  }
  return (-1)
}

isTerminated <- function(subgroup){
  getUniqueValue = checkifSameClass(subgroup$Result)
  if(getUniqueValue != -1){
    #print(getUniqueValue$Result)
    return (getUniqueValue)
  }else{
    if(sum(subgroup["Result"]== 0) > (sum(subgroup["Result"] == 1))){
      return (0)
    }
    else{
      return(1)
    }
  }
  return(1)
}

#Find the Information Gain of all attributes
findIG <- function(Classes,groups){
count <- matrix(0L, nrow = length(Classes), ncol = 1)
true_count <- matrix(0L, nrow = length(Classes), ncol = 1)
false_count <- matrix(0L, nrow = length(Classes), ncol = 1)
entropy <- matrix(0L, nrow = length(Classes), ncol = 1)
total <- nrow(groups[[1]])+nrow(groups[[2]])
Ptrue <- 0
Pfalse <- 0
IG <- 0 # Information Gain
c=1

# False Group - Result = Reject
false_count[0,1] = sum(groups[[1]]$Result == 0)
# False Group - Result = Accept
false_count[1,1] = sum(groups[[1]]$Result == 1)
# True Group - Result = Reject
true_count[0,1] = sum(groups[[2]]$Result == 0)
# False Group - Result = Accept
true_count[1,1] = sum(groups[[2]]$Result == 1)

#Total count of 0th class
count[0,1] <- false_count[0,1] + true_count[0,1]
#Total count of 1st class
count[1,1] <- false_count[1,1] + true_count[1,1]

while(c <= length(Classes)){
  #Probability of true 
  Ptrue <- true_count[c,1]/total
  #Probability of false
  Pfalse <- false_count[c,1]/total
  if(Ptrue !=0 && Pfalse !=0){
    l2T = log2(Ptrue)
    l2F = log2(Pfalse)
  }else{
    l2T = 0 
    l2F = 0
  }
  entropy[c,1]=-(Ptrue * l2T) - (Pfalse* l2F)
  #Find the Avg Entropy
  IG <- IG + ((count[c,1]/total) * entropy[c,1])
  c= c+1
}
return(IG)
}
chooseBestAttribute <- function (Information_Gains){
  index = which(Information_Gains == max(Information_Gains))+1
  return(index)
}

Information_Gains = vector()
splitValues <- vector(mode = "list", length = 5)
names(splitValues) <- c("index","value","groups","left","right")
getSplit <- function(Train_Data){
for(iter in 2:ncol(Train_Data)){
    value <- round(mean(Train_Data[,iter]),1)
    groups <- split(Train_Data, Train_Data[,iter] < value)
    Information_Gains[iter] <- findIG(Classes,groups)
    Information_Gains <- na.omit(Information_Gains)
    #groups <- split(Train_Data, Train_Data[,index] < value)
    #left <- groups$'FALSE'
    #right <- groups$'TRUE'
    # groups[[1]] <- groups[[1]][-index]
    # groups[[2]] <- groups[[2]][-index]
    #splitValues[[4]] <- left
    #splitValues[[5]] <- right
}
  index = chooseBestAttribute(Information_Gains)
  if(length(index)>1){
    return()
  }
  value <- round(mean(Train_Data[,index]),1)
  groups <- split(Train_Data, Train_Data[,index] < value)
  #print(groups[[1]])
  # print(index)
  # print(names(Train_Data)[index])
  cat(sprintf("%s < %f\n",names(Train_Data)[index],value))
  for(l in 1:length(groups)){
    groups[[l]] <- groups[[l]][-index]
  }
  splitValues[[1]] <- index
  splitValues[[2]] <- value
  splitValues[[3]] <- groups
  return(splitValues)
}

Split <- function(splitValues, min_size, max_depth, depth){
  cat(sprintf("depth:%s\n",depth))
  left = splitValues$groups$'FALSE'
  right = splitValues$groups$'TRUE'
  if ((nrow(left) == 0) || (nrow(right) == 0) || is.null(left) || is.null(right)){
    # splitValues$left = isTerminated(left + right)
    # splitValues$right = isTerminated(left + right)
    #print("func 1")
    if(is.null(left)){
      print("Reached best split in Right Tree")
      if (isTerminated(right) == 1)
        print("Final Status: Accepted")
      else
        print("Final Status: Rejected")
      splitValues$right = isTerminated(right)
      #print(splitValues$right)
    }else{
      print("Reached best split in Left Tree")
      if (isTerminated(left) == 1)
        print("Final Status: Accepted")
      else
        print("Final Status: Rejected")
      splitValues$left = isTerminated(left)
      #print(splitValues$left)
    }
    return()
  }
  if (depth >= max_depth){
     splitValues$left = isTerminated(left)
     splitValues$right = isTerminated(right)
     print("Reached best split in Left Tree:")
     if (isTerminated(left) == 1)
       print("Final Status: Accepted")
     else
       print("Final Status: Rejected")
     print("Reached best split in Right Tree:")
     if (isTerminated(right) == 1)
       print("Final Status: Accepted")
     else
       print("Final Status: Rejected")
    return()
  }
  if(nrow(left) > min_size){
    print("Left side:")
    splitValues$left = getSplit(left)
    Split(splitValues$left, min_size,max_depth, depth+1)
  }else{
    splitValues$left = isTerminated(left)
  }
  if(nrow(right) > min_size){
    print("Right side:")
    splitValues$right = getSplit(right)
    Split(splitValues$right, min_size,max_depth, depth+1)
  }else
    splitValues$right = isTerminated(right)
}
  
min_size = 1
max_depth = ncol(Train_Data)-2
depth = 1
splitValues = getSplit(Train_Data)
Split(splitValues, min_size, max_depth, depth)

#Applying the model to Test Data     
#Convert the Result - a numeric attribute to a set of class to find the subset of the data based on the class 'Result' - Accept or Reject
Test_Data$Result<-as.factor(Test_Data$Result)
Classes<-levels(Test_Data$Result) # We have classes 0 - Reject & 1 - Accept

predictedResult <- matrix(0L, nrow = nrow(Test_Data), ncol = 1)

for (row in 1:nrow(Test_Data)){
  if(Test_Data[row, which(names(Test_Data) == "TOEFL")] < 91.1){
    if(Test_Data[row, which(names(Test_Data) == "Percentage")] < 77.71){
      if(Test_Data[row, which(names(Test_Data) == "GRE")] < 298.8){
        predictedResult[row, 1] = 0
      }else#for Gre 298
        predictedResult[row, 1] = 0
    }else{#for percentage 77
      if(Test_Data[row, which(names(Test_Data) == "GRE")] < 302.8){
        predictedResult[row, 1] = 0
      }else #for Gre 302
        predictedResult[row, 1] = 1
    }#right side completed
  }else{#starting left side
    if(Test_Data[row, which(names(Test_Data) == "Percentage")] < 79.4){
      if(Test_Data[row, which(names(Test_Data) == "GRE")] < 300.6){
        predictedResult[row, 1] = 0
      }else #for GRE 300
        predictedResult[row, 1] = 1
    }else{#for percent 79
      if(Test_Data[row, which(names(Test_Data) == "AWA")] < 4.1){
        predictedResult[row, 1] = 1
      }else# for awa 4
        predictedResult[row, 1] = 1
    }
  }#end else for left side
}# ended for loop
# Error Value
error = 1- sum(predictedResult == Test_Data$Result)/nrow(Test_Data)

sprintf("Applying Decision Tree Model to Test Data, we get error as: %f",error)