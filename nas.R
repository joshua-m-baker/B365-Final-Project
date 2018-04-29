library("ggplot2")
options(scipen=100)
train <- read.csv("train.csv", header = TRUE)
str(train)
ncol(train)
t = train
claim = as.numeric(table(t$target)[2])
t[t<0] = -1
# There are 59 variables
# There two data types integer and numberic, with integer values being the prodominate type. 49 to 10
#

# Keep track of features with missing entries
missing = c()
par(mfrow=c(3,2))
# iterate through all features
for(f in colnames(train)){
  # Find number of missing entries
  missing_len = length(train[train[,f] <0,f])
  
  # If there are missing entries find additional info
  if(missing_len > 0){
    
    # Append to missing vector
    missing = append(missing, f)
    
    # Calculate missing percentage vs all rows
    missing_perc = missing_len / nrow(t) * 100
    # Calculate the precentage of values in the feature that are both missing and have a target id of 1
    target_perc = nrow(subset(t, t[f]<0 & t['target']==1))/length(t[t[,f] <0,f]) *100
    target_perc2 = nrow(subset(t, t[f]<0 & t['target']==1))/claim *100 
    print(c(f, missing_len, missing_perc, target_perc2))
    print(class(t[,f]))   # Prints the datatype of feature
    
    # plot the histogram of entries and convert to probabilties instead of frequencies
    h <- hist(t[,f], plot=FALSE)
    h$counts=h$counts/sum(h$counts)
    plot(h, main=f, xlab= "Value",ylab="Probability")
    
  }
}

# There are 13 features with missing entries

# First Variables with really high percentage of missing values
# Two main features stick out: "ps_car_03_cat" and "ps_car_05_cat". With 69% and 44.8%`missing entries respectively.
# Both features are binary categorical variables describing the car that's being insured. Given the fact this is a binary
# variable that describes the car and that theres an overwhelming percentage of missing values we thought it might be best to delete these features.
# As a quick check before we remove them, we examine the percentage of missing values with the target value of 1 vs the number of total claims.
# Surprisingly, "ps_car_03_cat" feature with 69.1% of it's entries missing with the target 1 accounts for 62% of the total claims! Same goes for
# "ps_car_05_cat" feature with 44.8% of it's entries missing with the target 1 accounts for 39% of the total claims! Here it seems that the value missing
# can be an important feature in predicting claims. Therefore, we replace -1 with 3 and set NA's to their own category. Changing the binary value to an ordinal to perserve 
# the no response/data characterist of our data. 
# Note: 2 was decided vs -1 to remove the negative value

#Feature            # of missing       % of entries missing       % missing claim vs total claim
#"ps_car_03_cat"    "411231"           "69.0898368984496"        "61.9987093205495"
#"ps_car_05_cat"    "266551"           "44.7825312661707"        "38.9600811284226"

# Convert -1 to 2
for(feature in c("ps_car_03_cat","ps_car_05_cat")){
  num = length(t[t[,feature] <0,feature])
  t[t[,feature] <0,feature] = rep(2,num)
}

# Second other Catergorical values
# Feature            # of missing       % of entries missing     % missing claim vs total claim
# "ps_ind_02_cat"      "216"            "0.0362895909356666"     "0.184382778648474" 
# "ps_ind_04_cat"      "83"             "0.0139446113317608"     "0.152115792384991" 
# "ps_ind_05_cat"     "5809"              "0.975954785857812"    "2.23103162164654" 
# "ps_car_07_cat"    "11489"            "1.93023662157349"       "4.13939338065825"
# "ps_car_09_cat"      "569"            "0.0955961909370107"     "0.239697612243016" 
# "ps_car_01_cat"      "107"                "0.0179767881023904" "0.156725361851203" 
# All of these features are catergorical, with low % of missing entries (< 2%) and main very low % missing entry claim vs total claim.
# Therefore, we will replace these missing values from a discrete random variable model after the feature. This samples the missing values from a distribution
# matching the actual values of the feature.

par(mfrow=c(1,3))
# This function creates a Discrete Random Variable DRV
# This assigns the a probabilty to each actual value, uses these probabilites to create a DRV and then replaces missing values from the DRV  
randomVariable = function(feature){
  values = c()              # Stores the discrete values
  
  # Find distinct values using table
  vals = as.data.frame(table(t[feature]))
  
  probs = c()               # Stores the probabilities of each discrete value
  base = nrow(t)            # Base number of values in feature
  
  # Iterate through values
  for(i in c(1:nrow(vals))){
    val = as.numeric(as.character(vals[i,1]))
    if(val <0){
      base = base - vals[i,2] 
    } else {
      values = append(values,val)
      probs = append(probs,as.numeric(as.character(vals[i,2]))/base) 
    }
  }
  
  print(values)
  new_vals = sample(x = values, size = vals[1,2], replace = T, prob = probs)
  t[t[,feature] <0,feature] = new_vals
  if(1){
    print(ggplot(t, aes(x= t[, feature])) + geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth=.5, colour="black", fill="white")+
            labs(x = "Value", y = "Probabilty", title="Probability Distribution of Train before"))
    print(ggplot() + aes(as.vector(new_vals)) + geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth=.5, colour="black", fill="white")+
            labs(x = "Value", y = "Probabilty", title="New Values"))

    print(ggplot(t, aes(x= t[, feature])) + geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth=.5, colour="black", fill="white")+
            labs(x = "Value", y = "Probabilty", title="Train with replaced values"))
  }
  print(sum(t[,feature] < 0))
  return(t)
}

# Replace values from a feature based DRV
for(feature in c("ps_ind_02_cat","ps_ind_04_cat","ps_ind_05_cat","ps_car_01_cat","ps_car_07_cat","ps_car_09_cat")){
  t<-randomVariable(feature)
}

# Third Countinous numeric values, range beteen 0 and 1
# Feature            # of missing       % of entries missing     % missing claim vs total claim
# "ps_car_14"        "42620"            "7.16047391517644" "7.94228819028303"
# This single continous feature has a value between 0 and 1, The fact there's a low number of missing entries for this feature (7.2%) combined with a 
# low % of missing that are a claim vs total claim, makes this a good candidate to replace missing with valus drawn from a Continous Random Variable
# model after the distribution of the feature itself. This feature has a lognormal distribution
contRandVar = function(feature){
  
  actual = t[t[,feature]>=0,feature]
  print(c(mean(actual),sd(actual)))
  hist(actual, main="Actual")
  log_actual = log10(actual)
  hist(log_actual, main="Log10 Actual")
  
  ## Find expected value and standard deviation used in rlnorm with the log10 of the non-missing values
  expected = mean(log_actual)
  stand = sd(log_actual)
  location = log(expected^2/sqrt(stand^2+expected^2))
  shape = sqrt(log(1+(stand^2/expected^2)))
  
  # Create a test sample to check if valid/close
  test = rlnorm(n=50000,location-.1, shape)
  
  # Visual inspection of the test to the actual
  print(c(mean(test),sd(test), class(test)))
  hist(actual, main="Actual",
  prob = TRUE, # show densities instead of frequencies
  xlab = "Value")
  lines(density(test), # density plot
        lwd = 2, # thickness of line
        col = "chocolate3")
  
  # Replace missing entries
  num = length(t[t[,feature]<0,feature])
  print(num)
  hist(t[,feature])
  new_vals = rlnorm(n=num,location-.1, shape)
  t[t[,feature]<0,feature] = new_vals
  hist(t[,feature])
  
  # plot graphs for visual check
  hist(actual, main="Missing Replaced",
       prob = TRUE, # show densities instead of frequencies
       xlab = "Value")
  lines(density(new_vals), # density plot
        lwd = 2, # thickness of line
        col = "chocolate3")
  return(t)
}

t<-contRandVar("ps_car_14")
t<-contRandVar("ps_reg_03")



# Fourth Features with very low missing entries
# Feature            # of missing       % of entries missing     % missing claim vs total claim
# "ps_car_02_cat"        "5"                    "0.000840036827214505" "0"  
# "ps_car_11"            "5"                    "0.000840036827214505" "0" 
# "ps_car_12"            "1"                    "0.000168007365442901" "0" 
# The number of missing entries are so low we'll just replace with the mode

#Credit - Ken Williams https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_repl = function(feature){
  # Non-missing values
  actual = t[t[,feature]>=0,feature]
  # replace value is set to mode
  replace = Mode(actual)
  # number of missing
  num = length(t[t[,feature]<0,feature])
  # Repalce values
  t[t[,feature]<0,feature] = rep(replace*num)
  return(t)
}

# Replace the missing values with mode
for(feature in c("ps_car_11","ps_car_12","ps_car_02_cat")){
  t<-mode_repl(feature) 
}

summary(t)
write.table(t, "train_p.csv", sep= ",")

