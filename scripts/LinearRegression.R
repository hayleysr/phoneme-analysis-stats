setwd("D:\\26 WAYNE\\3.2\\Data Science\\Project\\RawAcapellaData\\csv\\clean")
remove(list=ls())

songTitle = "Blank Space - Taylor Swift"
data = read.csv("blankspace-taylorswift-clean.csv", header=T, stringsAsFactors=T)
data$phoneme = gsub("\\t", "", data$phoneme)
data = data[data$phoneme != "",]

plot(data$frequency ~ data$bin, lty=1)

# Descriptive Statistics:
#   Split all phonemes into separate categories
unique_phonemes = unique(data$phoneme)
for(phoneme in unique_phonemes){
  data[[phoneme]] = ifelse(data$phoneme == phoneme,1,0)
}

# Boxplot of each phoneme's pitch
#first we're gonna need a matrix
byphoneme = split(data$frequency, data$phoneme)
boxplot(byphoneme, main=songTitle, xlab="Phoneme", ylab="Frequency of occurrences")


# First question: Do certain phonemes correspond to higher or lower pitches?
#logistic regression
GLM = glm(as.factor(data$frequency) ~ data$phoneme, binomial)
summary(GLM)

# Second question: Are phoneme and pitch linearly related?
# Generate matrix with a couple permutations of each phoneme combo
unique_phonemes = unique(data$phoneme)
# Permutation 1: Low to High, Back to Front
permutation1 = c("aobr", "aobu", "aofu", "apfu", "oibp", "uibu", "eifu", "ombr",
                 "umcu", "emfu", "udbu", "edfu", "ulbp", "ilfu", "rlfp", "ucbp", 
                 "uccp", "icfu")
phoneme1 = vector(length=length(data$phoneme))
for(i in seq_along(data$phoneme)){
  index = which(permutation1 == data$phoneme[i])
  phoneme1[i] = ifelse(length(index) > 0, index, NA)
}
data$phoneme1 = phoneme1
LM1 = lm(data$phoneme1 ~ data$frequency)
summary(LM1)
plot(data$phoneme1 ~ data$frequency, main=songTitle, xlab="Phoneme", ylab="Note Freq")
abline(LM1, col="red", lwd=3)

# Permutation 2: Low to High
p2_val = c("aobr", "aobu", "aofu", "apfu", "oibp", "uibu", "eifu", "ombr",
           "umcu", "emfu", "udbu", "edfu", "ulbp", "ilfu", "rlfp", "ucbp", 
           "uccp", "icfu")
p2_index = c(1, 1, 1, 1, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7)
permutation2 = data.frame(values = p2_val, index = p2_index)

phoneme2 = vector(length=length(data$phoneme))
for(i in seq_along(data$phoneme)){
  index = permutation2$index[which(permutation2$values == data$phoneme[i])]
  phoneme2[i] = ifelse(length(index) > 0, index, NA)
}
data$phoneme2 = phoneme2
LM2 = lm(data$phoneme2 ~ data$frequency)
plot(data$phoneme2 ~ data$frequency, main=songTitle, xlab="Phoneme", ylab="Note Freq")
abline(LM2, col="red", lwd=3)
summary(LM2)
# Permutation 3: Back to Front
p3_val = c("aobr", "aobu", "aofu", "apfu", "oibp", "uibu", "eifu", "ombr",
           "umcu", "emfu", "udbu", "edfu", "ulbp", "ilfu", "rlfp", "ucbp", 
           "uccp", "icfu")
p3_index = c(1, 1, 3, 3, 1, 1, 3, 1, 2, 3, 3, 1, 1, 3, 3, 1, 2, 3)
permutation3 = data.frame(values = p3_val, index = p3_index)

phoneme3 = vector(length=length(data$phoneme))
for(i in seq_along(data$phoneme)){
  index = permutation3$index[which(permutation3$values == data$phoneme[i])]
  phoneme3[i] = ifelse(length(index) > 0, index, NA)
}
data$phoneme3 = phoneme3
LM3 = lm(data$phoneme3 ~ data$frequency)
plot(data$phoneme3 ~ data$frequency, main=songTitle, xlab="Phoneme", ylab="Note Freq")
abline(LM3, col="red", lwd=3)
summary(LM3)

# Permutation 4: AEIOUR
p4_val = c("aobr", "aobu", "aofu", "apfu", "oibp", "uibu", "eifu", "ombr",
           "umcu", "emfu", "udbu", "edfu", "ulbp", "ilfu", "rlfp", "ucbp", 
           "uccp", "icfu")
p4_index = c(1, 1, 1, 1, 4, 5, 2, 4, 5, 2, 2, 5, 5, 3, 6, 5, 5, 3)
permutation4 = data.frame(values = p4_val, index = p4_index)

phoneme4 = vector(length=length(data$phoneme))
for(i in seq_along(data$phoneme)){
  index = permutation4$index[which(permutation4$values == data$phoneme[i])]
  phoneme4[i] = ifelse(length(index) > 0, index, NA)
}
data$phoneme4 = phoneme4
LM4 = lm(data$phoneme4 ~ data$frequency)
plot(data$phoneme4 ~ data$frequency,main=songTitle, xlab="Phoneme", ylab="Note Freq")
abline(LM4, col="red", lwd=3)
summary(LM4)

# TODO: MLR
# TODO: check independence?
LM_multi = lm(data$frequency ~ data$phoneme1 + data$phoneme2 + data$phoneme3)
summary(LM7)
# TODO: throw in another song as a prediction set?
PredError = function(x){
  sqrt(mean(predict(x, TestSet) - TestSet$Price)^2)
}
PredError(LM1)
PredError(LM5)
PredError(LM6)
PredError(LM7)
plot(data$phoneme6 ~ data$frequency)
abline(LM7, col="red", lwd=3)

# Check residuals
qqnorm(resid(LM1))
qqline(resid(LM1), col="red")
qqnorm(resid(LM2))
qqline(resid(LM2), col="red")
qqnorm(resid(LM3))
qqline(resid(LM3), col="red")
qqnorm(resid(LM4))
qqline(resid(LM4), col="red")

#Third question: Are phoneme and change in pitch related?
  # Assume that we're using the top-to-bottom relation of the data set
#Step 1: make a version of the data that only includes when the pitch changes 
  #by a half step or greater (check if letter is different)
data_change = data.frame(frequency = numeric(length(data$frequency) - 1),
                          phoneme = numeric(length(data$phoneme2) - 1))
for (i in seq_len(length(data$frequency) - 1)) {
  data_change$frequency[i] = data$frequency[i + 1] - data$frequency[i]
  data_change$phoneme[i] = data$phoneme2[i + 1] - data$phoneme2[i]
}
data_change = data_change[!is.na(data_change$frequency),]
data_change = data_change[data_change$phoneme != 0,]
data_change = data_change[data_change$frequency >= 5 | data_change$frequency <= -5,]

LM5 = lm(data_change$phoneme ~ data_change$frequency)
plot(data_change$phoneme ~ data_change$frequency)
abline(LM5, col="red", lwd=3)
summary(LM5)
