#Author:  Syeda Rija Zaineb
#Date: October 29, 2021

#Assignment 5: Principal Component Analysis (PCA) and Factor Analysis in R

#Using Big Five Personality Test web-based personality assessment dataset

#Libraries
library(DescTools)
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartlett's Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions
library(ade4) #PCA Visualizations
##############################################################################################

#Read in Datasets

big_dataset <- read.csv(file="Big5.csv", header=TRUE, sep=",")

#Check Sample Size and Number of Variables
dim(big_dataset)
#19719 -Sample Size and 50 variables

#Show for first 6 rows of data
head(big_dataset)

names(big_dataset)

################################################################################################
#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(big_dataset))
#0 missing values (0 cells with missing data)


#Treat Missing Values

#Listwise Deletion
#big_dataset_2 <- na.omit(big_dataset)

#Check new data has no missing data
#sum(is.na(big_dataset_2))

#################################################################################################################

#Show Structure of Dataset
str(big_dataset, list.len=ncol(big_dataset))

str(big_dataset)

#Show column Numbers
names(big_dataset)

#Shapiro-Wilk test (<1,000 sample size)
#shapiro.test(big_dataset$E1)

#Anderson-Darling Test (1,000-2,000 Sample Size)
library(DescTools)

AndersonDarlingTest(big_dataset$E1)

#Jarque-Bera test (>3,000 Sample Size)

JarqueBeraTest(big_dataset$E1)

library(psych)
describe(big_dataset)

#########################################################
#Exploratory Analysis Graphing

#In order to use the graphing functions, you need to chunk the data into smaller subsets of data.
names(big_dataset)
#GGpairs
p2 <- ggpairs(big_dataset[,c(1:10)])
p2
ggsave("p2.png")

p3 <- ggpairs(big_dataset[,c(11:20)])
p3
ggsave("p3.png")

p4 <- ggpairs(big_dataset[,c(21:30)])
p4
ggsave("p4.png")

p5 <- ggpairs(big_dataset[,c(31:40)])
p5
ggsave("p5.png")

p6 <- ggpairs(big_dataset[,c(41:50)])
p6

# To save the ggplot as png
ggsave("p6.png")

#Boxplots
boxplot(big_dataset[,1:10],data=big_dataset, main="Extraversion", xlab="Extraversion", col = c("blue","green"))


#Check for Multicollinearity with Correlations
B<-cor(big_dataset, method="spearman")
B

round(B,2)

#big5
corrplot(cor(B,method="spearman"), method = "number", type = "lower")
corrplot(cor(B,method="spearman"), type = "lower")

#GGplot Correlation
ggcorr(big_dataset[,1:20], method = c("pairwise","spearman"), label=TRUE)

ggcorr(big_dataset, method = c("pairwise","spearman"))

# Run a correlation test to see how correlated the variables are.  Which correlations are significant
options("scipen"=100, "digits"=3)

round(cor(big_dataset), 2)
BCorrTest = corr.test(big_dataset, adjust="none")
BCorrTest

B = BCorrTest$p
B
round(B,2)

# Now, for each element, see if it is < .01 (or whatever significance) and set the entry to 
# true = significant or false
BTest = ifelse(B < .01, T, F)
BTest

# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix
colSums(BTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)



corrplot(cor(big_dataset, method = "spearman"))

##################################################
#PCA/FA
##################################################

#Test KMO Sampling Adequacy

library(psych)
KMO(big_dataset)
#Overall MSA =  0.91

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(big_dataset)
#p-value < 2.22e-16 (Very Small Number)

library (stats)
bartlett.test(big_dataset)
library(psych)
describe(big_dataset)

#Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(big_dataset, check.keys = TRUE)
#raw_alpha = 0.88

##################################################

#######################################################
#Parallel Analysis (Horn's parallel analysis)

#Created a Psychologist John L. Horn in 1965

#Closest to Heuristic Determination of Number of Components or Factors

#Compares actual eigenvalues with ones from a Monto-Carlo simulated dataset of
#the same size

#Dependent upon sample size, correlation coefficient, and how items fall on 
#components

library(psych)

comp <- fa.parallel(big_dataset)
comp


#######################################################

#######################################################
#Create PCA
p = prcomp(big_dataset, center=T, scale=T)
p

#Check Scree Plot
plot(p)
abline(1, 0)

#Check PCA Summary Information
summary(p)
print(p)

########################################################

# The Psych package has a wonderful PCA function that allows many more options
# including build-in factor rotation, specifying a number of factors to include 
# and automatic "score" generation

#Best Way to Conduct PCA Analysis

#music2<-music[,3:19]
#names(music2)
#names(music)

p2 = psych::principal(big_dataset, rotate="varimax", nfactors=5, scores=TRUE)
p2

p3<-print(p2$loadings, cutoff=.39, sort=T)

names(big_dataset)
big_dataset1 <- big_dataset[,c(1:13, 15:50)]
names(big_dataset1)

p4 = psych::principal(big_dataset1, rotate="varimax", nfactors=5, scores=TRUE)
p4
p5<-print(p4$loadings, cutoff=.39, sort=T)

#PCAs Other Available Information

ls(p2)

p2$values
p2$communality
p2$rot.mat

p4$values
sort(p4$communality)
p4$rot.mat

########################################################################################
#Calculating scores

scores <- p2$scores

cor(scores)

summary(scores)

scores_1 <- scores[,1]

min_score <- min(scores_1)
min_score

max_score <- max(scores_1)
max_score

summary(scores_1)

scores_2 <- scores[,2]
scores_3 <- scores[,3]
scores_4 <- scores[,4]
scores_5 <- scores[,5]

summary(scores_2)
summary(scores_3)
summary(scores_4)
summary(scores_5)

########################################################################################
#Calculating scores

scores <- p4$scores

cor(scores)

summary(scores)

scores_1 <- scores[,1]

min_score <- min(scores_1)
min_score

max_score <- max(scores_1)
max_score

summary(scores_1)

scores_2 <- scores[,2]
scores_3 <- scores[,3]
scores_4 <- scores[,4]
scores_5 <- scores[,5]

summary(scores_2)
summary(scores_3)
summary(scores_4)
summary(scores_5)

#Conducting Factor Analysis

fit = factanal(big_dataset, 5)
print(fit$loadings, cutoff=.31, sort=T)

names(big_dataset1)
big_dataset1 <- big_dataset[,c(1:13, 15:50)]
fit = factanal(big_dataset, 5)
print(fit$loadings, cutoff=.39, sort=T)

fit = factanal(big_dataset1, 5)
print(fit$loadings, cutoff=.39, sort=T)

#######################################################################################
#Additional PCA Visualizations

#Using Factoextra
library(factoextra)

p3 <- prcomp(big_dataset, scale = TRUE) 
fviz_eig(p3)

#PCA Individuals
pI<-fviz_pca_ind(p3,
                 col.ind = "cos2", # Color by the quality of representation
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE     # Avoid text overlapping
)
pI

#PCA Variables
pca_var<-fviz_pca_var(p3,
                      col.var = "contrib", # Color by contributions to the PC
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE     # Avoid text overlapping
)

pca_var

#Biplot
bi_plot<-fviz_pca_biplot(p3, repel = TRUE,
                         col.var = "#2E9FDF", # Variables color
                         col.ind = "#696969"  # Individuals color
)

bi_plot

library("FactoMineR")
p4 <- PCA(big_dataset, graph = FALSE)
#IF graph is set to true, it will provide the individual and variable maps

#Shows all the objects or functions available in PCA
print(p4)

#Options for providing screeplot
fviz_eig(p4, addlabels = TRUE, ylim = c(0, 35))
fviz_screeplot(p4, addlabels = TRUE, ylim = c(0, 35))

variables <- get_pca_var(p4)
variables

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables$contrib)

library("corrplot")
corrplot(variables$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(p4, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(p4, choice = "var", axes = 2, top = 10)


library(ade4)
p5 <- dudi.pca(big_dataset,
               scannf = FALSE,   # Hide scree plot
               nf = 4          # Number of components kept in the results
)
fviz_screeplot(p5, addlabels = TRUE, ylim = c(0, 35))

variables2 <- get_pca_var(p5)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables2$contrib, 11)

library("corrplot")
corrplot(variables2$contrib, is.corr=FALSE) 

