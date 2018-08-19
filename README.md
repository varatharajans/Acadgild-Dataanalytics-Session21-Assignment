                       # Acadgild-Dataanalytics-Session21-Assignment
                  DATA ANALYTICS WITH R, EXCEL AND TABLEAU SESSION 21 ASSIGNMENT 
                                  session21_assign_pca.R
                                           Seshan
                                Sat Aug 18 20:03:45 2018
Data Set 
2. Perform the below given activities: 
a. Apply PCA to the dataset and show proportion of variance 
b. Perform PCA using SVD approach 
c. Show the graphs of PCA components



setwd("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session21")
library(readr)
epi_r <- read.csv("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session21/epi_r.csv")
View(epi_r)
data<-epi_r
View(data)
head(data, n=10)
# data sets in package
data(package="arules")
# Split data
dt <- split(data$rating, data$arizona)
dt
# Loading arules package
require(arules)
require(arulesViz)

# Convert data to transaction level
dt2 = as(dt,"transactions")
dt2
summary(dt2)
inspect(dt2)
# Most Frequent Items
itemFrequency(dt2, type = "relative")
itemFrequencyPlot(dt2,topN = 5)
# with support parameters
itemFrequency(dt2, type = "relative")
itemFrequencyPlot(dt2,support= 0.10)
# aggregated data
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8))
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, minlen = 3))
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, maxlen = 4))
rules
summary(rules)

inspect(rules[1:10]) # to view first 10 rules

#Convert rules into data frame
rules3 = as(rules, "data.frame")
write(rules, "C:/Users/Seshan/Desktop/PCA//rules2.csv", sep=",")

# Show only particular product rules
inspect( subset( rules, subset = rhs %pin% "0" )[1:10])

# Show the top 10 rules
options(digits=2)
inspect(rules[1:10])

# Get Summary Information

summary(rules)
plot(rules)
plot(rules, method = "graph", interactive = T)

# Sort by Lift
rules<-sort(rules, by="lift", decreasing=TRUE)

# Sort by Lift
rules<-sort(rules, by="lift", decreasing=TRUE)

# Remove Unnecessary Rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules[!redundant]
rules<-rules.pruned
rules
#Clean Rules
rules3$rules=gsub("\\{", "", rules3$rules)
rules3$rules=gsub("\\}", "", rules3$rules)
rules3$rules=gsub("\"", "", rules3$rules)

#Split the rule
library(splitstackshape)
Rules4=cSplit(rules3, "rules","=>")
names(Rules4)[names(Rules4) == 'rules_1'] <- 'LHS'
Rules5=cSplit(Rules4, "LHS",",")
Rules6=subset(Rules5, select= -c(rules_2))
names(Rules6)[names(Rules6) == 'rules_3'] <- 'RHS'

# What are customers likely to buy before they purchase "Product A"
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="0"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

# What are customers likely to buy if they purchased "Product A"
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8),appearance = list(default="rhs",lhs="0"),control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
rules
support<-seq(0.01,0.1,0.01)
support
rules_count<-c(472,128,46,26,14, 10, 10,8,8,8)
rules_count
plot(support,rules_count,type = "l",main="Number of rules at different support %",col="darkred",lwd=3)

conf<-seq(0.10,1.0,0.10)
conf

rules_count<-c(472,231,125,62,15,0,0,0,0,0)
rules_count

plot(conf,rules_count,type = "l",main="Number of rules at different confidence %",col="darkred",lwd=3)
#rules_ec <- eclat(epi_r, parameter = list(supp = 0.05))
#summary(rules_ec)
#sorting out the most relevant rules
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:5])

########################################
library(factoextra)

library("factoextra")
data1<-na.exclude(data)
na.omit(data1)
data1.active <- data1[2:100, 2:6]
na.exclude(data1.active)
View(data1.active)
head(data1.active[, 2:5])

#Compute PCA in R using prcomp()
library(factoextra)
res.pca <- prcomp(data1.active, scale = TRUE)
res.pca
summary(res.pca)
fviz_eig(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2", # Color by the quality of representation gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE     # Avoid text overlapping)
fviz_pca_var(res.pca, col.var = "contrib", # Color by contributions to the PCgradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE     # Avoid text overlapping)
fviz_pca_biplot(res.pca, repel = TRUE,col.var = "#2E9FDF", # Variables color col.ind = "#696969"  # Individuals color)
library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val
# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation



                             Session 21 Assignment 2nd additional exercise
                                           session21_pci.R
                                                 Seshan
Thu Aug 16 15:46:09 2018
setwd("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session21")
library(readr)
epi_r <- read.csv("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session21/epi_r.csv")
View(epi_r)
data<-epi_r
View(data)

a <- aggregate(data[,-1], by=list(data[,1]), paste, collapse=",")
a$combined <- apply(a[,2:ncol(a)], 1, paste, collapse=",")
a$combined <- gsub(",NA","",a$combined) ## this column contains the totality of all ingredients for a cuisine

cuisines <- as.data.frame(table(data[,1])) ## Number of recipes for each cuisine
freq <- lapply(lapply(strsplit(a$combined,","), table), as.data.frame) ## Frequency of ingredients
names(freq) <- a[,1]
prop <- lapply(seq_along(freq), function(i) { colnames(freq[[i]])[2] <- names(freq)[i] freq[[i]][,2] <- freq[[i]][,2]/cuisines[i,2] ## proportion (normalized frequency) freq[[i]]})
names(prop) <- a[,1] ## this is a list of 26 elements, one for each cuisine
final <- Reduce(function(...) merge(..., all=TRUE, by="Var1"), prop)
row.names(final) <- final[,1]
final <- final[,-1]
final[is.na(final)] <- 0 ## If ingredient missing in all recipes, proportion set to zero
final <- t(final) ## proportion matrix
s <- sort(apply(final, 2, sd), decreasing=TRUE)
## Selecting ingredients with maximum variation in frequency among cuisines and
## Using standardized proportions for final analysis
final_imp <- scale(subset(final, select=names(which(s > 0.1)))) 
## heatmap 
library(gplots) ## 
## Attaching package: 'gplots'
