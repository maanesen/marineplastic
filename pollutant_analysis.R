# to clear the memory
rm(list = ls())

# if you want to use the excel data file 
pacman::p_load(
  tidyverse,    # data management packages 
  readxl        # handle Excel data
)

library(dplyr)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(gplots)
library(corrplot)

data <- read_excel("pollutants_matrix_CA.xlsx") 

names(data)

# form the 3 subsets

data <- subset(data, data$STOCKH == 1)

# check what type of data you have
class(data$DISCONT)

# if you need to transform your data from integers to numeric 
data$DISCONT <- as.numeric(as.integer(data$DISCONT))

attach(data)

# selects the variables to be used in the analysis

# harmful effects on humans:
data.RED <- cbind(CARC, HA_TOX, MUTAG, BIRTH, REP_TOX, ENDA, BAN, SUBS)

# environmental impacts:
data.RED <- cbind(OZON, CC, BAN, SUBS, AQUA_TOX, LAND_TOX)

# testing relationship between regulation, substitutes and prohibition
data.RED <- cbind(STOCKH, OSPAR, EU_POP, EPA_Air, EPA_Wat, EUR_R, EU_COS, EU_SVHC, EU_WF, EU_RoH, DISC, SUBS) # , MONTR)

# testing relationship between origin of substance and prohibition
data.RED <- cbind(AIR, WATER, FOOD, PLAST, HUMAN, DISC, SUBS)

# testing relationship between type of spread of harmful effects and prohibition
data.RED <- cbind(DISC, SUBS, UNIF, POINT)

# testing relationship between group og chemicals and whether they are prohibited or have substitutes
data.RED <- cbind(POP, VOC, BFR, PBT, PCB, PCDD, PAH, DISC, SUBS)

# testing relationship between being in the Stockholm convention, being in plastic and being prohibited
data.RED <- cbind(STOCKH, PLAST, DISC) # , MONTR)


# convert the data as a contingency table
data.CA <- as.table(as.matrix(data.RED))

# for small datasets the chi-square test can be used to test for dependencies between rows and columns
chisq <- chisq.test(data.RED)
chisq


# this is the main command producing the overall results of the correspondence analysis, ncp is the number of Dimensions you think is needed to cover all the variation in the data
CA(data.CA, ncp=10, axes=c(1,2), graph=TRUE)

# this command stores the correspondence analysis results in a separate file called res.ca
res.ca <- CA(data.CA, graph=FALSE)

# this command gives some central factors of the CA
summary(res.ca)


# now we can collect various information from the overall result file res.ca
eig.val <- get_eigenvalue(res.ca)
eig.val

# this code shows how much of the variation in the observations that is explained by Dimensions 1, 2, ......
fviz_screeplot(res.ca, addlabels=TRUE, ylim=c(0, 50))


# next, we can look at the observations (rows)
row <- get_ca_row(res.ca)
row

head(row$coord)

fviz_ca_row(res.ca, repel=TRUE)

head(row$cos2, 5)

head(row$contrib)

corrplot(row$contrib, is.corr=FALSE)

fviz_ca_row(res.ca, choice="row", axes=1:2, top = 50)


# finally, we can look at the variables (columns)
col <- get_ca_col(res.ca)
col

head(col$coord)

head(col$cos2)

head(col$contrib)

col$contrib
col$inertia
col$coord

# cos2 measure the degree of association between the variables and a particular axis
fviz_ca_col(res.ca, choice="col", axes=c(1,3), col.col="cos2", gradient.cols=c("red", "yellow", "blue"), repel=TRUE, title="") 


fviz_contrib(res.ca, choice="col", axes=2)

fviz_cos2(res.ca, choice="col", axes=1:2)

corrplot(col$contrib, is.corr=FALSE)

# alternative command for the biplots (doesn't work)
plot(res.ca, choice="col", axes=c(1, 2), gradient.cols=c("red", "yellow", "blue"), repel=TRUE, cex=0.5)


# biplot including both rows and columns and with arrows for all rows (substances) 
fviz_ca_biplot(res.ca, axes=c(1,2), repel=TRUE)
# biplot including both columns and rows with arrows
fviz_ca_biplot(res.ca, map="colprincipal", arrow=c(TRUE, TRUE), repel=TRUE)


covar(DISC, UNIF)







