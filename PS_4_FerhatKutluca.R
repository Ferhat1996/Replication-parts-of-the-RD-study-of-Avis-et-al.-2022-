# Money and Politics: The Effects of Campaign Spending Limits on 
# Political Entry on the Small Party Excluding Incumbent perspective empirical example

#--------------------------  Load RD Packages  --------------------------------#

#install.packages("(rdrobust")
#install.packages("(RDHonest")
#install.packages("(haven")

library(rdrobust)
library(RDHonest)
library(haven)


#------------------- Load and understand the data -----------------------------#


# Load the dataset of candidates 2016 / change with your dictionary
candidates2016 <- read_dta("C:/Users/Lenovo/Desktop/LMU Courses/Nonparametric Econometrics/PS_4/data/data/candidates2016.dta")

# Display column names to identify relevant variables
colnames(candidates2016)

# Define the running variable and the outcome variable for RD analysis
X <- candidates2016$lz # running variable is max_mayor
X_control <- log(candidates2016$max_mayor) - log(142857) # control running variable

Control <- data.frame(X,X_control)

# mean_bot6paryex = Small party (exc incumbent)
Y <- candidates2016$mean_bot6partyex # Outcome variable is mean_bot6partyex 

# Summarize running and outcome variables to understand their distribution
summary(X)
summary(Y)

# Clean the data: remove missing values and create a focused dataset
Data <- na.omit(data.frame(X, Y))

X <- Data$X
Y <- Data$Y
          
# Plot the relationship between the running and outcome variables of raw data
plot(X,Y, pch=16, cex=0.25, 
     xlab = "Small party (excluding incumbent)",
     ylab = "2016 Max Spending")

# Plot the data with binning for a clearer view of the distribution
RDHonest::plot_RDscatter(Y ~ X, avg=20, 
                         ylab = "Small party (excluding incumbent)",
                         xlab = "2016 Max Spending")


# Enhanced RD plot / By default it adds a fourth-order polynomial fit
rdrobust::rdplot(y=Y, x=X, 
                 y.label = "Small party (excluding incumbent)",
                 x.label = "2016 Max Spending")


#-------------------------- Running variable ----------------------------------#


# Create a data frame which include maximum spending of 2012 and 2016 selections in each municipality 
data_hist <- data.frame(candidates2016$cod_munic, candidates2016$max_mayor, candidates2016$max_valordespesa)

# Remove missing values and duplicates in the same municipalities
data_hist <- unique(na.omit(data_hist))

# Convert data to numeric format for analysis
max_spending_2012 <- as.numeric(data_hist[[2]])
max_spending_2016 <- as.numeric(data_hist[[3]])

# Summarize data to determine x limits in the graph
summary(max_spending_2012)
summary(max_spending_2016)




#---------------------- Running variable Analysis -----------------------------#

# Check for manipulation in the running variable using density plots
# This step is crucial to validate the RD design assumptions

#------------------------ Running variable 2016 -------------------------------#

# Running variable should have a continuous distribution and no jumps in density
cap_2016 = 108039

# "Manipulation testing"
rdd <- rddensity::rddensity(max_spending_2016, c = cap_2016, p=1,) # kernel = "triangular" is the default option.

rddensity::rdplotdensity(rdd, X=max_spending_2016, 
                         title = "Question 3 Figure 1",
                         ylab = "Density",
                         xlab = "2016 Max Spending",
                         plotRange = c(10000,200000))

#------------------------ Running variable 2012 -------------------------------#

# Running variable should have a continuous distribution and no jumps in density
cap_2012 = 142857

# "Manipulation testing"
rdd <- rddensity::rddensity(max_spending_2012, c = cap_2012, p=1) # kernel = "triangular" is the default option.

rddensity::rdplotdensity(rdd, X=max_spending_2012, 
                         title = "Question 3 Figure 2",
                         ylab = "Density",
                         xlab = "2012 Max Spending",
                         plotRange = c(10000,200000))

#------------------------- RD analysis ----------------------------------------#
#------------------------------------------------------------------------------#

# Both packages use the triangular kernel by default.


#-------------------------------- rdrobust ------------------------------------#

# Determine optimum bandwidth on the approach developed by Calonico et al. (2014) 
bw <- rdbwselect_2014(Y, X, c=0, p=1, bwselect = "CCT")

# Output of the results
print(bw)

# Extract the optimal bandwidth (h) value
h<- bw$bws[1, 1]  # The first element in the bws matrix is optimal bandwith

# Print the extracted value
print(h)

# Constrain data according to optimum bandwidth
data <- Data[which(X >= -h & X <= h), ]

# Take only one observation for all municipalities
data <- unique(data)

X_new <- data$X
Y_new <- data$Y

# RD plot within the optimal bandwidth to visualize the local effect
rdrobust::rdplot(y=Y_new, x=X_new, p=1, h=h, c=0,
                 kernel = "tri",
                 title = "Question_4",
                 y.label = "Small party (excluding incumbent)",
                 x.label = "2016 Max Spending")


# Perform RD analysis within the optimal bandwidth and display results
reg1 <- rdrobust::rdrobust(x=X_new, y=Y_new, h=h, c=0, p=1) # kernel = "tri" is the default option.
summary(reg1)

