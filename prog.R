#!/usr/bin/Rscript
# Mazar Farran
# Project 3: Pi Statistics
# November 29, 2014

# Text file processing -- Given by Prof. Ghosal

# Load in the required libraries
library(stringr)  # requires installing ggplot2

# connect to the file
connecter <- file("1milliondigitsofpi.txt", "r", blocking = FALSE)

# read the data
z <- scan(connecter, what="character", skip=0)
close(connecter)

# join all digits
# number of pi digits considered
pidigits <- 20000  # twenty thousand, not two hundred thousand!
y <- ""  # string containing first 20000 digits (excluding 3)
j <- 2   # skip "3."

while (str_length(y) <= pidigits) {
    y <- str_c(y, z[j])  # concatenate
    j <- j + 1
}

n <- str_length(y);
print(n)

# End text file processing.


# Step 1: I choose the digit 1

findDistances <- function(n, y, len) {
    distances <- vector()
    currDistance <- 0
    aschar <- as.character(n) # maybe more efficient

    for(i in 1:len) {
        if(substr(y, i, i) == aschar) {
            distances <- c(distances, currDistance)
            currDistance <- 0
        } else {
            currDistance <- currDistance + 1
        } # if-else
    } # for
    distances[-1]  # discard the first value, which is nonsense
    return(distances)
} # findDistances

distances <- findDistances(1, y, n)

# scatter
#png("./plot1.png")
#plot(table(distances), main="Distances Between 1\'s in Pi", xlab="Distance", ylab="Frequency")
#dev.off()
# box and whisker
#png("./plot2.png")
#boxplot(distances, main="Distances Between 1\'s in Pi", xlab="", ylab="Distance")
#dev.off()
# quantile
print("Quantile Values:", quote=F)
print(quantile(distances))
# mean and variance
popMean <- mean(distances)
popVar <- var(distances) * ((n - 1) / n) # needs adjusting
print("Population mean:", quote=F)
print(popMean)
print("Population variance:", quote=F)
print(popVar)

# Step 2: Randomly Do X experiments of size N, record the sample mean and var

# helper, returns a random number between 1 and the size of 'distances'
pickone <- function() {
    return(floor(runif(1) * length(distances)) + 1)
}

randomSample <- function(Nne, Nss) {
    means <- vector() # holds results
    vars <- vector()

    for(sample in 1:Nne) {
        result <- vector()
        for(trial in 1:Nss) {
            choice <- pickone()
            result <- c(result, distances[[choice]])
        } # all trials
        means <- c(means, mean(result))
        vars <- c(vars, var(result))
    } # all samples
    #print("Sample mean", quote=F)
    #print(mean(means))
    #png("./samplemeans4.png")
    #plot(density(means), main="Distribution of Sample Means", xlab="Mean", ylab="Density")
    #png("./samplevars4.png")
    #plot(density(vars), main="Distribution of Sample Variances", xlab="Variance", ylab="Density")
    #dev.off()
    #print("Sample variance:", quote=F)
    #print(mean(vars))
    return(means)
}

#randomSample(100, 40)

#######################
#####  STEP 3
#######################

# Part 1
normal10 <- rnorm(100, mean=popMean, sd=sqrt(popVar / 30))
actual10 <- randomSample(100, 30)

png("./qq30.png")
qqplot(x=actual10, y=normal10, main="Distance Dist. vs. Norm. Dist.(Nss = 30)", xlab="Normal Distribution", ylab="Distance Distribution")
dev.off()












