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
pidigits <- 200000  # two hundred thousand!
y <- ""  # string containing first 20000 digits (excluding 3)
j <- 2   # skip "3."

while (str_length(y) <= pidigits) {
    y <- str_c(y, z[j])  # concatenate
    j <- j + 1
}

n <- str_length(y);
print(n)

# End text file processing.

##############################
# STEP 1: I choose the digit 1
##############################

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

# Finds distances of all digits. VERY SLOW. Expect to wait 10-15 
# min if you don't have  the distance files already
if(file.exists("distances.txt")) {
    distances <- scan(file="distances.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances <- findDistances(1, y, n) # SLOW the first time. Give it a minute.
    write(distances, file="distances.txt")
}

if(file.exists("distances9.txt")) {
    distances9 <- scan(file="distances9.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances9 <- findDistances(9, y, n) # SLOW the first time. Give it a minute.
    write(distances9, file="distances9.txt")
}

if(file.exists("distances8.txt")) {
    distances8 <- scan(file="distances8.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances8 <- findDistances(8, y, n) # SLOW the first time. Give it a minute.
    write(distances8, file="distances8.txt")
}

if(file.exists("distances7.txt")) {
    distances7 <- scan(file="distances7.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances7 <- findDistances(7, y, n) # SLOW the first time. Give it a minute.
    write(distances7, file="distances7.txt")
}

if(file.exists("distances6.txt")) {
    distances6 <- scan(file="distances6.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances6 <- findDistances(6, y, n) # SLOW the first time. Give it a minute.
    write(distances6, file="distances6.txt")
}

if(file.exists("distances5.txt")) {
    distances5 <- scan(file="distances5.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances5 <- findDistances(5, y, n) # SLOW the first time. Give it a minute.
    write(distances5, file="distances5.txt")
}

if(file.exists("distances4.txt")) {
    distances4 <- scan(file="distances4.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances4 <- findDistances(4, y, n) # SLOW the first time. Give it a minute.
    write(distances4, file="distances4.txt")
}

if(file.exists("distances3.txt")) {
    distances3 <- scan(file="distances3.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances3 <- findDistances(3, y, n) # SLOW the first time. Give it a minute.
    write(distances3, file="distances3.txt")
}

if(file.exists("distances2.txt")) {
    distances2 <- scan(file="distances2.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances2 <- findDistances(2, y, n) # SLOW the first time. Give it a minute.
    write(distances2, file="distances2.txt")
}

if(file.exists("distances0.txt")) {
    distances0 <- scan(file="distances0.txt")
} else {
    print("Computing distances from scratch. Give it a minute.", quote=F)
    distances0 <- findDistances(0, y, n) # SLOW the first time. Give it a minute.
    write(distances0, file="distances0.txt")
}

# end find distances

png("./plot1.png")
plot(distances, main="Distances Between 1\'s in Pi", xlab="Index", ylab="Frequency")
dev.off()
# box and whisker
png("./plot2.png")
boxplot(distances, main="Distances Between 1\'s in Pi", xlab="", ylab="Distance")
dev.off()
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

##################################
# STEP 2: Randomly Do X experiments of size N, record sample mean and var
##################################

# helper, returns a random number between 1 and the size of 'distances'
#pickone <- function() {
#    return(floor(runif(1) * length(theDistances)) + 1)
#}

randomSample <- function(Nne, Nss, file1="NULL", file2="NULL", theDistances=distances) {
    means <- vector() # holds results
    vars <- vector()

    for(sample in 1:Nne) {
        result <- vector()
        for(trial in 1:Nss) {
            choice <- floor(runif(1) * length(theDistances)) + 1
            result <- c(result, theDistances[[choice]])
        } # all trials
        means <- c(means, mean(result))
        vars <- c(vars, var(result))
    } # all samples
    if(file1 != "NULL"  && file2 != "NULL") { # for Step 2
        png(file1)
        plot(density(means), main="Distribution of Sample Means", xlab="Mean", ylab="Density")
        png(file2)
        plot(density(vars), main="Distribution of Sample Variances", xlab="Variance", ylab="Density")
        dev.off()
        return()
    }
    return(means) # for Step 3
}

randomSample(100, 10, file1="./samplemeans1.png", file2="./samplevars1.png")
randomSample(100, 20, file1="./samplemeans2.png", file2="./samplevars2.png")
randomSample(100, 30, file1="./samplemeans3.png", file2="./samplevars3.png")
randomSample(100, 40, file1="./samplemeans4.png", file2="./samplevars4.png")

#######################
#####  STEP 3 : CLT
#######################

# Part 1-2
normal10 <- rnorm(100, mean=popMean, sd=sqrt(popVar / 10))
actual10 <- randomSample(100, 10)
png("./qq10.png")
qqplot(x=actual10, y=normal10, main="Distance Dist. vs. Norm. Dist.(Nss = 10)", xlab="Normal Distribution", ylab="Distance Distribution")
dev.off()

normal10 <- rnorm(100, mean=popMean, sd=sqrt(popVar / 30))
actual10 <- randomSample(100, 30)
png("./qq30.png")
qqplot(x=actual10, y=normal10, main="Distance Dist. vs. Norm. Dist.(Nss = 30)", xlab="Normal Distribution", ylab="Distance Distribution")
dev.off()

normal10 <- rnorm(100, mean=popMean, sd=sqrt(popVar / 50))
actual10 <- randomSample(100, 50)
png("./qq50.png")
qqplot(x=actual10, y=normal10, main="Distance Dist. vs. Norm. Dist.(Nss = 50)", xlab="Normal Distribution", ylab="Distance Distribution")
dev.off()

###################################
#####  STEP 4: Confidence Intervals
###################################

# This is purely analytical, see the included writeup.

##########################################
#####  STEP 5: Correlation of Sample Means
##########################################

# first compute all sample and pop means
p0 <- mean(distances0)
p1 <- mean(distances)
p2 <- mean(distances2)
p3 <- mean(distances3)
p4 <- mean(distances4)
p5 <- mean(distances5)
p6 <- mean(distances6)
p7 <- mean(distances7)
p8 <- mean(distances8)
p9 <- mean(distances9)
popmeans <- c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)

s0 <- randomSample(Nne=1, Nss=30, theDistances=distances0)
s1 <- randomSample(Nne=1, Nss=30)
s2 <- randomSample(Nne=1, Nss=30, theDistances=distances2)
s3 <- randomSample(Nne=1, Nss=30, theDistances=distances3)
s4 <- randomSample(Nne=1, Nss=30, theDistances=distances4)
s5 <- randomSample(Nne=1, Nss=30, theDistances=distances5)
s6 <- randomSample(Nne=1, Nss=30, theDistances=distances6)
s7 <- randomSample(Nne=1, Nss=30, theDistances=distances7)
s8 <- randomSample(Nne=1, Nss=30, theDistances=distances8)
s9 <- randomSample(Nne=1, Nss=30, theDistances=distances9)
sampmeans <- c(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9)

fit <- lm(sampmeans ~ c(0:9))

# plot them both
png("allpopmeans.png")
plot(popmeans, main="Population means for the digits 0-9", ylab="Mean distance")
png("allsampmeans.png")
plot(sampmeans, main="Sample means for the digits 0-9", ylab="Mean distance")
abline(a=fit$coefficients[[1]], b=fit$coefficients[[2]])
dev.off()

print("Correlation between digit and mean distance", quote=F)
print(cor(sampmeans, c(0:9)))

##########################
#####  STEP 6: The Digit 9
##########################

sampleNines <- function() {
    total <- 0
    for(i in 1:100) {
        choose <- floor(runif(1) * nchar(y)) + 1
        if(substr(y, choose, choose) == as.character(9)) {
            total <- total + 1
        }
    }
    return(total / 100)
}

popNines <- function() {
    max <- floor(nchar(y)/100) - 1
    result <- vector()
    occurrences <- 0
    for(i in 1:max) {
        front <- i * 100
        back <- front - 100 + 1
        for(j in back:front)
        {
            if(substr(y, j, j) == as.character(9)) {
                occurrences <- occurrences + 1
            }
        }
        result <- c(result, occurrences)
        occurrences <- 0
    }
    return(result)
}

poissonnines <- rpois(100, (1/sampleNines()))
png("poisson.png")
hist(poissonnines, main="Poisson Estimation of distribution of nines")
dev.off()

popnines <- popNines()
png("consecutive.png")
hist(popnines, main="Actual distribution of 9s")
dev.off()

png("qqnine.png")
qqplot(x=poissonnines, y=popnines, main="Pois Dist of 9 vs. Population Dist.", xlab="Poisson Distribution", ylab="Occurrence Distribution")

print("All good!  The plots should be in the working directory", quote=F)
