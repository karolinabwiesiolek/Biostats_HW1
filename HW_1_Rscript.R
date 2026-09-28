
  
#Question 8a

d <- read.csv("homework1_clinic.csv", #reading in the data
              colClasses=c(id="character"))

dim(d)

with(d, table(program, improved))

total <- table(d$program)
print(total)

improved_n <- tapply(d$improved,
                     d$program,
                     sum,
                     na.rm=TRUE)

print(improved_n)

rate <- tapply(d$improved,
               d$program,
               mean,
               na.rm=TRUE)

print(rate)

missing_wait <- tapply(is.na(d$wait_min),
                       d$program,
                       sum)

print(missing_wait)


#Question 8b

difference <- rate["Yes"] - rate["No"]

print(difference)
print(100 * difference)


#Question 8c

barplot(rate,
        ylim=c(0,1),
        xlab="Program",
        ylab="Improvement proportion",
        main="")


#Question 8d
## Among the participants the observed improvement proportion was 66.67% in the yes program group and 33.33% on the No program group. The difference between the two is 33.33 percentage points. This difference does not have to be automatically a causal effect because participants were not randomly assigned to the programs. For example, if the baseline severity could affect both program choice and improvement the participants with more severe baseline conditions could be more likely to choose the program and might also have a different probability of improving.


#question 10

#10A


B <- 100000
theory_without <- (3/8) * (2/7) * (1/6)
theory_with <- (3/8)^3

print(theory_without)
print(theory_with)



#10B


B <- 100000
set.seed(813110)

draws <- replicate(B,
                   sample.int(8,3,replace=FALSE))

all3 <- colSums(draws <= 3) == 3

successes_without <- sum(all3)
empirical_without <- mean(all3)

print(successes_without)
print(empirical_without)
print(theory_without)


#10C

set.seed(813111)

draws2 <- replicate(B,
                    sample.int(8,3,replace=TRUE))

all3_2 <- colSums(draws2 <= 3) == 3

successes_with <- sum(all3_2)
empirical_with <- mean(all3_2)

print(successes_with)
print(empirical_with)
print(theory_with)




running <- cumsum(all3) / seq_len(B)

print(running[100])
print(running[1000])
print(running[10000])
print(running[100000])

plot(1:B, running,
     type = "l",
     log = "x",
     xlab = "Number of trials",
     ylab = "Running probability")

abline(h = theory_without,
       lty= 2)



#10d

# the probabilities are different because without the replacement, the number of allergic people changes after each selection. with replacement, the population stays the same, so the probability stays to be 3/8 for every draw. 
# The law of large numbers does not guarantee an exact results after 25 trials or that every trial will get clsoer to the theoretical probability. It means that with many trials, the empirical prbability tends to get closer to the theoretical probability. 



pdf("R_plots.pdf")

barplot(rate,
        ylim=c(0,1),
        xlab="Program",
        ylab="Improvement proportion",
        main="")

plot(1:B, running,
     type="l",
     log="x",
     xlab="Number of trials",
     ylab="Running probability")

abline(h=theory_without,
       lty=2)

dev.off()

