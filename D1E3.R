##1.load required packages
library(tidyverse)

##2. Set initial outbreak
InfectedA = 1
##3. Create time vector
time = c(1:20)
##3. Construct outbreak loop
for (i in 1:19)
  {InfectedA[i+1] = InfectedA[i] * 3} ##Intfected people infected 2 more each

##4. Construct data.frame of Outbreak
OutbreakA = data.frame("time" = time, "InfectedA" = InfectedA)

##5. Plot line graph of OutbreakA
ggplot(OutbreakA, aes(x=time, y=InfectedA)) +
  geom_line()

##6. Construct OutbreakB with recovery
##7. Initial OutbreakB Individual
InfectedB = 1
Recovery = runif(20, min = 0, max = 0.5) ##Recovery rate, such that each indivual has random 25% recovery
mean(Recovery)
hist(Recovery, breaks = 10) ##view distribution

for (i in 1:19)
  InfectedB[i+1] = ((InfectedB[i] - (InfectedB[i] * Recovery[i])) * 3)

##7.create data frame of OutbreakB
OutbreakB = data.frame("time" = time, "InfectedB" = InfectedB)

##8. Plot OutbreakB graph
ggplot (OutbreakB, aes(x=time, y=InfectedB)) +
  geom_line()