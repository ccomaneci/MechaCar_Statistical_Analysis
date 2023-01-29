library(dplyr)


## Part 1

# Import and read MechaCar_mpg CSV file
mecha_car_mpg <- read.csv(file='MechaCar_mpg.csv',check.names = F, stringsAsFactors = F)

# Create linear regression
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_car_mpg)

# Create summary
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_car_mpg))


## Part 2

# Import Suspension_Coil CSV file
suspension_coils <- read.csv(file="Suspension_Coil.csv")

# Create summary

total_summary <- suspension_coils %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

lot_summary <- suspension_coils %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))


## Part 3

# Write t.test to determine PSI
t.test(suspension_coils$PSI, mu = 1500)

Lot1 = subset(suspension_coils, Manufacturing_Lot == 'Lot1')
Lot2 = subset(suspension_coils, Manufacturing_Lot == 'Lot2')
Lot3 = subset(suspension_coils, Manufacturing_Lot == 'Lot3')

t.test(Lot1$PSI,mu=1500)
t.test(Lot2$PSI,mu=1500)
t.test(Lot3$PSI,mu=1500)

