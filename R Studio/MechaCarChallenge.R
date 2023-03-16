library(dplyr)
#PART 1
Mecha_df <- read.csv("MechaCar_mpg.csv")
summary(Mecha_df)
mecha_lm = lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data=Mecha_df)
summary(mecha_lm)
        
        
  # PART 2

Suspension_df <- read.csv("Suspension_Coil.csv")
Suspension_Sumary <- Suspension_df %>%
  summarise(Mean = mean(PSI),Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

Lot_Sumary <- Suspension_df %>%
  group_by(Manufacturing_Lot) %>%
  summarise(Mean = mean(PSI),Median = median(PSI), Variance = var(PSI), SD = sd(PSI))


# PART 3


t.test(Suspension_df$PSI, mu = 1500)
t.test(subset(Suspension_df, Manufacturing_Lot=="Lot1")$PSI, mu = 1500)
t.test(subset(Suspension_df, Manufacturing_Lot=="Lot2")$PSI, mu = 1500)
t.test(subset(Suspension_df, Manufacturing_Lot=="Lot3")$PSI, mu = 1500)
