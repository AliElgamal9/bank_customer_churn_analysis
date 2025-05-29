# First load needed libraries for calculating t-test, levene-test, and effect size
library(car)
library(dplyr)
library(lsr)

# make sure the data is data.frame
class(bank_data)

# filter only france customers
france_female_customers <- subset(bank_data, Geography == "France" & Gender == "Female")

# mark the account status as factor, the factor to group by 
france_female_customers$AccountStatus <- as.factor(france_female_customers$AccountStatus)

# --- traditional significant p-values
# p < 0.001	Very highly significant (***)
# p < 0.01	Highly significant (**)
# p < 0.05	Statistically significant (*)
# p ≥ 0.05	Not statistically significant 

# --- traditional significant effect size for Cohen's d
# 0.2 = small
# 0.5 = medium
# 0.8 = large

# each test check go in this steps
# 1- run levene-test for variance homogeneity
# 2- run t-test
# 3- calculate cohen's d value for effect size, only if there is significant result
# 4- highlight results

# test for satisfaction score ----------------------------------------
leveneTest(Satisfaction.Score ~ AccountStatus, data = france_female_customers)
t.test(Satisfaction.Score ~ AccountStatus, data = france_female_customers, var.equal = TRUE)
# no significant difference (p-value = 0.5548), difference CI = [-0.07, 0.1]

# test for estimated salary ------------------------------------------
leveneTest(EstimatedSalary ~ AccountStatus, data = france_female_customers)
t.test(EstimatedSalary ~ AccountStatus, data = france_female_customers, var.equal = TRUE)
cohen.d(EstimatedSalary ~ AccountStatus, data = france_female_customers)
# significant difference (p-value = 0.04775), diff CI = [43, 8662] closed accounts higher 
# effect size is 0.07 (negligible)

# test for membership years ------------------------------------------
leveneTest(MembershipYears ~ AccountStatus, data = france_female_customers)
t.test(MembershipYears ~ AccountStatus, data = france_female_customers, var.equal = TRUE)
# no significant difference (p-value =  0.997), diff CI = [-.2, .2]

# test for credit score ----------------------------------------------
leveneTest(CreditScore ~ AccountStatus, data = france_female_customers)
t.test(CreditScore ~ AccountStatus, data = france_female_customers, var.equal = TRUE)
cohen.d(CreditScore ~ AccountStatus, data = france_female_customers)
# significant difference (p-value = 0.01493), diff CI = [1-16] closed accounts lower
# means-> closed:642  open:651 (not even significant in bank customers ranking)
# effect size is 0.09 (negligible)

# test for age -------------------------------------------------------
leveneTest(Age ~ AccountStatus, data = france_female_customers)
t.test(Age ~ AccountStatus, data = france_female_customers, var.equal = FALSE)
cohen.d(Age ~ AccountStatus, data = france_female_customers)
# significant difference (p-value < 2.2e-16), diff CI = [7, 8.6] closed higher
# effect size is 0.78 (medium)

# test for gender ------------------------------------------------------
gender_table <- table(france_female_customers$Gender, france_female_customers$AccountStatus)
# display table
print(gender_table)
# chi-square for categorical variable
chisq.test(gender_table)
# instead of Cohen's d for nominal, use Cramer's v for categorical variable
cramersV(gender_table)
# significant difference (p-value = 4.859e-13)
# effect size is 0.1 (small)

# calculate each category closure probability
female_closed <- gender_table["Female", "Closed"]
female_total  <- sum(gender_table["Female", ])
male_closed   <- gender_table["Male", "Closed"]
male_total    <- sum(gender_table["Male", ])
female_ci <- binom.test(female_closed, female_total)$conf.int
male_ci <- binom.test(male_closed, male_total)$conf.int
cat("Female Closed Proportion 95% CI:", female_ci, "\n")
cat("Male Closed Proportion 95% CI:", male_ci, "\n")
# Closed Proportion 95% CI: male [11, 14]%
# Closed Proportion 95% CI: female [18, 22]%

