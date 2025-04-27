
#lab 14
### (1) Error Bars on Bar Plots

# Q1. Enter the following means and standard errors into R
means <- c(20.34,19.49,25.68)
stderr <- c(0.83,1.51,1.39)

# Create bar plot
# Q2. Create a barplot with grey bars labeled ‘A’, ‘B’, and ‘C’.
#     Add a title: "Errors on bar plot". Use ylim to ensure space for error bars.
bar_centers <- barplot(means, names.arg = c('A', 'B', 'C'),
                       col = "grey", ylim = c(0, max(means + stderr) + 2),
                       main = "Errors on bar plot")
box()

# Add error bars using arrows()
arrows(bar_centers, means + stderr,
       bar_centers, means - stderr,
       angle = 90, code = 3, length = 0.06, col = "red")
#1.2
#enter the data for means and standard means
# Data
x <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y <- c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors <- c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)

# Plot points
plot(x, y, pch = 19, xlab = "concentration", ylab = "optical activity",
     main = "Error bars on data points", ylim = c(min(y - errors), max(y + errors)))

# Add vertical error bars
arrows(x, y + errors, x, y - errors,
       angle = 90, code = 3, length = 0.06, col = "blue")

#1.3
# Vectors
x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
y <- c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)

# Covariance
cov(x, y)

# Pearson’s correlation coefficient
cor(x, y)

# Multivariate correlation matrix
cor(longley)  # Built-in dataset in R

# ex -2 (One Sample test)

# Function to perform one-sample Z-test

one_sample_Ztest <- function(x, sigma, muzero, alpha, null, threshold = 1e6) {
  n <- length(x)
  sample_mean <- mean(x)
  se <- sigma / sqrt(n)  # Standard error
  z_value <- (sample_mean - muzero) / se

  # Calculate p-value based on the type of null hypothesis
  if (null == "equal") {
    p_value <- 2 * pnorm(-abs(z_value))  # Two-tailed test
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis (two-tailed)",
                         "Fail to reject the null hypothesis (two-tailed)")
  } else if (null == "less_than_or_equal") {
    p_value <- pnorm(z_value)  # Right-tailed test (H1: mu > muzero)
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis (mu > muzero)",
                         "Fail to reject the null hypothesis (mu <= muzero)")
  } else if (null == "more_than_or_equal") {
    p_value <- 1 - pnorm(z_value)  # Left-tailed test (H1: mu < muzero)
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis (mu < muzero)",
                         "Fail to reject the null hypothesis (mu >= muzero)")
  } else {
    stop("Invalid null hypothesis type. Use 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }

  # Return results
  return(list(
    p_value = p_value,
    z_value = z_value,
    conclusion = conclusion
  ))
}

# Test case
x <- c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
       137.4, 145.6, 135.6, 135.4, 121.5)
sigma <- 14.5
muzero <- 124.6
alpha <- 0.05

# Perform the test (two-tailed)
result1 <- one_sample_Ztest(x, sigma, muzero, alpha, "equal")

# Perform a one-tailed test (if needed)
result2 <- one_sample_Ztest(x, sigma, muzero, alpha, "less_than_or_equal")  # mu > muzero
result3 <- one_sample_Ztest(x, sigma, muzero, alpha, "more_than_or_equal")  # mu < muzero

# Display the results
cat("Two-tailed test:\n")
cat("  p-value:", result1$p_value, "\n")
cat("  Z-value:", result1$z_value, "\n")
cat("  Conclusion:", result1$conclusion, "\n")

# If you want a one-tailed p-value, just divide the two-tailed p-value by 2
cat("\np_value for alpha/2 (if applicable):", result1$p_value / 2, "\n" )
#As we can see that in both

#question2 one sample t test
one_sample_t_test <- function(x, muzero, alpha, null,threshold=1e6) {
  n <- length(x)
  sample_mean <- mean(x)
  sample_sd <- sd(x)
  se <- sample_sd / sqrt(n)  # Standard error
  t_value <- (sample_mean - muzero) / se
  df <- n - 1  # Degrees of freedom

  # Calculate p-value based on the type of null hypothesis
  if (null == "equal") {
    p_value <- 2 * pt(-abs(t_value), df)  # Two-tailed test
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis (two-tailed)",
                         "Fail to reject the null hypothesis (two-tailed)")
  } else if (null == "less_than_or_equal") {
    p_value <- pt(t_value, df)  # Left-tailed test (H1: mu > muzero)
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis (mu > muzero)",
                         "Fail to reject the null hypothesis (mu <= muzero)")
  } else if (null == "more_than_or_equal") {
    p_value <- 1 - pt(t_value, df)  # Right-tailed test (H1: mu < muzero)
    conclusion <- ifelse(p_value < alpha,
                         "Reject the null hypothesis (mu < muzero)",
                         "Fail to reject the null hypothesis (mu >= muzero)")
  } else {
    stop("Invalid null hypothesis type. Use 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }

  # Return results
  result <- c(p_value = p_value, t_value = t_value, conclusion = conclusion)
  return(result)
}
x <- c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
       96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)
muzero <- 100
alpha <- 0.05

# Perform the test (two-tailed)
result <- one_sample_t_test(x, muzero, alpha, "equal",threshold=1e6)

# Display the results
cat("p-value:", result["p_value"], "\n")
cat("t-value:", result["t_value"], "\n")
cat("Conclusion:", result["conclusion"], "\n")
#question no 3

# Perform exact binomial test (binom.test) as requested
# x = number of successes = 710
# n = total trials = 2600
# p = hypothesized proportion = 0.25
# alternative = "greater" as specified
binom_result <- binom.test(x = 710, n = 2600, p = 0.25, alternative = "greater")

# Perform normal approximation test (prop.test) as requested
# correct = TRUE by default for continuity correction
prop_result <- prop.test(x = 710, n = 2600, p = 0.25, alternative = "greater", correct = TRUE)

# Print both results as required
cat("--- Exact Binomial Test (binom.test) ---\n")
print(binom_result)
cat("\n--- Normal Approximation Test (prop.test) ---\n")
print(prop_result)
#question4

One_sample_variance_test <- function(x, test_sigma, alpha, tail_type = "two-tailed") {
  # Step 1: Calculate sample statistics
  n <- length(x)  # Sample size
  sample_variance <- var(x)  # Sample variance

  # Step 2: Compute the chi-squared test statistic
  chi_squared_stat <- (n - 1) * sample_variance / (test_sigma^2)

  # Step 3: Determine critical values based on the tail type
  if (tail_type == "two-tailed") {
    # Two-tailed test: Calculate both the lower and upper critical values
    lower_critical_value <- qchisq(alpha / 2, df = n - 1)  # Lower bound (left tail)
    upper_critical_value <- qchisq(1 - alpha / 2, df = n - 1)  # Upper bound (right tail)
    conclusion <- if (chi_squared_stat < lower_critical_value || chi_squared_stat > upper_critical_value) {
      "Reject H0 (Variance differs from hypothesized value)"
    } else {
      "Fail to reject H0 (Variance is equal to hypothesized value)"
    }
  } else if (tail_type == "right-tailed") {
    # Right-tailed test: Calculate only the upper critical value
    upper_critical_value <- qchisq(1 - alpha, df = n - 1)
    conclusion <- if (chi_squared_stat > upper_critical_value) {
      "Reject H0 (Variance is greater than hypothesized value)"
    } else {
      "Fail to reject H0 (Variance is less than or equal to hypothesized value)"
    }
    # Only calculate lower critical value for two-tailed or left-tailed tests
    lower_critical_value <- NA  # No need for lower critical value here
  } else if (tail_type == "left-tailed") {
    # Left-tailed test: Calculate only the lower critical value
    lower_critical_value <- qchisq(alpha, df = n - 1)
    conclusion <- if (chi_squared_stat < lower_critical_value) {
      "Reject H0 (Variance is less than hypothesized value)"
    } else {
      "Fail to reject H0 (Variance is greater than or equal to hypothesized value)"
    }
    # No need for upper critical value for left-tailed test
    upper_critical_value <- NA
  } else {
    stop("Invalid tail_type. Choose from 'left-tailed', 'right-tailed', or 'two-tailed'.")
  }

  # Output the results
  cat("Sample size (n):", n, "\n")
  cat("Sample variance (s^2):", round(sample_variance, 4), "\n")
  cat("Hypothesized variance (σ^2):", test_sigma^2, "\n")
  cat("Chi-squared statistic:", round(chi_squared_stat, 4), "\n")
  if (!is.na(lower_critical_value)) {
    cat("Lower critical value:", round(lower_critical_value, 4), "\n")
  }
  if (!is.na(upper_critical_value)) {
    cat("Upper critical value:", round(upper_critical_value, 4), "\n")
  }
  cat("Conclusion:", conclusion, "\n")

  # Return the test results
  return(list(chi_squared_stat = chi_squared_stat,
              lower_critical_value = lower_critical_value,
              upper_critical_value = upper_critical_value,
              conclusion = conclusion))
}

# Data set
x <- c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)

# Hypothesized variance (σ^2) and significance level (alpha)
test_sigma <- 29  # Hypothesized standard deviation (σ)
alpha <- 0.05  # Significance level

# Perform the one-sample variance test (two-tailed)
result_two_tailed <- one_sample_variance_test(x, test_sigma, alpha, tail_type = "two-tailed")

# Perform the one-sample variance test (right-tailed)
result_right_tailed <- one_sample_variance_test(x, test_sigma, alpha, tail_type = "right-tailed")

# Perform the one-sample variance test (left-tailed)
result_left_tailed <- one_sample_variance_test(x, test_sigma, alpha, tail_type = "left-tailed")

# The asymmetry of the Chi-squared distribution causes the critical values for different types of tests
# (right-tailed, left-tailed, and two-tailed) to differ. This skewness explains why the rejection regions are
# positioned differently for each type of hypothesis test, and why conclusions can change depending on the
# test direction (left, right, or two-tailed). For right-tailed tests, we focus on the right tail, while for
# left-tailed tests, we focus on the left tail. For a two-tailed test, both tails are considered.
#question5

# Given data
x <- c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)

# Perform Wilcoxon signed rank test
# H0: μ ≥ 160 (mu = 160, alternative = "less")
wilcox_result <- wilcox.test(
  x,
  mu = 160,               # Hypothesized median
  alternative = "less",   # H1: μ < 160 (since H0: μ ≥ 160)
  conf.int = TRUE,        # Compute confidence interval
  conf.level = 0.95       # 95% confidence level
)

# Print results
print(wilcox_result)

#two sample_z_test
#1a)
two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha, null_hypothesis = "equal") {
  # Calculate sample means and sizes
  x1_bar <- mean(x1)
  x2_bar <- mean(x2)
  n1 <- length(x1)
  n2 <- length(x2)

  # Calculate the Z-statistic based on the chosen null hypothesis
  if (null_hypothesis == "equal") {
    null_value <- 0
  } else if (null_hypothesis == "greater") {
    null_value <- 0  # Tests if mu1 >= mu2
  } else if (null_hypothesis == "less") {
    null_value <- 0  # Tests if mu1 <= mu2
  } else {
    stop("Invalid null hypothesis. Choose 'equal', 'greater', or 'less'.")
  }

  Z <- (x1_bar - x2_bar - null_value) / sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))

  # Find the critical Z-value based on the significance level and type of test
  if (null_hypothesis == "equal") {
    # Two-tailed test
    Z_critical <- qnorm(1 - alpha / 2)
  } else if (null_hypothesis == "greater") {
    # Right-tailed test
    Z_critical <- qnorm(1 - alpha)
  } else if (null_hypothesis == "less") {
    # Left-tailed test
    Z_critical <- qnorm(alpha)
  }

  # Calculate p-value for the test
  if (null_hypothesis == "equal") {
    p_value <- 2 * (1 - pnorm(abs(Z)))  # Two-tailed p-value
  } else if (null_hypothesis == "greater") {
    p_value <- 1 - pnorm(Z)  # Right-tailed p-value
  } else if (null_hypothesis == "less") {
    p_value <- pnorm(Z)  # Left-tailed p-value
  }

  # Conclusion based on the Z-statistic and critical value
  if (abs(Z) > Z_critical) {
    conclusion <- paste("Reject H0: There is enough evidence to reject the null hypothesis (", null_hypothesis, ").", sep = "")
  } else {
    conclusion <- paste("Fail to reject H0: There is not enough evidence to reject the null hypothesis (", null_hypothesis, ").", sep = "")
  }

  # Output results
  return(list(
    Z_statistic = Z,
    Z_critical = Z_critical,
    p_value = p_value,
    Conclusion = conclusion
  ))
}

x1 = c( 258.0, 271.5, 189.1, 216.5, 237.2, 222.0, 231.3, 181.7, 220.0, 179.3, 238.1, 217.7,
        246.2, 241.5, 233.8, 222.3, 199.2, 167.9, 216.2, 240.4, 235.3, 187.0, 233.7, 214.7,
        174.6, 246.3, 185.7, 207.0, 244.3, 237.7, 245.2, 228.3, 201.8, 218.3, 242.7, 213.8,
        231.9, 257.3, 208.4, 250.7, 198.3, 206.7, 259.7, 253.3, 200.3, 196.6, 210.6, 257.6,
        173.5, 267.5, 167.2, 227.1, 172.1, 197.6, 256.9, 203.7, 195.1, 237.4, 210.2, 208.8,
        218.0, 205.1, 241.1, 216.8, 223.6, 191.0, 225.9, 215.1, 233.1, 243.0)


x2 = c( 221.0, 213.0, 199.3, 211.2, 225.2, 229.1, 253.9, 194.6, 243.0, 221.9, 230.9, 221.1,
        206.7, 217.2, 215.8, 203.0, 234.0, 196.3, 235.8, 234.3, 244.7, 248.8, 200.5, 232.0,
        233.3, 220.6, 289.2, 244.9, 230.8, 182.9, 199.3, 263.2, 220.6, 266.7, 258.0, 243.9,
        178.1, 200.7, 270.2, 224.4, 222.4, 234.6, 296.7, 202.3, 277.9, 204.3, 221.1, 257.0,
        243.4, 239.4, 230.0, 263.5, 241.3, 216.6, 227.9, 230.1, 230.5, 188.6, 289.3, 234.4,
        267.5, 256.0, 246.5, 210.5, 270.6, 295.5, 195.8, 235.3, 245.4, 245.4)

sigma_x1 <- 24.6  # Standard deviation for sample 1
sigma_x2 <- 27.8  # Standard deviation for sample 2
alpha <- 0.05     # Significance level

# Perform the two-sample Z test for different null hypotheses

# Test for equal means (two-tailed)
result_equal <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, null_hypothesis = "equal")

# Test for greater means (right-tailed)
result_greater <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, null_hypothesis = "greater")

# Test for less means (left-tailed)
result_less <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, null_hypothesis = "less")

# # Print the results for each test
# cat("Result for Two-Tailed Test (Equal Means): \n")
# cat("Z-statistic:", result_equal$Z_statistic, "\n")
# cat("Critical Z-value:", result_equal$Z_critical, "\n")
# cat("P-value:", result_equal$p_value, "\n")
# cat("Conclusion:", result_equal$Conclusion, "\n\n")
#
# cat("Result for Right-Tailed Test (Greater Means): \n")
# cat("Z-statistic:", result_greater$Z_statistic, "\n")
# cat("Critical Z-value:", result_greater$Z_critical, "\n")
# cat("P-value:", result_greater$p_value, "\n")
# cat("Conclusion:", result_greater$Conclusion, "\n\n")

cat("Result for Left-Tailed Test (Less Means): \n")
cat("Z-statistic:", result_less$Z_statistic, "\n")
cat("Critical Z-value:", result_less$Z_critical, "\n")
cat("P-value:", result_less$p_value, "\n")
cat("Conclusion:", result_less$Conclusion, "\n")

#2a)Welch test
# Data sets
Xvar <- c(4.95, 5.37, 4.70, 4.96, 4.72, 5.17, 5.28, 5.12, 5.26, 5.48)
Yvar <- c(4.65, 4.86, 4.57, 4.56, 4.96, 4.63, 5.04, 4.92, 5.37, 4.58, 4.26, 4.40)

# Perform Welch's t-test (for unequal variances)
result_welch <- t.test(Xvar, Yvar, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

# Print the summary of the test
print(result_welch)
#2b)# Data sets for paired t-test
data_before <- c(95, 106, 79, 71, 90, 79, 71, 77, 103, 103, 92, 63, 82, 76)
data_after <- c(97, 116, 82, 81, 82, 86, 107, 86, 94, 91, 85, 98, 91, 87)

# Perform paired t-test
result_paired <- t.test(data_before, data_after, alternative = "two.sided", paired = TRUE, conf.level = 0.95)

# Print the summary of the paired t-test
print(result_paired)

#3a) two-sample proportion test

# Data for the two groups
x <- c(520, 550)  # successes (men and women who use antibiotics)
n <- c(600, 600)  # total trials (total men and women surveyed)

# Perform two-sample proportion test
result_prop_test <- prop.test(x, n, alternative = "two.sided", conf.level = 0.95)

# Print the result
print(result_prop_test)

#3b) Fischer exact test
# Contingency table for tobacco abuse in higher and lower income groups
table_data <- matrix(c(11, 42, 17, 39), nrow = 2, byrow = TRUE)

# Perform Fisher's exact test
result_fisher_test <- fisher.test(table_data, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95)

# Print the result
print(result_fisher_test)

#4 Two sample-variance test
two_sample_variance_test <- function(x, y, alpha = 0.05) {
  # Sample variances
  var_x <- var(x)
  var_y <- var(y)

  # Sample sizes
  n_x <- length(x)
  n_y <- length(y)

  # F-statistic (larger variance first for robustness)
  F_statistic <- max(var_x, var_y) / min(var_x, var_y)

  # Degrees of freedom for each sample
  df_x <- n_x - 1
  df_y <- n_y - 1

  # Critical value for F-distribution (right-tailed test)
  critical_value <- qf(1 - alpha, df_x, df_y)

  # p-value from the F-distribution
  p_value <- pf(F_statistic, df_x, df_y, lower.tail = FALSE)

  # Conclusion
  conclusion <- ifelse(F_statistic > critical_value,
                       "Reject H0: The variances are different",
                       "Fail to reject H0: The variances are equal")

  # Return the results as a list
  return(list(F_statistic = F_statistic, p_value = p_value, critical_value = critical_value, conclusion = conclusion))
}

# Data sets
x <- c(1067.7, 984.3, 998.8, 1025.9, 1060.9, 959.1, 1013.8,
       1047.0, 987.8, 1051.0, 885.2, 1049.5, 1098.2, 1001.5,
       1011.1, 991.6)

y <- c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
       1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4,
       1091.8, 952.1)

# Run the two-sample variance test with alpha = 0.05
test_result <- two_sample_variance_test(x, y, alpha = 0.05)

# Print the results
print(test_result)

#Wilcoxon signed rank test for two dependent samples

# Data for Pre_therapy and Post_therapy
Pre_therapy <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)

# Carrying out the Wilcoxon signed-rank test
wilcoxon_result <- wilcox.test(Pre_therapy, Post_therapy, alternative = "greater", paired = TRUE, conf.level = 0.95)

# Print the results
print(wilcoxon_result)

#6 Wilcoxon Rank Sum Test (Mann-Whitney Test) for Unpaired Samples

# Data for drug and placebo
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)

# Perform the Wilcoxon rank sum test (Mann-Whitney test) with alternative hypothesis that placebo has a smaller mean
wilcoxon_result <- wilcox.test(drug, placebo, alternative = "less", conf.level = 0.95)

# Print the results
print(wilcoxon_result)

#7) Kruskal-Wallis Test
# Data for the four groups
group_1 <- c(220, 214, 203, 184, 186, 200, 165)
group_2 <- c(262, 193, 225, 200, 164, 266, 179)
group_3 <- c(272, 192, 190, 208, 231, 235, 141)
group_4 <- c(190, 255, 247, 278, 230, 269, 289)

# Reformat data using rep() for labels
x <- c(group_1, group_2, group_3, group_4)
y <- rep(1:4, each = 7)  # Group labels for each value

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(x ~ as.factor(y))

# Print the results
print(kruskal_result)

#8)Chi-square-goodness fit

# Observed and Expected frequencies
observed <- c(32, 82, 77, 49)
expected <- c(40, 80, 80, 40)

# Perform the Chi-square goodness-of-fit test
chi_square_result <- chisq.test(observed, p = expected / sum(expected), rescale.p = TRUE)

# Print the results
print(chi_square_result)

# Load required packages
library(ggplot2)
library(dplyr)

### 1. Titanic Data Analysis
# (a) Histogram plots by passenger class
titanicData <- read.csv("titanic.csv")

# Plot histograms
par(mfrow=c(3,1))
hist(titanicData$age[titanicData$passenger_class=="1st"], breaks=30, main="1st Class", xlab="Age", col="lightblue")
hist(titanicData$age[titanicData$passenger_class=="2nd"], breaks=30, main="2nd Class", xlab="Age", col="lightgreen")
hist(titanicData$age[titanicData$passenger_class=="3rd"], breaks=30, main="3rd Class", xlab="Age", col="lightpink")

# (b) Calculate group means and standard deviations
titanic_by_class <- group_by(titanicData, passenger_class)
summary_stats <- summarise(titanic_by_class,
                           group_mean = mean(age, na.rm=TRUE),
                           group_sd = sd(age, na.rm=TRUE))
print(summary_stats)

# Conclusion about standard deviations
print("Check if standard deviations are similar to justify ANOVA")

# (c) Fit ANOVA model
lmresults <- lm(age ~ passenger_class, data=titanicData)
anova_results <- anova(lmresults)
print(anova_results)

# (d) Tukey-Kramer test
tukey_result <- TukeyHSD(aov(lmresults))
print(tukey_result)

# (e) Kruskal-Wallis test
kruskal_result <- kruskal.test(age ~ passenger_class, data=titanicData)
print(kruskal_result)

### 2. Cuckoo Egg Size Problem
# (a) Multiple histogram by host species
cuckooData <- read.csv("cuckooeggs.csv")

ggplot(cuckooData, aes(x=length, fill=host_species)) +
  geom_histogram(position="dodge", bins=30) +
  facet_wrap(~host_species) +
  labs(title="Cuckoo Egg Lengths by Host Species", x="Egg Length", y="Count")

# (b) Table of means and standard deviations
cuckoo_by_host <- group_by(cuckooData, host_species)
cuckoo_summary <- summarise(cuckoo_by_host,
                            mean_length = mean(length, na.rm=TRUE),
                            sd_length = sd(length, na.rm=TRUE))
print(cuckoo_summary)

# (c) ANOVA for cuckoo eggs
cuckoo_lm <- lm(length ~ host_species, data=cuckooData)
cuckoo_anova <- anova(cuckoo_lm)
print(cuckoo_anova)

# (d) Tukey-Kramer test for cuckoo eggs
cuckoo_tukey <- TukeyHSD(aov(cuckoo_lm))
print(cuckoo_tukey)

### 3. Maize and Malaria Problem
# (a) Multiple histogram of incidence by maize yield
malariaData <- read.csv("malaria_vs_maize.csv")

ggplot(malariaData, aes(x=incidence_rate, fill=maize_yield)) +
  geom_histogram(position="dodge", bins=30) +
  facet_wrap(~maize_yield) +
  labs(title="Malaria Incidence by Maize Production Level", x="Incidence Rate", y="Count")

# (b) Standard deviation of incidence rate by maize yield
malaria_by_maize <- group_by(malariaData, maize_yield)
malaria_summary <- summarise(malaria_by_maize,
                             sd_incidence = sd(incidence_rate, na.rm=TRUE))
print(malaria_summary)

# (c) Log transform incidence rate
malariaData$log_incidence <- log(malariaData$incidence_rate + 1)

# Plot again
ggplot(malariaData, aes(x=log_incidence, fill=maize_yield)) +
  geom_histogram(position="dodge", bins=30) +
  facet_wrap(~maize_yield) +
  labs(title="Log-transformed Malaria Incidence by Maize Production", x="Log Incidence", y="Count")

# Standard deviation of log-incidence
malaria_log_summary <- summarise(group_by(malariaData, maize_yield),
                                 sd_log_incidence = sd(log_incidence, na.rm=TRUE))
print(malaria_log_summary)

# (d) ANOVA on log-transformed data
malaria_lm <- lm(log_incidence ~ maize_yield, data=malariaData)
malaria_anova <- anova(malaria_lm)
print(malaria_anova)

### 4. Circadian Rhythms of Diseased Animals
# (a) Histograms of lifespan
circadianData <- read.csv("circadian_mutant_health.csv")

ggplot(circadianData, aes(x=lifespan, fill=group)) +
  geom_histogram(position="dodge", bins=30) +
  facet_wrap(~group) +
  labs(title="Lifespan of Flies by Group", x="Lifespan (days)", y="Count")

# (b) Kruskal-Wallis test
circadian_kruskal <- kruskal.test(lifespan ~ group, data=circadianData)
print(circadian_kruskal)







