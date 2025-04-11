#ex1.1
x<-seq(1,100)
print(x)
s<-sample(x,10)
print(s)
print(x)
# it is happening without replacement
print(sample(x,10,replace=TRUE))#with replacement
print(x)
#ex1.2
install.packages("gtools")
library(gtools)
x<-c("A","B","C","D")
per<-permutations(n=length(x),r=3,v=x,repeats.allowed=TRUE)
print(per)
comb<-combinations(n=length(x),r=3,v=x)
#r is the size of the target , v is the source vector,n is the size of the source vector
print(comb)
#ex2.1
#a)probability value for the combination numbers
print(dbinom(3,10,0.4))# syntax is dbinom(m,n,p)
#b)cummulative probability
cum_prob->pbinom(3,10,0.4))
print(cum_prob)
#c) m value corresponding to cummulative probability of 0.8
print(qbinom(0.8,10,0.4))
#m=4
#d)print 5 points randomly
print(rbinom(5,10,0.4))
#e)plot PDF
# Parameters
x <- seq(0, 1, by = 0.01)

# Set parameters
n <- 10  # Number of trials
p1 <- 0.5  # Probability of success
p2 <- 0.7  # Second probability for comparison

# Define range of possible successes
x_vals <- 0:n

# Calculate binomial probabilities for both p values
pdf_p1 <- dbinom(x_vals, size = n, prob = p1)
pdf_p2 <- dbinom(x_vals, size = n, prob = p2)

# Plot PDF for p1
plot(x_vals, pdf_p1, type = "b", col = "blue", lwd = 2,
     xlab = "Number of Successes", ylab = "Probability",
     main = "Binomial PDF for p=0.5 and p=0.7")
lines(x_vals, pdf_p2, type = "b", col = "red", lwd = 2)
legend("topright", legend = c("p = 0.5", "p = 0.7"),
       col = c("blue", "red"), lwd = 2)
#f)
# Generate 100 and 10,000 random samples from Binomial distribution
set.seed(123)  # For reproducibility
samples_100 <- rbinom(100, size = n, prob = p1)
samples_10000 <- rbinom(10000, size = n, prob = p1)

# Create frequency tables
freq_100 <- table(samples_100)
freq_10000 <- table(samples_10000)

# Plot frequency tables in a 2x1 grid
par(mfrow = c(2, 1))  # 2 rows, 1 column layout

# Plot for 100 samples
barplot(freq_100, col = "skyblue",
        main = "Frequency of 100 Samples",
        xlab = "Number of Successes", ylab = "Frequency")

# Plot for 10,000 samples
barplot(freq_10000, col = "orange",
        main = "Frequency of 10,000 Samples",
        xlab = "Number of Successes", ylab = "Frequency")
#2.2
#a)# Set parameters
N <- 100  # Total population size
K <- 70   # Number of success states in the population
n <- 12   # Number of draws (without replacement)
p <- 0.3  # Probability (not used in hypergeometric, included for reference)

# Define the range of possible values
x_vals <- 0:n

# Compute hypergeometric PDF
pdf_vals <- dhyper(x_vals, K, N - K, n)

# Plot the PDF as a histogram type plot
barplot(pdf_vals, names.arg = x_vals, col = "purple",
        main = "Hypergeometric PDF: N=100, K=70, n=12",
        xlab = "Number of Successes", ylab = "Probability")

# Add text with parameter names and values
text(6, max(pdf_vals) * 0.9, labels = paste(
  "N =", N, "\nK =", K, "\nn =", n, "\np =", p
), col = "red")

# (b) Compute cumulative probability up to x = 10
cum_prob_10 <- phyper(10, K, N - K, n)
rou<-round(cum_prob_10, 3)
print(rou)


# (c) Obtain x value for cumulative probability of 0.9
x_90 <- qhyper(0.9, K, N - K, n)
cat("x value for cumulative probability of 0.9:", x_90, "\n")

# (d) Sample 5 random points from the hypergeometric distribution
set.seed(123)  # For reproducibility
samples_5 <- rhyper(5, K, N - K, n)

# Print sampled points with two significant digits
cat("Randomly sampled points (2 significant digits):", format(round(samples_5, 2), nsmall = 2), "\n")

#2.3
#a)plot 2 probability density functions
# Set the layout for 1x2 grid
par(mfrow = c(1, 2))

# Define the range of trials
x <- 0:20

# Plot for p = 0.3
p1 <- 0.3
y1 <- dgeom(x, p1)
plot(x, y1, type = "h", col = "blue", lwd = 2,
     xlab = "Number of Trials", ylab = "Probability",
     main = "Geometric PDF (p = 0.3)")

# Plot for p = 0.8
p2 <- 0.8
y2 <- dgeom(x, p2)
plot(x, y2, type = "h", col = "red", lwd = 2,
     xlab = "Number of Trials", ylab = "Probability",
     main = "Geometric PDF (p = 0.8)"
     
     #b)cummalative frequency
     cum_prob<-pgeom(4,0.3)#for probaility of 0.3
     print(cum_prob)
     cum_prob1<-pgeom(4,0.8)#for probability of0.8
     print(cum_prob1)
     #c)compute the value of m
     val<-qgeom(0.2,0.3)#for p1=0.3
     print(val)
     val_1<- qgeom (0.2,0.8)
     print(val_1)
     #d) 6 random deviates
     ran_1<-rgeom(6,0.4)
     print(ran_1)
     
     #2.4
     #a)PDF
     prob<-dnbinom(5,3,0.3)
     print(prob)
     #b)compute cummalative PDF upto
     pro1<-pnbinom(5,3,0.3)
     print(pro1)
     #c)finding y value
     val_y<-qnbinom(0.5,3,0.3)
     print(val_y)
     #d)print 4
     rand_4<-rnbinom(4,3,0.3)
     print(rand_4)
     #e)plot
     p<-0.3
     r<-10
     y_vals<-0:50 # range of y before failures
     
     
     nb_pro<-dnbinom(y_vals,r,p)
     print(nb_pro)
     
     plot(y_vals,nb_pro,type="h",col="pink",lwd=2,main="Negative Binomial Distribution(r=10,p=0.3)",xlab="Number of Failures(y)",ylab="Probability")
     #f) histogram
     # Parameters
     r <- 10  # Number of successes
     p <- 0.3  # Probability of success
     
     # Generate 10,000 random deviates
     nb_data <- rnbinom(10000, size = r, prob = p)
     
     # Plot the frequency histogram
     hist(nb_data, breaks = 50, col = "skyblue", border = "black",
          main = "Histogram of 10,000 Random Deviates (r=10, p=0.3)",
          xlab = "Number of Failures (y)", ylab = "Frequency")
     #2.5
     #a)Poisson Probability
     pp<-dpois(7,10)
     print(pp)
     #b)cummulative probability
     cum_pois_prob<-ppois(7,10)
     print(cum_pois_prob)
     # for smaller p value binomial is merging to poisson
     
     # (c) Make two barplots showing a binomial probability distribution with n = 1000, p =0.3 and a Poisson PDF with λ = np. Do the two distributions agree? Why? Why not?
     n <- 5000
     p <- 0.018
     lambda_pois <- n * p  # Poisson equivalent of Binomial
     
     # Generate probability mass functions
     x_vals <- 0:1000  # Range of possible values
     binom_pmf <- dbinom(x_vals, n, p)
     poisson_pmf <- dpois(x_vals, lambda_pois)
     
     # Plot in a 1x2 grid: Binomial and Poisson separately
     par(mfrow = c(1, 2))  # 1x2 grid for individual plots
     
     # Binomial Distribution Plot
     plot(x_vals, binom_pmf, type = "h", col = "blue", lwd = 1.5, xlim =c(25,75),
          main = "Binomial Distribution (n=5000, p=0.05)",
          xlab = "Number of Successes", ylab = "Probability")
     
     # Poisson Distribution Plot
     plot(x_vals, poisson_pmf, type = "h", col = "green", lwd = 1.5, xlim =c(25,75),
          main = "Poisson Approximation (λ=250)",
          xlab = "Number of Events", ylab = "Probability")
     
     # Reset layout to 1 plot
     par(mfrow = c(1, 1))
     
     # Overlay plot for Binomial vs Poisson
     plot(x_vals, binom_pmf, type = "h", col = "blue", lwd = 1.5,xlim=c(25,75),
          main = "Binomial vs Poisson Approximation",
          xlab = "Number of Events/Successes", ylab = "Probability",
          ylim = c(0, max(binom_pmf, poisson_pmf)))
     
     # Overlay Poisson Distribution on the same plot
     lines(x_vals, poisson_pmf, type = "h", col = "green", lwd = 1.2)
     
     # Add a legend to differentiate the distributions
     legend("topright", legend = c("Binomial (n=5000, p=0.05)", "Poisson (λ=250)"),
            col = c("blue", "green"), lty = 1, lwd = 1.5)
     
     #d)quantile
     quan<-qpois(0.22,10)
     print(quan)
     
     #e)histogram
     nb_da<-rpois(10000,9)
     
     hist(nb_da,breaks=50,col="violetred1",main="Histogram",xlab="lambda",ylab="Frequency")
     
     #2.6 gaussian
     
     
     # Load necessary libraries
     library(MASS)
     
     
     # (a) Compute unit normal PDF for μ = 12, σ = 2
     mu <- 12
     sigma <- 2
     pdf_value <- dnorm(12, mean = mu, sd = sigma)
     print(pdf_value)
     
     
     # (b) Cumulative probability for Z = 2.0 and check with 1 - CPDF(Z = -2)
     cpdf_value <- pnorm(2)
     print(cpdf_value)
     cpdf_neg2 <- 1 - pnorm(-2)
     print(cpdf_neg2)
     # (c) Plot normal curve for μ = 12, σ = 2 and add text box
     x_vals <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 100)
     y_vals <- dnorm(x_vals, mean = mu, sd = sigma)
     plot(x_vals, y_vals, type = "l", col = "hotpink", lwd = 2,
          main = "Normal Distribution (μ = 12, σ = 2)",
          xlab = "X", ylab = "Density")
     text(mu, max(y_vals) * 0.9, expression(paste(mu == 12, ", ", sigma == 2)), cex = 1.2, col = "red")
     
     
     # (d) 75th quantile for normal distribution with μ = 12, σ = 2
     quantile_75 <- qnorm(0.75, mean = mu, sd = sigma)
     print(quantile_75)
     
     # (e) Generate 10,000 random samples and plot histogram with normal curve
     normal_samples <- rnorm(10000, mean = mu, sd = sigma)
     hist(normal_samples, breaks = 50, col = "magenta", border = "black",
          main = "Histogram of 10,000 Samples (μ = 12, σ = 2)",
          xlab = "X", ylab = "Frequency", probability = TRUE)
     lines(x_vals, y_vals, col = "salmon", lwd = 2)
     
     # (f) Histogram plot of a ‘normalised’ binomial distribution with μ = np = 10 and p = 0.5.
     # Set parameters
     # Binomial parameters
     # for n value less - use less breaks, for higer n use higher breaks
     n <- 1000
     p <- 0.5
     mu_bin <- n * p
     sigma_bin <- sqrt(n * p * (1 - p))
     
     # Generate binomial samples
     m <- rbinom(10000, size = n, prob = p)
     
     # Compute W
     W <- (m - mu_bin) / sigma_bin
     
     # Plot histogram of W
     hist(W, breaks = 15, probability = TRUE, col = "skyblue",
          main = "Normalized Binomial vs Standard Normal",
          xlab = "W", xlim = c(-4, 4))
     
     # Overlay standard normal curve
     curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4, add = TRUE, col = "red", lwd = 2)
     
     legend("topright", legend = c("Normalized Binomial", "Standard Normal N(0,1)"),
            fill = c("skyblue", NA), border = NA, col = c("skyblue", "red"), lwd = 2,inset=0.05)
     
     # (h) Generate correlated normal data using MASS package
     xy <- mvrnorm(1000, mu = c(50, 60), Sigma = matrix(c(4, 3.7, 3.7, 9), 2))
     covariance_matrix <- var(xy)
     print(covariance_matrix)
     
     # Extract x and y and plot them
     x <- xy[, 1]
     y <- xy[, 2]
     plot(x, y, main = "Scatter Plot of x and y", xlab = "x", ylab = "y")
     print(var(x))
     print(var(y))
     
     # Check if sum of variances equals variance of sum
     sum_var <- var(x) + var(y)
     var_sum <- var(x + y)
     print(paste("Sum of individual variances:", sum_var))
     print(paste("Variance of the sum:", var_sum))
     
     # Compute covariance using correlation coefficient
     corr_coeff <- cor(x, y)
     computed_cov <- corr_coeff * sqrt(var(x)) * sqrt(var(y))
     print(paste("Reported covariance:", covariance_matrix[1, 2]))6
     print(paste("Computed covariance:", computed_cov))
     #2.7
     
     # (a) Generate 5 uniform random numbers between 0 and 1
     uniform_0_1 <- runif(5, min = 0, max = 1)
     print("5 Uniform random numbers between 0 and 1:")
     print(uniform_0_1)
     
     
     # (b) Generate 5 random samples from a uniform distribution between 50 and 100
     uniform_50_100 <- runif(5, min = 50, max = 100)
     print("5 Uniform random samples between 50 and 100:")
     print(uniform_50_100)
     
     
     # (c) Generate 10,000 uniform deviates and plot a histogram with x-limits 1 and 2
     uniform_samples <- runif(10000, min = 1, max = 2)
     hist(uniform_samples, breaks = 50, col = "darkred", border = "black",
          main = "Histogram of 10,000 Uniform Deviates (1 to 2)",
          xlab = "X", xlim = c(1, 2), probability = TRUE)
     
     #2.8 exponential
     #a)PDF
     expo<-dexp(3,2)
     print(expo)
     #b)quantile
     quan_expo<-qexp(0.995,2)
     print(quan_expo)
     #c)plot
     # Define lambda values
     lambda_values <- c(2, 10, 100)
     
     # Create a sequence of x values from 0 to 2 for plotting
     x_vals <- seq(0, 2, length.out = 1000)
     
     # Plot the CDF for λ = 2
     plot(x_vals, pexp(x_vals, rate = lambda_values[1]), type = "l", col = "red",
          lwd = 2, ylim = c(0, 1), xlab = "x", ylab = "Cumulative Probability",
          main = "Exponential CDFs for λ = 2, 10, and 100")
     
     # Add CDF for λ = 10
     lines(x_vals, pexp(x_vals, rate = lambda_values[2]), col = "blue", lwd = 2)
     
     # Add CDF for λ = 100
     lines(x_vals, pexp(x_vals, rate = lambda_values[3]), col = "green", lwd = 2)
     
     # Add a legend
     legend("bottomright", legend = c("λ = 2", "λ = 10", "λ = 100"),
            col = c("red", "blue", "green"), lwd = 2)
     
     #d)4 random
     quan_rand<-rexp(4,2)
     print(quan_rand)
     #2.9 gamma distribution
     # (a) Plot PDFs with different alpha and theta values on a 1x2 grid
     par(mfrow = c(1, 2))  # Set a 1x2 grid for plotting
     
     # Define x values for plotting
     x_vals <- seq(0, 30, length.out = 1000)
     
     # Plot 1: Vary alpha, θ = 4
     theta_fixed <- 4
     alpha_vals_1 <- c(1, 2, 3, 4)
     colors_1 <- c("black", "blue", "red", "magenta")
     
     plot(x_vals, dgamma(x_vals, shape = alpha_vals_1[1], scale = theta_fixed), type = "l",
          col = colors_1[1], lwd = 2, xlab = "x", ylab = "Density", main = "Varying Alpha, θ = 4")
     
     for (i in 2:length(alpha_vals_1)) {
       lines(x_vals, dgamma(x_vals, shape = alpha_vals_1[i], scale = theta_fixed), col = colors_1[i], lwd = 2)
     }
     
     legend("topright", legend = paste0("α = ", alpha_vals_1), col = colors_1, lwd = 2)
     
     # Plot 2: Vary theta, α = 4
     alpha_fixed <- 4
     theta_vals_2 <- c(1, 2, 3, 4)
     colors_2 <- c("black", "blue", "red", "magenta")
     
     plot(x_vals, dgamma(x_vals, shape = alpha_fixed, scale = theta_vals_2[1]), type = "l",
          col = colors_2[1], lwd = 2, xlab = "x", ylab = "Density", main = "Varying Theta, α = 4")
     
     for (i in 2:length(theta_vals_2)) {
       lines(x_vals, dgamma(x_vals, shape = alpha_fixed, scale = theta_vals_2[i]), col = colors_2[i], lwd = 2)
     }
     
     legend("topright", legend = paste0("θ = ", theta_vals_2), col = colors_2, lwd = 2)
     
     # (b) Compute and print the probability density at x = 6, α = 4, θ = 1
     pdf_value <- dgamma(6, shape = 4, scale = 1)
     cat("PDF value at x = 6, α = 4, θ = 1:", pdf_value, "\n")
     
     # (c) Compute and print the cumulative probability up to x = 6
     cdf_value <- pgamma(6, shape = 4, scale = 1)
     cat("Cumulative probability up to x = 6:", cdf_value, "\n")
     
     # (d) Compute the x value corresponding to a cumulative probability of 0.95
     x_val_95 <- qgamma(0.95, shape = 4, scale = 1)
     cat("x value for 95% cumulative probability:", x_val_95, "\n")
     
     # (e) Generate 10,000 random deviates and plot a histogram
     random_deviates <- rgamma(10000, shape = 4, scale = 1)
     
     # Plot histogram of the deviates
     hist(random_deviates, breaks = 50, col = "lightblue", main = "Histogram of 10,000 Random Deviates",
          xlab = "x", ylab = "Frequency")
     #2.10 chi square
     # (a) Plot the Chi-Square distribution with degrees of freedom 2, 3, 5, and 10
     par(mfrow = c(1, 1))  # Single plot
     x_vals <- seq(0, 20, length.out = 1000)
     df_vals <- c(2, 3, 5, 10)  # Degrees of freedom
     colors <- c("red", "blue", "green", "purple")
     
     plot(x_vals, dchisq(x_vals, df = df_vals[1]), type = "l", col = colors[1], lwd = 2,
          xlab = "x", ylab = "Density", main = "Chi-Square Distribution for Different Degrees of Freedom")
     
     for (i in 2:length(df_vals)) {
       lines(x_vals, dchisq(x_vals, df = df_vals[i]), col = colors[i], lwd = 2)
     }
     
     legend("topright", legend = paste0("df = ", df_vals), col = colors, lwd = 2)
     
     # (b) Compute and print the probability density for x = 6 and 5 degrees of freedom
     pdf_value <- dchisq(6, df = 5)
     cat("PDF value at x = 6, df = 5:", pdf_value, "\n")
     
     # (c) Compute and print the cumulative probability up to x = 6 and 10 degrees of freedom
     cdf_value <- pchisq(6, df = 10)
     cat("Cumulative probability up to x = 6, df = 10:", cdf_value, "\n")
     
     # (d) Obtain the 85th quantile for this distribution with 6 degrees of freedom
     quantile_85 <- qchisq(0.85, df = 6)
     cat("85th quantile for df = 6:", quantile_85, "\n")
     
     # (e) Generate 10,000 random deviates and plot a histogram with 30 bins, red bars, and text
     random_deviates <- rchisq(10000, df = 6)
     
     hist(random_deviates, breaks = 30, col = "red", main = "Histogram of 10,000 Random Deviates",
          xlab = "x", ylab = "Frequency")
     text(x = 15, y = 700, labels = "r = 6", col = "black", cex = 1.5)
     
     # (f) Compute Z^2 = (x - μ)^2 / σ^2 and plot the Chi-Square PDF with 1 degree of freedom
     mu <- 2
     sigma <- 1
     x_vals_z2 <- seq(0, 20, length.out = 1000)
     z2_vals <- (x_vals_z2 - mu)^2 / sigma^2
     
     plot(x_vals_z2, dchisq(z2_vals, df = 1), type = "l", col = "blue", lwd = 2,
          xlab = "Z^2", ylab = "Density", main = "Chi-Square Distribution with df = 1")
     
     #3.1 CLT from(population)
     # Demonstration of the Central Limit Theorem using Uniform Distribution
     
     # (i) Generate 10,000 samples of 5 uniform random deviates each
     set.seed(42)  # For reproducibility
     samples <- matrix(runif(10000 * 5, min = 0, max = 10), ncol = 5, byrow = TRUE)
     
     # (ii) Calculate means and plot histogram
     sample_means <- rowMeans(samples)
     mean_of_means <- mean(sample_means)
     std_of_means <- sd(sample_means)
     
     # Create histogram
     hist(sample_means, breaks = 30, col = "lightblue",
          main = "Distribution of Sample Means (n=5) from Uniform[0,10]",
          xlab = "Sample Mean", ylab = "Frequency",
          xlim = c(0, 10), border = "black")
     
     # Print statistics
     cat("Mean of sample means:", mean_of_means, "\n")
     cat("Standard deviation of sample means:", std_of_means, "\n")
     
     # (iii) Generate normal probability densities
     x <- seq(0, 10, by = 0.1)
     normal_pdf <- dnorm(x, mean = mean_of_means, sd = std_of_means)
     
     # (iv) Scale the normal PDF for our histogram
     bin_width <- 0.5  # Estimated bin width
     scaling_factor <- 10000 * bin_width
     scaled_pdf <- normal_pdf * scaling_factor
     
     # (v) Plot normal curve on top of histogram
     lines(x, scaled_pdf, col = "red", lwd = 2)
     
     # Add legend
     legend("topright", legend = c("Sample Means", "Normal Curve"),
            col = c("lightblue", "red"), lwd = c(NA, 2), pch = c(15, NA))
     
     # 3.2(from vector)
     # (i) Single Dice Throw - Uniform Distribution
     # Generate 10,000 samples of a single dice throw
     a <- sample(1:6, replace = TRUE, 10000)
     
     # Plot the distribution of the samples
     barplot(table(a) / length(a), col = "skyblue",
             main = "Uniform Distribution of Single Dice Throw",
             xlab = "Dice Face", ylab = "Relative Frequency")
     
     # (ii) Two Dice Throw - Triangular Distribution
     
     # Generate 10,000 samples of two dice throws and sum them
     b1 <- sample(1:6, replace = TRUE, 10000)
     b2 <- sample(1:6, replace = TRUE, 10000)
     b <- b1 + b2
     
     # Plot histogram of the sum of two dice
     hist(b, breaks = 11, col = "lightgreen",
          main = "Sum of Two Dice (Triangular Shape)",
          xlab = "Sum of Dice", ylab = "Frequency", probability = TRUE)
     # (iii) Three Dice Throw - Bell-Shaped Distribution
     
     # Generate 10,000 samples of three dice throws and sum them
     c1 <- sample(1:6, replace = TRUE, 10000)
     c2 <- sample(1:6, replace = TRUE, 10000)
     c3 <- sample(1:6, replace = TRUE, 10000)
     c <- c1 + c2 + c3
     
     # Plot histogram of the sum of three dice
     hist(c, breaks = 16, col = "orange",
          main = "Sum of Three Dice (Bell-Shaped Distribution)",
          xlab = "Sum of Dice", ylab = "Frequency", probability = TRUE)
     
     
     # (iv) Five Dice Throw - Approximating Normal Curve
     
     # Generate 10,000 samples of five dice throws and sum them
     d1 <- sample(1:6, replace = TRUE, 10000)
     d2 <- sample(1:6, replace = TRUE, 10000)
     d3 <- sample(1:6, replace = TRUE, 10000)
     d4 <- sample(1:6, replace = TRUE, 10000)
     d5 <- sample(1:6, replace = TRUE, 10000)
     d <- d1 + d2 + d3 + d4 + d5
     
     # Plot histogram of the sum of five dice
     hist_info <- hist(d, breaks = 26, col = "purple",
                       main = "Sum of Five Dice (Approaching Normal Curve)",
                       xlab = "Sum of Dice", ylab = "Frequency", probability = TRUE)
     
     # Calculate mean and standard deviation
     mean_d <- mean(d)
     sd_d <- sd(d)
     
     # Generate normal PDF based on the mean and SD of the dice
     x_vals <- seq(min(d), max(d), length.out = 100)
     normal_curve <- dnorm(x_vals, mean = mean_d, sd = sd_d)
     
     # Scale normal curve to match histogram height
     scaling_factor <- max(hist_info$density) / max(normal_curve)
     lines(x_vals, normal_curve * scaling_factor, col = "red", lwd = 2)
     
     # Ex 12 - ROC
     
     # Step 1: Install and Load Required Package
     # install.packages("pROC")  # Run this only if you don't have it
     library(pROC)
     
     # Step 2: Load the Data
     wine_data <- read.csv("/home/ibab/R/Lab10/winequality-white.csv", sep = ";")
     
     # Step 3: Create Binary Classification Columns for Various Thresholds
     wine_data$quality_6 <- ifelse(wine_data$quality >= 6, 1, 0)
     wine_data$quality_7 <- ifelse(wine_data$quality >= 7, 1, 0)
     wine_data$quality_8 <- ifelse(wine_data$quality >= 8, 1, 0)
     wine_data$quality_9 <- ifelse(wine_data$quality >= 9, 1, 0)
     wine_data$quality_10 <- ifelse(wine_data$quality == 10, 1, 0)
     
     # Step 4: Plot ROC Curves for Each Threshold
     
     # Threshold 6
     plot.roc(wine_data$quality_6, wine_data$alcohol,
              main = "ROC Curve - Threshold: Quality >= 6",
              legacy.axes = TRUE,
              ci = TRUE,
              print.auc = TRUE,
              identity.lwd = 2,
              print.thres = TRUE)
     
     # Threshold 7
     plot.roc(wine_data$quality_7, wine_data$alcohol,
              main = "ROC Curve - Threshold: Quality >= 7",
              legacy.axes = TRUE,
              ci = TRUE,
              print.auc = TRUE,
              identity.lwd = 2,
              print.thres = TRUE)
     
     # Threshold 8
     plot.roc(wine_data$quality_8, wine_data$alcohol,
              main = "ROC Curve - Threshold: Quality >= 8",
              legacy.axes = TRUE,
              ci = TRUE,
              print.auc = TRUE,
              identity.lwd = 2,
              print.thres = TRUE)
     
     # Threshold 9
     plot.roc(wine_data$quality_9, wine_data$alcohol,
              main = "ROC Curve - Threshold: Quality >= 9",
              legacy.axes = TRUE,
              ci = TRUE,
              print.auc = TRUE,
              identity.lwd = 2,
              print.thres = TRUE)
     
     # Threshold 10
     plot.roc(wine_data$quality_10, wine_data$alcohol,
              main = "ROC Curve - Threshold: Quality == 10",
              legacy.axes = TRUE,
              ci = TRUE,
              print.auc = TRUE,
              identity.lwd = 2,
              print.thres = TRUE)
     
     
     
     