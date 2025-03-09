xval=seq(1,10,0.5)
yval=30*xval/(2+xval)
plot(xval,yval,pch=19,cex=4,col="blue",type="s",lty=4)
par(mfrow=c(1,2))
x=seq(-pi,pi,0.1)
y=sin(x)
plot(x,y,type="I")
plot (x,ypch=c(4,5,6,),col=c("red","blue","violet","green"))
#1) Plot the point (2,4) with square point character and magenta color
plot(2, 4, pch=15, col="magenta",  xlab="X", ylab="Y", main="Point Plot")

# (2) Sequence of function values and plotting sin(x) and cos(x)
x_vals <- seq(-pi, pi, length.out=100)
y_sin <- sin(x_vals)
y_cos <- cos(x_vals)

plot(x_vals, y_sin, type="o", col="blue", pch=8, xlab="x", ylab="Function Value", main="Sine and Cosine")
points(x_vals, y_cos, col="red", pch=4)
lines(x_vals, y_cos, col="red")
legend("topright", legend=c("sin(x)", "cos(x)"), col=c("blue", "red"), pch=c(8, 4), lty=1)

# (3) Bar graph similar to Fig. 4.2.1 (Example Bar Data)
x <- c(1,2,3,4,5,6,7,8)
bar_data <- c(62,47,39,39,58,37,4,11)
hist<-sum(bar_data)
prob_data <- bar_data / hist

# Create bar plot and store bar positions
bar_positions <- barplot(y, ylim=c(0, 0.25),
                         names.arg=x, width=0.1, space=0.4,
                         ylab="Probability",
                         xlab=expression(italic(x)~"(number of assistance programs)"),
                         axes=FALSE, col="gray80", border="black")

# Add Y-axis with small tick marks inside
axis(2, at=seq(0, 0.25, by=0.05), las=1, tck=0.02)  

# Add X-axis with small tick marks **inside the plot**
axis(1, at=bar_positions, labels=x, tick=TRUE, tck=0.02)  

# Draw a clear X-axis line
abline(h=0, col="black", lwd=2)


# (4) 2x3 Grid of plots
8
# (i) x vs cos(x) with red color and lines
plot(x_vals, cos(x_vals), type="l", col="red", main="Cos(x)", xlab="x", ylab="cos(x)")

# (ii) x vs (x^(2/3)) + 4.2 with violet color, points and lines
plot(x_vals,(x_vals^2)/3+4.2,type="o",col="violet",lwd=2,lty=1,main="(x^2)/3+4.2",xlab="x",ylab="function value")
binomial_pmf <- function(n, k, p) {
  factorial(n) / (factorial(k) * factorial(n - k)) * (p^k) * ((1 - p)^(n - k))
}

# Function to generate Binomial Distribution (Manually)
generate_binomial <- function(n, p, samples) {
  results <- numeric(samples)
  for (i in 1:samples) {
    success <- 0
    for (j in 1:n) {
      if (runif(1) < p) {  # Simulating a Bernoulli trial
        success <- success + 1
      }
    }
    results[i] <- success
  }
  return(results)
}

# (iii) Manually Generated Histogram for Binomial(n=12, p=0.3)
binom_data1 <- generate_binomial(12, 0.3, 1000)  # Generate 1000 samples
freq_table1 <- table(binom_data1)  # Create a frequency table
barplot(freq_table1, col="lightblue", border="black", main="Binomial (n=12, p=0.3)",
        xlab="Value", ylab="Frequency")

# (iv) Manually Generated Histogram for Binomial(n=12, p=0.8)
binom_data2 <- generate_binomial(12, 0.8, 1000)  # Generate 1000 samples
freq_table2 <- table(binom_data2)  # Create a frequency table
barplot(freq_table2, col="lightgreen", border="black", main="Binomial (n=12, p=0.8)",
        xlab="Value", ylab="Frequency")

# (v) Histogram Plot Using type='h' for x=1:10 and y=50x/(x+2)
x_vals <- 1:10
y_vals <- numeric(length(x_vals))

# Calculate y values manually
for (i in seq_along(x_vals)) {
  y_vals[i] <- 50 * x_vals[i] / (x_vals[i] + 2)
}

# Define alternating colors
colors <- rep(c("blue", "orange"), length.out=length(x_vals))

# Manually plot histogram-like vertical lines
plot(x_vals, y_vals, type="h", col=colors, lwd=2, main="Custom Histogram",
     xlab="x", ylab="y Function")


# (5) Recreate slide plot with title "This is a graph"
x<-c(1,3,5,7,9,11)
y<-c(2,7,5,10,8,10)
plot(x,y,lty="dashed",col="pink",type="o",xlab = "Time",ylab = "Performance",main = "This is a graph",col.main="blue",lwd=3)

labels=c(1,3,5,7,9,11)
text(x+0.3,y,labels,col="red")
legend("topleft", inset=0.05, lty="dashed", legend="Per curve", 
       col="pink")
#6
# Set parameters for the hypergeometric distribution
N <- 500  # Total population size
K <- 50   # Number of successes in the population
n <- 30   # Sample size
num_samples <- 1000  # Number of random samples to generate

# Function to simulate the hypergeometric distribution
simulate_hypergeom <- function(N, K, n) {
  # Create the population: K successes (1's) and N-K failures (0's)
  population <- c(rep(1, K), rep(0, N - K))
  
  # Sample n elements without replacement
  sample <- sample(population, size = n, replace = FALSE)
  
  # Count the number of successes (1's) in the sample
  return(sum(sample))
}

# Generate a large number of random samples
samples <- numeric(num_samples)
for (i in 1:num_samples) {
  samples[i] <- simulate_hypergeom(N, K, n)
}

# Plot the histogram of the hypergeometric distribution
hist(samples, probability = TRUE, col = "lightblue", 
     main = "Simulated Hypergeometric Distribution (N=500, K=50, n=30)", 
     xlab = "Number of successes in sample", ylab = "Density", 
     border = "black")

#ex 7
library(ggplot2)

# Set parameters for the hypergeometric distribution
N <- 100  # Total population size
K <- 50   # Number of successes in the population
max_n <- N  # Maximum value of n

# Create a 3x3 grid of 9 plots
par(mfrow = c(3, 3))

# Loop through different values of n to plot the convergence
for (n in seq(10, N, by = 10)) {
  
  # Generate a hypergeometric distribution
  hypergeom_vals <- rhyper(1000, m = K, n = N - K, k = n)
  
  # Generate a binomial distribution with the same parameters
  binom_vals <- rbinom(1000, size = n, prob = K / N)
  
  # Plot the histograms for both distributions on the same plot
  hist(hypergeom_vals, probability = TRUE, col = rgb(0, 0, 1, 0.5), 
       xlim = c(0, N), ylim = c(0, 0.15), main = paste("n =", n), 
       xlab = "Value", ylab = "Density")
  
  # Add the binomial distribution as a red line
  lines(density(binom_vals), col = "red", lwd = 2)
  
  # Add a legend to differentiate the distributions
  legend("topright", legend = c("Hypergeometric", "Binomial"), 
         fill = c(rgb(0, 0, 1, 0.5), "red"))
}

# Reset plotting parameters
par(mfrow = c(1, 1))

 
   
#ex8    
     # Load necessary library
     library(ggplot2)
     
     # Define the x range for plotting
     x <- 0:50  # Range of values for the Poisson distribution (can be adjusted)
     
     # Define the lambda values for the Poisson distributions
     lambda_values <- c(3, 20, 45)
     
     # Create an empty plot
     plot(x, dpois(x, lambda = lambda_values[1]), type = "n", xlab = "x", ylab = "Probability", 
          main = "Poisson Distributions for Different λ values")
     
     # Add the Poisson distributions to the plot
     for (lambda in lambda_values) {
       lines(x, dpois(x, lambda), type = "b", pch = 19, col = rainbow(length(lambda_values))[which(lambda_values == lambda)], 
             lwd = 2, label = paste("λ =", lambda))
     }
     
     # Add a legend
     legend("topright", legend = paste("λ =", lambda_values), col = rainbow(length(lambda_values)), 
            pch = 19, lwd = 2)
     
   
     
#ex 9
     library(ggplot2)
     
     # Step (i): Load the data and plot the histogram of the height variable
     data <- read.csv("/Users/khusbuagarwal/Downloads/SOCR-HeightWeight.csv ")  
     # (i) Plot histogram for height and calculate mean and standard deviation
     height_data <- data$height
     
     # Calculate mean and standard deviation for height
     height_mean <- mean(height_data)
     height_sd <- sd(height_data)
     
     # Plot histogram for height
     ggplot(data, aes(x = height)) +
       geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
       labs(title = "Histogram of Heights", x = "Height", y = "Frequency") +
       theme_minimal()
     
     # Print the mean and standard deviation for height
     print(paste("Height Mean: ", height_mean))
     print(paste("Height Standard Deviation: ", height_sd))
     
     # (ii) Plot histogram for weight and calculate mean and standard deviation
     weight_data <- data$weight
     
     # Calculate mean and standard deviation for weight
     weight_mean <- mean(weight_data)
     weight_sd <- sd(weight_data)
     
     # Plot histogram for weight
     ggplot(data, aes(x = weight)) +
       geom_histogram(binwidth = 2, fill = "lightgreen", color = "black", alpha = 0.7) +
       labs(title = "Histogram of Weights", x = "Weight", y = "Frequency") +
       theme_minimal()
     
     # Print the mean and standard deviation for weight
     print(paste("Weight Mean: ", weight_mean))
     print(paste("Weight Standard Deviation: ", weight_sd))
     
     # (iii) Plot the Gaussian curve for Z-transformation (Standardized Z-Score)
     
     # Create a function for the Gaussian PDF (Normal Distribution)
     gaussian_curve <- function(x, mean, sd) {
       return(dnorm(x, mean = mean, sd = sd))
     }
     
     # Create Z-values for height and weight
     z_height <- (height_data - height_mean) / height_sd
     z_weight <- (weight_data - weight_mean) / weight_sd
     
     # Plot the Gaussian curve for height
     ggplot(data.frame(z = z_height), aes(x = z)) +
       geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
       stat_function(fun = gaussian_curve, args = list(mean = 0, sd = 1), color = "red", size = 1) +
       labs(title = "Z-Score Transformation for Height", x = "Z-Score", y = "Density") +
       theme_minimal()
     
     # Plot the Gaussian curve for weight
     ggplot(data.frame(z = z_weight), aes(x = z)) +
       geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
       stat_function(fun = gaussian_curve, args = list(mean = 0, sd = 1), color = "red", size = 1) +
       labs(title = "Z-Score Transformation for Weight", x = "Z-Score", y = "Density") +
       theme_minimal()
     
     # (iv) Plot histograms with decreasing bin sizes in a 1x3 grid
     
     par(mfrow = c(1, 3))  # Set up the plotting grid
     
     # Plot histograms with different bin sizes for height
     hist(height_data, breaks = 50, main = "Height (Bins = 50)", xlab = "Height", col = "lightblue", border = "black")
     hist(height_data, breaks = 30, main = "Height (Bins = 30)", xlab = "Height", col = "lightblue", border = "black")
     hist(height_data, breaks = 10, main = "Height (Bins = 10)", xlab = "Height", col = "lightblue", border = "black")
     
     # Reset plot grid
     par(mfrow = c(1, 1))
     
   
#ex 10
     # Load necessary library
     library(ggplot2)
     
     # Set the lower and upper bounds for the Uniform distribution
     a <- 1
     b <- 2
     
     # Define the x range
     x <- seq(0, 2.5, length.out = 500)
     
     # Calculate the PDF and CDF for the Uniform distribution
     pdf_vals <- dunif(x, min = a, max = b)
     cdf_vals <- punif(x, min = a, max = b)
     
     # Create a data frame for plotting
     data <- data.frame(x = x, pdf = pdf_vals, cdf = cdf_vals)
     
     # Plot the PDF and CDF
     ggplot(data, aes(x = x)) +
       # Plot the PDF
       geom_line(aes(y = pdf), color = "blue") +
       # Shade the region under the PDF up to x = 1.5
       geom_area(data = subset(data, x <= 1.5), aes(y = pdf), fill = "lightblue", alpha = 0.5) +
       # Plot the CDF on a secondary axis
       geom_line(aes(y = cdf), color = "red", linetype = "dashed") +
       # Labels and title
       labs(title = "Uniform Distribution U(1,2): PDF and CDF",
            x = "x", y = "Density") +
       theme_minimal() +
       theme(legend.position = "top") +
       scale_y_continuous(
         name = "Density",
         sec.axis = sec_axis(~., name = "CDF")
       )
    
#ex 11
     # Load necessary library
     library(ggplot2)
     
     # Set the rate parameter (lambda) for the Exponential distribution
     lambda <- 10
     
     # Define the x range
     x <- seq(0, 1, length.out = 500)
     
     # Calculate the PDF and CDF for the Exponential distribution
     pdf_vals <- dexp(x, rate = lambda)
     cdf_vals <- pexp(x, rate = lambda)
     
     # Create a data frame for plotting
     data <- data.frame(x = x, pdf = pdf_vals, cdf = cdf_vals)
     
     # Plot the PDF and CDF
     ggplot(data, aes(x = x)) +
       # Plot the PDF
       geom_line(aes(y = pdf), color = "blue") +
       # Shade the region under the PDF up to x = 2.8
       geom_area(data = subset(data, x <= 2.8), aes(y = pdf), fill = "lightblue", alpha = 0.5) +
       # Plot the CDF on a secondary axis
       geom_line(aes(y = cdf), color = "red", linetype = "dashed") +
       # Labels and title
       labs(title = "Exponential Distribution (λ=10): PDF and CDF",
            x = "x", y = "Density") +
       theme_minimal() +
       theme(legend.position = "top") +
       scale_y_continuous(
         name = "Density",
         sec.axis = sec_axis(~., name = "CDF")
       )
     
    
# ex 12
     # Load necessary library
     library(ggplot2)
     
     # Set the shape (alpha) and scale (theta) parameters for the Gamma distribution
     alpha <- 5
     theta <- 3
     
     # Define the x range
     x <- seq(0, 30, length.out = 500)
     
     # Calculate the PDF and CDF for the Gamma distribution
     pdf_vals <- dgamma(x, shape = alpha, scale = theta)
     cdf_vals <- pgamma(x, shape = alpha, scale = theta)
     
     # Create a data frame for plotting
     data <- data.frame(x = x, pdf = pdf_vals, cdf = cdf_vals)
     
     # Plot the PDF and CDF
     ggplot(data, aes(x = x)) +
       # Plot the PDF
       geom_line(aes(y = pdf), color = "blue") +
       # Plot the CDF on a secondary axis
       geom_line(aes(y = cdf), color = "red", linetype = "dashed") +
       # Labels and title
       labs(title = "Gamma Distribution (α=5, θ=3): PDF and CDF",
            x = "x", y = "Density") +
       theme_minimal() +
       theme(legend.position = "top") +
       scale_y_continuous(
         name = "Density",
         sec.axis = sec_axis(~., name = "CDF")
       )
     
#ex13
     # Load necessary library
     library(ggplot2)
     
     # Set degrees of freedom
     df <- 20
     
     # Define the x range
     x <- seq(0, 50, length.out = 500)
     
     # Calculate the PDF and CDF
     pdf_vals <- dchisq(x, df)
     cdf_vals <- pchisq(x, df)
     
     # Create a data frame for plotting
     data <- data.frame(x = x, pdf = pdf_vals, cdf = cdf_vals)
     
     # Plot the PDF with shaded area
     ggplot(data, aes(x = x)) +
       # Plot the PDF
       geom_line(aes(y = pdf), color = "blue") +
       # Shade the region under the PDF up to x = 1
       geom_area(data = subset(data, x <= 1), aes(y = pdf), fill = "lightblue", alpha = 0.5) +
       # Plot the CDF on a secondary axis
       geom_line(aes(y = cdf), color = "red", linetype = "dashed") +
       # Labels and title
       labs(title = "Chi-square Distribution (df=20): PDF and CDF",
            x = "x", y = "Density") +
       theme_minimal() +
       theme(legend.position = "top") +
       scale_y_continuous(
         name = "Density",
         sec.axis = sec_axis(~., name = "CDF")
       )
   


     
     