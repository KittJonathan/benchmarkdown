# Packages ----

library(tidyverse)

theme_set(theme_bw())

# Load the data ----

file_url <- "https://raw.githubusercontent.com/StatQuest/sigs/refs/heads/main/chapter_01/spend_n_save.txt"

# Drawing a statistical distribution ----

## The clunky three-step method for drawing a statistical distribution ----

# Create an array of x-axis coordinates
x.axis <- seq(from = -5, to = 5, by = 0.1)

# Print out the first 10 values
x.axis[1:10]

# Create an array of y-axis coordinates that correspond to each value
# in x.axis
y.axis <- dnorm(x = x.axis, mean = 0, sd = 1)

# Print out the first 10 values
y.axis[1:10]

# Plot 
plot(x.axis, y.axis)
plot(x.axis, y.axis, type = "l")

# Change the color and thickness of the line
plot(x.axis, y.axis, type = "l",
     col = "blue", lwd = 20)

# Do the same, using tidyverse

tibble(x = seq(-5, 5, 0.1)) |> 
  mutate(y = dnorm(x, mean = 0, sd = 1)) |> 
  ggplot(aes(x, y)) +
  # geom_point() +
  geom_line(color = "blue", linewidth = 10)

## Using curve() to draw a normal distribution ----

curve(dnorm(x=x, mean=0, sd=1), # distribution function and parameters we want to draw
      from=-5, # minimum x-axis value
      to=5, # maximum x-axis value
      n=101) # the number of points to draw lines between

curve(dnorm(x), from = -5, to = 5, n = 101)

curve(dnorm(x), from = -5, to = 5, n = 101,
      col = "blue", lwd = 20)

# Translate into ggplot2

ggplot(data = tibble(x = c(-5, 5)),
       aes(x)) +
  stat_function(fun = dnorm, n = 101)

# Save plot

p <- ggplot(data = tibble(x = c(-5, 5)),
       aes(x)) +
  stat_function(fun = dnorm, n = 101)

ggsave(filename = "posts/test.png", plot = p, dpi = 320, height = 6, width = 12)

# Drawing an exponential distribution ----

exp.mean <- 2

curve(dexp(x=x, rate = 1/exp.mean),
      from = 0, to = 10, n = 101,
      col = "orange", lwd = 20)

ggplot(data = tibble(x = c(0, 10)),
       aes(x)) +
  stat_function(fun = dexp, args = list(rate = 1/exp.mean),
                n = 101)

# Fitting a statistical distribution to a histogram ----

spend_n_save <- read_tsv(file_url) |> 
  # clean_names() default settings transform column names using snake case 
  janitor::clean_names() |> 
  # the 'id' variable is categorical, we transform it into a factor
  mutate(id = factor(id))

head(spend_n_save)

ggplot() +
  geom_histogram(data = spend_n_save,
                 aes(x = num_apples),
                 color = "black", fill = "white", bins = 19) +
  labs(title = "Histogram of number of apples")

pop.mean <- mean(spend_n_save$num_apples)
pop.mean

pop.var <-  var(spend_n_save$num_apples)
pop.var

stats <- spend_n_save |> 
  rstatix::get_summary_stats(type = "mean_sd") |> 
  select(variable, n, mean, sd_unbiased = sd) |> 
  mutate(sd_biased = sqrt(sd_unbiased^2 * (n - 1) / n))
  
stats

val_range <- range(spend_n_save$num_apples)

ggplot(data = spend_n_save, aes(x = num_apples)) +
  geom_histogram(aes(y = after_stat(density)),
                 color = "black", fill = "white", bins = 19) +
  stat_function(fun = dnorm, args = list(mean = stats$mean, sd = stats$sd_unbiased),
                xlim = val_range, n = 101, col = "red", linewidth = 0.5) +
  labs(title = "Histogram of number of apples")

# Calculating probabilities with statistical distributions ----

pnorm(q=10, mean=stats$mean, sd=stats$sd_unbiased)

pnorm(q=15, mean=stats$mean, sd=stats$sd_unbiased)

pnorm(q=30, mean=stats$mean, sd=stats$sd_unbiased, lower.tail = FALSE)

# BONUS Generating random numbers from statistical distributions ----

set.seed(42)
rand.values <- rnorm(n = 5, mean = stats$mean, sd = stats$sd_biased)
rand.values.2 <- rnorm(n = 5000, mean = stats$mean, sd = stats$sd_biased)
rand.values

mean(rand.values)

ggplot(data = spend_n_save, aes(x = num_apples)) +
  geom_histogram(aes(y = after_stat(density)),
                 color = "black", fill = "white", bins = 19) +
  stat_function(fun = dnorm, args = list(mean = stats$mean, sd = stats$sd_unbiased),
                xlim = val_range, n = 101, col = "blue", linewidth = 0.5) +
  geom_vline(xintercept = mean(rand.values.2), col = "red") +
  labs(title = "Histogram of number of apples")
