# Libraries

library(ggplot2)

# Fake data for examples

env <- gl(6, 100)
geno <- rep(gl(50, 2), 6)
rep <- rep(gl(2, 1), 300)
fb <- data.frame(env = env, geno = geno, rep = rep)
envef <- rnorm(6, 0, 3)
genoef <- rnorm(100, 2)

foo <- function(x, envef, genoef) {
    100 + envef[as.numeric(x[1])] + genoef[as.numeric(x[2])] + rnorm(1)
}

fb[, "y"] <- apply(fb[, c("env", "geno")], 1, foo, envef, genoef)


# Boxplot and dotplot
# Plots genotypes. Two options:
# 1. Original values
# 2. Means over replications
# Use the by argument to get facets

plot_box <- function(trait, by = NULL, dots = c("no", "yes"), fb) {
  
  dots <- match.arg(dots)
  
  if (dots == "no") {
    if(is.null(by)) {
      ggplot(fb, aes_string(shQuote(""), trait)) +
        geom_boxplot() +
        xlab("")
      } else {
        ggplot(fb, aes_string(by, trait)) +
          geom_boxplot()
      }
  } else {
    if(is.null(by)) {
      ggplot(fb, aes_string(shQuote(""), trait)) +
        geom_boxplot() +
        xlab("") +
        geom_jitter(width = 0.35)
      } else {
        ggplot(fb, aes_string(by, trait)) +
          geom_boxplot() +
          geom_jitter(width = 0.35)
      }
  }
  
}

plot_box("y", fb = fb)
plot_box("y", dots = "yes", fb = fb)
plot_box("y", "env", fb = fb)
plot_box("y", "env", "yes", fb)


# Histogram
# Plots genotypes. Two options:
# 1. Original values
# 2. Means over replications
# Use the by argument to get facets

plot_hist <- function(trait, bins, by = NULL, fb) {
  
  if(is.null(by)) {
    ggplot(fb) +
      geom_histogram(aes_string(trait), bins = bins, col = 1, alpha = 0.5)
  } else {
    ggplot(fb) +
      geom_histogram(aes_string(trait), bins = bins, col = 1, alpha = 0.5) +
      facet_wrap(by)
  }

}

plot_hist("y", 15, fb = fb)
plot_hist("y", 20, "env", fb)


# Density plot
# Plots genotypes. Two options:
# 1. Original values
# 2. Means over replications
# Plots genotypes by environment. Two options:
# 1. Original values
# 2. Means over replications

plot_dens <- function(trait, by = NULL, fb) {
    
    if(is.null(by)) {
      ggplot(fb) +
        geom_density(aes_string(trait))
    } else {
      ggplot(fb) +
        geom_density(aes_string(trait)) +
        facet_wrap(by)
    }
    
}

plot_dens("y", fb = fb)
plot_dens("y", "env", fb)
  
  
# AMMI and GGE
# Only for MET data

plot_ammi <- function() {
  
  ggplot(fb)
  
}

# Scatterplot
# Plots two traits.
# Options:
# 1. Original values.
# 2. Means over replications.
# 3. Means over replications and environments.
# With facets:
# One facet for each environment
# With several traits:
# Matrix scatterplot

plot_scat <- function() {
  
  ggplot(fb)
  
}

