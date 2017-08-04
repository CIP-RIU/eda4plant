# Fake data for examples

env <- gl(6, 100)
geno <- rep(gl(50, 2), 6)
rep <- rep(gl(2, 1), 300)

fb <- data.frame(env = env, geno = geno, rep = rep)

fb$y <- rnorm(600, 100 + as.numeric(geno) * rnorm(1) + as.numeric(env))

# Dotplot
# Plots replications by genotype
# Recomended for up to 15 genotypes
# Plots genotypes by environment. Two options:
# 1. Original values
# 2. Means over replications

plot_dot <- function() {

  ggplot(fb)
  
}

# Boxplot
# Plots genotypes. Two options:
# 1. Original values
# 2. Means over replications
# Plots genotypes by environment. Two options:
# 1. Original values
# 2. Means over replications

plot_box <- function() {
  
  ggplot(fb)
  
}

# Histogram
# Plots genotypes. Two options:
# 1. Original values
# 2. Means over replications
# Plots genotypes by environment. Two options:
# 1. Original values
# 2. Means over replications

plot_hist <- function(trait, bins, fb) {
  
  ggplot(fb) +
    geom_histogram(aes_string(trait), bins = bins, col = 1, alpha = 0.5)

}

plot_hist("y", 20, fb)


# Density plot
# Plots genotypes. Two options:
# 1. Original values
# 2. Means over replications
# Plots genotypes by environment. Two options:
# 1. Original values
# 2. Means over replications

plot_dens <- function() {
  
  ggplot(fb)
  
}

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

