# Libraries
# 
# library(ggplot2)
# library(ggrepel)
# library(GGally)
# library(st4gi)
# 
# # Fake data for examples
# 
# env <- gl(6, 100)
# geno <- rep(gl(50, 2), 6)
# rep <- rep(gl(2, 1), 300)
# fb <- data.frame(env = env, geno = geno, rep = rep)
# 
# envef <- rnorm(6, 0, 3)
# genoef <- rnorm(100, 2)
# #
# foo <- function(x, envef, genoef) {
#     100 + envef[as.numeric(x[1])] + genoef[as.numeric(x[2])] + rnorm(1)
# }
# 
# fb[, "y1"] <- apply(fb[, c("env", "geno")], 1, foo, envef, genoef)
# fb$y2 <- 20 + fb$y1 * 0.2 + rnorm(600)
# fb$y3 <- 50 - fb$y1 * 0.1 + 0.2 * fb$y2 + rnorm(600)
# #


#' Boxplot and dotplot
#' 
#' Return a boxplot or dotplot chart
#' 
#' @param trait trait
#' @param by grouped by
#' @param dots use dot 
#' @param fb field book data
#' @description Plots two types of graphics: a boxplot and doplot.
#' @author Omar Benites Raul Eyzaguirre
#' @export


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

# plot_box("y1", fb = fb)
# plot_box("y1", dots = "yes", fb = fb)
# plot_box("y1", "env", fb = fb)
# plot_box("y1", "env", "yes", fb)
# 

#' Histogram for plant breeding traits
#' 
#' @param trait trait
#' @param bins number of partitions or intervals.
#' @param by grouped by
#' @param fb field book data
#' @author Raul Eyzaguirre Omar Benites
#' @description Plots two types of histogram: By trait and by trait x genotypes.
#' @export
#' 

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
# 
# plot_hist("y1", 15, fb = fb)
# plot_hist("y1", 20, "env", fb)


#' Density plot
#' @description Plots genotypes. Two options:  1. original values or 2, means over replications
#' @param trait trait
#' @param by grouped by
#' @param fb field book data
#' @author Raul Eyzaguirre Omar Benites
#' @export
#' 
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

# plot_dens("y1", fb = fb)
# plot_dens("y1", "env", fb)
#   
#   
#'  Scatterplot for two traits
#' @description Options: 1. Original values, 2. Means over replications and  3. Means over replications and environments. 
#' With facets: One facet for each environment
#' @param trait.1 first trait
#' @param trait.2 second trait
#' @param by grouped by
#' @param fb field book data
#' @author Raul Eyzaguirre Omar Benites
#' @export
#' 
plot_scat <- function(trait.1, trait.2, by = NULL, fb) {
  
  if (is.null(by)) {
    ggplot(fb, aes_string(trait.1, trait.2)) +
      geom_point() +
      geom_smooth()    
  } else {
    ggplot(fb, aes_string(trait.1, trait.2)) +
      geom_point() +
      geom_smooth() +
      facet_wrap(by)
  }
  
}

# plot_scat("y1", "y2", fb = fb)
# plot_scat("y1", "y2", "env", fb)


#' Scatterplot for more than two traits
#' 
#' @param traits names of the traits. At least 2 to display a dispersion matrix.
#' @param fb field book data
#' @author Raul Eyzaguirre Omar Benites
#' @description Options: 1. Original values. 2. Means over replications. 
#' 3. Means over replications and environments.
#' @export
#' 
plot_pairs <- function(traits, fb) {
  
  ggpairs(data = fb, columns = traits, lower = list(continuous = "smooth"))
  
}

#plot_pairs(traits, fb)


#' AMMI and GGE 
#' @param model model for the ammi or gge  
#' @param biplot 1. genotype and environment effects. 2. principal components
#' @description Only for MET data should run function ammi first.
#' @author Raul Eyzaguirre
#' @export
#' 
plot_ammi <- function(model, biplot) {
  
  # arguments
  Trait <- Method <- Overall_mean <- Genotype_means <- Environment_means <- Interaction_means <- PC_values_genotypes <- Contribution_PCs <- Cont <- NULL
  
  method <- model$Method
  trait <- model$Trait
  overall.mean <- model$Overall_mean
  geno.mean <- model$Genotype_means
  env.mean <- model$Environment_means
  int.mean <- model$Interaction_means
  G <- model$PC_values_genotypes
  E <- model$PC_values_environments
  PC.cont <- model$Contribution_PCs$Cont
  
  #  Biplot 1
  
  if (biplot == 1) {
    
    title <- paste(method, " biplot1 for ", trait, sep = "")
    xlab <- "Genotype and environment effects"
    ylab <- paste("PC1 (", format(PC.cont[1], digits = 3), "%)")
    
    xcorg <- geno.mean - overall.mean
    xcore <- env.mean - overall.mean
    
    fbg <- data.frame(names = rownames(int.mean), x = xcorg, y = G[, 1], group = "g")
    fbe <- data.frame(names = colnames(int.mean), x = xcore, y = E[, 1], group = "e")
    fb <- rbind(fbg, fbe)
    
  }
  
  # Biplot 2
  
  if (biplot == 2) {
    
    title <- paste(method, " biplot2 for ", trait, sep = "")
    xlab <- paste("PC1 (", format(PC.cont[1], digits = 3), "%)")
    ylab <- paste("PC2 (", format(PC.cont[2], digits = 3), "%)")
    
    fbg <- data.frame(names = rownames(int.mean), x = G[, 1], y = G[, 2], group = "g")
    fbe <- data.frame(names = colnames(int.mean), x = E[, 1], y = E[, 2], group = "e")
    fb <- rbind(fbg, fbe)
    
  }  
  
  ggplot(fb, aes(x, y, label = names, color = factor(group))) + 
    geom_point() + 
    geom_text_repel() +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    labs(title = title, x = xlab, y = ylab) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none")
  
}

# model <- ammi("y", "geno", "env", "rep", met8x12)
# plot_ammi(model, 1)
# plot_ammi(model, 2)
# 
# model <- ammi("y", "geno", "env", "rep", met8x12, method = "gge")
# plot_ammi(model, 1)
# plot_ammi(model, 2)
