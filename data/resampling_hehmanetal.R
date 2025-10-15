# NOTE: Downloaded from https://osf.io/82dsj/ on June 11, 2025 - edited to remove loading of packages
# Hehman, E., Xie, S. Y., Ofosu, E. K., & Nespoli, G. A. (2018). Assessing the point at which averages are stable: A tool illustrated in the context of person perception. https://doi.org/10.31234/osf.io/2n6jq
# Original code below.

# Resampling Package ----------------------------------------------------------
# This package contains functions for the resampling procedure from our paper.
#
# Written by Gabe Nespoli
# https://github.com/gabenespoli/resampling

# function RSSample -----------------------------------------------------------
RSSample <- function(Rating,
                     Stimulus = factor(),
                     iterations = 100,
                     repetitions = 500,
                     replacement = TRUE,
                     confidence = 0.95,
                     ci.method = 2,
                     center.data = TRUE) {
    # Sample data
    # Sample data. For each subset defined by unique combinations of the values
    #   of Stimulus, randomly sample an increasing number of values
    #   from Rating. This can be repeated many times at each iteration to make
    #   the sampling more robust.
    #
    # Args:
    #   Rating:       Vector of ratings to sample.
    #   Stimulus:     Factor with stimulus tokens to cycle through.
    #   iterations:   Maximum number of values to sample. Sampling will begin
    #                 with 1 value, and continue to increase by 1 until n.
    #                 value. Default 100.
    #   repetitions:  The number of times to sample at each iteration.
    #                 Default 500.
    #   replacement:  Whether to sample with replacement. Default TRUE.
    #   confidence:   Confidence of confidence interval. Default 0.95.
    #   ci.method:    1 = normal distribution, 2 = percentile
    #   center.data:  Center the data by subtracting the mean from all values.
    #                 Default TRUE.
    #
    # Returns:
    #   A named list (sampled) with the following variables:
    #   data: [data frame] A data frame with columns Stimulus, and a column
    #         for each iteration (n1, n2, n3, etc.)
    #   ci:   [data frame] A data frame with columns, n, LL (lower limit of
    #         the confidence interval), and UL (upper limit).
    
    # check that inputs are the same length
    if ((length(Stimulus) != 0) &&
        (length(Stimulus) != length(Rating))) {
        stop("The length of Rating and Stimulus must be equal.")
    }
    
    # split Rating into a list of vectors by Stimulus so we can more
    #   do things separately for each stimulus
    # TODO: test that this works with Stimulus=factor()
    if (length(Stimulus) != 0) {
        Rating.by.Stimulus <- split(Rating, Stimulus, drop = TRUE)
    } else {
        Rating.by.Stimulus <- list(data = Rating)
    }
    
    # center the ratings separately for each stimulus
    if (center.data == TRUE) {
        Rating.by.Stimulus <- lapply(Rating.by.Stimulus, RSMeanCenter)
    }
    
    # loop iterations
    ci <- data.frame() # create container for ci data
    for (n in 1:iterations) {
        time.n <- proc.time() # start time for this iteration
        
        # display progress
        # e.g., "1/24 (competent): Sampling 100 value(s) 500 time(s)... "
        cat(paste(
            "  Sampling ",
            n,
            " value(s) ",
            repetitions,
            " time(s)... ",
            sep = ""
        ))
        
        # do sampling for this n; get all repetitions
        # this returns a named list of stimuli, each item is `repetitions` long
        sampled.n.reps <- lapply(Rating.by.Stimulus,
                                 RSDoSampling,
                                 n,
                                 replacement,
                                 repetitions)
        
        # calculate CI for this n and add to ci dataframe
        thisCI <- RSGetCI(unlist(sampled.n.reps), confidence, ci.method)
        ci[n, "n"]  <- n
        ci[n, "LL"] <- thisCI[1]
        ci[n, "UL"] <- thisCI[2]
        
        # convert to a data frame, add repetition column
        sampled.n <- stack(sampled.n.reps)
        sampled.n$rep <- 1:repetitions
        sampled.n <- sampled.n[, c(2, 3, 1)] # reorder cols
        names(sampled.n) <- c("Stimulus", "Repetition", as.character(n))
        
        # add this n to the other n's sampled data
        if (n > 1) {
            sampled <- merge(sampled, sampled.n)
        } else if (n == 1) {
            sampled <- sampled.n
        }
        
        # display processing time for this iteration
        time.n <- proc.time() - time.n
        cat(paste(round(time.n["elapsed"]), " seconds.", sep = ""))
        cat("\n")
    } # end looping iterations
    
    cat("Done sampling.\n")
    return(list(data = sampled, ci = ci))
}

# function RSDoSamplng --------------------------------------------------------
RSDoSampling <- function(x,
                         n,
                         replacement = TRUE,
                         repetitions = 500) {
    # Repeatedly sample values from a vector.
    #
    # Args:
    #   x:            Vector to sample.
    #   n:            [numeric] Number of values to sample and average at each
    #                 repetition. Default 1.
    #   replacement:  [boolean] TRUE to sample with replacement; FALSE to sample
    #                 without replacement. Default TRUE.
    #   repetitions:  [numeric] The number of times to sample. Default 500.
    #
    # Returns:
    #   A vector the size of the number of repetitions.
    y <- rep(NA, repetitions) # container for repetitions
    for (i in 1:repetitions) {
        y[i] <- mean(sample(x, n, replace = replacement))
    }
    return(y)
}

# function RSMeanCenter -------------------------------------------------------
RSMeanCenter <- function(x) {
    # Center a vector on its mean by subtracting the mean from all values.
    x <- x - mean(x)
}

# function RSGetCI ------------------------------------------------------------
RSGetCI <- function(x, y = 0.95, method = 2) {
    # caller function for different confidence interval methods
    # Args:
    #   x:      Vector of values to get CI.
    #   y:      Numberic input to function being called. Default 0.95.
    #   method: Which function to call. See those functions for detail about
    #           their input. Use 1 for RSGetNormalRange, 2 for RSGetPercentile,
    #           and 3 for RSGetReducedRange.
    #
    # Returns:
    #   CI <- c(LL, UL), where LL is the lower limit and UL is the upper limit.
    if (method == 1) {
        CI <- RSGetNormalRange(x, confidence = y)
    } else if (method == 2) {
        CI <- RSGetPercentile(x, percentile = y)
    } else if (method == 3) {
        CI <- RSGetReducedRange(x, y = y)
    } else {
        stop("Invalid method for getting CI.")
    }
    return(CI)
}

# function RSGetPercentile ----------------------------------------------------
RSGetPercentile <- function(x, percentile = 0.95) {
    # Get the upper and lower percentile such that a 'percentile' amount of the
    #   values in x are contained within them.
    #
    # Args:
    #
    #
    # Returns:
    #   A 1-by-2 vector containing the lower and upper limit of the percentil CI.
    n     <- length(x)
    pct   <- (1 - percentile) / 2
    ind   <- floor(n * pct)
    x     <- sort(x)
    LL    <- x[ind]
    UL    <- x[n - ind]
    return(c(LL, UL))
}

# function RSGetNormalRange ---------------------------------------------------
RSGetNormalRange <- function(x, confidence = 0.95) {
    # Get a confidence interval (CI) for a vector. Uses the following formula:
    #   mean +- ((z*sd)/sqrt(n)), where z is the critical value.
    #
    # Args:
    #   x:          Vector from which to calculate a CI.
    #   confidence: How much confidence the interval should have. Enter as a
    #               probability between 0 and 1. Default 0.95.
    #
    # Returns:
    #   A 1-by-2 vector containing the lower and upper limit of the CI.
    
    # split the confidence in half, one each for LL and UL
    # e.g., for 0.95 this will be 1.96; confidence = 0.975 for each of LL and UL
    z     <- qnorm(confidence + ((1 - confidence) / 2))
    n     <- length(x)
    meanx <- mean(x)
    sdx   <- sd(x)
    delta <- z * (sdx / sqrt(n))
    LL    <- meanx - delta # lower limit of CI
    UL    <- meanx + delta # upper limit of CI
    return(c(LL, UL))
}

# function RSGetPointOfStability ----------------------------------------------
RSGetPointOfStability <- function(ci, threshold, inarow = 1) {
    # Get the point of stability.
    #
    # Args:
    #   ci:         Data frame of confidence intervals on which to base point of
    #               stability, as output from the RSSample function.
    #   threshold:  Threshold below which the CI must go before the data are
    #               considered stable. The absolute value of both the upper limit
    #               (UL) and the lower limit (LL) must be below the absolute value
    #               of the threshold.
    #   inarow:     Number of consecutive data points that must stay below the
    #               threshold to be considered stable.
    #
    # Returns:
    #   The n value at which the data are stable.
    threshold <- abs(threshold)
    ci.min <- apply(abs(cbind(ci$LL, ci$UL)), 1, min)
    stable <- ci.min < threshold
    stable.inarow <- logical()
    offset <- inarow - 1
    len <- length(stable) - offset
    for (i in 1:len) {
        j <- i + offset
        stable.inarow[i] <- all(stable[i:j])
    }
    pos <- min(which(stable.inarow == TRUE))
    return(pos)
}

# function RSFigure -----------------------------------------------------------
RSFigure <- function(sampled,
                     ci,
                     title = "",
                     figPNGname = "",
                     threshold = NA,
                     inarow = 1) {
    # Create plots for resampling project.
    #
    # Args:
    #   sampled:    [data.frame] The sampled data from RSSample.
    #   ci:         [data.frame] The confidence intervals from RSSample.
    #   title:      [string] Title for the figure.
    #   figPNGname: [string] Filename to save the figure to disk.
    #   threshold:  [numeric] Plot vertical line at point of stability based on
    #               this threshold. See RSGetPointOfStability for more info.
    #   inarow:     [numeric] See RSGetPointOfStability for more info.
    #
    # Returns:
    #   Plot object.
    
    # reshape for plotting
    sampled <- reshape(sampled,
                       direction = "long",
                       varying = list(3:ncol(sampled)))
    names(sampled) <- c("Stimulus", "Repetition", "n", "Rating", "id")
    
    # colour options
    cols <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))
    #cols<-colorRampPalette(brewer.pal(11, "Spectral"))
    
    myPals <- cols(length(unique(sampled$Stimulus)))
    ci.colour <- "#C75DAB"
    ci.linetype <- "solid"
    ci.size <- 1
    
    # plot each iteration and repetition
    p <- ggplot(sampled, aes(x = n, col = Stimulus)) +
        geom_line(aes(y = Rating), linewidth = 1, alpha = .2) +
        theme_minimal() +
        theme(legend.position = "none") +
        theme(
            panel.grid.major = element_line(colour = "#e8e8e8"),
            panel.grid.minor = element_line(colour = "#e8e8e8")
        ) +
        scale_color_manual(values = myPals)
    #scale_fill_viridis(option="mako",discrete=FALSE)+
    theme(legend.position = "none") +
        xlab("n") +
        ylab("Rating") +
        ggtitle(title) +
        theme(
            axis.text = element_text(size = 24),
            axis.title = element_text(size = 24, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 20)
        )
    
    # add lines for CI
    p <- p +
        geom_line(
            data = ci,
            aes(x = as.numeric(n), y = UL),
            colour = ci.colour,
            linetype = ci.linetype,
            linewidth = ci.size
        ) +
        geom_line(
            data = ci,
            aes(x = as.numeric(n), y = LL),
            colour = ci.colour,
            linetype = ci.linetype,
            linewidth = ci.size
        )
    
    # add point of stability
    if (!is.na(threshold)) {
        pos <- RSGetPointOfStability(ci, threshold, inarow)
        p <- p +
            geom_vline(
                xintercept = pos,
                colour = ci.colour,
                linetype = ci.linetype,
                linewidth = ci.size
            )
    }
    
    # references for filesizes: Medium: h=22/3, w=30/3. Large: h=35/3, w=38/3
    if (figPNGname != "") {
        ggsave(
            plot = p,
            figPNGname,
            h = 27 / 3,
            w = 30 / 3,
            type = "cairo-png",
            dpi = 72
        )
    }
    
    return(p)
}

# function RSSimulation -------------------------------------------------------
RSSimulation <- function(ssize = 1000,
                         smean = 0,
                         ssd = 1) {
    # stimulate some normally-distributed data
    data <- rnorm(ssize, smean, ssd)
    
    # TODO: restrict values to 1-7 likert
    
    # Run through RS functions
    temp <- RSSample(data)
    sampled <- temp$data
    ci <- temp$ci
    
    title <- paste("mean=", smean, ", ", "sd=", ssd, sep = "")
    p <- RSFigure(sampled, ci, title = title, threshold = 1)
    p
}
