setwd("/media/sf_Computing/DataScience/Columbia/muser")
library(ggplot2)
library(reshape2)
data_loc <- "data"

mus_fils <- list(c("Muse-Dave.csv", "Dave"),
                 c("Muse-Jai.csv", "Jai"))

# Function reads a file, picks off the EEG data and returns it with a time offset.
get_mus_eeg <- function(data_loc, fil, desc) {
        mus_dat <- read.csv(file.path(data_loc, fil))
        is_complete <- complete.cases(mus_dat$timestamps,
                                      mus_dat$eeg_1,
                                      mus_dat$eeg_2,
                                      mus_dat$eeg_3,
                                      mus_dat$eeg_4)
        mus_eeg <- mus_dat[is_complete, 1:5]
        base_time <- mus_eeg[1, 1]
        mus_eeg$time_offset <- mus_eeg[ , 1] - base_time
        mus_eeg <- mus_eeg[ , c("time_offset", "eeg_1", "eeg_2", "eeg_3", "eeg_4")]
        # Melt it.
        result <- melt(mus_eeg, id.vars = "time_offset")
        result <- list(result, desc)
}

# Function plots a melted data frame with facets for each EEG.
plot_mus_eeg <- function(mus_eeg) {
        ggplot(data = mus_eeg[[1]], aes(time_offset)) +
                geom_line(aes(y = value)) +
                facet_grid(variable ~ . ) +
                xlab("Time (sec)") +
                ylab("Muse Output (what units?)") +
                ggtitle(mus_eeg[[2]])
        
}

# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

# Get the EEG data in a list of melted data frames.
mus_eeg <- lapply(mus_fils, function(x) {
        get_mus_eeg(data_loc = data_loc,
                    fil = x[1],
                    desc = x[2])
})


# Plot for each file in the list.
g <- lapply(mus_eeg, plot_mus_eeg)

multiplot(g[[1]], g[[2]], cols = 1)
