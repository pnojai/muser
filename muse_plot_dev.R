setwd("/media/sf_Computing/DataScience/Columbia/muse")
library(ggplot2)
library(reshape2)
data_loc <- "data"

mus_fils <- list(c("Muse-orig.csv", "Dave"),
                 c("Muse_orig_copy.csv", "Jai"))

# Function reads a file, picks off the EEG data and returns it with a time offset.
get_mus_eeg <- function(data_loc, fil) {
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
        result
}

plot_mus_eeg <- function(mus_eeg_df) {
        ggplot(data = mus_eeg_df, aes(time_offset)) +
                geom_line(aes(y = value)) +
                facet_grid(variable ~ . ) +
                xlab("Time (sec)") +
                ylab("Muse Output (what units?)") +
                ggtitle("Subject 1")
        
}
# Get the EEG data in a list of melted data frames.
mus_eeg <- lapply(mus_fils, function(x) {
        get_mus_eeg(data_loc, x)
})

# Plot for each file in the list.
lapply(mus_eeg, plot_mus_eeg)

# # Before I melted.
# qplot(time_offset, eeg_1, data = mus_eeg[[1]], geom = "line")
# qplot(time_offset, eeg_2, data = mus_eeg[[1]], geom = "line")
# qplot(time_offset, eeg_3, data = mus_eeg[[1]], geom = "line")
# qplot(time_offset, eeg_4, data = mus_eeg[[1]], geom = "line")
# 
# ggplot(data = mus_eeg[[1]], aes(time_offset)) +
#         geom_line(aes(y = eeg_1, color = "eeg_1")) +
#         geom_line(aes(y = eeg_2, color = "eeg_2")) +
#         geom_line(aes(y = eeg_3, color = "eeg_3")) +
#         geom_line(aes(y = eeg_4, color = "eeg_4"))
        
qplot(time_offset, value, data = mus_eeg[[1]], facets = variable ~ . , geom = "line")

ggplot(data = mus_eeg[[1]], aes(time_offset)) +
        geom_line(aes(y = value)) +
        facet_grid(variable ~ . ) +
        xlab("Time (sec)") +
        ylab("Muse Output (what units?)") +
        ggtitle("Subject 1")
