setwd('~/Documents/assiment/')

english_freq_words <- read.csv("English.csv", header = TRUE)

# type conversion
english_freq_words$Rank <- as.integer(as.character(english_freq_words$Rank))
english_freq_words$Count <- as.numeric(as.character(english_freq_words$Count))

# Make linear plot
plot(english_freq_words$Rank, english_freq_words$Count)

# Semi-log plot
plot(english_freq_words$Rank, english_freq_words$Count, log='y')

# Log-log plot
plot(english_freq_words$Rank, english_freq_words$Count, log='xy')

# Pearson correlation coefficient
cor.test(log(english_freq_words$Rank), log(english_freq_words$Count))

# Finding a and b in the power law Count = b * Rank^a
model <- lm(log(Count) ~ log(Rank), data=english_freq_words)
a <- model$coefficients["log(Rank)"]
log_b <- model$coefficients["(Intercept)"]
b <- exp(log_b)

# plot the fit with respect to the raw data
plot(english_freq_words$Rank, english_freq_words$Count, log='xy')
lines(english_freq_words$Rank, b * english_freq_words$Rank^a, col="red")


