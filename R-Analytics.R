# reading data from csv, specify interested columns using the last " [,c(column var)] "
countries_data <- read.csv("GGI2013.csv")
names(countries_data)
# full data being related 
countries_data_selected <- countries_data[ , c('X2013', 'X2012', 'X2011', 'X2010', 'X2009', 'X2008', 'X2007', 'X2006')]

# smoothed data: 2013, 2010 and 2006
countries_data_smoothed_61013 <- countries_data[ , c('X2013', 'X2010', 'X2006')]
# creating histograms of each year, individually
hist(countries_data_smoothed_61013$X2013, main = "Histogram of 2013 Data", xlab = "2013 Values", ylab = "Frequency", col = "green")
hist(countries_data_smoothed_61013$X2010, main = "Histogram of 2010 Data", xlab = "2010 Values", ylab = "Frequency", col = "blue")
hist(countries_data_smoothed_61013$X2006, main = "Histogram of 2006 Data", xlab = "2006 Values", ylab = "Frequency", col = "yellow")
# box and whisker plot: 2013, 2010 and 2006
# preparing data first
twothirteen <- countries_data_smoothed_61013$X2013
twoten <- countries_data_smoothed_61013$X2010
twosix <- countries_data_smoothed_61013$X2006
# actual boxplot
boxplot(twothirteen, twoten, twosix,
        main = "Data from 2013, 2010 and 2006 boxplots",
        at = c(1,2,3),
        names = c("2013", "2010", "2006"),
        las = 1,
        col = c("green", "blue","yellow"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
        )

# calculating mean
# na.rm: a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds
mean_2013 <- mean(twothirteen, na.rm = TRUE)
mean_2010 <- mean(twoten, na.rm = TRUE)
mean_2006 <- mean(twosix, na.rm = TRUE)
# calculating standard deviation
sd_2013 <-sd(twothirteen, na.rm = TRUE)
sd_2010 <-sd(twoten, na.rm = TRUE)
sd_2006 <-sd(twosix, na.rm = TRUE)
mean_2013
mean_2010
mean_2006
sd_2013
sd_2010
sd_2006

# z score formula
z_score_2013 <-(twothirteen - mean_2013) / sd_2013
z_score_2010 <-(twoten - mean_2010) / sd_2010
z_score_2006 <-(twosix - mean_2006) / sd_2006
z_score_2013
z_score_2010
z_score_2006

# confidence interval
# counting sample size first, since we already have mean and sd, ommiting NA by 'na.commit'
n_2013 <- length(na.omit(twothirteen))
n_2010 <- length(na.omit(twoten))
n_2006 <- length(na.omit(twosix))
# now for the formula... set Z-score = 1.96 (confidence interval)
# calculate margin of error firstly and then for the REAL FORMULA
Z <- 1.96
margin_of_error_2013 <- Z * (sd_2013/ sqrt(n_2013))
margin_of_error_2010 <- Z * (sd_2010/ sqrt(n_2010))
margin_of_error_2006 <- Z * (sd_2006/ sqrt(n_2006))
# finally, the confidence interval...
lower_bound_2013 <- mean_2013 - margin_of_error_2013
upper_bound_2013 <- mean_2013 - margin_of_error_2013

lower_bound_2010 <- mean_2010 - margin_of_error_2010
upper_bound_2010 <- mean_2010 - margin_of_error_2010

lower_bound_2006 <- mean_2006 - margin_of_error_2006
upper_bound_2006 <- mean_2006 - margin_of_error_2006

c(lower_bound_2013, upper_bound_2013)
c(lower_bound_2010, upper_bound_2010)
c(lower_bound_2006, upper_bound_2006)

# performing a paired t-test
  for_comp_2013 <- countries_data$X2013
  for_comp_2006 <- countries_data$X2006
  t_test_result_2013_2006 <- t.test(for_comp_2013, for_comp_2006, paired = TRUE)
  print(t_test_result_2013_2006)


