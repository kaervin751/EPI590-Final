library(tidyverse)
library(here)
library(gtsummary)
library(ggplot2)
medal_cols <- c("id", "name", "sex", "age", "height", "weight", "team", "NOC", "games",
								"year", "season", "city", "sport", "event", "medal")
medals <- read_csv(here::here("data", "raw","athlete_events.csv"),
									 skip = 1, col_names = medal_cols)

#Question 1
tbl_summary(
	medals,
	by = sex,
	include = c(NOC, season, height, weight),
	label = list(
		NOC ~ "Country",
		season ~ "Summer/Winter",
		height ~ "Height",
		weight ~ "Weight"
	),
	missing_text = "Missing")|>
	add_p(test = list(all_continuous () ~ "t.test",
										all_categorical() ~ "chisq.test"))|>
	add_overall(col_label = "**Total**|>")|>
	bold_labels()|>
	modify_footnote(update = everything() ~ NA)|>
	modify_header(label = "**Variable**", p.value = "**P**")

#Question 2
linear_mod <- lm(age ~ height + weight, data = medals)
tbl_regression(
	linear_mod,
	intercept = TRUE)

#creating a histogram
histogram<- ggplot(medals, aes(x=height))+
	geom_histogram(binwidth = 10, fill = "white", color = "black") +
	theme_minimal() +
	labs(title = "Distribution of Heights", x = "Height (cm)", y = "Frequency")


histogram2<- hist(medals$height,
		 main = "Frequency of height of olympians",
		 xlab = "Height in cm",
		 ylab = "Frequency"
		 )

ggsave("histogram.png", plot=histogram, width = 8, height = 6, dpi=100)

ggsave(plot = histogram,
			 filename = here::here("docs", "histogram.png"))

#Question 4 - write a function
x <- medals$height

new_mean <- function(x){
	n <- length(na.omit(x))
	mean_val <- sum(x, na.rm = TRUE)/n
	return(mean_val)}

new_mean(x)

mean(medals$height, na.rm=TRUE)
