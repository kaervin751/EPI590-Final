library(tidyverse)
library(here)
library(gtsummary)

medal_cols <- c("id", "name", "sex", "age", "height", "weight", "team", "NOC", "games",
								"year", "season", "city", "sport", "event", "medal")
medals <- read_csv(here::here("data", "raw","athlete_events.csv"),
									 skip = 1, col_names = medal_cols)
keep.variables <- c("name", "sex", "height", "weight", "NOC", "season", "sport", "event", "medal")
medals2 <- medals[1:12000, keep.variables]

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
