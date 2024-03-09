pretty_mode <- function(mode) {
	case_match(
		as.character(mode),
		c("drivealone", "drive_alone") ~ "Drive Alone",
		"carpool" ~ "Carpool",
		"auto" ~ "Auto",
		"transit" ~ "Transit",
		"nonmotor" ~ "Non-motorized",
		"all" ~ "All",
		.default = as.character(mode)
	) %>%
		factor(levels = c(
			"All",
			"Drive Alone",
			"Carpool",
			"Auto",
			"Transit",
			"Non-motorized")
		)
}

pretty_purpose <- function(purpose) {
	case_match(
		as.character(purpose),
		"hbw" ~ "Home-based Work",
		"hbo" ~ "Home-based Other",
		"nhb" ~ "Non\u2013home-based",
		"all" ~ "All",
		.default = str_to_title(purpose)
	) %>%
		factor(levels = c(
			"All",
			"Home-based Work",
			"Home-based Other",
			"Non\u2013home-based",
			"Work", "School", "Other", "At-Work")
		)
}

pretty_model <- function(model) {
	case_match(
		model,
		"asim" ~ "ActivitySim",
		"cube" ~ "WFRC Model",
		"popsim" ~ "PopulationSim"
	)
}
