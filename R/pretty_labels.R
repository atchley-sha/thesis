pretty_mode <- function(mode) {
	case_match(
		as.character(mode),
		c("drivealone", "drive_alone", "da") ~ "Drive Alone",
		"carpool" ~ "Carpool",
		"sr2" ~ "Carpool (2)",
		c("sr3", "sr3p") ~ "Carpool (3+)",
		"auto" ~ "Auto",
		"transit" ~ "Transit",
		"local" ~ "Local Transit",
		"crt" ~ "Commuter Rail",
		"bus" ~ "Bus",
		"rail" ~ "Rail",
		c("rh", "TNC") ~ "Ridehail",
		"nonmotor" ~ "Non-motorized",
		"all" ~ "All",
		.default = as.character(mode)
	) %>%
		factor(levels = c(
			"All",
			"Drive Alone",
			"Carpool",
			"Carpool (2)",
			"Carpool (3+)",
			"Auto",
			"Transit",
			"Local Transit",
			"Bus",
			"Rail",
			"Commuter Rail",
			"Ridehail",
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
		c("cube", "wfrc") ~ "WF Model",
		"popsim" ~ "PopulationSim"
	)
}
