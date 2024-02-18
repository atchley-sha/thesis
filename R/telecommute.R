# Functions related to telecommuting that are not for a specific scenario

compare_telecommute <- function(cube_tc, asim_tc) {
	asim_wide <- asim_tc %>%
		pivot_wider(names_from = "days", values_from = "value")
	cube_tc %>%
		select(-any_of(c("year", "type"))) %>%
		left_join(asim_wide)
}
