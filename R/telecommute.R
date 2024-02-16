# Functions related to telecommuting that are not for a specific scenario

compare_telecommute <- function(cube_tc, asim_tc, jc_transl) {
	cube_tc %>%
		left_join(asim_tc) %>%
		left_join(jc_transl) %>%
		relocate(name) %>%
		select(-jobcode)
}
