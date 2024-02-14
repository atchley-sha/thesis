read_cube_taz_se_file <- function(cube_taz_se_file) {
	cube_taz_se_file %>%
		read_csv() %>%
		rename(TAZ = `;TAZID`, medinc = AVGINCOME)
}

read_cube_taz_inc_groups_file <- function(cube_taz_inc_groups_file) {
	cube_taz_inc_groups_file %>%
		read_csv() %>%
		select(TAZ = Z, INC1:INC4) %>%
		pivot_longer(
			-TAZ,
			names_to = "inc_group", values_to = "n", names_prefix = "INC")
}

read_trip_matrix <- function(omx_file) {
	omx_file %>%
		read_all_omx(names = c("auto", "transit", "nonmotor")) %>%
		pivot_longer(-c(origin, destination), names_to = "mode", values_to = "trips") %>%
		mutate(trips = trips/100) #The trip matrices are multiplied by 100
}
