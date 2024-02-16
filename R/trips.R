# Functions related to trips that are not for a specific scenario

join_trip_distances <- function(trips, distances) {
	trips %>%
		left_join(distances, join_by(origin, destination))
}

join_dist_to_taz <- function(df, dist_transl) {
	df %>%
		left_join(dist_transl, join_by(origin == TAZ)) %>%
		rename(o_DIST = DIST) %>%
		left_join(dist_transl, join_by(destination == TAZ)) %>%
		rename(d_DIST = DIST)
}

#' @export
sample_trips <- function(combined_trips, prop = 0.1, weight = FALSE){
	grouped <- combined_trips %>%
		filter(trips > 0) %>%
		group_by(model, mode, purpose)

	if(weight) return(slice_sample(grouped, prop = prop, weight_by = trips))
	if(!weight) return(slice_sample(grouped, prop = prop))
}

#' @export
sum_trips_by_district <- function(trips, dist_transl) {
	trips %>%
		join_dist_to_taz(dist_transl) %>%
		# group_by(pick(any_of(
		# 	c("model", "o_DIST", "d_DIST", "purpose", "mode")))) %>%
		group_by(o_DIST, d_DIST, purpose, mode) %>%
		summarise(
			trips = sum(trips)
		) %>%
		rename(origin = o_DIST, destination = d_DIST)
}

#' @export
get_trip_diff <- function(trip_list = list()) {
	trip_list[[1]] %>%
		full_join(
			trip_list[[2]],
			join_by(origin, destination, purpose, mode),
			suffix = c("_1", "_2")) %>%
		mutate(across(
			c(trips_1, trips_2),
			\(x) replace_na(x, 0)
		)) %>%
		mutate(
			diff = trips_1 - trips_2
			# pct_diff = diff / trips_1
		) %>%
		rename_with(
			\(x) case_match(
				x,
				"trips_1" ~ names(trip_list)[1],
				"trips_2" ~ names(trip_list)[2]
			),
			.cols = c(trips_1, trips_2)
		)
}

calculate_mode_split_diff_pct <- function(trip_diff, model) {
	trip_diff %>%
		select(-any_of(c("origin", "destination", "diff"))) %>%
		group_by(purpose, mode) %>%
		summarise(across(everything(), \(x) sum(x)), .groups = "drop") %>%
		mutate(
			diff = pull(., 3) - pull(., 4),
			diff_pct = diff / pull(., 4),
			diff = NULL
		) %>%
		rename_with(\(x) paste(model, x, sep = "_"), -c(purpose, mode))
}
