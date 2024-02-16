theme_map <- function(zoom = TRUE) {
	list(
		theme(
			axis.text = element_blank(),
			axis.ticks = element_blank(),
			panel.grid = element_blank(),
			axis.title = element_blank()
		),
		if(zoom) {
			coord_sf(
				xlim =  c(-112.15,-111.6),
				ylim = c(40.2,40.8),
				expand = FALSE,
				crs = 4326)}
	)
}

transform_pseudo_log_positive <- function(sigma = 1, base = exp(1)) {
	new_transform(
		"pseudo_log_positive",
		function(x) ifelse(x >= 0, asinh(x/(2 * sigma))/log(base), x),
		function(y) ifelse(y >= 0, 2 * sigma * sinh(y * log(base)), y)
	)
}

scale_percent_diff <- function(
		high_val, low_val = -1, midpoint = 0,
		sigma = 1, base = exp(1),
		low = muted("red"), mid = "white", high = muted("blue"),
		n_positive_breaks = 4, n_negative_breaks = 4,
		positive_breaks = seq(midpoint, high_val, abs(high_val - midpoint)/n_negative_breaks),
		negative_breaks = seq(low_val, midpoint, abs(midpoint - low_val)/n_positive_breaks),
		na.value = NA
) {
	scale_fill_gradient2(
		limits = c(low_val, high_val),
		oob = oob_squish,
		low = low, mid = mid, high = high,
		midpoint = midpoint,
		na.value = na.value,
		labels = label_percent(),
		breaks = unique(c(negative_breaks, positive_breaks)),
		transform = transform_pseudo_log_positive(sigma, base),
		guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE)
	)
}
