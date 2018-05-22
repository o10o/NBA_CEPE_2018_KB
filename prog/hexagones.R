hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

calculate_hex_coords = function(shots, binwidths) {
  xbnds = hex_bounds(shots$loc_x, binwidths[1])
  xbins = diff(xbnds) / binwidths[1]
  ybnds = hex_bounds(shots$loc_y, binwidths[2])
  ybins = diff(ybnds) / binwidths[2]
  
  hb = hexbin(
    x = shots$loc_x,
    y = shots$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots = mutate(shots, hexbin_id = hb@cID)
  
  hexbin_stats = shots %>%
    group_by(hexbin_id) %>%
    summarize(
      hex_attempts = n(),
      hex_pct = mean(real_shot_made_flag)
    )
  
  hexbin_ids_to_zones = shots %>%
    group_by(hexbin_id, shot_zone_range, shot_zone_area) %>%
    summarize(attempts = n()) %>%
    ungroup() %>%
    arrange(hexbin_id, desc(attempts)) %>%
    group_by(hexbin_id) %>%
    filter(row_number() == 1) %>%
    select(hexbin_id, shot_zone_range, shot_zone_area)
  
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx = hb@xbins / diff(hb@xbnds)
  sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers = hcell2xy(hb)
  
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
    data.frame(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }
  ))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}

calculate_hexbins_from_shots = function(shots, binwidths = c(1, 1), min_radius_factor = 0.6, fg_pct_limits = c(0.2, 0.7)) {
  if (nrow(shots) == 0) {
    return(list())
  }
  
  grouped_shots = group_by(shots, shot_zone_range, shot_zone_area)
  
  zone_stats = grouped_shots %>%
    summarize(
      zone_attempts = n(),
      zone_pct = mean(real_shot_made_flag)
    )
  
  hex_data = calculate_hex_coords(shots, binwidths = binwidths)
  
  join_keys = c("shot_zone_area", "shot_zone_range")
  
  hex_data = hex_data %>%
    inner_join(zone_stats, by = join_keys)
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  hex_data = mutate(hex_data,
                    radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]))
  
  list(hex_data = hex_data, fg_pct_limits = fg_pct_limits)
}

generate_hex_chart = function(hex_data, alpha_range = c(0.85, 0.98)) {
  
    fill_limit = hex_data$fg_pct_limits
    fill_label = "FG%"
    label_formatter = scales::percent
  
  #chargement du terrain
  court<-readPNG("./www/Lakers2.PNG")
  gcourt<- rasterGrob(court, width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)
  
  #chargement des lignes    
  gcourt2<-rasterGrob(readPNG("./www/Lakers3.PNG"), width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)
  
  #force la transparence pour les pixels non noirs
  gcourt2$raster[gcourt2$raster=="#FFFFFFFF"]="#FFFFFF00" 
  
  ggplot(data = hex_data$hex_data) +
    annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) + 
    annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) +
    geom_polygon(data = hex_data$hex_data,
                 aes_string(x = "adj_x", y = "adj_y", group = "hexbin_id",
                            fill = "bounded_fg_pct", alpha = "hex_attempts"),
                 size = 0) +
    scale_fill_distiller(type="qual",paste0(fill_label, "   "),
                         palette = "Set3",
                         limit = fill_limit,
                         labels = label_formatter,
                         guide = guide_colorbar(barwidth = 2)) +
    scale_alpha_continuous(guide = FALSE, range = alpha_range, trans = "sqrt") +
    scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
    scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
    coord_equal()+
    theme(legend.text = element_text(size = rel(0.6)),
          axis.title = element_blank(),
          axis.text = element_blank(),
          plot.margin=margin(0,0,0,0),
          legend.position="right")
}
