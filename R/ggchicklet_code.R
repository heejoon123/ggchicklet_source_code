#' Reference Documentation URL: https://github.com/hrbrmstr/ggchicklet/tree/master 
#' Date File Created: May 30, 2024
#' Purpose: Making a code file for ggchicklet to use with newer versions of R.

# OVERALL FUNCTIONS # 
`%l0%` <- function(x, y) if (length(x) == 0) y else x
`%||%` <- function(x, y) if (is.null(x)) y else x

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

# BARPLOT RECTANGLE FUNCTIONS # 
GeomRrect <- ggplot2::ggproto(
  "GeomRrect", ggplot2::Geom,
  
  default_aes = ggplot2::aes(colour = NA, fill = "grey35", size = 0.5, linetype = 1, alpha = NA),
  
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  
  draw_panel = function(self, data, panel_params, coord, radius = grid::unit(6, "pt")) {
    
    coords <- coord$transform(data, panel_params)
    
    lapply(1:length(coords$xmin), function(i) {
      
      grid::roundrectGrob(
        coords$xmin[i], coords$ymax[i],
        width = (coords$xmax[i] - coords$xmin[i]),
        height = (coords$ymax[i] - coords$ymin)[i],
        r = radius,
        default.units = "native",
        just = c("left", "top"),
        gp = grid::gpar(
          col = coords$colour[i],
          fill = alpha(coords$fill[i], coords$alpha[i]),
          lwd = coords$size[i] * .pt,
          lty = coords$linetype[i],
          lineend = "butt")
      )
      
    }) -> gl
    
    grobs <- do.call(grid::gList, gl)
    ggname("geom_rrect", grid::grobTree(children = grobs))
    
  },
  
  draw_key = ggplot2::draw_key_polygon
)

geom_rrect <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                       radius = grid::unit(6, "pt"), ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  
  layer(data = data, mapping = mapping, stat = stat, 
        geom = GeomRrect, position = position, 
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(radius = radius, na.rm = na.rm, ...))
}

# CURVE THE RECTANGLES # 
geom_chicklet <- function(mapping = NULL, data = NULL, position = ggplot2::position_stack(reverse = TRUE), 
                          radius = grid::unit(3, "pt"), ..., width = NULL, na.rm = FALSE, 
                          show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data, mapping = mapping, stat = "identity",
    geom = GeomChicklet, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = list(
      width = width, radius = radius, na.rm = na.rm, ...)
  )
}

draw_key_rrect <- function(data, params, size) { 
  grid::roundrectGrob(
    r = min(params$radius, unit(3, "pt")),
    default.units = "native",
    width = 1, height = 0.6,
    name = "lkey",
    gp = grid::gpar(
      col = params$color %l0% "white",
      fill = alpha(data$fill %||% data$colour %||% "grey20", data$alpha),
      lty = data$linetype %||% 1)
  )
}

GeomChicklet <- ggproto(
  "GeomChicklet", GeomRrect,
  
  required_aes = c("x", "y"),
  
  default_aes = ggplot2::aes(colour = "white", fill = "grey35", size = 0.5, linetype = 1, alpha = NA),
  
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    transform(data,
              ymin = pmin(y, 0), ymax = pmax(y, 0),
              xmin = x - width / 2, xmax = x + width / 2, width = NULL)
  },
  
  draw_panel = function(self, data, panel_params, coord,width = NULL, radius = grid::unit(3, "pt")) {
    ggproto_parent(GeomRrect, self)$draw_panel(data, panel_params, coord, radius = radius)
  },
  
  draw_key = draw_key_rrect
  
)









