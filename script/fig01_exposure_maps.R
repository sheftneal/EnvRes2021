# Figure 1: smoke plume example and spatial exposure summaries
#
# This script draws the three panels from prepared spatial objects. Panel A's
# example-day smoke and fire data are public NOAA HMS shapefiles committed to
# data/fig1_smokefire and are read below. 

source(file.path("script", "config.R"))
source(file.path("script", "plotting_helpers.R"))

library(classInt)
library(fields)
library(readr)
library(sf)
library(sp)

# Panel A: NOAA HMS smoke-plume polygons and active-fire points for the
# example day (June 17, 2008), converted to sp for compatibility with the
# base-graphics plotting calls used below.
sday <- as(st_read(paths$fig1_panel_a_smoke, quiet = TRUE), "Spatial")
fday <- as(st_read(paths$fig1_panel_a_fire, quiet = TRUE), "Spatial")

# fig1_panel_c must load:
#   avePM   - data frame with x, y, and pm columns
#   statloc - data frame with lon and lat columns for monitoring stations
load(paths$fig1_panel_c)

# fig1_panel_b is a data frame with x, y, and smoke_day columns.
aveSMK <- read_rds(paths$fig1_panel_b)
ca <- read_rds(paths$ca_counties)

palette_function <- colorRampPalette(
  add.alpha(c("#404096", "#63AD99", "#BEBC48", "#E66B33", "#D92120"), 0.5)
)


  #write pdf
      pdf(
          raw_figure_path("Fig1_raw.pdf"),
          width = 18,
          height = 6,
          useDingbats = FALSE
        ) 

        par(mfrow = c(1, 3))
        par(mar = c(5, 4, 0, 0))

      # Panel A: example smoke plume and active fires.
            plot(
              ca,
              lwd = 0.5,
              xlim = c(-127.2, -114.12949),
              col = "#63AD99",
              border = "white"
            )
            plot(sday, col = add.alpha("gray", 0.5), border = "gray30", lwd = 1, add = TRUE)
            points(
              fday,
              col = add.alpha("red", 0.8),
              pch = 21,
              bg = add.alpha("orange", 0.25),
              cex = 1.5
            )

          # Panel B: average annual smoke days.
          par(mar = c(4, 3, 0, 0))
                    smoke_intervals <- classIntervals(
                      aveSMK$smoke_day,
                      style = "fixed",
                      fixedBreaks = c(-1, seq(0, 40, 1), 100000)
                    )
                    smoke_colors <- findColours(smoke_intervals, palette_function(256))

                    plot(ca, lwd = 0.01, col = "gray80")
                    points(aveSMK$x, aveSMK$y, col = smoke_colors)
                    plot(ca, lwd = 0.1, border = "white", add = TRUE)

                    rect(-123.5, 34, -122.75, 37, col = NA, border = "black", lwd = 1.5)
                    plotrix::gradient.rect(
                      -123.5, 34, -122.75, 37,
                      col = add.alpha(plotrix::smoothColors(palette_function(2000)), 0.85),
                      gradient = "y",
                      border = "white"
                    )
                    text(x = -123.8, y = seq(34, 37, 3 / 4), labels = seq(0, 40, 10), cex = 1.5)

                    # Panel C: average PM2.5 and monitor locations.
                    pm_intervals <- classIntervals(
                      avePM$pm,
                      style = "fixed",
                      fixedBreaks = c(-1, seq(0, 19, 0.25), 100000)
                    )
                    pm_colors <- findColours(pm_intervals, palette_function(256))

                    plot(ca, lwd = 0.01, col = NA)
                    points(avePM$x, avePM$y, col = pm_colors)
                    plot(ca, lwd = 0.1, border = "white", add = TRUE)

                    rect(-123.5, 34, -122.75, 37, col = NA, border = "black", lwd = 1.5)
                    plotrix::gradient.rect(
                      -123.5, 34, -122.75, 37,
                      col = add.alpha(plotrix::smoothColors(palette_function(2000)), 0.85),
                      gradient = "y",
                      border = "white"
                    )
                    text(x = -123.8, y = seq(34, 37, 3 / 4), labels = seq(0, 20, 5), cex = 1.5)
                    points(
                      statloc$lon,
                      statloc$lat,
                      pch = 21,
                      bg = add.alpha("white", 0.25),
                      cex = 1.5,
                      col = add.alpha("black", 0.5)
                    )

          dev.off()
