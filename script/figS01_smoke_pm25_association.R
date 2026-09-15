# Figure S1: association between smoke days and total PM2.5
#
# Panel (a) summarizes the cross-sectional relationship. Panel (b) uses
# residual variation after zip-by-month and county-by-year fixed effects.

source(file.path("script", "config.R"))
source(file.path("script", "plotting_helpers.R"))

library(tidyverse)
library(lfe)



pm <- read_csv(paths$monthly_pm25)
smoke <- read_rds(paths$monthly_smoke)


pdat <- left_join(pm, smoke) %>% filter(!is.na(pm25) & !is.na(smoke_day))


county <- read_csv(paths$zip_county_crosswalk)
pdat <- left_join(pdat, county)

pdat$mzip_month <- paste(pdat$mzip, pdat$month, sep = "_")
pdat$county_year <- paste(pdat$county, pdat$year, sep = "_")


pdat$pm_r_v2 <- summary(felm(pm25 ~ 1 | mzip_month + county_year, data = pdat))$residuals
pdat$smoke_r_v2 <- summary(felm(smoke_day ~ 1 | mzip_month + county_year, data = pdat))$residuals




bindat <- pdat %>% mutate(smoke_day = round(smoke_day/3)*3) %>% group_by(smoke_day) %>% summarise(low = quantile(pm25, .25, na.rm = T), high = quantile(pm25, .75, na.rm = T), pm25 = median(pm25),count = n()) %>% filter(count>10)


pbindat <- pdat %>% mutate(smoke_day = round(round(smoke_r_v2)/3)*3) %>% group_by(smoke_day) %>% summarise(low = quantile(pm_r_v2, .25, na.rm = T), high = quantile(pm_r_v2, .75, na.rm = T), pm25 = median(pm_r_v2),count = n()) %>% filter(count>10)



#write pdf
        pdf(raw_figure_path("FigS1_raw.pdf"), width = 18, height = 8, useDingbats = FALSE)

            par(oma = c(0,0,0,0))
            par(mar = c(4,4,1,0))
            par(mfrow = c(1,2))


            plot(1,1, col = NA, axes = F, xlab = "",ylab = "", xlim= c(0,11), ylim = c(-5,40))
                rect(ybottom =0, ytop = bindat$pm25,xleft =1:10-0.4, xright = 1:10+0.4, col = 'gray90')
                axis(1, at = 1:10, labels =   paste(bindat$smoke_day, "-", names.arg = bindat$smoke_day+2, sep = ""),tick = F,line=-1.5)
                axis(2, tick = T, at = seq(0,40,5),line=-2,las=2)
                mtext(side = 2, text = "Average PM2.5 (ug/m3)",line=1)
                mtext(side = 1, text = "Smoke days in a month",line=2.5)

                    segments(x0 = 1:10, y0 = bindat$low, y1 = bindat$high)


                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> -0.5 &  pdat$smoke_day < 2.5)*5) + 0.6  , lwd = 0.01)

                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> 2.5 &  pdat$smoke_day < 5.5)*5) + 1.6  , lwd = 0.01)

                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> 5.5 &  pdat$smoke_day < 8.5)*5) + 2.6  , lwd = 0.01)

                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> 8.5 &  pdat$smoke_day < 11.5)*5) + 3.6  , lwd = 0.01)

                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> 11.5 &  pdat$smoke_day < 14.5)*5) + 4.6  , lwd = 0.01)

                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> 14.5 &  pdat$smoke_day < 17.5)*5) + 5.6  , lwd = 0.01)

                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> 17.5 &  pdat$smoke_day < 20.5)*5) + 6.6  , lwd = 0.01)

                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> 20.5 &  pdat$smoke_day < 23.5)*5) + 7.6  , lwd = 0.01)


                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> 23.5 &  pdat$smoke_day < 26.5)*5) + 8.6  , lwd = 0.01)

                    segments(y0 = -4.5, y1 = -1.5, x0 = seq(0,0.8, 0.8/sum( pdat$smoke_day> 26.5)*5) + 9.6  , lwd = 0.01)




            plot(1,1, col = NA, axes = F, xlab = "",ylab = "", xlim= c(0,10), ylim = c(-6,14))
                rect(ybottom =0, ytop = pbindat$pm25,xleft =1:9-0.4, xright = 1:9+0.4, col = 'gray90')
                axis(1, at = 1:9, labels =   paste(pbindat$smoke_day, "-", names.arg = pbindat$smoke_day+2, sep = ""),tick = F,line=-1.5)
                axis(2, tick = T, at = seq(-4,14,2),line=-2,las=2)
                mtext(side = 2, text = "PM2.5 Anomalies",line=1)
                mtext(side = 1, text = "Smoke Anomalies",line=1)

                    segments(x0 = 1:9, y0 = pbindat$low, y1 = pbindat$high)

                    segments(x0 = 0, x1 = 10, y0 = 0, col = 'gray', lwd = 0.75,lty=2)
                    segments(y0 = -5, y1 = 14, x0 = 3.5, col = 'gray', lwd = 0.75,lty=2)


                    segments(y0 = -5.75, y1 = -4.25, x0 = 0.8*(pdat$smoke_r_v2[ pdat$smoke_r_v2> -9.5 &  pdat$smoke_r_v2 < -6.5] - -9.5)/3 + 0.6  , lwd = 0.05)

                    segments(y0 = -5.75, y1 = -4.25, x0 = 0.8*(pdat$smoke_r_v2[ pdat$smoke_r_v2> -6.5 &  pdat$smoke_r_v2 < -3.5] - -6.5)/3 + 1.6  , lwd = 0.05 )

                    segments(y0 = -5.75, y1 = -4.25, x0 = 0.8*(pdat$smoke_r_v2[ pdat$smoke_r_v2> -3.5 &  pdat$smoke_r_v2 < -0.5] - -3.5)/3 + 2.6  , lwd = 0.05 )

                    segments(y0 = -5.75, y1 = -4.25, x0 = 0.8*(pdat$smoke_r_v2[ pdat$smoke_r_v2> -0.5 &  pdat$smoke_r_v2 < 2.5] - -0.5)/3 + 3.6  , lwd = 0.05 )

                    segments(y0 = -5.75, y1 = -4.25, x0 = 0.8*(pdat$smoke_r_v2[ pdat$smoke_r_v2> 2.5 &  pdat$smoke_r_v2 < 5.5] - 2.5)/3 + 4.6  , lwd = 0.05 )

                    segments(y0 = -5.75, y1 = -4.25, x0 = 0.8*(pdat$smoke_r_v2[ pdat$smoke_r_v2> 5.5 &  pdat$smoke_r_v2 < 8.5] - 5.5)/3 + 5.6  , lwd = 0.05 )

                    segments(y0 = -5.75, y1 = -4.25, x0 = 0.8*(pdat$smoke_r_v2[ pdat$smoke_r_v2> 8.5 &  pdat$smoke_r_v2 < 11.5] - 8.5)/3 + 6.6  , lwd = 0.05 )

                    segments(y0 = -5.75, y1 = -4.25, x0 = 0.8*(pdat$smoke_r_v2[ pdat$smoke_r_v2> 11.5 &  pdat$smoke_r_v2 < 14.5] - 11.5)/3 + 7.6  , lwd = 0.05 )

                    segments(y0 = -5.75, y1 = -4.25, x0 = 0.8*(pdat$smoke_r_v2[ pdat$smoke_r_v2> 14.5 &  pdat$smoke_r_v2 < 17.5] - 14.5)/3 + 8.6  , lwd = 0.05 )

        dev.off()












