#############################################
#
# R notes for icebreakeR workshop
#   Miren Berasategi
#   June 6, 2012
#
#############################################

# checks the first few rows
> head(ufc)
  plot tree species dbh height
1    1    1          NA     NA
2    2    1      DF 390    205
3    2    2      WL 480    330
4    3    1      WC 150     NA
5    3    2      GF 520    300
6    3    3      WC 310     NA

# counts missing values by column
> colSums(is.na(ufc))
   plot    tree species     dbh  height 
      0       0       0      10     246 

# creates new columns/variables, appending unit to variable name for semantics
> ufc$height_m <- ufc$height/10
> ufc$dbh_cm <- ufc$dbh/10

# examines structure
> str(ufc)
'data.frame':	637 obs. of  7 variables:
 $ plot    : int  1 2 2 3 3 3 3 3 3 3 ...
 $ tree    : int  1 1 2 1 2 3 4 5 6 7 ...
 $ species : Factor w/ 13 levels "","DF","ES","F",..: 1 2 12 11 6 11 11 11 11 11 ...
 $ dbh     : int  NA 390 480 150 520 310 280 360 340 260 ...
 $ height  : int  NA 205 330 NA 300 NA NA 207 NA NA ...
 $ height_m: num  NA 20.5 33 NA 30 NA NA 20.7 NA NA ...
 $ dbh_cm  : num  NA 39 48 15 52 31 28 36 34 26 ...

# returns a vector containing the minimum and maximum of all the given arguments. 
> range(ufc$dbh_cm)
[1] NA NA
> range(ufc$height_m, na.rm = TRUE)
[1]  0 48

# converts 0 values in height_m to NA, rechecks range
> ufc$height_m[ufc$height_m < 0.1] <- NA
> range(ufc$height_m, na.rm = TRUE)
[1]  3.4 48.0


> table(ufc$species)

     DF  ES   F  FG  GF  HW  LP  PP  SF  WC  WL  WP 
 10  77   3   1   2 185   5   7   4  14 251  34  44 

 # merges F, FG and GF variables into GF
> ufc$species[ufc$species %in% c("F", "FG")] <- "GF"
> ufc$species <- factor(ufc$species)
> table(ufc$species)

     DF  ES  GF  HW  LP  PP  SF  WC  WL  WP 
 10  77   3 188   5   7   4  14 251  34  44 

 # counts empty values for each species type
> table(ufc$species[is.na(ufc$height_m)])

     DF  ES  GF  HW  LP  PP  SF  WC  WL  WP 
 10  20   1  70   0   4   2   4 112  12  12 

 # draw graphics
> boxplot(dbh_cm ~ species, data = ufc, ylab = "Dbh (cm)")
> boxplot(height_m ~ species, data = ufc, ylab = "Height (m)")
> scatter.smooth(ufc$dbh_cm, ufc$height_m)
> hist(ufc$dbh_cm, breaks = (0:50) * 2.5, col = "darkseagreen3", main = "")

# draw advanced graphcs 
> library(lattice)
> histogram(~dbh_cm | species, data = ufc)

> hd_lm_1 <- lm(height_m ~ dbh_cm, data = ufc)
> summary(hd_lm_1)

Call:
lm(formula = height_m ~ dbh_cm, data = ufc)

Residuals:
     Min       1Q   Median       3Q      Max 
-27.5066  -2.8162   0.0805   2.6976  13.2052 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 12.46781    0.53074   23.49   <2e-16 ***
dbh_cm       0.32067    0.01309   24.50   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 4.643 on 388 degrees of freedom
  (247 observations deleted due to missingness)
Multiple R-squared: 0.6075,	Adjusted R-squared: 0.6065 
F-statistic: 600.5 on 1 and 388 DF,  p-value: < 2.2e-16 

> scatter.smooth(ufc$dbh_cm, ufc$height_m)
> abline(hd_lm_1, col = "red")
> hd_lm_2 <- lm(height_m ~ dbh_cm * species, data = ufc)
> xyplot(height_m ~ dbh_cm | species, panel = function(x, y, ...) { panel.xyplot(x, y) panel.abline(lm(y ~ x), col = "blue") panel.abline(hd_lm_1, col = "darkgreen") if (sum(!is.na(y)) > 2) { panel.loess(x, y, span = 1, col = "red") } }, subset = species != "", xlab = "Dbh (cm)", ylab = "Height (m)", data = ufc)
Error: inesperado símbolo en "xyplot(height_m ~ dbh_cm | species, panel = function(x, y, ...) { panel.xyplot(x, y) panel.abline"
>     + }
Error: inesperado '}' en "    + }"
> xyplot(height_m ~ dbh_cm | species, panel = function(x, y, ...) {
+ 
+ 
+ 

>  panel.xyplot(x, y)
Error en panel.xyplot(x, y) : objeto 'x' no encontrado
>  panel.abline(lm(y ~ x), col = "blue")
Error en eval(expr, envir, enclos) : objeto 'y' no encontrado
> xyplot(height_m ~ dbh_cm | species, panel = function(x, y, ...) { 
+   panel.xyplot(x, y) 
+   panel.abline(lm(y ~ x), col = "blue")
+   panel.abline(hd_lm_1, col = "darkgreen")
+   if (sum(!is.na(y)) > 2) {
+     panel.loess(x, y, span = 1, col = "red")
+   }
+ }, subset = species != "", xlab = "Dbh (cm)",
+ ylab = "Height (m)", data = ufc)
> library(nlme)
> hd_lme <- lme(I(log(height_m)) ~ I(log(dbh_cm)) * species,
+               random = ~ I(log(dbh_cm)) | plot,
+               na.action = na.exclude,
+               data = ufc)
> predicted_log_heights <- predict(hd_lme, na.action=na.exclude, newdata=ufc)
> ufc$p_height_m[!is.na(ufc$dbh_cm)] <- exp(predicted_log_heights)
> ufc$p_height_m[!is.na(ufc$height_m)] <- ufc$height_m[!is.na(ufc$height_m)]
> summary(ufc$p_height_m)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NAs 
   3.40   18.51   23.00   23.66   28.51   48.05   10.00 
> vol_fvs_ni_bdft <- function(spp, dbh_in, ht_ft){
+   bf_params <-
+     data.frame(
+       species = c("WP", "WL", "DF", "GF", "WH", "WC", "LP", "ES",
+                     "SF", "PP", "HW"),
+       b0_small = c(26.729, 29.790, 25.332, 34.127, 37.314, 10.472,
+                      8.059, 11.851, 11.403, 50.340, 37.314),
+       b1_small = c(0.01189, 0.00997, 0.01003, 0.01293, 0.01203,
+                      0.00878, 0.01208, 0.01149, 0.01011, 0.01201, 0.01203),
+       b0_large = c(32.516, 85.150, 9.522, 10.603, 50.680, 4.064,
+                      14.111, 1.620, 124.425, 298.784, 50.680),
+       b1_large = c(0.01181, 0.00841, 0.01011, 0.01218, 0.01306,
+                      0.00799, 0.01103, 0.01158, 0.00694, 0.01595, 0.01306))
+   dimensions <- data.frame(dbh_in = dbh_in,
+                              ht_ft = ht_ft,
+                              species = as.character(spp),
+                              this_order = 1:length(spp))
+   dimensions <- merge(y=dimensions, x=bf_params, all.y=TRUE, all.x=FALSE)
+   dimensions <- dimensions[order(dimensions$this_order, decreasing=FALSE),]
+   b0 <- with(dimensions, ifelse(dbh_in <= 20.5, b0_small, b0_large))
+   b1 <- with(dimensions, ifelse(dbh_in <= 20.5, b1_small, b1_large))
+   volumes_bdft <- b0 + b1 * dimensions$dbh_in^2 * dimensions$ht_ft
+   return(volumes_bdft)
+   }
> cm_to_inches <- 1/2.54
> m_to_feet <- 3.281
> bd_ft_to_m3 <- 0.002359737
> ufc$vol_m3 <- with(ufc, vol_fvs_ni_bdft(species,
+                                         dbh_cm * cm_to_inches,
+                                         p_height_m * m_to_feet) * bd_ft_to_m3)
> ufc$g_ma2 <- ufc$dbh_cm^2 * pi/40000
> ufc$tree_factor <- ufc_baf/ufc$g_ma2
> ufc$vol_m3_ha <- ufc$vol_m3 * ufc$tree_factor
> str(ufc)
'data.frame':	637 obs. of  12 variables:
 $ plot       : int  1 2 2 3 3 3 3 3 3 3 ...
 $ tree       : int  1 1 2 1 2 3 4 5 6 7 ...
 $ species    : Factor w/ 11 levels "","DF","ES","GF",..: 1 2 10 9 4 9 9 9 9 9 ...
 $ dbh        : int  NA 390 480 150 520 310 280 360 340 260 ...
 $ height     : int  NA 205 330 NA 300 NA NA 207 NA NA ...
 $ height_m   : num  NA 20.5 33 NA 30 NA NA 20.7 NA NA ...
 $ dbh_cm     : num  NA 39 48 15 52 31 28 36 34 26 ...
 $ p_height_m : num  NA 20.5 33 13.8 30 ...
 $ vol_m3     : num  NA 0.4351 0.98 0.0575 1.3392 ...
 $ g_ma2      : num  NA 0.1195 0.181 0.0177 0.2124 ...
 $ tree_factor: num  NA 58.6 38.7 396.1 33 ...
 $ vol_m3_ha  : num  NA 25.5 37.9 22.8 44.1 ...
> ufc_plot <- as.data.frame(cbind(c(1:144), rep(c(12:1),12),
+                                   rep(c(1:12), rep(12,12))))
> names(ufc_plot) = c("plot","north.n","east.n")
> ufc_plot$north = (ufc_plot$north.n - 0.5) * 134.11
> ufc_plot$east = (ufc_plot$east.n - 0.5) * 167.64
> ufc_plot$vol_m3_ha <- tapply(ufc$vol_m3_ha, ufc$plot,
+                              sum, na.rm = TRUE)
> contourplot(vol_m3_ha ~ east * north, main = expression(paste("Volume (",
+                                                               m^3, "/ha)", sep = "")), xlab = "East (m)",
+             ylab = "North (m)", region = TRUE, col.regions = terrain.colors(11)[11:1],
+             data = ufc_plot)
> save("ufc_plot", file="../images/ufc_plot.RData")
> mean(ufc_plot$vol_m3_ha) * ufc_area
[1] 44856.21
> (mean(ufc_plot$vol_m3_ha) + qt(c(0.025,0.975),
+                                df = length(ufc_plot$vol_m3_ha) - 1) *
+                                  sd(ufc_plot$vol_m3_ha) / sqrt(length(ufc_plot$vol_m3_ha))) * ufc_area
[1] 39978.39 49734.04
> vol_by_species <- tapply(ufc$vol_m3_ha, list(ufc$plot, ufc$species), sum)
> vol_by_species[is.na(vol_by_species)] <- 0
> boxplot(as.data.frame(vol_by_species))
> (totals <- apply(vol_by_species, 2, mean) * ufc_area)
                   DF         ES         GF         HW         LP         PP         SF         WC 
    0.0000  5303.6005   210.4520 17817.4133   479.8852   453.5305   499.5319   913.3555 12686.8227 
        WL         WP 
 2540.6045  3951.0148 
> (ci_bars <- apply(vol_by_species, 2, sd) / sqrt(144) * 1.96 * ufc_area)
                 DF        ES        GF        HW        LP        PP        SF        WC        WL 
   0.0000 1538.9513  311.4001 3493.9574  645.3071  592.3508  506.4334  703.1405 2464.3451 1333.0400 
       WP 
1623.2950 
> boxplot(as.data.frame(vol_by_species),
+           ylab=expression(paste("Volume (", m^3, ha^-1, ")")))
> lines(1:11, (totals - ci_bars) / ufc_area, col="blue")
> lines(1:11, (totals + ci_bars) / ufc_area, col="blue")
> lines(1:11, totals / ufc_area, col="red")