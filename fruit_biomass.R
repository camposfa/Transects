## ---- workspace ----
Sys.setenv(TZ='UTC')
list.of.packages <- list("ggplot2", "RColorBrewer", "plyr", "scales", 
                         "reshape2", "RODBC", "mgcv", "xtable", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

## ---- pheno_read_pace ----
# Must create SSH tunnel first (plink)
# ch <- odbcConnect(dsn = "PACE")
# pheno.q <- "SELECT 
# tblTaxon.SpeciesName, 
# tblTaxon.CodeName, 
# tblPhenologyTree.ID 
# AS TreeID,
# tblPhenologyMonthly.Year, 
# tblPhenologyMonthly.Month, 
# tblPhenologyMonthly.DateOf, 
# tblPhenologyMonthly.FruitCoverID, 
# tblPhenologyMonthly.FruitMaturityID 
# FROM 
# (tblTaxon INNER JOIN tblPhenologyTree 
# ON tblTaxon.ID = tblPhenologyTree.TaxonID) 
# INNER JOIN 
# tblPhenologyMonthly 
# ON tblPhenologyTree.ID = 
# tblPhenologyMonthly.PhenologyTreeID
# ORDER BY 
# tblTaxon.SpeciesName, 
# tblPhenologyMonthly.Year, 
# tblPhenologyMonthly.Month"
# pheno <- sqlQuery(ch, pheno.q)
# odbcCloseAll()
# write.csv(pheno, file = "pace_pheno.csv", row.names = FALSE)


## ---- pheno_read_csv ----
pheno <- read.csv("pace_pheno.csv")


## ---- pheno_table_raw ----
print(xtable(head(pheno)), type = "html")


## ---- pheno_clean ----
# Rename variables for consistent style
names(pheno) <- c("species_name", 
                  "code_name", 
                  "tree_id", 
                  "year_of", 
                  "month_of", 
                  "date_of", 
                  "fruit_coverage", 
                  "fruit_maturity")
pheno$tree_id <- factor(pheno$tree_id)
pheno$month_of <- factor(pheno$month_of, 
                         labels = month.abb[1:12])
pheno$date_of <- as.Date(pheno$date_of)

# Fix maturity code 5 (change to zero)
pheno[which(pheno$fruit_maturity == 5), ]$fruit_maturity <- 0

# Exclude 2006 pheno data because no maturity info
pheno <- subset(pheno, year_of > 2006)

# Calculate number of trees of each species
count_tree_species <- ddply(ddply(pheno,
                                   .(species_name, tree_id),
                                   summarize, 
                                   length(tree_id)), 
                             .(species_name), 
                             summarize, 
                             num_trees = length(tree_id))

## ---- pheno_table_count_species ----
print(xtable(count_tree_species), type = "html")


## ---- pheno_index ----
pheno$index_avail <- (pheno$fruit_coverage / 4) * (pheno$fruit_maturity / 4)


## ---- pheno_filter ----
# MCAP: only one tree after 2006
# Ficus sp.: not useful if species unknown
# BOCE: no pheno trees after 2006
# HCOU: fruit not eaten

pheno <- subset(pheno, 
                species_name != "Bunchosia ocellata" &
                  species_name != "Ficus sp." &
                  species_name != "Mastichodendron capiri" &
                  species_name != "Hymenaea courbaril"
)

# Fix some name errors
pheno$species_name <- as.character(pheno$species_name)
pheno[pheno$code_name == "SGLN",]$species_name <- "Sapium glandulosum"
pheno$species_name <- factor(pheno$species_name)
pheno$code_name <- factor(pheno$code_name)

# Generate list of unique species, for later use
species <- unique(pheno[, 1:2])
row.names(species) <- NULL


## ---- pheno_table_count_monthly ----
# Number of trees each month
count_tree_monthly <- ddply(ddply(pheno,
                                  .(year_of, month_of, tree_id),
                                  summarize,
                                  length(tree_id)),
                            .(year_of, month_of),
                            summarize,
                            num_trees = length(tree_id))

print(xtable(count_tree_monthly), type = "html")


## ---- pheno_plot_index ----
ggplot(pheno, aes(x = month_of, y = index_avail)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25)) +
  facet_wrap(~code_name, ncol = 5)


## ---- pheno_smooth ----
pheno_gams <- list()

for(i in 1:length(levels(pheno$species_name)))
{
  pheno_s <- subset(pheno, species_name == levels(species_name)[i])
  
  mod_s <- gamm(index_avail ~ 
                  s(as.numeric(month_of), 
                    bs = "cc"), 
                data = pheno_s, 
                random = list(tree_id = ~1), 
                knots = list(month_of = c(1, 13)))
  
  pheno_gams[[i]] <- mod_s$gam
}

# Create data frame of model results for all species
fruit_avail <- matrix(nrow = length(levels(pheno$species_name)), 
                      ncol = 12)
for(i in 1:length(levels(pheno$species_name)))
{
  for(j in 1:12)
  {
    pred <- predict(pheno_gams[[i]], 
                    newdata = subset(pheno, 
                                     species_name == 
                                       levels(species_name)[i]))
    pred[pred < 0] <- 0
    temp <- data.frame(cbind(subset(pheno, 
                                    species_name == 
                                      levels(species_name)[i])$month_of, 
                             pred))
    fruit_avail[i,j] <- mean(subset(temp, V1 == j)$pred)
  }
}

fruit_avail <- data.frame(fruit_avail)
names(fruit_avail) <- rep(month.abb, 1)
fruit_avail$species_name <- levels(pheno$species_name)

# Merge to obtain species code names
fruit_avail <- merge(fruit_avail, 
                     species, 
                     by.x = "species_name", 
                     by.y = "species_name", 
                     sort = F)

# Convert from wide to long format
fruit_avail <- melt(fruit_avail,
                    id.vars = c("species_name", "code_name"))
names(fruit_avail)[3] <- "month_of"


## ---- pheno_plot_smooth ----
ggplot(fruit_avail, aes(x = month_of, y = value)) +
  geom_bar(stat = "identity", width = 0.7) + 
  facet_wrap(~code_name, ncol = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25),
        panel.grid.minor = element_blank()) +
  labs(x = "Month", y = "Fruit Availability Index")


## ---- tr_read_csv ----
tr <- read.csv("Transects.csv")


## ---- tr_clean ----
# Rename variables for consistent style
names(tr) <- c("tree_id", "transect_id", 
               "old_transect_id", "transect_start", 
               "transect_end", "date_of", "seq_num", 
               "x", "y", "species_name", 
               "prop_in_transect", "n_stems", 
               "area", "cbh", "dbh")

# Fix issue with species name
tr$species_name <- as.character(tr$species_name)
tr[tr$species_name == "Maclura tinctoria x",]$species_name <- 
  "Maclura tinctoria"
tr$species_name <- factor(tr$species_name)

# Get transect trees from target pheno species
tr_pheno <- tr[tr$species_name %in% species$species_name,]

# Merge to obtain code names
tr_pheno <- merge(tr_pheno,
                  species, 
                  by.x = "species_name", 
                  by.y = "species_name", 
                  sort = F)

# Sort
tr_pheno <- tr_pheno[with(tr_pheno, order(tree_id)),]

# Clean up column order and row names
tr_pheno <- tr_pheno[, c(2:7, 1, 16, 8:15)]
row.names(tr_pheno) <- NULL

# Fill in fixed DBH for bromeliads, since it is not recorded
# Using 5 cm per fruiting plant
# Also ensure that each has a positive n_stems
tr_pheno[(tr_pheno$code_name=="BPLU" | 
            tr_pheno$code_name=="BPIN"),]$dbh <- 5
tr_pheno[((tr_pheno$code_name=="BPLU" | 
             tr_pheno$code_name=="BPIN") & 
            is.na(tr_pheno$n_stems)),]$n_stems <- 1

# Weight bromeliad DBH by n_stems
tr_pheno[(tr_pheno$code_name=="BPLU" | 
            tr_pheno$code_name=="BPIN"),]$dbh <- 
  tr_pheno[(tr_pheno$code_name=="BPLU" | 
              tr_pheno$code_name=="BPIN"),]$dbh * 
  tr_pheno[(tr_pheno$code_name=="BPLU" | 
              tr_pheno$code_name=="BPIN"),]$n_stems


## ---- fpv_read_csv ----
fpv <- read.csv("AllFPV.csv")


## ---- fpv_clean ----
# Create dbh column
fpv$dbh <- sqrt((4 * fpv$Area) / pi)

# Fill in fixed DBH for bromeliads, since it is not recorded
# Using 5 cm per fruiting plant
# Also ensure that each bromeliad fpv has a positive NFruiting
fpv[(fpv$Code=="BPLU" | fpv$Code=="BPIN"),]$dbh <- 5
fpv[(fpv$Code=="BPLU" | fpv$Code=="BPIN") & 
      is.na(fpv$NFruiting),]$NFruiting <- 1

# Weight bromeliad DBH by NFruiting
fpv[(fpv$Code=="BPLU" | fpv$Code=="BPIN"),]$dbh <- 
  fpv[(fpv$Code=="BPLU" | fpv$Code=="BPIN"),]$dbh * 
  fpv[(fpv$Code=="BPLU" | fpv$Code=="BPIN"),]$NFruiting

# Remove NA values and extra columns, because not useful here
fpv <- subset(fpv, !is.na(dbh))[, c(8, 3, 26)]

# Rename variables for consistent style
names(fpv) <- c("time_stamp", "code_name", "dbh")

# Restrict to pheno species and sort by species & dbh
fpv <- fpv[fpv$code_name %in% species$code_name, ]
fpv <- fpv[with(fpv, order(code_name, dbh)), ]


## ---- fpv_min_dbh ----
# Store uncorrected min dbhs
min_dbh <- ddply(fpv, 
                 .(code_name), 
                 summarize, 
                 threshold_dbh = min(dbh, na.rm=TRUE), 
                 n.trees = length(dbh))


## ---- fpv_plot_dbh ----
# Look at unchecked min dbhs, log tranformed
ggplot(fpv, aes(x = code_name, y = log(dbh))) + 
  geom_boxplot(width=0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


## ---- fpv_fix_dbh_verbose ----
# # Check species with the most egregious lower outliers:
# # ACOL, AEDU, ARET, BCRA, FCOT, FMOR, FOBT, KCAL, 
# # LSPE, MCHI, MTIN, RTHU, SOBO
# 
# # I tried automating this, but it didn't work well
# 
# head(subset(fpv, code_name == "ACOL"))
# # Fairly continuous, but fruit on trees < 1  cm does not seem possible. 
# # 4th smallest (0.9549297) seems reasonable
# min_dbh[min_dbh$code_name == "ACOL", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "ACOL"))$dbh[4]
# 
# head(subset(fpv, code_name == "AEDU"))
# # Take 2nd smallest
# min_dbh[min_dbh$code_name == "AEDU", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "AEDU"))$dbh[2]
# 
# head(subset(fpv, code_name == "ARET"))
# # Take 2nd smallest
# min_dbh[min_dbh$code_name == "ARET", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "ARET"))$dbh[2]
# 
# head(subset(fpv, code_name == "BCRA"))
# # Take 2nd smallest
# min_dbh[min_dbh$code_name == "BCRA", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "BCRA"))$dbh[2]
# 
# head(subset(fpv, code_name == "FCOT"))
# # Take 2nd smallest
# min_dbh[min_dbh$code_name == "FCOT", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "FCOT"))$dbh[2]
# 
# head(subset(fpv, code_name == "FMOR"))
# # Take 2nd smallest
# min_dbh[min_dbh$code_name == "FMOR", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "FMOR"))$dbh[2]
# 
# head(subset(fpv, code_name == "FOBT"))
# # Seems okay, don't change
# 
# head(subset(fpv, code_name == "KCAL"))
# # Take 2nd smallest
# min_dbh[min_dbh$code_name == "KCAL", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "KCAL"))$dbh[2]
# 
# head(subset(fpv, code_name == "LSPE"))
# # Take 2nd smallest
# min_dbh[min_dbh$code_name == "LSPE", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "LSPE"))$dbh[2]
# 
# head(subset(fpv, code_name == "MCHI"))
# # Take 2nd smallest
# min_dbh[min_dbh$code_name == "MCHI", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "MCHI"))$dbh[2]
# 
# head(subset(fpv, code_name == "MTIN"))
# # Take 3rd smallest
# min_dbh[min_dbh$code_name == "MTIN", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "MTIN"))$dbh[3]
# 
# head(subset(fpv, code_name == "RTHU"))
# # Seems continuous, don't change
# 
# head(subset(fpv, code_name == "SOBO"))
# # Take 2nd smallest
# min_dbh[min_dbh$code_name == "SOBO", ]$threshold_dbh <- 
#   head(subset(fpv, code_name == "SOBO"))$dbh[2]


## ---- fpv_fix_dbh_brief ----
# Fix species with the most egregious lower outliers:
# ACOL, AEDU, ARET, BCRA, FCOT, FMOR, KCAL, 
# LSPE, MCHI, MTIN, SOBO
min_dbh[min_dbh$code_name == "ACOL", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "ACOL"))$dbh[4]

min_dbh[min_dbh$code_name == "AEDU", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "AEDU"))$dbh[2]

min_dbh[min_dbh$code_name == "ARET", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "ARET"))$dbh[2]

min_dbh[min_dbh$code_name == "BCRA", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "BCRA"))$dbh[2]

min_dbh[min_dbh$code_name == "FCOT", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "FCOT"))$dbh[2]

min_dbh[min_dbh$code_name == "FMOR", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "FMOR"))$dbh[2]

min_dbh[min_dbh$code_name == "KCAL", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "KCAL"))$dbh[2]

min_dbh[min_dbh$code_name == "LSPE", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "LSPE"))$dbh[2]

min_dbh[min_dbh$code_name == "MCHI", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "MCHI"))$dbh[2]

min_dbh[min_dbh$code_name == "MTIN", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "MTIN"))$dbh[3]

min_dbh[min_dbh$code_name == "SOBO", ]$threshold_dbh <- 
  head(subset(fpv, code_name == "SOBO"))$dbh[2]


## ---- fpv_dbh_extra ----
min_dbh[min_dbh$code_name == "TAME", ]$threshold_dbh <- 
  rev(sort(subset(tr, species_name == "Trichilia americana")$dbh))[1]

min_dbh[min_dbh$code_name == "SEXC", ]$threshold_dbh <- 
  rev(sort(subset(tr, species_name == "Sciadodendron excelsum")$dbh))[1]

min_dbh[min_dbh$code_name == "SGLN", ]$threshold_dbh <- 
  rev(sort(subset(tr, species_name == "Sapium glandulosum")$dbh))[2]


## ---- tr_filter ----
# Restrict transect data to pheno species
tr_pheno <- tr_pheno[tr_pheno$code_name %in% species$code_name, ]

# Remove transect trees without dbh
tr_pheno <- subset(tr_pheno, !is.na(dbh))


## ---- tr_test_usable ----
# Set "usable" flag to indicate if tree dbh >= threshold
tr_pheno$usable <- FALSE
for(i in 1:nrow(tr_pheno))
{
  tr_pheno[i, ]$usable <- tr_pheno[i, ]$dbh >= 
    (min_dbh[which(as.character(min_dbh$code_name) == 
                     as.character(tr_pheno[i, ]$code_name)),]$threshold_dbh)
}

# Count number of usable transect trees for each species
count_usable <- ddply(tr_pheno,
                      .(code_name),
                      function(df) 
                        length(df[df$usable == TRUE, ]$tree_id))
names(count_usable)[2] <- "num_trees"


## ---- tr_table_usable ----
print(xtable(count_usable), type = "html")


## ---- biomass_total ----
biomass <- ddply(tr_pheno, .(code_name), 
                 function(df) 
                   sum(df[df$usable == TRUE,]$prop_in_transect * 
                         47 * (df[df$usable == TRUE,]$dbh ^ 1.9), 
                       na.rm=TRUE) / 1000)
names(biomass)[2] <- "biomass_total_kg"

# Biomass per hectare
biomass$biomass_max_kg_ha <- (biomass$biomass_total_kg / (151 * 200 / 10000))

# Total basal area
biomass$area_total <- ddply(tr_pheno, 
                            .(code_name), 
                            function(df) sum(df[df$usable == 
                                                  TRUE, ]$area, 
                                             na.rm=TRUE))[, 2]


## ---- biomass_plot_max ----
ggplot(biomass, aes(x = code_name, 
                    y = biomass_max_kg_ha)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(x = "Code", y = "Potential peak biomass (kg / ha)")


## ---- biomass_available1 ----
biomass_avail <- merge(fruit_avail, 
                       biomass, 
                       by.x = "code_name", 
                       by.y = "code_name")
biomass_avail <- biomass_avail[with(biomass_avail, 
                                    order(code_name, month_of)), ]

# Monthly biomass for each species
biomass_avail$biomass_monthly_kg <- 
  biomass_avail$value * biomass_avail$biomass_max_kg_ha


## ---- biomass_monthly1 ----
fruit_seas_avail <- ddply(biomass_avail, 
                          .(month_of), 
                          summarize, 
                          combined_monthly_kg = 
                            sum(biomass_monthly_kg, na.rm = TRUE))

## ---- biomass_plot_monthly1 ----
ggplot(fruit_seas_avail, 
       aes(x = month_of, 
           y = combined_monthly_kg)) + 
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Fruit biomass (kg / ha)")

## ---- biomass_available2 ----
biomass_avail <- biomass_avail[biomass_avail$code_name != "GULM" &
                                 biomass_avail$code_name != "LCAN" &
                                 biomass_avail$code_name != "LSPE" &
                                 biomass_avail$code_name != "BUNG" &
                                 biomass_avail$code_name != "CCAN", ]

fruit_seas_avail <- ddply(biomass_avail, 
                          .(month_of), 
                          summarize, 
                          combined_monthly_kg = sum(biomass_monthly_kg, 
                                                    na.rm=TRUE))

## ---- biomass_plot_monthly2 ----
ggplot(fruit_seas_avail, 
       aes(x = month_of, 
           y = combined_monthly_kg)) + 
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Fruit biomass (kg / ha)")

# Write csv for later use
# write.csv(fruit_seas_avail, "biomass_monthly.csv", row.names = FALSE)

## ---- biomass_sum ----
sum(fruit_seas_avail$combined_monthly_kg)


## ---- bioimass_interpolation ----
# Add one additional january to the end for a complete year
biomass_monthly <- rbind(fruit_seas_avail, fruit_seas_avail[1, ])

# Create actual dates
biomass_monthly$month_num <- match(biomass_monthly$month_of, 
                                   month.abb)
biomass_monthly$date_of <- ymd(paste("2011", 
                                     biomass_monthly$month_num, 
                                     "1", 
                                     sep = "-"))
biomass_monthly$day_of_year <- yday(biomass_monthly$date_of)

# Change last day_of_year to 366
biomass_monthly[13, ]$day_of_year <- 366

# Rearrange and create day of year column
biomass_monthly <- biomass_monthly[, c(1,4,5,2)]
biomass_daily <- data.frame(day_of_year = seq(1:365))

# Spline
biomass_daily$spline <- spline(x = biomass_monthly$day_of_year, 
                               y = biomass_monthly$combined_monthly_kg, 
                               n=365)$y

## ---- biomass_interpolation_plot ----
plot(spline(x = biomass_monthly$day_of_year, 
            y = biomass_monthly$combined_monthly_kg, 
            n=365))
points(x = biomass_monthly$day_of_year, 
       y = biomass_monthly$combined_monthly_kg, 
       col="red", 
       pch=16)

## ---- biomass_study_period ----
start_date <- as.Date("2009-11-16")
end_date <- as.Date("2012-12-15")

biomass_dates <- data.frame(date_of = seq(start_date, 
                                          end_date, 
                                          by="1 day"))
biomass_dates$day_of_year <- yday(biomass_dates$date_of)

biomass_dates <- merge(biomass_dates, biomass_daily, 
                       by.x = "day_of_year", 
                       by.y = "day_of_year")[, c(2,1,3)]

biomass_dates <- biomass_dates[with(biomass_dates, 
                                    order(date_of)), ]

## ---- biomass_study_period_plot ----
ggplot() + 
  geom_area(data = biomass_dates, 
            aes(x = as.Date(date_of), y = spline), 
            alpha = 0.5) + 
  geom_point(data = subset(biomass_dates, day(date_of) == 1),
             aes(x = as.Date(date_of), y = spline),
             color = "red") +
  scale_x_date(breaks="1 months", labels = date_format("%b-%y")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.minor = element_blank()) +
  labs(x = "Month", y = "Available fruit biomass (kg/ha)")
  