


# Phenolgy and Biomass

******

<br>

Source files available on [github](https://github.com/camposfa/Transects)

# Major steps
1. Load phenology data and clean it up: [jump](#id1)
2. Smooth phenology data to obtain monthly pheno indices: [jump](#id2)
3. Load transect data and clean it up: [jump](#id3)
4. Specify minimum DBH thresholds for phenology species in transects: [jump](#id4)
5. Estimate maximum potential biomass for each species: [jump](#id5)
6. Combine phenology with transect data to obtain actual monthly biomass estimates: [jump](#id6)

Prepare R environment

```r
setwd("C:/Users/Fernando/Dropbox/R/Transects")
Sys.setenv(TZ='UTC')
x <- list("ggplot2", 
          "RColorBrewer", 
          "plyr", 
          "scales", 
          "reshape2", 
          "RODBC", 
          "mgcv",
          "xtable",
          "lubridate")
lapply(x, require, character.only = T)
```


******

<br>

<a id="id1"></a>
## Step 1: Load phenology data and clean it up

### Obtain Data

#### Option 1
- If you have access, pull current data from PACE database
- Must create SSH tunnel first using plink
- Change dsn="PACE" to whatever your PACE ODBC connection is called
- _Remove comments if using!_

```r
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
```


#### Option 2
- [Contact me](mailto:camposfa@gmail.com) if you are interested in the raw data

```r
pheno <- read.csv("pace_pheno.csv")
```



### Clean up data

```r
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
```



### Unique trees for each pheno species
<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Sun Nov 10 13:53:52 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> species_name </TH> <TH> num_trees </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Alibertia edulis </TD> <TD align="right">   8 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> Allophylus occidentalis </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Annona reticulata </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> Apeiba tibourbou </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Bauhinia ungulata </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> Bromelia pinguin </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> Bromelia plumieri </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD> Bursera simaruba </TD> <TD align="right">  14 </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> Byrsonima crassifolia </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD> Calycophyllum candidissimum </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> Cassia grandis </TD> <TD align="right">   5 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD> Cecropia peltata </TD> <TD align="right">  14 </TD> </TR>
  <TR> <TD align="right"> 13 </TD> <TD> Cordia guanacastensis </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 14 </TD> <TD> Cordia panamensis </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD align="right"> 15 </TD> <TD> Curatella americana </TD> <TD align="right">   7 </TD> </TR>
  <TR> <TD align="right"> 16 </TD> <TD> Diospyros salicifolia </TD> <TD align="right">   7 </TD> </TR>
  <TR> <TD align="right"> 17 </TD> <TD> Diphysa americana </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 18 </TD> <TD> Dipterodendron costaricense </TD> <TD align="right">   5 </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD> Eugenia salamensis </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD align="right"> 20 </TD> <TD> Ficus cotinifolia </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD align="right"> 21 </TD> <TD> Ficus goldmani </TD> <TD align="right">   6 </TD> </TR>
  <TR> <TD align="right"> 22 </TD> <TD> Ficus hondurensis </TD> <TD align="right">   6 </TD> </TR>
  <TR> <TD align="right"> 23 </TD> <TD> Ficus morazaniana </TD> <TD align="right">   6 </TD> </TR>
  <TR> <TD align="right"> 24 </TD> <TD> Ficus obtusifolia </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD align="right"> 25 </TD> <TD> Ficus ovalis </TD> <TD align="right">   6 </TD> </TR>
  <TR> <TD align="right"> 26 </TD> <TD> Ficus sp. </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD align="right"> 27 </TD> <TD> Genipa americana </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD align="right"> 28 </TD> <TD> Guazuma ulmifolia </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 29 </TD> <TD> Guettarda macrosperma </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD align="right"> 30 </TD> <TD> Hymenaea courbaril </TD> <TD align="right">   9 </TD> </TR>
  <TR> <TD align="right"> 31 </TD> <TD> Jacquinia nervosa </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD align="right"> 32 </TD> <TD> Karwinskia calderoni </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD align="right"> 33 </TD> <TD> Licania arborea </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 34 </TD> <TD> Luehea candida </TD> <TD align="right">  15 </TD> </TR>
  <TR> <TD align="right"> 35 </TD> <TD> Luehea speciosa </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 36 </TD> <TD> Maclura tinctoria </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD align="right"> 37 </TD> <TD> Malvaviscus arboreus </TD> <TD align="right">   9 </TD> </TR>
  <TR> <TD align="right"> 38 </TD> <TD> Manilkara chicle </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD align="right"> 39 </TD> <TD> Mastichodendron capiri </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD align="right"> 40 </TD> <TD> Muntingia calabura </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD align="right"> 41 </TD> <TD> Psidium guajava </TD> <TD align="right">   4 </TD> </TR>
  <TR> <TD align="right"> 42 </TD> <TD> Randia monantha </TD> <TD align="right">  14 </TD> </TR>
  <TR> <TD align="right"> 43 </TD> <TD> Randia thurberi </TD> <TD align="right">   9 </TD> </TR>
  <TR> <TD align="right"> 44 </TD> <TD> Sapium grandulosum </TD> <TD align="right">   5 </TD> </TR>
  <TR> <TD align="right"> 45 </TD> <TD> Sciadodendron excelsum </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD align="right"> 46 </TD> <TD> Sebastiana pavoniana </TD> <TD align="right">   7 </TD> </TR>
  <TR> <TD align="right"> 47 </TD> <TD> Simarouba glauca </TD> <TD align="right">   9 </TD> </TR>
  <TR> <TD align="right"> 48 </TD> <TD> Sloanea terniflora </TD> <TD align="right">   4 </TD> </TR>
  <TR> <TD align="right"> 49 </TD> <TD> Spondias mombin </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD align="right"> 50 </TD> <TD> Spondias purpurea </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD align="right"> 51 </TD> <TD> Stemmadenia obovata </TD> <TD align="right">  13 </TD> </TR>
  <TR> <TD align="right"> 52 </TD> <TD> Tabebuia ochracea </TD> <TD align="right">   4 </TD> </TR>
  <TR> <TD align="right"> 53 </TD> <TD> Trichilia americana </TD> <TD align="right">   5 </TD> </TR>
  <TR> <TD align="right"> 54 </TD> <TD> Trichilia martiana </TD> <TD align="right">   7 </TD> </TR>
  <TR> <TD align="right"> 55 </TD> <TD> Vachellia collinsii </TD> <TD align="right">  25 </TD> </TR>
  <TR> <TD align="right"> 56 </TD> <TD> Zuelania guidonia </TD> <TD align="right">  10 </TD> </TR>
   </TABLE>


******

### Calculate pheno indices for each phenology record
- index_avail = (Maturity / 4) x (Coverage / 4)
- index_avail is maximum (1) if full coverage and maturity
- index_avail is zero if either coverage or maturity is zero

```r
pheno$index_avail <- (pheno$fruit_coverage / 4) * (pheno$fruit_maturity / 4)
```


### Filter pheno data (remove some species)

```r
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
```


### Monthly plot of index_availability indices for selected species

```r
ggplot(pheno, aes(x = month_of, y = index_avail)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25)) +
  facet_wrap(~code_name, ncol = 5)
```

![plot of chunk pheno_plot_index](figure/fruit/pheno_plot_index.png) 


******

<br>

<a id="id2"></a>
## Step 2: Smooth phenology data to obtain monthly pheno indices

### GAM smoothing of phenology data using availability index
- Applied to each species separately
- Smoothed with cyclic cubic regression spline, which forces the ends (January / January) to match
- Using tree_id as random effect

```r
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
```


### Plot all smoothed pheno index_availability estimates

```r
ggplot(fruit_avail, aes(x = month_of, y = value)) +
  geom_bar(stat = "identity", width = 0.7) + 
  facet_wrap(~code_name, ncol = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25),
        panel.grid.minor = element_blank()) +
  labs(x = "Month", y = "Fruit Availability Index")
```

![plot of chunk pheno_plot_smooth](figure/fruit/pheno_plot_smooth.png) 


Pheno data for some species looks very problematic, notably ARET, RMON, SOBO, TAME, and TOCH

******

<br>

<a id="id3"></a>
## Step 3: Load transect data and clean it up

### Load transect data
- [Contact me](mailto:camposfa@gmail.com) if you are interested in the raw data

```r
tr <- read.csv("Transects.csv")
```


### Clean up transect data

```r
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
```


******

<br>

<a id="id4"></a>
## Step 4: Specify minimum DBH thresholds for phenology species in transects

### Load FPV data
- [Contact me](mailto:camposfa@gmail.com) if you are interested in the raw data

```r
fpv <- read.csv("AllFPV.csv")
```


### Clean up the FPV data

```r
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
```


### Find minimum DBHs from FPV data

```r
# Store uncorrected min dbhs
min_dbh <- ddply(fpv, 
                 .(code_name), 
                 summarize, 
                 threshold_dbh = min(dbh, na.rm=TRUE), 
                 n.trees = length(dbh))
```


### Plot DBHs for each species to identify problem cases

```r
# Look at unchecked min dbhs, log tranformed
ggplot(fpv, aes(x = code_name, y = log(dbh))) + 
  geom_boxplot(width=0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
```

![plot of chunk fpv_plot_dbh](figure/fruit/fpv_plot_dbh.png) 


### Set min dbh for problem species case-by-case
- I tried automating this, but it didn't work well
- If you want to see the detail, you can contact me or  check the "fpv_fix_dbh_verbose" chunk of code in the source file

```r
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
```


### Adjust minimum dbh thresholds for 3 additional species: TAME, SEXC, SGLN
These thresholds have been fudged slightly to include the largest tree(s) of that species in the transects, each of which falls just below the FPV-based cutoff. 

```r
min_dbh[min_dbh$code_name == "TAME", ]$threshold_dbh <- 
  rev(sort(subset(tr, species_name == "Trichilia americana")$dbh))[1]

min_dbh[min_dbh$code_name == "SEXC", ]$threshold_dbh <- 
  rev(sort(subset(tr, species_name == "Sciadodendron excelsum")$dbh))[1]

min_dbh[min_dbh$code_name == "SGLN", ]$threshold_dbh <- 
  rev(sort(subset(tr, species_name == "Sapium glandulosum")$dbh))[2]
```


### Subset tr_pheno to include only pheno species

```r
# Restrict transect data to pheno species
tr_pheno <- tr_pheno[tr_pheno$code_name %in% species$code_name, ]

# Remove transect trees without dbh
tr_pheno <- subset(tr_pheno, !is.na(dbh))
```


### Determine if each tree in the transect data meets the minimum DBH criteria

```r
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
```


### Number of usable trees in transects
<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Sun Nov 10 13:56:05 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> code_name </TH> <TH> num_trees </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> ACOL </TD> <TD align="right"> 696 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> AEDU </TD> <TD align="right"> 147 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> AOCC </TD> <TD align="right">  61 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> ARET </TD> <TD align="right">  15 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> ATIB </TD> <TD align="right">   8 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> BCRA </TD> <TD align="right">  59 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> BPIN </TD> <TD align="right"> 235 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD> BPLU </TD> <TD align="right"> 113 </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> BSIM </TD> <TD align="right">  74 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD> BUNG </TD> <TD align="right">  81 </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> CAME </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD> CCAN </TD> <TD align="right">  30 </TD> </TR>
  <TR> <TD align="right"> 13 </TD> <TD> CGUA </TD> <TD align="right"> 139 </TD> </TR>
  <TR> <TD align="right"> 14 </TD> <TD> CPAN </TD> <TD align="right">  44 </TD> </TR>
  <TR> <TD align="right"> 15 </TD> <TD> CPEL </TD> <TD align="right">   5 </TD> </TR>
  <TR> <TD align="right"> 16 </TD> <TD> DAME </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD align="right"> 17 </TD> <TD> DCOS </TD> <TD align="right">   7 </TD> </TR>
  <TR> <TD align="right"> 18 </TD> <TD> DSAL </TD> <TD align="right">  72 </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD> ESAL </TD> <TD align="right">  34 </TD> </TR>
  <TR> <TD align="right"> 20 </TD> <TD> FCOT </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD align="right"> 21 </TD> <TD> FOVA </TD> <TD align="right">   0 </TD> </TR>
  <TR> <TD align="right"> 22 </TD> <TD> GAME </TD> <TD align="right">  45 </TD> </TR>
  <TR> <TD align="right"> 23 </TD> <TD> GMAC </TD> <TD align="right">  14 </TD> </TR>
  <TR> <TD align="right"> 24 </TD> <TD> GULM </TD> <TD align="right">  33 </TD> </TR>
  <TR> <TD align="right"> 25 </TD> <TD> JPUN </TD> <TD align="right">  14 </TD> </TR>
  <TR> <TD align="right"> 26 </TD> <TD> KCAL </TD> <TD align="right">   8 </TD> </TR>
  <TR> <TD align="right"> 27 </TD> <TD> LARB </TD> <TD align="right">   0 </TD> </TR>
  <TR> <TD align="right"> 28 </TD> <TD> LCAN </TD> <TD align="right">  56 </TD> </TR>
  <TR> <TD align="right"> 29 </TD> <TD> LSPE </TD> <TD align="right">  46 </TD> </TR>
  <TR> <TD align="right"> 30 </TD> <TD> MARB </TD> <TD align="right"> 183 </TD> </TR>
  <TR> <TD align="right"> 31 </TD> <TD> MCHI </TD> <TD align="right">  15 </TD> </TR>
  <TR> <TD align="right"> 32 </TD> <TD> MTIN </TD> <TD align="right">  13 </TD> </TR>
  <TR> <TD align="right"> 33 </TD> <TD> PGUA </TD> <TD align="right">   6 </TD> </TR>
  <TR> <TD align="right"> 34 </TD> <TD> RMON </TD> <TD align="right">  41 </TD> </TR>
  <TR> <TD align="right"> 35 </TD> <TD> RTHU </TD> <TD align="right">  16 </TD> </TR>
  <TR> <TD align="right"> 36 </TD> <TD> SEXC </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD align="right"> 37 </TD> <TD> SGLA </TD> <TD align="right">   4 </TD> </TR>
  <TR> <TD align="right"> 38 </TD> <TD> SGLN </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD align="right"> 39 </TD> <TD> SMOM </TD> <TD align="right">  34 </TD> </TR>
  <TR> <TD align="right"> 40 </TD> <TD> SOBO </TD> <TD align="right">  76 </TD> </TR>
  <TR> <TD align="right"> 41 </TD> <TD> SPAV </TD> <TD align="right">  64 </TD> </TR>
  <TR> <TD align="right"> 42 </TD> <TD> SPUR </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD align="right"> 43 </TD> <TD> STER </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD align="right"> 44 </TD> <TD> TAME </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD align="right"> 45 </TD> <TD> TMAR </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD align="right"> 46 </TD> <TD> TOCH </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD align="right"> 47 </TD> <TD> ZGUI </TD> <TD align="right">  13 </TD> </TR>
   </TABLE>


******

<br>

<a id="id5"></a>
## Step 5: Estimate maximum potential biomass for each species

### Biomass estimates
- Assumes every tree is fruiting at max potential
- Calculated by applying Peters 1998 formula on each tree and multiplying by the proportion of the tree in transect, then summed for each species

```r
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
```


# Plot max potential biomass for each species

```r
ggplot(biomass, aes(x = code_name, 
                    y = biomass_max_kg_ha)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(x = "Code", y = "Potential peak biomass (kg / ha)")
```

![plot of chunk biomass_plot_max](figure/fruit/biomass_plot_max.png) 


******

<br>

<a id="id6"></a>
## Step 6: Combine phenology with transect data to obtain actual monthly biomass estimates

### Merge fruit index_availability and and biomass data sets

```r
biomass_avail <- merge(fruit_avail, 
                       biomass, 
                       by.x = "code_name", 
                       by.y = "code_name")
biomass_avail <- biomass_avail[with(biomass_avail, 
                                    order(code_name, month_of)), ]

# Monthly biomass for each species
biomass_avail$biomass_monthly_kg <- 
  biomass_avail$value * biomass_avail$biomass_max_kg_ha
```


### Aggregate to obtain sum for all species in each month

```r
fruit_seas_avail <- ddply(biomass_avail, 
                          .(month_of), 
                          summarize, 
                          combined_monthly_kg = 
                            sum(biomass_monthly_kg, na.rm = TRUE))
```


### Plot of monthly biomass estimates

```r
ggplot(fruit_seas_avail, 
       aes(x = month_of, 
           y = combined_monthly_kg)) + 
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Fruit biomass (kg / ha)")
```

![plot of chunk biomass_plot_monthly1](figure/fruit/biomass_plot_monthly1.png) 


The data are strongly affected by a few large, common species that are either very rarely eaten or have very small, wind-dispersed seeds for which Peters' biomass formula is inappropriate. Therefore, I'm repeating the calculation without these species.

### Without GULM and wind-dispersed seeds: LCAN, LSPE, BUNG, CCAN

```r
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
```


### Plot of monthly biomass estimates

```r
ggplot(fruit_seas_avail, 
       aes(x = month_of, 
           y = combined_monthly_kg)) + 
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Fruit biomass (kg / ha)")
```

![plot of chunk biomass_plot_monthly2](figure/fruit/biomass_plot_monthly2.png) 

```r

# Write csv for later use
# write.csv(fruit_seas_avail, "biomass_monthly.csv", row.names = FALSE)
```


### Total fruit production per hectare over entire year (excluding GULM and LCAN)

```r
sum(fruit_seas_avail$combined_monthly_kg)
```

```
## [1] 1667
```


### Interpolation of monthly to daily biomass

```r
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
```


### Plot interpolated biomass over entire annual cycle

```r
plot(spline(x = biomass_monthly$day_of_year, 
            y = biomass_monthly$combined_monthly_kg, 
            n=365))
points(x = biomass_monthly$day_of_year, 
       y = biomass_monthly$combined_monthly_kg, 
       col="red", 
       pch=16)
```

![plot of chunk biomass_interpolation_plot](figure/fruit/biomass_interpolation_plot.png) 


### Calculate over my 2010 - 2011 study period

```r
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
```


### Plot daily biomass estimate over my study period

```r
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
```

![plot of chunk biomass_study_period_plot](figure/fruit/biomass_study_period_plot.png) 

