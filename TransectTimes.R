ch <- odbcConnect(dsn = "PACE")
tr.q <- "SELECT 
  tblVegetationTransect.ID, 
  tblVegetationTransectDayTree.ID, 
  tblVegetationTransectDayTree.TreeSpeciesID, 
  tblGPSPoint.DateTime, tblGPSPoint.UtmEasting, 
  tblGPSPoint.UtmNorthing 
  FROM ((tblVegetationTransect 
  INNER JOIN tblVegetationTransectDay ON 
  tblVegetationTransect.ID = tblVegetationTransectDay.VegetationTransectID) 
  INNER JOIN tblVegetationTransectDayTree ON 
  tblVegetationTransectDay.ID = 
  tblVegetationTransectDayTree.VegetationTransectDayID) 
  INNER JOIN tblGPSPoint ON 
  tblVegetationTransectDayTree.GpsID = tblGPSPoint.ID 
  ORDER BY 
  tblVegetationTransect.ID, 
  tblVegetationTransectDayTree.ID, 
  tblGPSPoint.DateTime"
tr_times <- sqlQuery(ch, tr.q)
odbcCloseAll()

tr_times <- tr_times[, -3]
names(tr_times) <- c("tran_id", "tree_id", "timestamp", "x", "y")
tr_times$timestamp <- as.POSIXct(tr_times$timestamp)

tr_times <- tr_times[!is.na(tr_times$timestamp), ]

tr_ints <- ddply(tr_times, 
           "tran_id", 
           function(df) c(min = min(df$timestamp), 
                         max = max(df$timestamp)))

tr_ints$diff <- tr_ints$max - tr_ints$min

tr_hrs <- (as.numeric(median(tr_ints$diff)) * 151) / 60

# Transects took 93.5 ~ 100 hours = 200 points