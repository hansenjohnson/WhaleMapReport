## functions ##

library(sf)

subset_canadian = function(df, crs_string = "+init=epsg:3857"){
  
  # remove NAs from df coordinates
  if(anyNA(df$lat)|anyNA(df$lon)){
    df = df %>% filter(!is.na(lat) & !is.na(lon))
    message('Removing data with missing coordinates')
  }
  
  # catch and return empty input data
  if(nrow(df)==0){
    message('No data!')
    return(df)
  }
  
  # define bounding box of canadian region
  bb = data.frame(
    lon = c(-72, -67.279444, -67.743056, -67.468056, -65.699722, -65, -40, -40, -72, -72),
    lat = c(46, 44.186667, 42.887222, 42.518889, 40.451389, 40, 40, 67, 67, 46)
  )
  
  # coordinate reference
  crs_ref = st_crs(crs_string)
  
  # convert to polygon and create sfc
  can = st_sfc(st_polygon(list(as.matrix(bb))), crs = crs_ref)
  
  # convert to spatial features
  df_sf = st_as_sf(df, coords = c("lon", "lat"), crs = crs_ref, agr = "constant", remove = FALSE)
  
  # spatial subsets
  df_in = st_within(x = df_sf, y = can, sparse = FALSE, prepared = FALSE)[,1]
  df_can = df_sf[df_in,]
  
  # convert back to data.frame
  out = as.data.frame(df_can)
  out$geometry = NULL
  
  return(out)
}