alphabet = c('0','1','2','3','4','5','6','7','8','9','b','c','d','e','f','g','h','j','k','m','n','p','q','r','s','t','u','v','w','x','y','z')
bits = c(16, 8, 4, 2, 1)

#' Encode latitude and longitude to a Geohash
#' 
#' @param latitude Coordinate latitude
#' @param longitude Coordinate longitude
#' @param precision Precision/length of the resulting Geohash
#' @return The Geohash string
#' @seealso geohash_encode.default
#' @seealso geohash_encode.data.frame
#' @examples
#' geohash_encode(52, 7, precision=6)
#' geohash_encode(52.54123, 7.67123)
geohash_encode <- function(x, ...) { 
  UseMethod("geohash_encode", x) 
}

#' Encode latitude and longitude to a Geohash
#' 
#' @param latitude Coordinate latitude
#' @param longitude Coordinate longitude
#' @param precision Precision/length of the resulting Geohash
#' @return The Geohash string
#' @examples
#' geohash_encode(52, 7, precision=6)
#' geohash_encode(52.54123, 7.67123)
geohash_encode.default <- function(latitude, longitude, precision=12) {
  lat_min = -90.0
  lat_max = 90.0
  lon_min = -180.0
  lon_max = 180.0
  
  geohash = c()
  bit = 0
  ch = 0
  even = TRUE
  
  while(length(geohash) < precision) {
    if(even) { # current position in string is odd or even? assume 1st=even and then alternate with each char
      mid = (lon_min+lon_max)/2
      if(longitude > mid) {
        ch = bitwOr(ch, bits[bit+1])
        lon_min = mid
        # lon_max = lon_max
      } else {
        # lon_min = lon_min
        lon_max = mid
      }
    } else { # not even
      mid = (lat_min+lat_max)/2
      if(latitude > mid) {
        ch = bitwOr(ch, bits[bit+1])
        lat_min = mid
        # lat_max = lon_max
      } else {
        # lat_min = lon_min
        lat_max = mid
      }
    }
    even = !even
    if(bit < 4) {
      bit = bit+1
    } else {
      geohash[length(geohash)+1] = alphabet[ch+1]
      bit = 0
      ch = 0
    }
  }
  geohashString = paste(geohash, collapse = '')
  return(geohashString)
}

#' Encode dataframes containing latitude and longitude to a dataframe containing Geohashes
#' 
#' @param df A data frame where you have specified coordinates in the latitude and longitude column
#' @param precision Precision/length of the resulting Geohash
#' @return Data.frame with the Geohash strings
#' @examples
#' lats=c(52,47,-18)
#' lons=c(7,20,0)
#' df = data.frame(latitude=lats, longitude=lons)
#' geohash_encode(df)
geohash_encode.data.frame <- function(df, precision=12) {
  x <- mapply(geohash_encode, df$latitude, df$longitude, precision)
  resultdf = data.frame(latitude=df$latitude, longitude=df$longitude, geohash=x, precision=precision)
  return( resultdf )
}

geohash_decode <- function(x, ...) { 
  UseMethod("geohash_decode", x) 
}

#' Decode a Geohashes to a dataframe containing latitude and longitude
#' 
#' @param geohash Geohash string
#' @return Data.frame with latitude, longitude, and their respective error values
#' @examples
#' geohash_decode("u1hzz6")
#' geohash_decode("u1m2fw")
#' geohash_decode("u1m2fwtr49m2fwtr49m2")
geohash_decode.default <- function(geohash) {
  geohash_c = unlist(strsplit(geohash, split="")) # split input string to collection c()
  lat_min = -90.0
  lat_max = 90.0
  lon_min = -180.0
  lon_max = 180.0
  lat_err = 90.0
  lon_err = 180.0
  even = TRUE
  
  for(i in 1:length(geohash_c)){
    ch = geohash_c[i]
    cd = match(ch, alphabet)-1 # find the current char's position in the alphabet
    for(bit in bits) {
      if(even) {
        lon_err = lon_err/2
        if(bitwAnd(cd, bit) > 0) { # bitwise and
          lon_min = (lon_min+lon_max)/2
          # lon_max = lon_max
        } else {
          # lon_min = lon_min
          lon_max = (lon_min+lon_max)/2
        }
      } else { # not even
        lat_err = lat_err/2
        if(bitwAnd(cd, bit) > 0) { # bitwise and
          lat_min = (lat_min+lat_max)/2
          # lat_max = lat_max
        } else {
          # lat_min = lat_min
          lat_max = (lat_min+lat_max)/2
        }
      }
      even = !even
    }
    
  }
  lat = (lat_min+lat_max)/2
  lon = (lon_min+lon_max)/2
  return(data.frame(latitude=lat, longitude=lon, latitude_error=lat_err, longitude_error=lon_err))
}

#' Decode lists containing Geohashes to a dataframe containing latitude and longitude
#' 
#' @param li List of Geohash strings
#' @return Data.frame with latitude, longitude, and their respective error values
#' @examples
#' geohash_decode(list("u1m2fw", "u1m2fwtr49m2fwtr49m2"))
geohash_decode.list <- function(li) {
  x <- sapply(li, geohash_decode, simplify=TRUE)
  return(as.data.frame(t(x)))
}

# This function was meant to strip redundant numbers from the Geohash,
# but R internals already do that
#
# geohash_decode <- function(geohash) {
#   decoded = geohash_decode_exactly(geohash)
#   lat_precision = max(1, as.integer( round( -log10(decoded$latitude_error) ) )-1)
#   lon_precision = max(1, as.integer( round( -log10(decoded$longitude_error) ) )-1)
#   lats = round(decoded$latitude, lat_precision)
#   lons = round(decoded$longitude, lon_precision)
#   return(data.frame(latitude=lats, longitude=lons, latitude_error=decoded$latitude_error, longitude_error=decoded$longitude_error))
# }