geohash_verify <- function(igeohash) {
  result = .C("R_GEOHASH_verify_hash",
          geohash = charToRaw(igeohash),
          isValid = as.integer(0)
  );
  return(result)
}

geohash_encode <- function(ilat, ilon, ilen) {
  result = .C("R_GEOHASH_encode",
              latitude = as.double(ilat),
              longitude = as.double(ilon),
              length = as.integer(ilen),
              geohash = as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9))
  );
  return(result)
}