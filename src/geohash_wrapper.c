#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <R.h>
#include "geohash_wrapper.h"

void
R_GEOHASH_verify_hash(const char *geohash, int *result) 
{
  *result = GEOHASH_verify_hash(geohash);
}

void
R_GEOHASH_encode(double lat, double lon, unsigned int len, char **result)
{
  char *encoded;
  encoded = (char *)malloc(sizeof(char) * (len + 1));
  encoded = GEOHASH_encode(lat, lon, len);
  **result = *encoded;
}