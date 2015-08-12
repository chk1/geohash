#if defined(__cplusplus)
extern "C" {
#endif

void R_GEOHASH_verify_hash(const char *geohash, int *result);
void R_GEOHASH_encode(double lat, double lon, unsigned int len, char **result);

#if defined(__cplusplus)
}
#endif