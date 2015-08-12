\name{geohash-package}
\alias{geohash-package}
\alias{geohash}
\docType{package}
\title{
Convert coordinates to and from Geohash
}
\description{
Convert coordinates between latitude/longitudes format and Geohash. The encoding and decoding functions are derived from Leonard Norrgard's Geohash Python library.
}
\details{
\tabular{ll}{
Package: \tab geohash\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2015-08-12\cr
License: \tab GNU Affero General Public License\cr
}

}
\author{
Christoph Kisfeld

Maintainer: Christoph Kisfeld <christoph.kisfeld@gmail.com>
}
\references{
https://en.wikipedia.org/wiki/Geohash
http://geohash.org/
https://github.com/vinsci/geohash/
}

\keyword{ package, geohash }
\examples{
# Decode a Geohash
geohash_decode("u1m2fw")
# Encode coordinates to Geohash
geohash_encode(52.2, 7.5, precision=6)
}
