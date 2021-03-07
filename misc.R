#### MISC library
####
#### Includes the following functions:
####
#### calDist
#### bsumm
#### RandContacts
#### ReedFrost1
#### ReedFrost2
#### ReedFrost3
#### rpert
#### rgBeta
#### rtriang
####

# calculate the distance based on the Eucludian using UTM coordinates
calDist <<- function(Index) {
  distance <- sqrt((aHerd$north - aHerd$north[Index]) ^ 2 +
                   (aHerd$east - aHerd$east[Index]) ^ 2) / 1000
  return(distance)
}

# Summary statistics
bsumm <- function(sim.vals, graph = "none", rnd = 0, alpha = 0.05, ...) {
  pct <- c(alpha / 2, 1 - alpha / 2)
  out <- c(mean(sim.vals),
           median(sim.vals),
           sqrt(var(sim.vals)),
           quantile(sim.vals, probs = pct))
  names(out) <- c("mean",
                  "median",
                  "sd",
                  paste(pct[1] * 100, "%", sep = ""),
                  paste(pct[2] * 100, "%", sep = ""))

  if (graph=="density") {
    plot(density(sim.vals, from = min(sim.vals)),...)

  } else {

    if(graph=="hist") hist(sim.vals,
                           freq = FALSE,
                           xlab = paste("N =", length(sim.vals)),...)
  }

  return(round(out,digits=rnd))
}

# randomize contact.vectors for fractions
RandContacts <- function(contact.vector) {
  floor(contact.vector) +
  rbinom(length(contact.vector),
         1,
         contact.vector - floor(contact.vector))
}

# Tom's Reed-Frost function (recalculate entire intraherd epidemic curve each
# day)
ReedFrost1 <- function(T, iLag, susc, pr) {
  TotSus <- susc
  inf    <- 0
  T      <- max(1,T)

  if(iLag < 1) iLag <- 0
  m <- c(1, rep(0, T + iLag))

  for (x in 1:T) {
    inf         <- inf + m[x]
    m[x + iLag] <- susc * (1 - (1 - pr) ^ inf)
    susc        <- susc - m[x + iLag]
  }

  return(min(1,sum(m[1:T])/TotSus))
}

# Tom's version, corrected to change 0 iLag to 1; may not matter
ReedFrost2 <- function(T, iLag, susc, pr) {
  TotSus <- susc
  iLag   <- iLag + (iLag == 0)
  inf    <- 0
  T      <- max(1, T)
  m      <- c(1, rep(0,50))

  for (x in 1:T) {
    inf       <- inf + m[x]
    m[x+iLag] <- susc * (1 - (1 - pr) ^ inf)
    susc      <- susc - m[x + iLag]
  }

  return(min(1, sum(m[1:T]) / TotSus))
}

## 0/1 infectious status
ReedFrost3 <- function(T, iLag, susc, pr) {
  (T > iLag)
}


## Function to generate n PERT beta random variates with min a, mode l, and max
## b.
rpert <- function(n, a, l, b) {
  mu <- (a + 4 * l + b) / 6

  if (mu == l) {
    v <- w <- 3

  }  else {

    v <- (mu - a) * (2 * l - a - b) / (l - mu) / (b - a)
    w <- v * (b - mu) / (mu - a)
  }

  a + (b - a) * rbeta(n, v, w)
}

## Function to generate n Generalized BETA random variates, alpha a, beta b,
## MIN min, MAX max.
rgBeta <- function(n, a, b, min, max) {
	min + ((max - min) * (rbeta(n, a, b)))
}

## Function to generate n triangular distributed variates
rtriang <- function(n, min = 0, mode = NULL, max = 1) {

  if (is.null(mode)) mode <- (max + min) / 2

  ##Error checking
	if (min > max) {
    warning("rtriang: min>max, swx values")
    temp <- min
    min  <- max
    max  <- temp
  }

  if ((mode < min) | (mode > max)) {
          stop("Mode(",mode,") outside range (",min,", ",max,")")
  }

  if (max == min) return(rep(mode,n))

  ##distrib creation
	xvals  <- runif(n)
  hivals <- (xvals > (mode / (max - min)))
	xvals[!hivals] <- (min + (sqrt((xvals[!hivals]) * ((mode - min) * (max - min)))))
	xvals[hivals]  <- (max - (sqrt((1 - xvals[hivals]) * ((max - mode) * (max - min)))))
	return(xvals)
}


###########################
## maxUnity
##
## Getting around pmin(1,...)
## and not checking for NA's
###########################
maxUnity <- function (el) {
  change     <- el > 1
  el[el > 1] <-1
  el
}


###########################################################################
## randInfoFile
##
## Generate random herd info file using:
## locations selected within a circle with approximately uniform pdf
## specified herd type frequencies (defaults from Bates 3-county data)
##
## Input:
## lat       latitude of circle center (degrees)
## long      longitude of circle center (degrees)
## radius    radius of circle (kilometers) -- length on earth surface
## dens      herd density (herds/km^2) -- only used if radius not specified
## n         number of herds to generate
## filename  output file name -- file only written if name provided
## freqs     vector of herd type frequencies
##
## Output:   nx5 data frame with herd IDs, latitude, longitude, herd type,
##           and herd status; optionally written to a file.
##
###########################################################################
randInfoFile <- function(lat    = 36.4,
                         long   = -119.4,
                         radius = NULL,
                         dens   = 0.043,
                         n      = 1000,
                         filename = NULL,
                         freqs = c(0.260,0.036,0.173,0.057,0.014,0.010,0.003,
                                   0.034,0.001,0.028,0.031,0.351,0.002)) {
  earthRad <- 6371.3   # kilometers

  if (is.null(radius)) {
    arc <- acos(1 - n / dens / (2 * pi * earthRad ^ 2))

  } else {

    arc <- radius / earthRad
  }

  coords   <- rSphereSurf(n      = n,
                          arc    = arc,
                          center = c(long, lat),
                          units  = "degrees")
  ID       <- 1:n
  herdType <- sample(1:length(freqs), n, TRUE, freqs)
  status   <- rep(1, n)
  out      <- data.frame(ID,
                         lat  = coords$lat,
                         long = coords$long,
                         herdType,
                         status)

  if (!is.null(filename)) write.table(out,
                                      file      = filename,
                                      row.names = FALSE,
                                      sep       = ",")
  out
}


#######################################################################
## rSphereSurf
##
## Generate random locations within a circle on the surface of a sphere
##
## Input:
## n        number of coordinate pairs
## arc      arc length in radians from center to circle edge
## center   coordinate pair (lon, lat) for center of circle
## units    units for both "center" and output
##
## Output:  nx2 data frame with longtiudes and latitudes
##
#######################################################################
rSphereSurf <- function(n, arc, center = c(0,0), units = "degrees") {
  degPerRad <- 180 / pi   # degrees per radian

  if (units == "degrees") {
    center = center / degPerRad

  } else {

    if (units != "radians") stop("Units not recognized.")
  }

  if (arc > pi) stop("Circle is too large--exceeds sphere circumference!")

  theta <- runif(n,0,2*pi)  # random direction from center of circle
  r     <- acos(1-runif(n,0,1)*(1-cos(arc))) # random arc length
  lon1  <- center[1]
  lat1  <- center[2]
  lat   <- asin(sin(lat1) * cos(r) + cos(lat1) * sin(r) * cos(theta))

  if (cos(lat1) > .Machine$double.eps) {
    dlon <- atan2(sin(theta) * sin(r) * cos(lat1),
                  cos(r) - sin(lat1) * sin(lat))
  } else {
    dlon <- theta
  }

  long <- (lon1 - dlon + pi) %% (2 * pi) - pi
  coords <- data.frame(long, lat)

  if (units=="degrees") coords = coords * degPerRad
  coords
}

