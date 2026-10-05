# Dockerfile: the course test API (Plumber) as a container.
#
# Build it from the repo root (the build context is exemplars/deploy/):
#   docker build -f exemplars/deploy/plumber/Dockerfile -t sts-test-api exemplars/deploy
# Run it:
#   docker run --rm -p 8080:8080 sts-test-api        then open http://localhost:8080/echo?msg=hi
#   docker run --rm -e PORT=9000 -p 9000:9000 sts-test-api    (any port: the app reads $PORT)
#
# Base image: rocker/r-ver, pinned to one R version. Its package repository is
# a dated snapshot of Posit Package Manager that serves ready-built Linux
# binaries, so installing plumber takes seconds, not a compile, and the same
# Dockerfile builds the same image next month.
# The test API needs plumber and nothing else. It does NOT use sf, so we skip
# the big geospatial base (rocker/geospatial, ~2 GB) and its system libraries.
# If your own API needs sf, change the FROM line to rocker/geospatial:4.4.2.
FROM rocker/r-ver:4.4.2

# System library plumber needs (the sodium package links against libsodium),
# plus curl/ssl headers in case a package has to be built from source.
RUN apt-get update \
 && apt-get install -y --no-install-recommends libsodium-dev libcurl4-openssl-dev libssl-dev \
 && rm -rf /var/lib/apt/lists/*

# R packages. Add yours to this line (e.g. plumber dplyr readr).
RUN install2.r --error --skipinstalled plumber

# The app: plumber.R (the routes), run.R (starts the server on $PORT), and data/.
WORKDIR /app
COPY test-app/api/plumber.R test-app/api/run.R ./
COPY test-app/api/data/ ./data/

# DigitalOcean App Platform tells the app which port to use through $PORT
# (8080 unless the app spec says otherwise). 8080 is also our default.
ENV PORT=8080
EXPOSE 8080

# The start command lives HERE, so the platform's "Run command" stays empty.
CMD ["Rscript", "run.R"]
