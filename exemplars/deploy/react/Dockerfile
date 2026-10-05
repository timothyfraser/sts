# Dockerfile: the Vite front end, built and served as a static site.
#
# Build it from the repo root (the build context is exemplars/deploy/):
#   docker build -f exemplars/deploy/react/Dockerfile \
#     --build-arg VITE_API_URL=https://your-api.example -t sts-test-web exemplars/deploy
# Run it:
#   docker run --rm -p 8080:8080 sts-test-web        then open http://localhost:8080/
#   docker run --rm -e PORT=9000 -p 9000:9000 sts-test-web    (any port: nginx reads $PORT)
#
# Two stages. Stage 1 uses Node to run `npm run build` and make dist/.
# Stage 2 is a small web server (nginx on Alpine, ~20 MB) holding only dist/:
# no Node, no node_modules, no source code in the final image.
# Works for any Vite app (React or plain): point APP_DIR at its folder,
# relative to the build context.

# ---- Stage 1: build ----
FROM node:22-alpine AS build
ARG APP_DIR=test-app/web
# VITE_ variables are read at BUILD time and baked into the files in dist/.
# DigitalOcean passes BUILD_TIME env vars from the app spec as build args.
ARG VITE_API_URL=""
WORKDIR /src
# Install first, from the lockfile only, so Docker can reuse this layer
# when you change code but not packages.
COPY ${APP_DIR}/package.json ${APP_DIR}/package-lock.json ./
RUN npm ci --no-audit --no-fund
COPY ${APP_DIR}/ ./
RUN VITE_API_URL="$VITE_API_URL" npm run build

# ---- Stage 2: serve ----
FROM nginx:1.27-alpine
# At start-up the nginx image fills ${PORT} in every file under
# /etc/nginx/templates/ and writes the result to /etc/nginx/conf.d/.
# Only variables that are set get filled in, so nginx's own $uri is left alone.
ENV PORT=8080
RUN mkdir -p /etc/nginx/templates && printf '%s\n' \
  'server {' \
  '  listen ${PORT};' \
  '  root /usr/share/nginx/html;' \
  '  index index.html;' \
  '  # Unknown paths fall back to index.html (single-page apps with client-side routes).' \
  '  location / { try_files $uri $uri/ /index.html; }' \
  '}' > /etc/nginx/templates/default.conf.template
COPY --from=build /src/dist/ /usr/share/nginx/html/
EXPOSE 8080
# No CMD: the nginx image's own start command runs the template step, then nginx.
