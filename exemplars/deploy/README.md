# Deploy it twice: DigitalOcean and Posit Connect

One tiny app, two hosting routes. You deploy the **same** test app to
DigitalOcean App Platform once, and to the course's Posit Connect server, so you
can see what a hosting platform needs from you and what it does for you.

| | DigitalOcean App Platform | Posit Connect |
|---|---|---|
| **Cost** | **DigitalOcean once ($5/month, students may keep it)** | **Connect free for the course** |
| Account | Your own, with a payment method | Your course login on the course server |
| What it builds from | A `Dockerfile` in your GitHub repo | A `manifest.json` (R API) or a built `dist/` (web page) |
| Who starts the app | The Dockerfile's `CMD` (Run command left empty) | Connect runs `plumber.R` itself |
| Port | Your app reads `$PORT` (App Platform sets it; 8080 by default) | Connect picks; you never set a port |
| Redeploy | Push to the branch (Autodeploy) | Push, and a GitHub Actions workflow publishes |
| Secrets and settings | App Settings > Environment Variables | The content's Vars panel |
| URL | `https://<app-name>-<random>.ondigitalocean.app` | `https://<server>/<your-content-name>/` |

## What is in this folder

```
exemplars/deploy/
├── test-app/
│   ├── api/                 the test API: the one app you deploy twice
│   │   ├── plumber.R        routes /echo, /sum, /plot (the course's standard test API, unchanged)
│   │   ├── run.R            container start script: listens on $PORT, adds CORS
│   │   ├── data/            a small CSV, shipped with the app on both routes
│   │   ├── manifest.json    what Connect installs (written by manifestme.R)
│   │   └── manifestme.R     rewrites manifest.json
│   └── web/                 a one-page Vite front end that calls the three routes
├── plumber/
│   ├── Dockerfile           the API as a container (R + plumber, ~1 GB, no sf)
│   └── .do/app.yaml         the DigitalOcean App Platform spec for the API
├── react/
│   ├── Dockerfile           build the page with Node, serve dist/ with nginx on $PORT
│   └── .do/app.yaml         the DigitalOcean App Platform spec for the page
└── .dockerignore            keeps node_modules/ and dist/ out of the build
```

Both Dockerfiles use `exemplars/deploy/` as the build context. From the repo root:

```bash
docker build -f exemplars/deploy/plumber/Dockerfile -t sts-test-api exemplars/deploy
docker run --rm -p 8080:8080 sts-test-api          # http://localhost:8080/echo?msg=hi

docker build -f exemplars/deploy/react/Dockerfile \
  --build-arg VITE_API_URL=http://localhost:8080 -t sts-test-web exemplars/deploy
docker run --rm -p 8081:8080 sts-test-web          # http://localhost:8081/
```

No Docker? Run the API with R instead: `cd exemplars/deploy/test-app/api` then
`Rscript run.R` (port 8080). Run the page with `cd exemplars/deploy/test-app/web`,
`npm install`, `npm run dev`.

## Route 1: DigitalOcean App Platform (once)

App Platform builds your Dockerfile straight from GitHub and runs it. You never
touch a server. It costs money, so do it once, on purpose.

1. **Account.** Sign up at digitalocean.com and add a payment method. Create a Project.
2. **Your own copy.** Fork or copy this repository to your GitHub account.
   DigitalOcean deploys from a repo it can see, so the next step grants it one.
3. **Create App.** In your Project: **Create > App Platform > Git repository >
   GitHub**. Click **Edit your GitHub Permissions**, choose **Only select
   repositories**, pick your copy, and **Save**.
4. **Source.** Pick the repository and the branch. Set the source directory to
   `exemplars/deploy`. Turn **Autodeploy** on.
5. **Configure.** Set **Build strategy** to **Dockerfile** and the Dockerfile
   path to `exemplars/deploy/plumber/Dockerfile`. **Leave the Run command
   empty**: the Dockerfile's `CMD` starts the app. Set the HTTP port to `8080`.
   Pick the smallest instance size ($5/month).
6. **Or skip steps 4 and 5** and paste `plumber/.do/app.yaml` into the app's
   **Settings > App Spec** (change `repo:` and `branch:` to yours first). The
   spec is the same recipe written down. With the `doctl` tool:
   `doctl apps create --spec exemplars/deploy/plumber/.do/app.yaml`.
7. **Check it.** When the build log ends, open the live URL and try
   `/echo?msg=hi` and `/__docs__/`. If it fails, read **Runtime Logs** first.

**About `$PORT`.** App Platform sends web traffic to the app's HTTP port and
also puts that number in the container's `PORT` variable. It is 8080 unless you
change it. `run.R` and the nginx image both listen on `$PORT`, so they work on
any number; an app that hard-codes a port only works when the two happen to match.

**The front end (optional, a second $5 app).** `react/.do/app.yaml` deploys the
page the same way. Set its `VITE_API_URL` to your API's live URL first: Vite
writes that value into the page at build time. The API's `ALLOWED_ORIGIN`
variable controls which pages may call it (`*` means any, fine for this toy).
A plain static site on App Platform (build command plus output directory, no
Dockerfile) is another way to host the page.

### Archive it when you are done

App Platform bills by the hour while an app exists. **Archive the app within
two weeks and the whole activity costs about $5.** In the control panel: open
the app, go to its **Settings** tab and choose **Archive** (or **Destroy** to delete it outright).
An archived app stops billing, keeps its settings, and can be restored later.
Check **Billing** a day later to confirm nothing is still running. Keeping it
running is your choice and your $5 a month.

## Route 2: Posit Connect (free for the course)

Connect runs R and static sites for you. There is no Dockerfile: Connect reads
`manifest.json`, installs the listed R packages, and runs `plumber.R` directly.

- **The API.** `test-app/api/manifest.json` describes the bundle: `plumber.R`
  plus `data/example.csv` (`run.R` is left out; Connect does not need it).
  After you change packages, rewrite it from the repo root with
  `Rscript exemplars/deploy/test-app/api/manifestme.R`. Publish it the same way
  as the course API in `exemplars/api-plumber/` (its `deployme.R` shows the
  rsconnect call; point it at `exemplars/deploy/test-app/api`).
- **The page.** Use the workflow in
  `exemplars/react-starter/.github/workflows/deploy.yml`. It builds the page and
  publishes `dist/` to Connect on every push. Copy it into your own repo's
  `.github/workflows/`, with this page's files at that repo's root, and set the
  secrets and variables its header lists. `vite.config.js` uses `base: './'`,
  so the same `dist/` works under a Connect path and at a DigitalOcean root.
- On Connect the page and the API share one server address, so the browser
  does not need CORS; on DigitalOcean they are two addresses and `run.R` adds it.

## Why the images look like this

- **Plumber image: `rocker/r-ver`, pinned.** It installs R packages as
  ready-built Linux binaries from a dated snapshot, so the build takes a minute
  and is repeatable. The test API needs only `plumber`, so the image skips
  `sf` and the large geospatial base. If your API reads spatial data with `sf`
  (as `exemplars/api-plumber/` does), change the `FROM` line to
  `rocker/geospatial` at the same R version, which ships sf's system libraries.
- **Web image: two stages.** Node builds `dist/`, then a small nginx image
  serves only `dist/`. The final image holds no Node, no `node_modules`, and no
  source. nginx fills `${PORT}` into its config when the container starts.
- **The start command is in the Dockerfile.** That is why the platform's
  **Run command** stays empty, on DigitalOcean and anywhere else that runs containers.

## Proof the Dockerfiles build

`.github/workflows/docker-build-templates.yml` builds both images with
`docker build`, runs each on `PORT=9090` (not the default) and calls it. It is
manual only (**Actions > docker-build-templates > Run workflow**), pushes no
image, and uses no secrets.
