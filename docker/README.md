# Deployments

I'm documenting how I currently deploy things.

I'd like to automate this workflow as much as possible, and I intend to do just
that. For now, I'm running things manually until I can design an generalization
that appeals to me.

## Dependencies
- `nix-build`
- `docker`
- `gcloud`

## Step-by-step

1. Use `nix-build` to create our Docker image for Cloud Run.

```shell
> nix-build ./cloud_run.nix
```

This outputs a Docker image at `./result`.

1. Load the built image (i.e. `./result`) into `docker` so that we can tag it
   and push it to the Google Container Registry (i.e. GCR).

```shell
> sudo docker load <./result
```

1. (Optionally) Run the image locally to verify its integrity.

```shell
> sudo docker run -d <name>:<tag>
```

1. Tag and push the image to GCR.

```shell
> sudo docker tag <name>:<label> gcr.io/<google-cloud-project-id>/<name>:<latest>
```

# TODO: Prefer using a command line tool like `gcloud` for these steps.

1. Visit Google Cloud Run; create a new service with "Create Service"; select
   the uploaded Docker image from the "Container Image URL" field; click
   "Create" to deploy.


## Notes

You may need to authorize `gcloud` by running the following:

```shell
> sudo gcloud auth login --no-launch-browser
```

You must use `sudo` here since the `docker` invocations are prefixed with `sudo`
as well.
