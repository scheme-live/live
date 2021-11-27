# Scheme Live

- Move fast. (Be a complement to SRFI and RnRS which move slower.)

- Make one collection of libraries that work cohesively together.

- Make one complete library for each job instead of several incomplete
  libraries. When we can't agree, resolve the the debate by talking
  instead of forking code.

- Prefer interfaces that are portable to different Scheme standards,
  Scheme implementations, and operating systems. Porting is done by
  whoever has time to do it.

- All libraries are collectively owned. In the unstable version, all
  libraries are subject to change.

- Release a stable version of our API periodically.

- Work with RnRS, SRFI, and other library authors to share interfaces
  and implementations whenever goals are aligned.

## Running tests locally

There is two way to run the tests locally with docker, and without
docker.

### Running tests locally

To run the tests locally, you can use the shell program associated
with your fantastic scheme such as:

```shell
./local/bin/scheme-live-fantastic
```

### Running tests locally with docker

Running tests with docker, is easier in the sense you only need
have docker installed. Then you can run the following command:

```shell
make IMPLEMENTATION=fantastic IMAGE=fantastic/scheme check-with-docker
```

Where `IMPLEMENTATION` match a file such as
`./local/bin/scheme-live-fantastic`, and `IMAGE` is the name of an
image available in docker hub.

## Continuation Integration

To add a Scheme implementation to continuation integration, you need
the following:

- A docker image available in the [docker
  hub](https://hub.docker.com/),
  e.g. [schemers/stklos](https://github.com/scheme-containers/stklos),
  see
  [scheme-containers](https://github.com/scheme-containers/project#scheme-containers)
  , `IMAGE` could be `my-docker-hub-user/fantastic-scheme`;

- You need a test runner called with the POSIX shell, see inside this
  repository the files such as `./local/bin/scheme-live-fantastic`
  where `fantastic` is `IMPLEMENTATION`;

- Then, you can add the implementation to the continuation integration
  strategy matrix inside
  `./.github/workflows/continuation-integration.yml`, for instance:

  ```yaml
  strategy:
    matrix:
      include:
        - IMAGE: schemers/chicken
          IMPLEMENTATION: chicken
        ...
        - IMAGE: my-docker-hub-user/fantastic-scheme
          IMPLEMENTATION: fantastic
  ```
