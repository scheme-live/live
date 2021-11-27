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

If you favorite Scheme is supported, and installed locally, you can
run the tests with the following command:

```shell
./local/bin/scheme-live fantastic check .
```

### Running tests locally with docker

Running tests with docker, is easier in the sense you only need
have docker installed. Then you can run the following command:

```shell
make IMPLEMENTATION=fantastic IMAGE=fantastic/scheme check-with-docker
```

Where `IMPLEMENTATION` must be supported by `./local/bin/scheme-live`,
and `IMAGE` is the name of an image available in docker hub.

## Getting started from git repository

Do the following command line dance:

```shell
mkdir -p ~/src/scheme/live/
cd ~/src/scheme/live/
git clone https://github.com/scheme-live/live/ git
cd ~/src/scheme/live/git/
git remote add upstream git@github.com:scheme-live/live.git
```

Then enter the development environment with the following:

```shell
./venv
```

It may not be required, and you only need to do it once per
installation: you can prepare your Ubuntu operating system with:

```shell
sudo make ubuntu-lts-prepare
```

You now have access to `scheme-live` command line program, that allows
many nifty stuff such as running the same portable program with one
line over many Scheme implementations. `scheme-live` will take care of
setting up the correct environment variables, and abstract the
differences of the underlying Scheme command line interface, so that
you are always perfectly focused on the task at hand.

In the following, we will install chibi, then run a Scheme `hello`
program:

```shell
% scheme-live chibi install
% hello
Hello, schemers!
```

If your favorite scheme is available in your operating system, you do
not need to install it again with `scheme-live`.

## Continuation Integration

To add a Scheme implementation to continuation integration, you need
the following:

- A docker image available in [docker
  hub](https://hub.docker.com/),
  e.g. [schemers/stklos](https://github.com/scheme-containers/stklos),
  see
  [scheme-containers](https://github.com/scheme-containers/project#scheme-containers)
  , `IMAGE` could be `my-docker-hub-user/fantastic-scheme`;

- You need to add support for you fantastic Scheme in the shell
  program at `./local/bin/scheme-live`, for more details read the
  section "How to add support for my fantastic Scheme in
  `scheme-live`".

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
