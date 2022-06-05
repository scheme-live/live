help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort


prepare-debian: ## Prepare a Debian host (call with sudo)
	apt update
	apt install --yes --no-install-recommends $(shell cat debian-system-dependencies.txt | tr '\n' ' ')

check: ## Run tests
	env | grep "^IMPLEMENTATION=" # requires an env variable called IMPLEMENTATION
	./local/bin/scheme-live $(IMPLEMENTATION) check $(PWD)

test: check  ## Run checks

html: ## Generate html from markdown documentation
	pandoc --metadata title="Scheme Live!" README.md --css styles.css --mathml --standalone --to=html5 --output index.html
	pandoc --metadata title="Scheme Live!" live/unstable/index.md --css ../../styles.css --mathml --standalone --to=html5 --output live/unstable/index.html

check-with-podman:
	env | grep "^IMPLEMENTATION=" # requires an env variable called IMPLEMENTATION
	podman run --volume $(PWD):/live --interactive --rm ghcr.io/scheme-live/schemers:stable bash -c 'cp /live/local/shell-subcommand.sh . && cd /live && SCHEME_LIVE_PREFIX=/ PATH=/opt/live/$(IMPLEMENTATION)/bin:/live/local/bin:/usr/bin/:/bin USER=$(USER) scheme-live $(IMPLEMENTATION) check'
