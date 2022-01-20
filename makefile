help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort


prepare-debian: ## Prepare a Debian host (call with sudo)
	apt update
	apt install --yes --no-install-recommends $(shell cat debian-system-dependencies.txt | tr '\n' ' ')

check-with-docker:  ## Run checks with docker
	env | grep "^IMPLEMENTATION=" # requires an implementation
	env | grep "^IMAGE=" # requires a docker image
	docker run --volume $(PWD):/live --interactive --rm $(IMAGE) /bin/sh -c "cd /live && apt update && apt install --yes $(shell cat debian-system-dependencies.txt | tr '\n' ' ') && ./venv make IMPLEMENTATION=$(IMPLEMENTATION) check"

check: ## Run tests
	env | grep "^IMPLEMENTATION=" # requires an env variable called IMPLEMENTATION
	./local/bin/scheme-live $(IMPLEMENTATION) check $(PWD)

test: check  ## Run checks

html: ## Generate html from markdown documentation
	pandoc --metadata title="Scheme Live!" README.md --css styles.css --mathml --standalone --to=html5 --output index.html
