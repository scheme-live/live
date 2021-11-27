help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort


ubuntu-lts-prepare: ## Prepare an Ubuntu LTS host (call with sudo)
	apt update
	apt install --no-install-recommends build-essential

check-with-docker:  ## Run checks with docker
	env | grep "^IMPLEMENTATION=" # requires an implementation
	env | grep "^IMAGE=" # requires a docker image
	docker run --volume $(PWD):/live --interactive --rm $(IMAGE) /bin/sh -c "apt update && apt install make && cd /live && make IMPLEMENTATION=$(IMPLEMENTATION) check"

check: ## Run checks
	env | grep "^IMPLEMENTATION=" # requires an env variable called IMPLEMENTATION
	./local/bin/scheme-live $(IMPLEMENTATION) $(PWD)
