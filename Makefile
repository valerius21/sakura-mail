.PHONY: build run test test-unit test-integration clean greenmail-up greenmail-down seed

build:
	dune build

run: build
	SAKURA_MAILDIR=.sakura-data dune exec sakura-mail

test: test-unit

test-unit: build
	dune test

test-integration: build greenmail-up seed
	dune test
	$(MAKE) greenmail-down

greenmail-up:
	docker compose up -d
	@echo "Waiting for Greenmail to start..."
	@sleep 3

greenmail-down:
	docker compose down

seed: greenmail-up
	cd test/seed && npm install && npm run seed

clean:
	dune clean
