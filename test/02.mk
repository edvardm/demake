.PHONY: deps
TEST_DIR := tests

.PHONY: dev-init
dev-init: .check-poetry deps quick-test ## prepare project ready for development

.PHONY: .check-poetry
.check-poetry:
	@command -v poetry 2>/dev/null || (echo "poetry not found, please see https://python-poetry.org/docs/#installation"; exit 1)

.PHONY: deps
deps:  ## install dependencies
	poetry install --no-root

.PHONY: quick-test
quick-test: opts ?= --durations 3
quick-test:
	pytest $(opts) -m "not slow" $(TEST_DIR)
