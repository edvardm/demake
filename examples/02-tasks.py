from invoke import task

TEST_DIR = "tests"

@task
def deps(c):
    c.run('poetry install --no-root')


@task
def _check_poetry(c):
    c.run('command -v poetry 2>/dev/null || (echo "poetry not found, please see https://python-poetry.org/docs/#installation"; exit 1)')


@task
def quick_test(c):
    c.run(f'pytest {opts} -m "not slow" {TEST_DIR}')


@task(pre=[_check_poetry, deps, quick_test])
def dev_init(c):
    pass
