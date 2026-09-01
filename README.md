# Registry Server

[![User Guide](https://img.shields.io/badge/docs-User%20Guide-informational)](https://guide.ds-wizard.org)
[![License](https://img.shields.io/github/license/ds-wizard/registry-server)](LICENSE.md)

*Registry Server is the registry of knowledge models, document templates and locales
for Data Stewardship Wizard instances.*

This repository is a read-only distribution: the sources are generated from the development
repository and pushed here, so pull requests cannot be merged directly. Discuss changes in an
issue first, see [CONTRIBUTING](CONTRIBUTING.md).

## Repository layout

```
app/registry-server   entry point
src/               Shared, RegistryLib, Registry
test/              hspec suites: shared registry
config/            configuration, committed templates as *.example
scripts/           build info, config expansion, strict build
```

## Requirements

 - [Stack](https://docs.haskellstack.org) (GHC 9.10.3, snapshot lts-24.37) and `hpack`
 - PostgreSQL 15
 - Optional: `fourmolu`, `hlint`, `cspell`

## Build and run

```bash
./scripts/expand-example-files.sh   # creates the gitignored configs from *.example
make build
make run
```

## Tests and code style

```bash
make test                                    # all suites (needs a running database)
make test-registry
make check                                   # hlint + fourmolu + cspell
make format
```

## License

This project is licensed under the Apache License v2.0 - see the [LICENSE](LICENSE.md) file.
