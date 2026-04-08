# Changelog

## Unreleased

- fix: disable SourceLink when no `.git` directory exists (jj without colocated git)
- Replace bespoke release/coverage/API/doc scripts with shared NuGet tools and reusable workflows
- Use auto-discovering example-projects in CI workflow
- Add NuGet Trusted Publishing comment; set `check-docs: false` for AnalyzerShim

## v0.3.1

- Add URL matching API (`Route.createMatcher`, `Route.matchUrl`)
- Add URL matching docs and example app usage
- Add per-file line and branch coverage checks with new tests
- Migrate to xUnit v3 and MTP coverage
- Add tests for uncovered code paths
- Remove unreachable code paths in `Route.fs`
- Remove git dependency from release script for non-colocated jj
- Update help text

## v0.3.0

- Add OpenAPI 3.0 JSON spec generation
- Add `falco-routes` CLI tool for OpenAPI spec generation
- Document OpenAPI spec generation in README and docs
- Consolidate precondition functions in `Extraction.fs`

## v0.2.0

- Add `Returns<'T>`, `JsonBody<'T>`, `FormBody<'T>`, async extractors, and route constraint docs
- Support ASP.NET route constraints
- Rewrite example app to use ASP.NET Core cookie auth instead of `X-User-Id` header
- Fix API extractor to properly detect version bump; only bump major version after 1.0

## v0.1.2

- Validate that routes are unique and unambiguous
- Make `validateUniqueness` internal to avoid bumping the minor version

## v0.1.1

- Fix a bug and increase code coverage
