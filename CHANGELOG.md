# Changelog

## Unreleased

## 0.3.3 - 2026-06-12

- feat: make `Route.validateUniqueness` public so it can be called as a standalone uniqueness/ambiguity check
- fix: percent-encode values substituted into `Route.link` URLs (and decode them on match) so reserved characters round-trip exactly through `Route.matchUrl`
- fix: `FormBody` maps repeated form fields (`tags=a&tags=b`) to JSON arrays instead of comma-joined strings, so list/array fields hydrate correctly
- fix: validate single-case DU wrappers of unsupported primitives (e.g. `Price of decimal`), emitting a clear error instead of silently misclassifying them as nested route unions
- perf: cache union-case reflection metadata keyed by `Type` to avoid recomputing it on every request
- docs: clarify that `SkipPrecondition` takes the inner type, not the `OverridablePreCondition<'T>` wrapper
- docs: document that `Route.link` substitutes only path-segment fields and omits query params

## 0.3.2 - 2026-05-28

- Update external dependencies: `Microsoft.SourceLink.GitHub` 10.0.201 → 10.0.300, `Microsoft.Testing.Extensions.CodeCoverage` 18.6.2 → 18.7.0
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
