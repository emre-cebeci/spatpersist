# spatialid release checklist

Completing technical checks does not authorize publication. Creating a Git
tag, GitHub release, public repository, package-registry submission, or other
public distribution requires explicit approval from the package author.

## Release-candidate preparation

- [x] Set the package version consistently in `DESCRIPTION`, `NEWS.md`, and
      `CITATION.cff`.
- [x] Provide installation, workflow, validation, and calibration guidance.
- [x] Provide citation metadata and an open-source license.
- [x] Test the intentional exported API.
- [x] Run package checks on release, development, and old-release R versions
      across Linux, macOS, and Windows.
- [x] Enforce the configured test-coverage threshold.
- [ ] Complete substantive calibration and adjudication for the intended
      research workflow outside the package repository.

## Final privacy gate

- [ ] Audit the current tree, ignored files, generated artifacts, and complete
      Git history for private data, local paths, credentials, and identifying
      metadata.
- [ ] Confirm all examples, tests, vignettes, benchmarks, and documentation use
      synthetic or openly licensed public material only.
- [ ] Remove unsafe files from Git history if the audit finds any, then repeat
      the audit against the rewritten repository.
- [ ] Inspect the exact source archive that would be distributed.

## Publication gate

- [ ] Obtain explicit author approval to publish.
- [ ] Confirm the intended publication channel and repository visibility.
- [ ] Confirm the final version number and release date.
- [ ] Re-run `R CMD check --as-cran` on the exact release commit.
- [ ] Create the release tag only from the verified commit.
- [ ] Publish only through the explicitly approved channel.
- [ ] Verify the published archive and repository contents after publication.
