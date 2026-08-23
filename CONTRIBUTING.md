# Contributing to spatialid

Thank you for helping improve `spatialid`.

## Protect private data

Issues, pull requests, logs, screenshots, fixtures, and examples in this
repository may become public. Use a minimal synthetic dataset or an openly
licensed public dataset. Do not upload confidential, restricted, unpublished,
or personally identifying data.

If a problem appears only with private data, reproduce its structure with
newly invented geometries and attributes before reporting it. Remove file
paths, credentials, names, and other identifying metadata from console output.

## Report a problem

Use the GitHub bug-report form and include:

- a minimal reproducible example made from synthetic or public data;
- the expected and actual behavior;
- the output of `sessionInfo()`; and
- any warnings or errors, after removing private information.

For questions about whether two real-world units should share an identity,
describe the general rule instead of submitting the underlying dataset. Such
decisions may depend on the research design and are not necessarily software
defects.

## Propose or implement a change

Open a feature request before starting a large change. Keep pull requests
focused, add or update tests, and update user-facing documentation when
behavior changes.

Run the standard checks from the repository root:

```sh
Rscript -e 'testthat::test_local()'
R CMD build .
R CMD check --no-manual spatialid_*.tar.gz
```

Generated documentation should remain synchronized with its roxygen source.
Add a concise entry to `NEWS.md` for user-visible changes.

By contributing, you agree that your contribution will be distributed under
the repository's MIT license.
