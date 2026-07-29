

# GIT setup
Development of the Haskell library, Surfer extension and documentation/tools happens
in the `haskell`, `surfer` and `common` branches.

These are merged together in `main`.

Version numbers are postfixed with `H` for Haskell, `S` for Surfer, and `D` for
documentation.

# Releasing

Assuming the changes have been merged to master.

Run tests!

- Update the version numbers of the changed parts.
- Update the changelog.

## Major version
Create a major version branch from `main`, called `<X>.<Y>`.

## Minor version
Merge the `main` branch into the release branch.

## Both

- Tag the last commit on the version branch with `v<X>.<Y>.<Z>[hsd]`

Get the docs to Hackage somehow???

# Bugfixes to older versions

## Backporting fixes
Cherry-pick bugfix commits into the version branch.

## Version-specific bugs
Update version branch directly.

## Both

Update version numbers and changelog in the version branch, tag and push docs.
