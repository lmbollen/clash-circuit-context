# Releasing clash-circuit-context

Versions follow the [Haskell Package Versioning Policy](https://pvp.haskell.org/):
`A.B.C.D`, where a change to `A.B` may break code that uses the package, a
change to `C` only adds to the API, and a change to `D` only fixes bugs.

The version is set in `clash-circuit-context.cabal` only.

## Releasing a new version

1. Create a branch from `main`, for example `release-0.1.1.0`.
2. Set `version:` in `clash-circuit-context.cabal`. Check that `tested-with`
   lists the GHCs that CI tests.
3. Collect the changelog entries into `CHANGELOG.md`, and delete them:
   ```sh
   qlog collect -v 0.1.1.0 -i CHANGELOG.md
   qlog clean -D
   ```
   Read the new section of `CHANGELOG.md`, and edit it where needed.
   Run these in the development shell (`nix develop`), which has `qlog`.
4. Check that the package is complete and that its documentation builds:
   ```sh
   cabal check
   nix build .#ghc9124.clash-circuit-context
   cabal sdist
   cabal haddock --haddock-for-hackage
   ```
   Unpack the tarball from `dist-newstyle/sdist/`, and run `cabal build` in it
   with `--project-file` pointing at `cabal.project.hackage`, so that it
   builds against released dependencies only.
5. Commit (`Release 0.1.1.0`), open a pull request and merge it after CI
   passes.
6. Tag the merge commit on `main`, and push the tag:
   ```sh
   git tag -a v0.1.1.0 -m "clash-circuit-context 0.1.1.0"
   git push origin v0.1.1.0
   ```
7. Upload the package and its documentation to Hackage. Upload a candidate
   first, and read its page:
   ```sh
   cabal upload dist-newstyle/sdist/clash-circuit-context-0.1.1.0.tar.gz
   cabal upload --documentation dist-newstyle/clash-circuit-context-0.1.1.0-docs.tar.gz
   ```
   Then publish both, with `--publish` added to the same commands.
8. Create a release on
   [GitHub](https://github.com/lmbollen/clash-circuit-context/releases/new) for
   the tag, with the new section of `CHANGELOG.md` as the description.
9. Tell the people that are subscribed to the fixed issues and the merged pull
   requests. Check out the commit before step 3, which still has the entries,
   and run:
   ```sh
   qlog gh msg -i -p -v 0.1.1.0 -o post_msgs.sh
   ```
   Read `post_msgs.sh`, then run it with `bash post_msgs.sh`.
