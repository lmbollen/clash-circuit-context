The changelog is managed using [qlog](https://github.com/QBayLogic/qlog/).
Run it with Nix, at the revision CI uses:

```sh
nix run github:QBayLogic/qlog/3bea2429e555e969984fbf897216cee74df2f73d -- entry
```

or install it as the `qlog` README describes. Later revisions of its flake
miss the `colorama` dependency, which is why the revision is pinned.

Each pull request that changes what users see adds an entry to
`changelog/entries`, for example with:

```sh
qlog entry
```

which asks for a title, the related issues, the category and the message.
Entries can also be written or edited by hand:

```md
---
issues: [27]
prs: []
---

# ADDED
`withWaveform` takes the directory to write the waveform to.
```

The categories are `ADDED`, `CHANGED`, `FIXED`, `DEPRECATED` and `REMOVED`. A
change without an issue or PR uses `no-links: true` instead of `issues` and
`prs`. Links to the issues and PRs are added to the end of the entry
automatically. Entries are Markdown, so Haskell code goes in a fenced block:

````md
```hs
writeWaveform "counter" (0, 100) (top clockGen resetGen enableGen)
```
````

Check the entries with `qlog check -a` (CI checks the entries that a pull
request adds or changes). At a release, the entries are collected into
`CHANGELOG.md`, see [RELEASING.md](../RELEASING.md).

The first release, 0.1.0.0, has no entries: its section in `CHANGELOG.md` is
written by hand, and describes the package as released, not how it got there.
