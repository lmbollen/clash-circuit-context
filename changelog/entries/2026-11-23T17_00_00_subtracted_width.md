---
no-links: true
---

# FIXED
A binding that the plugin does not record, of a width with a subtraction
such as `30 - ((1 + w) - y)`, no longer breaks the build when
`ghc-typelits-natnormalise` is enabled before the plugin. Before, the build
could fail with `Cannot satisfy: y <= 1 + w`: while the plugin's constraint
for the binding waited to be decided, natnormalise rewrote the width in it
and asked for conditions that nothing gives.
