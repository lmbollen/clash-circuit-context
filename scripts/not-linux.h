/* Included by check.sh into every Haskell module's preprocessing, so that
 * the code for platforms other than Linux (where a spill file is not mapped)
 * is built and tested on Linux too. */
#undef linux_HOST_OS
