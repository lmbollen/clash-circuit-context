/*
 * Copyright  :  (C) 2026, QBayLogic B.V.
 * License    :  BSD2 (see the file LICENSE)
 *
 * The system calls of a spill file on Linux
 * (Clash.CircuitContext.Internal.SpillFile): open it, reserve a region's
 * disk space, map a region into memory, unmap it, write bytes at an offset,
 * close it. Every function returns 0 or an errno value (a file descriptor,
 * or minus an errno value, for ccc_spill_open). Compiled on Linux only.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>

int ccc_spill_open(const char *path) {
  int fd = open(path, O_RDWR | O_CREAT | O_TRUNC | O_CLOEXEC, 0666);
  return fd < 0 ? -errno : fd;
}

/* Allocate the disk blocks of [off, off + len), so that a store into its
 * mapping cannot fail for want of space (which would raise SIGBUS). */
int ccc_spill_reserve(int fd, int64_t off, int64_t len) {
  int r;
  do r = posix_fallocate(fd, (off_t)off, (off_t)len); while (r == EINTR);
  return r;
}

/* Map [off, off + len) of the file, shared and writable, at *out. */
int ccc_spill_map(int fd, int64_t off, int64_t len, void **out) {
  void *p = mmap(NULL, (size_t)len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, (off_t)off);
  if (p == MAP_FAILED) { *out = NULL; return errno; }
  *out = p;
  return 0;
}

int ccc_spill_unmap(void *p, int64_t len) {
  return munmap(p, (size_t)len) == 0 ? 0 : errno;
}

/* Write all of [buf, buf + len) at off. */
int ccc_spill_write(int fd, const void *buf, int64_t len, int64_t off) {
  const char *b = buf;
  while (len > 0) {
    ssize_t n = pwrite(fd, b, (size_t)len, (off_t)off);
    if (n < 0) { if (errno == EINTR) continue; return errno; }
    b += n; len -= n; off += n;
  }
  return 0;
}

int ccc_spill_close(int fd) { return close(fd) == 0 ? 0 : errno; }

/* The size a mapping's offset is a multiple of. */
int64_t ccc_spill_page_size(void) { return (int64_t)sysconf(_SC_PAGESIZE); }
