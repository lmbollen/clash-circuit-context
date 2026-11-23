/*
 * Copyright  :  (C) 2026, QBayLogic B.V.
 * License    :  BSD2 (see the file LICENSE)
 *
 * The system calls of a spill file (Clash.CircuitContext.Internal.SpillFile):
 * open it, reserve a region's disk space, map a region into memory, unmap it,
 * write bytes at an offset, close it. Every function returns 0 or an errno
 * value (a file descriptor, or minus an errno value, for ccc_spill_open).
 *
 * Without shared writable mappings (Windows), ccc_spill_map fails with
 * ENOSYS and nothing is reserved ahead: the recorder writes each region when
 * it is full instead.
 */
#if !defined(_WIN32) && !defined(_FILE_OFFSET_BITS)
#define _FILE_OFFSET_BITS 64
#endif
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdint.h>

#ifdef _WIN32
#include <io.h>
#include <sys/stat.h>
#else
#include <sys/mman.h>
#include <unistd.h>
#endif

int ccc_spill_open(const char *path) {
#ifdef _WIN32
  int fd = _open(path, _O_RDWR | _O_CREAT | _O_TRUNC | _O_BINARY | _O_NOINHERIT,
                 _S_IREAD | _S_IWRITE);
#else
  int fd = open(path, O_RDWR | O_CREAT | O_TRUNC | O_CLOEXEC, 0666);
#endif
  return fd < 0 ? -errno : fd;
}

/* Allocate the disk blocks of [off, off + len), so that a store into its
 * mapping cannot fail for want of space (which would raise SIGBUS). */
int ccc_spill_reserve(int fd, int64_t off, int64_t len) {
#if defined(_WIN32)
  (void)fd; (void)off; (void)len;
  return 0;
#elif defined(__APPLE__)
  /* No posix_fallocate: preallocate from the physical end of the file (a
   * region is always allocated at the end), then extend the file. */
  fstore_t st = {F_ALLOCATECONTIG, F_PEOFPOSMODE, 0, (off_t)len, 0};
  if (fcntl(fd, F_PREALLOCATE, &st) == -1) {
    st.fst_flags = F_ALLOCATEALL;
    if (fcntl(fd, F_PREALLOCATE, &st) == -1) return errno;
  }
  return ftruncate(fd, (off_t)(off + len)) == 0 ? 0 : errno;
#else
  int r;
  do r = posix_fallocate(fd, (off_t)off, (off_t)len); while (r == EINTR);
  return r;
#endif
}

/* Map [off, off + len) of the file, shared and writable, at *out. */
int ccc_spill_map(int fd, int64_t off, int64_t len, void **out) {
#ifdef _WIN32
  (void)fd; (void)off; (void)len;
  *out = NULL;
  return ENOSYS;
#else
  void *p = mmap(NULL, (size_t)len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, (off_t)off);
  if (p == MAP_FAILED) { *out = NULL; return errno; }
  *out = p;
  return 0;
#endif
}

int ccc_spill_unmap(void *p, int64_t len) {
#ifdef _WIN32
  (void)p; (void)len;
  return ENOSYS;
#else
  return munmap(p, (size_t)len) == 0 ? 0 : errno;
#endif
}

/* Write all of [buf, buf + len) at off. */
int ccc_spill_write(int fd, const void *buf, int64_t len, int64_t off) {
  const char *b = buf;
  while (len > 0) {
#ifdef _WIN32
    if (_lseeki64(fd, off, SEEK_SET) < 0) return errno;
    int n = _write(fd, b, len > 0x40000000 ? 0x40000000 : (unsigned)len);
#else
    ssize_t n = pwrite(fd, b, (size_t)len, (off_t)off);
#endif
    if (n < 0) { if (errno == EINTR) continue; return errno; }
    b += n; len -= n; off += n;
  }
  return 0;
}

int ccc_spill_close(int fd) {
#ifdef _WIN32
  return _close(fd) == 0 ? 0 : errno;
#else
  return close(fd) == 0 ? 0 : errno;
#endif
}

/* The size a mapping's offset is a multiple of. */
int64_t ccc_spill_page_size(void) {
#ifdef _WIN32
  return 65536;
#else
  return (int64_t)sysconf(_SC_PAGESIZE);
#endif
}
