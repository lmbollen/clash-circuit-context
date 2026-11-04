/*
 * Copyright  :  (C) 2026, QBayLogic B.V.
 * License    :  BSD2 (see the file LICENSE)
 *
 * What Clash.CircuitContext.Internal.Fst reads from an FST file, kept in C
 * because it walks libfst's hierarchy structs.
 */

#include <string.h>

#include "fstapi.h"

/* 1 when the FST file at this path has this comment, 0 when it has not, and
 * -1 when it cannot be read as an FST file. */
int ccc_fst_has_comment(const char *path, const char *comment)
{
    fstReaderContext *ctx = fstReaderOpen(path);
    struct fstHier *h;
    int found = 0;

    if (!ctx) {
        return -1;
    }
    while ((h = fstReaderIterateHier(ctx))) {
        if (h->htyp == FST_HT_ATTRBEGIN && h->u.attr.typ == FST_AT_MISC &&
            h->u.attr.subtype == FST_MT_COMMENT && h->u.attr.name &&
            strcmp(h->u.attr.name, comment) == 0) {
            found = 1;
            break;
        }
    }
    fstReaderClose(ctx);
    return found;
}
