#ifndef SEEK_H
#define SEEK_H
#include "string.h"

#define QUOTE 34
#define BSLASH 92

static unsigned char *seek_quote(unsigned char *p, size_t l)
{
	unsigned char *q;
	unsigned char *r;

	for (q = p; l > 0; l -= (++q)-p) {
		q = memchr(q, QUOTE, l);

		if (q == NULL)
			return q; // garbage in, garbage out

		for (r = q-1; r >= p && *r == BSLASH; --r)
			(void) 0;

		if ((q - r) % 2 == 1)
			return q;
	}
	return NULL; // l has reached 0
}
#endif