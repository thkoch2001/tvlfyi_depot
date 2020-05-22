#ifndef REFSPEC_H
#define REFSPEC_H

#define TAG_REFSPEC "refs/tags/*:refs/tags/*"
extern const struct refspec_item *tag_refspec;

struct refspec_item {
	unsigned force : 1;
	unsigned pattern : 1;
	unsigned matching : 1;
	unsigned exact_sha1 : 1;

	char *src;
	char *dst;
};

#define REFSPEC_FETCH 1
#define REFSPEC_PUSH 0

#define REFSPEC_INIT_FETCH { .fetch = REFSPEC_FETCH }
#define REFSPEC_INIT_PUSH { .fetch = REFSPEC_PUSH }

/**
 * A struct refspec holds the parsed interpretation of a refspec.  If it will
 * force updates (starts with a '+'), force is true.  If it is a pattern
 * (sides end with '*') pattern is true.  src and dest are the two sides
 * (including '*' characters if present); if there is only one side, it is src,
 * and dst is NULL; if sides exist but are empty (i.e., the refspec either
 * starts or ends with ':'), the corresponding side is "".
 *
 * An array of strings can be parsed into an array of struct refspecs using
 * parse_fetch_refspec() or parse_push_refspec().
 *
 * remote_find_tracking(), given a remote and a struct refspec with either src
 * or dst filled out, will fill out the other such that the result is in the
 * "fetch" specification for the remote (note that this evaluates patterns and
 * returns a single result).
 */
struct refspec {
	struct refspec_item *items;
	int alloc;
	int nr;

	const char **raw;
	int raw_alloc;
	int raw_nr;

	int fetch;
};

int refspec_item_init(struct refspec_item *item, const char *refspec,
		      int fetch);
void refspec_item_init_or_die(struct refspec_item *item, const char *refspec,
			      int fetch);
void refspec_item_clear(struct refspec_item *item);
void refspec_init(struct refspec *rs, int fetch);
void refspec_append(struct refspec *rs, const char *refspec);
void refspec_appendn(struct refspec *rs, const char **refspecs, int nr);
void refspec_clear(struct refspec *rs);

int valid_fetch_refspec(const char *refspec);

struct argv_array;
/*
 * Determine what <prefix> values to pass to the peer in ref-prefix lines
 * (see Documentation/technical/protocol-v2.txt).
 */
void refspec_ref_prefixes(const struct refspec *rs,
			  struct argv_array *ref_prefixes);

#endif /* REFSPEC_H */
