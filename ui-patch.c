/* ui-patch.c: generate patch view
 *
 * Copyright (C) 2007 Lars Hjemli
 *
 * Licensed under GNU General Public License v2
 *   (see COPYING for full license text)
 */

#include "cgit.h"

static void print_line(char *line, int len)
{
	char c = line[len-1];

	line[len-1] = '\0';
	htmlf("%s\n", line);
	line[len-1] = c;
}

static void header(unsigned char *sha1, char *path1, int mode1,
		   unsigned char *sha2, char *path2, int mode2)
{
	char *abbrev1, *abbrev2;
	int subproject;

	subproject = (S_ISGITLINK(mode1) || S_ISGITLINK(mode2));
	htmlf("diff --git a/%s b/%s\n", path1, path2);

	if (is_null_sha1(sha1))
		path1 = "dev/null";
	if (is_null_sha1(sha2))
		path2 = "dev/null";

	if (mode1 == 0)
		htmlf("new file mode %.6o\n", mode2);

	if (mode2 == 0)
		htmlf("deleted file mode %.6o\n", mode1);

	if (!subproject) {
		abbrev1 = xstrdup(find_unique_abbrev(sha1, DEFAULT_ABBREV));
		abbrev2 = xstrdup(find_unique_abbrev(sha2, DEFAULT_ABBREV));
		htmlf("index %s..%s", abbrev1, abbrev2);
		free(abbrev1);
		free(abbrev2);
		if (mode1 != 0 && mode2 != 0) {
			htmlf(" %.6o", mode1);
			if (mode2 != mode1)
				htmlf("..%.6o", mode2);
		}
		htmlf("\n--- a/%s\n", path1);
		htmlf("+++ b/%s\n", path2);
	}
}

static void filepair_cb(struct diff_filepair *pair)
{
	header(pair->one->sha1, pair->one->path, pair->one->mode,
	       pair->two->sha1, pair->two->path, pair->two->mode);
	if (S_ISGITLINK(pair->one->mode) || S_ISGITLINK(pair->two->mode)) {
		if (S_ISGITLINK(pair->one->mode))
			print_line(fmt("-Subproject %s", sha1_to_hex(pair->one->sha1)), 52);
		if (S_ISGITLINK(pair->two->mode))
			print_line(fmt("+Subproject %s", sha1_to_hex(pair->two->sha1)), 52);
		return;
	}
	if (cgit_diff_files(pair->one->sha1, pair->two->sha1, print_line))
		html("Error running diff");
}

void cgit_print_patch(char *hex, struct cacheitem *item)
{
	struct commit *commit;
	struct commitinfo *info;
	unsigned char sha1[20], old_sha1[20];
	char *patchname;

	if (!hex)
		hex = cgit_query_head;

	if (get_sha1(hex, sha1)) {
		cgit_print_error(fmt("Bad object id: %s", hex));
		return;
	}
	commit = lookup_commit_reference(sha1);
	if (!commit) {
		cgit_print_error(fmt("Bad commit reference: %s", hex));
		return;
	}
	info = cgit_parse_commit(commit);
	hashcpy(old_sha1, commit->parents->item->object.sha1);

	patchname = fmt("%s.patch", sha1_to_hex(sha1));
	cgit_print_snapshot_start("text/plain", patchname, item);
	htmlf("From %s Mon Sep 17 00:00:00 2001\n", sha1_to_hex(sha1));
	htmlf("From: %s%s\n", info->author, info->author_email);
	html("Date: ");
	cgit_print_date(info->author_date, "%a, %d  %b  %Y  %H:%M:%S  %z%n");
	htmlf("Subject: %s\n\n%s", info->subject, info->msg);
	html("---\n");
	cgit_diff_tree(old_sha1, sha1, filepair_cb, NULL);
	html("--\n");
	htmlf("cgit %s\n", CGIT_VERSION);
	cgit_free_commitinfo(info);
}
