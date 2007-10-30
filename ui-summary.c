/* ui-summary.c: functions for generating repo summary page
 *
 * Copyright (C) 2006 Lars Hjemli
 *
 * Licensed under GNU General Public License v2
 *   (see COPYING for full license text)
 */

#include "cgit.h"

static int header;

static int cmp_age(int age1, int age2)
{
	if (age1 != 0 && age2 != 0)
		return age2 - age1;

	if (age1 == 0 && age2 == 0)
		return 0;

	if (age1 == 0)
		return +1;

	return -1;
}

static int cmp_ref_name(const void *a, const void *b)
{
	struct refinfo *r1 = *(struct refinfo **)a;
	struct refinfo *r2 = *(struct refinfo **)b;

	return strcmp(r1->refname, r2->refname);
}

static int cmp_branch_age(const void *a, const void *b)
{
	struct refinfo *r1 = *(struct refinfo **)a;
	struct refinfo *r2 = *(struct refinfo **)b;

	return cmp_age(r1->commit->committer_date, r2->commit->committer_date);
}

static int cmp_tag_age(const void *a, const void *b)
{
	struct refinfo *r1 = *(struct refinfo **)a;
	struct refinfo *r2 = *(struct refinfo **)b;

	return cmp_age(r1->tag->tagger_date, r2->tag->tagger_date);
}

static int print_branch(struct refinfo *ref)
{
	struct commitinfo *info = ref->commit;
	char *name = (char *)ref->refname;

	if (!info)
		return 1;
	html("<tr><td>");
	cgit_log_link(name, NULL, NULL, name, NULL, NULL, 0);
	html("</td><td>");

	if (ref->object->type == OBJ_COMMIT) {
		cgit_print_age(info->commit->date, -1, NULL);
		html("</td><td>");
		html_txt(info->author);
		html("</td><td>");
		cgit_commit_link(info->subject, NULL, NULL, name, NULL);
	} else {
		html("</td><td></td><td>");
		cgit_object_link(ref->object);
	}
	html("</td></tr>\n");
	return 0;
}

static void print_tag_header()
{
	html("<tr class='nohover'><th class='left'>Tag</th>"
	     "<th class='left'>Age</th>"
	     "<th class='left'>Author</th>"
	     "<th class='left'>Reference</th></tr>\n");
	header = 1;
}

static int print_tag(struct refinfo *ref)
{
	struct tag *tag;
	struct taginfo *info;
	char *url, *name = (char *)ref->refname;

	if (ref->object->type == OBJ_TAG) {
		tag = (struct tag *)ref->object;
		info = ref->tag;
		if (!tag || !info)
			return 1;
		html("<tr><td>");
		url = cgit_pageurl(cgit_query_repo, "tag",
				   fmt("id=%s", name));
		html_link_open(url, NULL, NULL);
		html_txt(name);
		html_link_close();
		html("</td><td>");
		if (info->tagger_date > 0)
			cgit_print_age(info->tagger_date, -1, NULL);
		html("</td><td>");
		if (info->tagger)
			html(info->tagger);
		html("</td><td>");
		cgit_object_link(tag->tagged);
		html("</td></tr>\n");
	} else {
		if (!header)
			print_tag_header();
		html("<tr><td>");
		html_txt(name);
		html("</td><td colspan='2'/><td>");
		cgit_object_link(ref->object);
		html("</td></tr>\n");
	}
	return 0;
}

static void print_refs_link(char *path)
{
	html("<tr class='nohover'><td colspan='4'>");
	cgit_refs_link("[...]", NULL, NULL, cgit_query_head, NULL, path);
	html("</td></tr>");
}

void cgit_print_branches(int maxcount)
{
	struct reflist list;
	int i;

	html("<tr class='nohover'><th class='left'>Branch</th>"
	     "<th class='left'>Idle</th>"
	     "<th class='left'>Author</th>"
	     "<th class='left'>Head commit</th></tr>\n");

	list.refs = NULL;
	list.alloc = list.count = 0;
	for_each_branch_ref(cgit_refs_cb, &list);

	if (maxcount == 0 || maxcount > list.count)
		maxcount = list.count;

	if (maxcount < list.count) {
		qsort(list.refs, list.count, sizeof(*list.refs), cmp_branch_age);
		qsort(list.refs, maxcount, sizeof(*list.refs), cmp_ref_name);
	}

	for(i=0; i<maxcount; i++)
		print_branch(list.refs[i]);

	if (maxcount < list.count)
		print_refs_link("heads");
}

void cgit_print_tags(int maxcount)
{
	struct reflist list;
	int i;

	header = 0;
	list.refs = NULL;
	list.alloc = list.count = 0;
	for_each_tag_ref(cgit_refs_cb, &list);
	if (list.count == 0)
		return;
	qsort(list.refs, list.count, sizeof(*list.refs), cmp_tag_age);
	if (!maxcount)
		maxcount = list.count;
	else if (maxcount > list.count)
		maxcount = list.count;
	print_tag_header();
	for(i=0; i<maxcount; i++)
		print_tag(list.refs[i]);

	if (maxcount < list.count)
		print_refs_link("tags");
}

void cgit_print_summary()
{
	if (cgit_repo->readme) {
		html("<div id='summary'>");
		html_include(cgit_repo->readme);
		html("</div>");
	}
	if (cgit_summary_log > 0)
		cgit_print_log(cgit_query_head, 0, cgit_summary_log, NULL,
			       NULL, NULL, 0);
	html("<table class='list nowrap'>");
	if (cgit_summary_log > 0)
		html("<tr class='nohover'><td colspan='4'>&nbsp;</td></tr>");
	cgit_print_branches(cgit_summary_branches);
	html("<tr class='nohover'><td colspan='4'>&nbsp;</td></tr>");
	cgit_print_tags(cgit_summary_tags);
	html("</table>");
}
