/* ui-clone.c: functions for http cloning, based on
 * git's http-backend.c by Shawn O. Pearce
 *
 * Copyright (C) 2006-2014 cgit Development Team <cgit@lists.zx2c4.com>
 *
 * Licensed under GNU General Public License v2
 *   (see COPYING for full license text)
 */

#include "cgit.h"
#include "ui-clone.h"
#include "html.h"
#include "ui-shared.h"

static int print_ref_info(const char *refname, const unsigned char *sha1,
                          int flags, void *cb_data)
{
	struct object *obj;

	if (!(obj = parse_object(sha1)))
		return 0;

	htmlf("%s\t%s\n", sha1_to_hex(sha1), refname);
	if (obj->type == OBJ_TAG) {
		if (!(obj = deref_tag(obj, refname, 0)))
			return 0;
		htmlf("%s\t%s^{}\n", sha1_to_hex(obj->sha1), refname);
	}
	return 0;
}

static void print_pack_info(void)
{
	struct packed_git *pack;
	char *offset;

	ctx.page.mimetype = "text/plain";
	ctx.page.filename = "objects/info/packs";
	cgit_print_http_headers();
	prepare_packed_git();
	for (pack = packed_git; pack; pack = pack->next) {
		if (pack->pack_local) {
			offset = strrchr(pack->pack_name, '/');
			if (offset && offset[1] != '\0')
				++offset;
			else
				offset = pack->pack_name;
			htmlf("P %s\n", offset);
		}
	}
}

static void send_file(char *path)
{
	struct stat st;

	if (stat(path, &st)) {
		switch (errno) {
		case ENOENT:
			html_status(404, "Not found", 0);
			break;
		case EACCES:
			html_status(403, "Forbidden", 0);
			break;
		default:
			html_status(400, "Bad request", 0);
		}
		return;
	}
	ctx.page.mimetype = "application/octet-stream";
	ctx.page.filename = path;
	skip_prefix(path, ctx.repo->path, &ctx.page.filename);
	skip_prefix(ctx.page.filename, "/", &ctx.page.filename);
	cgit_print_http_headers();
	html_include(path);
}

void cgit_clone_info(void)
{
	if (!ctx.qry.path || strcmp(ctx.qry.path, "refs")) {
		html_status(400, "Bad request", 0);
		return;
	}

	ctx.page.mimetype = "text/plain";
	ctx.page.filename = "info/refs";
	cgit_print_http_headers();
	for_each_ref(print_ref_info, NULL);
}

void cgit_clone_objects(void)
{
	if (!ctx.qry.path) {
		html_status(400, "Bad request", 0);
		return;
	}

	if (!strcmp(ctx.qry.path, "info/packs")) {
		print_pack_info();
		return;
	}

	send_file(git_path("objects/%s", ctx.qry.path));
}

void cgit_clone_head(void)
{
	send_file(git_path("%s", "HEAD"));
}
