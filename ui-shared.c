/* ui-shared.c: common web output functions
 *
 * Copyright (C) 2006 Lars Hjemli
 *
 * Licensed under GNU General Public License v2
 *   (see COPYING for full license text)
 */

#include "cgit.h"
#include "cmd.h"
#include "html.h"

const char cgit_doctype[] =
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
"  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n";

static char *http_date(time_t t)
{
	static char day[][4] =
		{"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
	static char month[][4] =
		{"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
	struct tm *tm = gmtime(&t);
	return fmt("%s, %02d %s %04d %02d:%02d:%02d GMT", day[tm->tm_wday],
		   tm->tm_mday, month[tm->tm_mon], 1900+tm->tm_year,
		   tm->tm_hour, tm->tm_min, tm->tm_sec);
}

void cgit_print_error(const char *msg)
{
	html("<div class='error'>");
	html_txt(msg);
	html("</div>\n");
}

char *cgit_httpscheme()
{
	if (ctx.env.https && !strcmp(ctx.env.https, "on"))
		return "https://";
	else
		return "http://";
}

char *cgit_hosturl()
{
	if (ctx.env.http_host)
		return ctx.env.http_host;
	if (!ctx.env.server_name)
		return NULL;
	if (!ctx.env.server_port || atoi(ctx.env.server_port) == 80)
		return ctx.env.server_name;
	return xstrdup(fmt("%s:%s", ctx.env.server_name, ctx.env.server_port));
}

char *cgit_rooturl()
{
	if (ctx.cfg.virtual_root)
		return fmt("%s/", ctx.cfg.virtual_root);
	else
		return ctx.cfg.script_name;
}

char *cgit_repourl(const char *reponame)
{
	if (ctx.cfg.virtual_root) {
		return fmt("%s/%s/", ctx.cfg.virtual_root, reponame);
	} else {
		return fmt("?r=%s", reponame);
	}
}

char *cgit_fileurl(const char *reponame, const char *pagename,
		   const char *filename, const char *query)
{
	char *tmp;
	char *delim;

	if (ctx.cfg.virtual_root) {
		tmp = fmt("%s/%s/%s/%s", ctx.cfg.virtual_root, reponame,
			  pagename, (filename ? filename:""));
		delim = "?";
	} else {
		tmp = fmt("?url=%s/%s/%s", reponame, pagename,
			  (filename ? filename : ""));
		delim = "&";
	}
	if (query)
		tmp = fmt("%s%s%s", tmp, delim, query);
	return tmp;
}

char *cgit_pageurl(const char *reponame, const char *pagename,
		   const char *query)
{
	return cgit_fileurl(reponame,pagename,0,query);
}

const char *cgit_repobasename(const char *reponame)
{
	/* I assume we don't need to store more than one repo basename */
	static char rvbuf[1024];
	int p;
	const char *rv;
	strncpy(rvbuf,reponame,sizeof(rvbuf));
	if(rvbuf[sizeof(rvbuf)-1])
		die("cgit_repobasename: truncated repository name '%s'", reponame);
	p = strlen(rvbuf)-1;
	/* strip trailing slashes */
	while(p && rvbuf[p]=='/') rvbuf[p--]=0;
	/* strip trailing .git */
	if(p>=3 && !strncmp(&rvbuf[p-3],".git",4)) {
		p -= 3; rvbuf[p--] = 0;
	}
	/* strip more trailing slashes if any */
	while( p && rvbuf[p]=='/') rvbuf[p--]=0;
	/* find last slash in the remaining string */
	rv = strrchr(rvbuf,'/');
	if(rv)
		return ++rv;
	return rvbuf;
}

char *cgit_currurl()
{
	if (!ctx.cfg.virtual_root)
		return ctx.cfg.script_name;
	else if (ctx.qry.page)
		return fmt("%s/%s/%s/", ctx.cfg.virtual_root, ctx.qry.repo, ctx.qry.page);
	else if (ctx.qry.repo)
		return fmt("%s/%s/", ctx.cfg.virtual_root, ctx.qry.repo);
	else
		return fmt("%s/", ctx.cfg.virtual_root);
}

static void site_url(const char *page, const char *search, int ofs)
{
	char *delim = "?";

	if (ctx.cfg.virtual_root) {
		html_attr(ctx.cfg.virtual_root);
		if (ctx.cfg.virtual_root[strlen(ctx.cfg.virtual_root) - 1] != '/')
			html("/");
	} else
		html(ctx.cfg.script_name);

	if (page) {
		htmlf("?p=%s", page);
		delim = "&";
	}
	if (search) {
		html(delim);
		html("q=");
		html_attr(search);
		delim = "&";
	}
	if (ofs) {
		html(delim);
		htmlf("ofs=%d", ofs);
	}
}

static void site_link(const char *page, const char *name, const char *title,
		      const char *class, const char *search, int ofs)
{
	html("<a");
	if (title) {
		html(" title='");
		html_attr(title);
		html("'");
	}
	if (class) {
		html(" class='");
		html_attr(class);
		html("'");
	}
	html(" href='");
	site_url(page, search, ofs);
	html("'>");
	html_txt(name);
	html("</a>");
}

void cgit_index_link(const char *name, const char *title, const char *class,
		     const char *pattern, int ofs)
{
	site_link(NULL, name, title, class, pattern, ofs);
}

static char *repolink(const char *title, const char *class, const char *page,
		      const char *head, const char *path)
{
	char *delim = "?";

	html("<a");
	if (title) {
		html(" title='");
		html_attr(title);
		html("'");
	}
	if (class) {
		html(" class='");
		html_attr(class);
		html("'");
	}
	html(" href='");
	if (ctx.cfg.virtual_root) {
		html_url_path(ctx.cfg.virtual_root);
		if (ctx.cfg.virtual_root[strlen(ctx.cfg.virtual_root) - 1] != '/')
			html("/");
		html_url_path(ctx.repo->url);
		if (ctx.repo->url[strlen(ctx.repo->url) - 1] != '/')
			html("/");
		if (page) {
			html_url_path(page);
			html("/");
			if (path)
				html_url_path(path);
		}
	} else {
		html(ctx.cfg.script_name);
		html("?url=");
		html_url_arg(ctx.repo->url);
		if (ctx.repo->url[strlen(ctx.repo->url) - 1] != '/')
			html("/");
		if (page) {
			html_url_arg(page);
			html("/");
			if (path)
				html_url_arg(path);
		}
		delim = "&amp;";
	}
	if (head && strcmp(head, ctx.repo->defbranch)) {
		html(delim);
		html("h=");
		html_url_arg(head);
		delim = "&amp;";
	}
	return fmt("%s", delim);
}

static void reporevlink(const char *page, const char *name, const char *title,
			const char *class, const char *head, const char *rev,
			const char *path)
{
	char *delim;

	delim = repolink(title, class, page, head, path);
	if (rev && ctx.qry.head != NULL && strcmp(rev, ctx.qry.head)) {
		html(delim);
		html("id=");
		html_url_arg(rev);
	}
	html("'>");
	html_txt(name);
	html("</a>");
}

void cgit_summary_link(const char *name, const char *title, const char *class,
		       const char *head)
{
	reporevlink(NULL, name, title, class, head, NULL, NULL);
}

void cgit_tag_link(const char *name, const char *title, const char *class,
		   const char *head, const char *rev)
{
	reporevlink("tag", name, title, class, head, rev, NULL);
}

void cgit_tree_link(const char *name, const char *title, const char *class,
		    const char *head, const char *rev, const char *path)
{
	reporevlink("tree", name, title, class, head, rev, path);
}

void cgit_plain_link(const char *name, const char *title, const char *class,
		     const char *head, const char *rev, const char *path)
{
	reporevlink("plain", name, title, class, head, rev, path);
}

void cgit_log_link(const char *name, const char *title, const char *class,
		   const char *head, const char *rev, const char *path,
		   int ofs, const char *grep, const char *pattern, int showmsg)
{
	char *delim;

	delim = repolink(title, class, "log", head, path);
	if (rev && strcmp(rev, ctx.qry.head)) {
		html(delim);
		html("id=");
		html_url_arg(rev);
		delim = "&";
	}
	if (grep && pattern) {
		html(delim);
		html("qt=");
		html_url_arg(grep);
		delim = "&";
		html(delim);
		html("q=");
		html_url_arg(pattern);
	}
	if (ofs > 0) {
		html(delim);
		html("ofs=");
		htmlf("%d", ofs);
		delim = "&";
	}
	if (showmsg) {
		html(delim);
		html("showmsg=1");
	}
	html("'>");
	html_txt(name);
	html("</a>");
}

void cgit_commit_link(char *name, const char *title, const char *class,
		      const char *head, const char *rev, const char *path,
		      int toggle_ssdiff)
{
	if (strlen(name) > ctx.cfg.max_msg_len && ctx.cfg.max_msg_len >= 15) {
		name[ctx.cfg.max_msg_len] = '\0';
		name[ctx.cfg.max_msg_len - 1] = '.';
		name[ctx.cfg.max_msg_len - 2] = '.';
		name[ctx.cfg.max_msg_len - 3] = '.';
	}

	char *delim;

	delim = repolink(title, class, "commit", head, path);
	if (rev && strcmp(rev, ctx.qry.head)) {
		html(delim);
		html("id=");
		html_url_arg(rev);
		delim = "&amp;";
	}
	if ((ctx.qry.ssdiff && !toggle_ssdiff) || (!ctx.qry.ssdiff && toggle_ssdiff)) {
		html(delim);
		html("ss=1");
		delim = "&amp;";
	}
	if (ctx.qry.context > 0 && ctx.qry.context != 3) {
		html(delim);
		html("context=");
		htmlf("%d", ctx.qry.context);
		delim = "&amp;";
	}
	if (ctx.qry.ignorews) {
		html(delim);
		html("ignorews=1");
		delim = "&amp;";
	}
	html("'>");
	html_txt(name);
	html("</a>");
}

void cgit_refs_link(const char *name, const char *title, const char *class,
		    const char *head, const char *rev, const char *path)
{
	reporevlink("refs", name, title, class, head, rev, path);
}

void cgit_snapshot_link(const char *name, const char *title, const char *class,
			const char *head, const char *rev,
			const char *archivename)
{
	reporevlink("snapshot", name, title, class, head, rev, archivename);
}

void cgit_diff_link(const char *name, const char *title, const char *class,
		    const char *head, const char *new_rev, const char *old_rev,
		    const char *path, int toggle_ssdiff)
{
	char *delim;

	delim = repolink(title, class, "diff", head, path);
	if (new_rev && ctx.qry.head != NULL && strcmp(new_rev, ctx.qry.head)) {
		html(delim);
		html("id=");
		html_url_arg(new_rev);
		delim = "&amp;";
	}
	if (old_rev) {
		html(delim);
		html("id2=");
		html_url_arg(old_rev);
		delim = "&amp;";
	}
	if ((ctx.qry.ssdiff && !toggle_ssdiff) || (!ctx.qry.ssdiff && toggle_ssdiff)) {
		html(delim);
		html("ss=1");
		delim = "&amp;";
	}
	if (ctx.qry.context > 0 && ctx.qry.context != 3) {
		html(delim);
		html("context=");
		htmlf("%d", ctx.qry.context);
		delim = "&amp;";
	}
	if (ctx.qry.ignorews) {
		html(delim);
		html("ignorews=1");
		delim = "&amp;";
	}
	html("'>");
	html_txt(name);
	html("</a>");
}

void cgit_patch_link(const char *name, const char *title, const char *class,
		     const char *head, const char *rev, const char *path)
{
	reporevlink("patch", name, title, class, head, rev, path);
}

void cgit_stats_link(const char *name, const char *title, const char *class,
		     const char *head, const char *path)
{
	reporevlink("stats", name, title, class, head, NULL, path);
}

void cgit_self_link(char *name, const char *title, const char *class,
		    struct cgit_context *ctx)
{
	if (!strcmp(ctx->qry.page, "repolist"))
		return cgit_index_link(name, title, class, ctx->qry.search,
				       ctx->qry.ofs);
	else if (!strcmp(ctx->qry.page, "summary"))
		return cgit_summary_link(name, title, class, ctx->qry.head);
	else if (!strcmp(ctx->qry.page, "tag"))
		return cgit_tag_link(name, title, class, ctx->qry.head,
				     ctx->qry.has_sha1 ? ctx->qry.sha1 : NULL);
	else if (!strcmp(ctx->qry.page, "tree"))
		return cgit_tree_link(name, title, class, ctx->qry.head,
				      ctx->qry.has_sha1 ? ctx->qry.sha1 : NULL,
				      ctx->qry.path);
	else if (!strcmp(ctx->qry.page, "plain"))
		return cgit_plain_link(name, title, class, ctx->qry.head,
				      ctx->qry.has_sha1 ? ctx->qry.sha1 : NULL,
				      ctx->qry.path);
	else if (!strcmp(ctx->qry.page, "log"))
		return cgit_log_link(name, title, class, ctx->qry.head,
				      ctx->qry.has_sha1 ? ctx->qry.sha1 : NULL,
				      ctx->qry.path, ctx->qry.ofs,
				      ctx->qry.grep, ctx->qry.search,
				      ctx->qry.showmsg);
	else if (!strcmp(ctx->qry.page, "commit"))
		return cgit_commit_link(name, title, class, ctx->qry.head,
				      ctx->qry.has_sha1 ? ctx->qry.sha1 : NULL,
				      ctx->qry.path, 0);
	else if (!strcmp(ctx->qry.page, "patch"))
		return cgit_patch_link(name, title, class, ctx->qry.head,
				      ctx->qry.has_sha1 ? ctx->qry.sha1 : NULL,
				      ctx->qry.path);
	else if (!strcmp(ctx->qry.page, "refs"))
		return cgit_refs_link(name, title, class, ctx->qry.head,
				      ctx->qry.has_sha1 ? ctx->qry.sha1 : NULL,
				      ctx->qry.path);
	else if (!strcmp(ctx->qry.page, "snapshot"))
		return cgit_snapshot_link(name, title, class, ctx->qry.head,
				      ctx->qry.has_sha1 ? ctx->qry.sha1 : NULL,
				      ctx->qry.path);
	else if (!strcmp(ctx->qry.page, "diff"))
		return cgit_diff_link(name, title, class, ctx->qry.head,
				      ctx->qry.sha1, ctx->qry.sha2,
				      ctx->qry.path, 0);
	else if (!strcmp(ctx->qry.page, "stats"))
		return cgit_stats_link(name, title, class, ctx->qry.head,
				      ctx->qry.path);

	/* Don't known how to make link for this page */
	repolink(title, class, ctx->qry.page, ctx->qry.head, ctx->qry.path);
	html("><!-- cgit_self_link() doesn't know how to make link for page '");
	html_txt(ctx->qry.page);
	html("' -->");
	html_txt(name);
	html("</a>");
}

void cgit_object_link(struct object *obj)
{
	char *page, *shortrev, *fullrev, *name;

	fullrev = sha1_to_hex(obj->sha1);
	shortrev = xstrdup(fullrev);
	shortrev[10] = '\0';
	if (obj->type == OBJ_COMMIT) {
                cgit_commit_link(fmt("commit %s...", shortrev), NULL, NULL,
				 ctx.qry.head, fullrev, NULL, 0);
		return;
	} else if (obj->type == OBJ_TREE)
		page = "tree";
	else if (obj->type == OBJ_TAG)
		page = "tag";
	else
		page = "blob";
	name = fmt("%s %s...", typename(obj->type), shortrev);
	reporevlink(page, name, NULL, NULL, ctx.qry.head, fullrev, NULL);
}

void cgit_print_date(time_t secs, const char *format, int local_time)
{
	char buf[64];
	struct tm *time;

	if (!secs)
		return;
	if(local_time)
		time = localtime(&secs);
	else
		time = gmtime(&secs);
	strftime(buf, sizeof(buf)-1, format, time);
	html_txt(buf);
}

void cgit_print_age(time_t t, time_t max_relative, const char *format)
{
	time_t now, secs;

	if (!t)
		return;
	time(&now);
	secs = now - t;

	if (secs > max_relative && max_relative >= 0) {
		cgit_print_date(t, format, ctx.cfg.local_time);
		return;
	}

	if (secs < TM_HOUR * 2) {
		htmlf("<span class='age-mins'>%.0f min.</span>",
		      secs * 1.0 / TM_MIN);
		return;
	}
	if (secs < TM_DAY * 2) {
		htmlf("<span class='age-hours'>%.0f hours</span>",
		      secs * 1.0 / TM_HOUR);
		return;
	}
	if (secs < TM_WEEK * 2) {
		htmlf("<span class='age-days'>%.0f days</span>",
		      secs * 1.0 / TM_DAY);
		return;
	}
	if (secs < TM_MONTH * 2) {
		htmlf("<span class='age-weeks'>%.0f weeks</span>",
		      secs * 1.0 / TM_WEEK);
		return;
	}
	if (secs < TM_YEAR * 2) {
		htmlf("<span class='age-months'>%.0f months</span>",
		      secs * 1.0 / TM_MONTH);
		return;
	}
	htmlf("<span class='age-years'>%.0f years</span>",
	      secs * 1.0 / TM_YEAR);
}

void cgit_print_http_headers(struct cgit_context *ctx)
{
	if (ctx->env.no_http && !strcmp(ctx->env.no_http, "1"))
		return;

	if (ctx->page.status)
		htmlf("Status: %d %s\n", ctx->page.status, ctx->page.statusmsg);
	if (ctx->page.mimetype && ctx->page.charset)
		htmlf("Content-Type: %s; charset=%s\n", ctx->page.mimetype,
		      ctx->page.charset);
	else if (ctx->page.mimetype)
		htmlf("Content-Type: %s\n", ctx->page.mimetype);
	if (ctx->page.size)
		htmlf("Content-Length: %ld\n", ctx->page.size);
	if (ctx->page.filename)
		htmlf("Content-Disposition: inline; filename=\"%s\"\n",
		      ctx->page.filename);
	htmlf("Last-Modified: %s\n", http_date(ctx->page.modified));
	htmlf("Expires: %s\n", http_date(ctx->page.expires));
	if (ctx->page.etag)
		htmlf("ETag: \"%s\"\n", ctx->page.etag);
	html("\n");
	if (ctx->env.request_method && !strcmp(ctx->env.request_method, "HEAD"))
		exit(0);
}

void cgit_print_docstart(struct cgit_context *ctx)
{
	if (ctx->cfg.embedded) {
		if (ctx->cfg.header)
			html_include(ctx->cfg.header);
		return;
	}

	char *host = cgit_hosturl();
	html(cgit_doctype);
	html("<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>\n");
	html("<head>\n");
	html("<title>");
	html_txt(ctx->page.title);
	html("</title>\n");
	htmlf("<meta name='generator' content='cgit %s'/>\n", cgit_version);
	if (ctx->cfg.robots && *ctx->cfg.robots)
		htmlf("<meta name='robots' content='%s'/>\n", ctx->cfg.robots);
	html("<link rel='stylesheet' type='text/css' href='");
	html_attr(ctx->cfg.css);
	html("'/>\n");
	if (ctx->cfg.favicon) {
		html("<link rel='shortcut icon' href='");
		html_attr(ctx->cfg.favicon);
		html("'/>\n");
	}
	if (host && ctx->repo) {
		html("<link rel='alternate' title='Atom feed' href='");
		html(cgit_httpscheme());
		html_attr(cgit_hosturl());
		html_attr(cgit_fileurl(ctx->repo->url, "atom", ctx->qry.vpath,
				       fmt("h=%s", ctx->qry.head)));
		html("' type='application/atom+xml'/>\n");
	}
	if (ctx->cfg.head_include)
		html_include(ctx->cfg.head_include);
	html("</head>\n");
	html("<body>\n");
	if (ctx->cfg.header)
		html_include(ctx->cfg.header);
}

void cgit_print_docend()
{
	html("</div> <!-- class=content -->\n");
	if (ctx.cfg.embedded) {
		html("</div> <!-- id=cgit -->\n");
		if (ctx.cfg.footer)
			html_include(ctx.cfg.footer);
		return;
	}
	if (ctx.cfg.footer)
		html_include(ctx.cfg.footer);
	else {
		htmlf("<div class='footer'>generated  by cgit %s at ",
			cgit_version);
		cgit_print_date(time(NULL), FMT_LONGDATE, ctx.cfg.local_time);
		html("</div>\n");
	}
	html("</div> <!-- id=cgit -->\n");
	html("</body>\n</html>\n");
}

int print_branch_option(const char *refname, const unsigned char *sha1,
			int flags, void *cb_data)
{
	char *name = (char *)refname;
	html_option(name, name, ctx.qry.head);
	return 0;
}

int print_archive_ref(const char *refname, const unsigned char *sha1,
		       int flags, void *cb_data)
{
	struct tag *tag;
	struct taginfo *info;
	struct object *obj;
	char buf[256], *url;
	unsigned char fileid[20];
	int *header = (int *)cb_data;

	if (prefixcmp(refname, "refs/archives"))
		return 0;
	strncpy(buf, refname+14, sizeof(buf));
	obj = parse_object(sha1);
	if (!obj)
		return 1;
	if (obj->type == OBJ_TAG) {
		tag = lookup_tag(sha1);
		if (!tag || parse_tag(tag) || !(info = cgit_parse_tag(tag)))
			return 0;
		hashcpy(fileid, tag->tagged->sha1);
	} else if (obj->type != OBJ_BLOB) {
		return 0;
	} else {
		hashcpy(fileid, sha1);
	}
	if (!*header) {
		html("<h1>download</h1>\n");
		*header = 1;
	}
	url = cgit_pageurl(ctx.qry.repo, "blob",
			   fmt("id=%s&amp;path=%s", sha1_to_hex(fileid),
			       buf));
	html_link_open(url, NULL, "menu");
	html_txt(strlpart(buf, 20));
	html_link_close();
	return 0;
}

void cgit_add_hidden_formfields(int incl_head, int incl_search,
				const char *page)
{
	char *url;

	if (!ctx.cfg.virtual_root) {
		url = fmt("%s/%s", ctx.qry.repo, page);
		if (ctx.qry.vpath)
			url = fmt("%s/%s", url, ctx.qry.vpath);
		html_hidden("url", url);
	}

	if (incl_head && ctx.qry.head && ctx.repo->defbranch &&
	    strcmp(ctx.qry.head, ctx.repo->defbranch))
		html_hidden("h", ctx.qry.head);

	if (ctx.qry.sha1)
		html_hidden("id", ctx.qry.sha1);
	if (ctx.qry.sha2)
		html_hidden("id2", ctx.qry.sha2);
	if (ctx.qry.showmsg)
		html_hidden("showmsg", "1");

	if (incl_search) {
		if (ctx.qry.grep)
			html_hidden("qt", ctx.qry.grep);
		if (ctx.qry.search)
			html_hidden("q", ctx.qry.search);
	}
}

static const char *hc(struct cgit_context *ctx, const char *page)
{
	return strcmp(ctx->qry.page, page) ? NULL : "active";
}

static void cgit_print_path_crumbs(struct cgit_context *ctx, char *path)
{
	char *old_path = ctx->qry.path;
	char *p = path, *q, *end = path + strlen(path);

	ctx->qry.path = NULL;
	cgit_self_link("root", NULL, NULL, ctx);
	ctx->qry.path = p = path;
	while (p < end) {
		if (!(q = strchr(p, '/')))
			q = end;
		*q = '\0';
		html_txt("/");
		cgit_self_link(p, NULL, NULL, ctx);
		if (q < end)
			*q = '/';
		p = q + 1;
	}
	ctx->qry.path = old_path;
}

static void print_header(struct cgit_context *ctx)
{
	html("<table id='header'>\n");
	html("<tr>\n");

	if (ctx->cfg.logo && ctx->cfg.logo[0] != 0) {
		html("<td class='logo' rowspan='2'><a href='");
		if (ctx->cfg.logo_link)
			html_attr(ctx->cfg.logo_link);
		else
			html_attr(cgit_rooturl());
		html("'><img src='");
		html_attr(ctx->cfg.logo);
		html("' alt='cgit logo'/></a></td>\n");
	}

	html("<td class='main'>");
	if (ctx->repo) {
		cgit_index_link("index", NULL, NULL, NULL, 0);
		html(" : ");
		cgit_summary_link(ctx->repo->name, ctx->repo->name, NULL, NULL);
		html("</td><td class='form'>");
		html("<form method='get' action=''>\n");
		cgit_add_hidden_formfields(0, 1, ctx->qry.page);
		html("<select name='h' onchange='this.form.submit();'>\n");
		for_each_branch_ref(print_branch_option, ctx->qry.head);
		html("</select> ");
		html("<input type='submit' name='' value='switch'/>");
		html("</form>");
	} else
		html_txt(ctx->cfg.root_title);
	html("</td></tr>\n");

	html("<tr><td class='sub'>");
	if (ctx->repo) {
		html_txt(ctx->repo->desc);
		html("</td><td class='sub right'>");
		html_txt(ctx->repo->owner);
	} else {
		if (ctx->cfg.root_desc)
			html_txt(ctx->cfg.root_desc);
		else if (ctx->cfg.index_info)
			html_include(ctx->cfg.index_info);
	}
	html("</td></tr></table>\n");
}

void cgit_print_pageheader(struct cgit_context *ctx)
{
	html("<div id='cgit'>");
	if (!ctx->cfg.noheader)
		print_header(ctx);

	html("<table class='tabs'><tr><td>\n");
	if (ctx->repo) {
		cgit_summary_link("summary", NULL, hc(ctx, "summary"),
				  ctx->qry.head);
		cgit_refs_link("refs", NULL, hc(ctx, "refs"), ctx->qry.head,
			       ctx->qry.sha1, NULL);
		cgit_log_link("log", NULL, hc(ctx, "log"), ctx->qry.head,
			      NULL, ctx->qry.vpath, 0, NULL, NULL,
			      ctx->qry.showmsg);
		cgit_tree_link("tree", NULL, hc(ctx, "tree"), ctx->qry.head,
			       ctx->qry.sha1, ctx->qry.vpath);
		cgit_commit_link("commit", NULL, hc(ctx, "commit"),
				 ctx->qry.head, ctx->qry.sha1, ctx->qry.vpath, 0);
		cgit_diff_link("diff", NULL, hc(ctx, "diff"), ctx->qry.head,
			       ctx->qry.sha1, ctx->qry.sha2, ctx->qry.vpath, 0);
		if (ctx->repo->max_stats)
			cgit_stats_link("stats", NULL, hc(ctx, "stats"),
					ctx->qry.head, ctx->qry.vpath);
		if (ctx->repo->readme)
			reporevlink("about", "about", NULL,
				    hc(ctx, "about"), ctx->qry.head, NULL,
				    NULL);
		html("</td><td class='form'>");
		html("<form class='right' method='get' action='");
		if (ctx->cfg.virtual_root)
			html_url_path(cgit_fileurl(ctx->qry.repo, "log",
						   ctx->qry.vpath, NULL));
		html("'>\n");
		cgit_add_hidden_formfields(1, 0, "log");
		html("<select name='qt'>\n");
		html_option("grep", "log msg", ctx->qry.grep);
		html_option("author", "author", ctx->qry.grep);
		html_option("committer", "committer", ctx->qry.grep);
		html_option("range", "range", ctx->qry.grep);
		html("</select>\n");
		html("<input class='txt' type='text' size='10' name='q' value='");
		html_attr(ctx->qry.search);
		html("'/>\n");
		html("<input type='submit' value='search'/>\n");
		html("</form>\n");
	} else {
		site_link(NULL, "index", NULL, hc(ctx, "repolist"), NULL, 0);
		if (ctx->cfg.root_readme)
			site_link("about", "about", NULL, hc(ctx, "about"),
				  NULL, 0);
		html("</td><td class='form'>");
		html("<form method='get' action='");
		html_attr(cgit_rooturl());
		html("'>\n");
		html("<input type='text' name='q' size='10' value='");
		html_attr(ctx->qry.search);
		html("'/>\n");
		html("<input type='submit' value='search'/>\n");
		html("</form>");
	}
	html("</td></tr></table>\n");
	if (ctx->qry.vpath) {
		html("<div class='path'>");
		html("path: ");
		cgit_print_path_crumbs(ctx, ctx->qry.vpath);
		html("</div>");
	}
	html("<div class='content'>");
}

void cgit_print_filemode(unsigned short mode)
{
	if (S_ISDIR(mode))
		html("d");
	else if (S_ISLNK(mode))
		html("l");
	else if (S_ISGITLINK(mode))
		html("m");
	else
		html("-");
	html_fileperm(mode >> 6);
	html_fileperm(mode >> 3);
	html_fileperm(mode);
}

void cgit_print_snapshot_links(const char *repo, const char *head,
			       const char *hex, int snapshots)
{
	const struct cgit_snapshot_format* f;
	char *prefix;
    	char *filename;
	unsigned char sha1[20];

	if (get_sha1(fmt("refs/tags/%s", hex), sha1) == 0 &&
	    (hex[0] == 'v' || hex[0] == 'V') && isdigit(hex[1]))
		hex++;
	prefix = xstrdup(fmt("%s-%s", cgit_repobasename(repo), hex));
	for (f = cgit_snapshot_formats; f->suffix; f++) {
		if (!(snapshots & f->bit))
			continue;
		filename = fmt("%s%s", prefix, f->suffix);
		cgit_snapshot_link(filename, NULL, NULL, NULL, NULL, filename);
		html("<br/>");
	}
}
