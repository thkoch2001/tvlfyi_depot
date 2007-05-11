/* shared.c: global vars + some callback functions
 *
 * Copyright (C) 2006 Lars Hjemli
 *
 * Licensed under GNU General Public License v2
 *   (see COPYING for full license text)
 */

#include "cgit.h"

struct repolist cgit_repolist;
struct repoinfo *cgit_repo;

char *cgit_root_title   = "Git repository browser";
char *cgit_css          = "/cgit.css";
char *cgit_logo         = "/git-logo.png";
char *cgit_logo_link    = "http://www.kernel.org/pub/software/scm/git/docs/";
char *cgit_module_link  = "./?repo=%s&page=commit&id=%s";
char *cgit_virtual_root = NULL;

char *cgit_cache_root   = "/var/cache/cgit";

int cgit_nocache               =  0;
int cgit_snapshots             =  0;
int cgit_max_lock_attempts     =  5;
int cgit_cache_root_ttl        =  5;
int cgit_cache_repo_ttl        =  5;
int cgit_cache_dynamic_ttl     =  5;
int cgit_cache_static_ttl      = -1;
int cgit_cache_max_create_time =  5;

int cgit_max_msg_len = 60;

char *cgit_repo_name    = NULL;
char *cgit_repo_desc    = NULL;
char *cgit_repo_owner   = NULL;

int cgit_query_has_symref = 0;
int cgit_query_has_sha1   = 0;

char *cgit_querystring  = NULL;
char *cgit_query_repo   = NULL;
char *cgit_query_page   = NULL;
char *cgit_query_head   = NULL;
char *cgit_query_search = NULL;
char *cgit_query_sha1   = NULL;
char *cgit_query_sha2   = NULL;
char *cgit_query_path   = NULL;
char *cgit_query_name   = NULL;
int   cgit_query_ofs    = 0;

int htmlfd = 0;

int chk_zero(int result, char *msg)
{
	if (result != 0)
		die("%s: %s", msg, strerror(errno));
	return result;
}

int chk_positive(int result, char *msg)
{
	if (result <= 0)
		die("%s: %s", msg, strerror(errno));
	return result;
}

struct repoinfo *add_repo(const char *url)
{
	struct repoinfo *ret;

	if (++cgit_repolist.count > cgit_repolist.length) {
		if (cgit_repolist.length == 0)
			cgit_repolist.length = 8;
		else
			cgit_repolist.length *= 2;
		cgit_repolist.repos = xrealloc(cgit_repolist.repos, 
					       cgit_repolist.length * 
					       sizeof(struct repoinfo));
	}

	ret = &cgit_repolist.repos[cgit_repolist.count-1];
	ret->url = xstrdup(url);
	ret->name = ret->url;
	ret->path = NULL;
	ret->desc = NULL;
	ret->owner = NULL;
	ret->snapshots = cgit_snapshots;
	ret->module_link = cgit_module_link;
	return ret;
}

void cgit_global_config_cb(const char *name, const char *value)
{
	if (!strcmp(name, "root-title"))
		cgit_root_title = xstrdup(value);
	else if (!strcmp(name, "css"))
		cgit_css = xstrdup(value);
	else if (!strcmp(name, "logo"))
		cgit_logo = xstrdup(value);
	else if (!strcmp(name, "logo-link"))
		cgit_logo_link = xstrdup(value);
	else if (!strcmp(name, "module-link"))
		cgit_module_link = xstrdup(value);
	else if (!strcmp(name, "virtual-root"))
		cgit_virtual_root = xstrdup(value);
	else if (!strcmp(name, "nocache"))
		cgit_nocache = atoi(value);
	else if (!strcmp(name, "snapshots"))
		cgit_snapshots = atoi(value);
	else if (!strcmp(name, "cache-root"))
		cgit_cache_root = xstrdup(value);
	else if (!strcmp(name, "cache-root-ttl"))
		cgit_cache_root_ttl = atoi(value);
	else if (!strcmp(name, "cache-repo-ttl"))
		cgit_cache_repo_ttl = atoi(value);
	else if (!strcmp(name, "cache-static-ttl"))
		cgit_cache_static_ttl = atoi(value);
	else if (!strcmp(name, "cache-dynamic-ttl"))
		cgit_cache_dynamic_ttl = atoi(value);
	else if (!strcmp(name, "max-message-length"))
		cgit_max_msg_len = atoi(value);
	else if (!strcmp(name, "repo.url"))
		cgit_repo = add_repo(value);
	else if (!strcmp(name, "repo.name"))
		cgit_repo->name = xstrdup(value);
	else if (cgit_repo && !strcmp(name, "repo.path"))
		cgit_repo->path = xstrdup(value);
	else if (cgit_repo && !strcmp(name, "repo.desc"))
		cgit_repo->desc = xstrdup(value);
	else if (cgit_repo && !strcmp(name, "repo.owner"))
		cgit_repo->owner = xstrdup(value);
	else if (cgit_repo && !strcmp(name, "repo.snapshots"))
		cgit_repo->snapshots = atoi(value);
	else if (cgit_repo && !strcmp(name, "repo.module-link"))
		cgit_repo->module_link= xstrdup(value);
}

void cgit_repo_config_cb(const char *name, const char *value)
{
	if (!strcmp(name, "name"))
		cgit_repo_name = xstrdup(value);
	else if (!strcmp(name, "desc"))
		cgit_repo_desc = xstrdup(value);
	else if (!strcmp(name, "owner"))
		cgit_repo_owner = xstrdup(value);
}

void cgit_querystring_cb(const char *name, const char *value)
{
	if (!strcmp(name,"r")) {
		cgit_query_repo = xstrdup(value);
	} else if (!strcmp(name, "p")) {
		cgit_query_page = xstrdup(value);
	} else if (!strcmp(name, "q")) {
		cgit_query_search = xstrdup(value);
	} else if (!strcmp(name, "h")) {
		cgit_query_head = xstrdup(value);
		cgit_query_has_symref = 1;
	} else if (!strcmp(name, "id")) {
		cgit_query_sha1 = xstrdup(value);
		cgit_query_has_sha1 = 1;
	} else if (!strcmp(name, "id2")) {
		cgit_query_sha2 = xstrdup(value);
		cgit_query_has_sha1 = 1;
	} else if (!strcmp(name, "ofs")) {
		cgit_query_ofs = atoi(value);
	} else if (!strcmp(name, "path")) {
		cgit_query_path = xstrdup(value);
	} else if (!strcmp(name, "name")) {
		cgit_query_name = xstrdup(value);
	}
}

void *cgit_free_commitinfo(struct commitinfo *info)
{
	free(info->author);
	free(info->author_email);
	free(info->committer);
	free(info->committer_email);
	free(info->subject);
	free(info);
	return NULL;
}

int hextoint(char c)
{
	if (c >= 'a' && c <= 'f')
		return 10 + c - 'a';
	else if (c >= 'A' && c <= 'F')
		return 10 + c - 'A';
	else if (c >= '0' && c <= '9')
		return c - '0';
	else
		return -1;
}

