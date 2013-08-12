#ifndef CGIT_H
#define CGIT_H


#include <git-compat-util.h>
#include <cache.h>
#include <grep.h>
#include <object.h>
#include <tree.h>
#include <commit.h>
#include <tag.h>
#include <diff.h>
#include <diffcore.h>
#include <argv-array.h>
#include <refs.h>
#include <revision.h>
#include <log-tree.h>
#include <archive.h>
#include <string-list.h>
#include <xdiff-interface.h>
#include <xdiff/xdiff.h>
#include <utf8.h>
#include <notes.h>
#include <graph.h>


/*
 * Dateformats used on misc. pages
 */
#define FMT_LONGDATE "%Y-%m-%d %H:%M:%S (%Z)"
#define FMT_SHORTDATE "%Y-%m-%d"
#define FMT_ATOMDATE "%Y-%m-%dT%H:%M:%SZ"


/*
 * Limits used for relative dates
 */
#define TM_MIN    60
#define TM_HOUR  (TM_MIN * 60)
#define TM_DAY   (TM_HOUR * 24)
#define TM_WEEK  (TM_DAY * 7)
#define TM_YEAR  (TM_DAY * 365)
#define TM_MONTH (TM_YEAR / 12.0)


/*
 * Default encoding
 */
#define PAGE_ENCODING "UTF-8"

typedef void (*configfn)(const char *name, const char *value);
typedef void (*filepair_fn)(struct diff_filepair *pair);
typedef void (*linediff_fn)(char *line, int len);

typedef enum {
	ABOUT, COMMIT, SOURCE
} filter_type;

struct cgit_filter {
	char *cmd;
	char **argv;
	int old_stdout;
	int pipe_fh[2];
	int pid;
	int exitstatus;
};

struct cgit_repo {
	char *url;
	char *name;
	char *path;
	char *desc;
	char *owner;
	char *defbranch;
	char *module_link;
	struct string_list readme;
	char *section;
	char *clone_url;
	char *logo;
	char *logo_link;
	int snapshots;
	int enable_commit_graph;
	int enable_log_filecount;
	int enable_log_linecount;
	int enable_remote_branches;
	int enable_subject_links;
	int max_stats;
	int branch_sort;
	int commit_sort;
	time_t mtime;
	struct cgit_filter *about_filter;
	struct cgit_filter *commit_filter;
	struct cgit_filter *source_filter;
	struct string_list submodules;
};

typedef void (*repo_config_fn)(struct cgit_repo *repo, const char *name,
	      const char *value);

struct cgit_repolist {
	int length;
	int count;
	struct cgit_repo *repos;
};

struct commitinfo {
	struct commit *commit;
	char *author;
	char *author_email;
	unsigned long author_date;
	char *committer;
	char *committer_email;
	unsigned long committer_date;
	char *subject;
	char *msg;
	char *msg_encoding;
};

struct taginfo {
	char *tagger;
	char *tagger_email;
	unsigned long tagger_date;
	char *msg;
};

struct refinfo {
	const char *refname;
	struct object *object;
	union {
		struct taginfo *tag;
		struct commitinfo *commit;
	};
};

struct reflist {
	struct refinfo **refs;
	int alloc;
	int count;
};

struct cgit_query {
	int has_symref;
	int has_sha1;
	int has_ssdiff;
	char *raw;
	char *repo;
	char *page;
	char *search;
	char *grep;
	char *head;
	char *sha1;
	char *sha2;
	char *path;
	char *name;
	char *mimetype;
	char *url;
	char *period;
	int   ofs;
	int nohead;
	char *sort;
	int showmsg;
	int ssdiff;
	int show_all;
	int context;
	int ignorews;
	char *vpath;
};

struct cgit_config {
	char *agefile;
	char *cache_root;
	char *clone_prefix;
	char *clone_url;
	char *css;
	char *favicon;
	char *footer;
	char *head_include;
	char *header;
	char *index_header;
	char *index_info;
	char *logo;
	char *logo_link;
	char *mimetype_file;
	char *module_link;
	char *project_list;
	struct string_list readme;
	char *robots;
	char *root_title;
	char *root_desc;
	char *root_readme;
	char *script_name;
	char *section;
	char *repository_sort;
	char *virtual_root;	/* Always ends with '/'. */
	char *strict_export;
	int cache_size;
	int cache_dynamic_ttl;
	int cache_max_create_time;
	int cache_repo_ttl;
	int cache_root_ttl;
	int cache_scanrc_ttl;
	int cache_static_ttl;
	int cache_about_ttl;
	int case_sensitive_sort;
	int embedded;
	int enable_filter_overrides;
	int enable_http_clone;
	int enable_index_links;
	int enable_index_owner;
	int enable_commit_graph;
	int enable_log_filecount;
	int enable_log_linecount;
	int enable_remote_branches;
	int enable_subject_links;
	int enable_tree_linenumbers;
	int enable_git_config;
	int local_time;
	int max_atom_items;
	int max_repo_count;
	int max_commit_count;
	int max_lock_attempts;
	int max_msg_len;
	int max_repodesc_len;
	int max_blob_size;
	int max_stats;
	int nocache;
	int noplainemail;
	int noheader;
	int renamelimit;
	int remove_suffix;
	int scan_hidden_path;
	int section_from_path;
	int snapshots;
	int section_sort;
	int summary_branches;
	int summary_log;
	int summary_tags;
	int ssdiff;
	int branch_sort;
	int commit_sort;
	struct string_list mimetypes;
	struct cgit_filter *about_filter;
	struct cgit_filter *commit_filter;
	struct cgit_filter *source_filter;
};

struct cgit_page {
	time_t modified;
	time_t expires;
	size_t size;
	const char *mimetype;
	const char *charset;
	const char *filename;
	const char *etag;
	const char *title;
	int status;
	const char *statusmsg;
};

struct cgit_environment {
	const char *cgit_config;
	const char *http_host;
	const char *https;
	const char *no_http;
	const char *path_info;
	const char *query_string;
	const char *request_method;
	const char *script_name;
	const char *server_name;
	const char *server_port;
};

struct cgit_context {
	struct cgit_environment env;
	struct cgit_query qry;
	struct cgit_config cfg;
	struct cgit_repo *repo;
	struct cgit_page page;
};

typedef int (*write_archive_fn_t)(const char *, const char *);

struct cgit_snapshot_format {
	const char *suffix;
	const char *mimetype;
	write_archive_fn_t write_func;
	int bit;
};

extern const char *cgit_version;

extern struct cgit_repolist cgit_repolist;
extern struct cgit_context ctx;
extern const struct cgit_snapshot_format cgit_snapshot_formats[];

extern char *cgit_default_repo_desc;
extern struct cgit_repo *cgit_add_repo(const char *url);
extern struct cgit_repo *cgit_get_repoinfo(const char *url);
extern void cgit_repo_config_cb(const char *name, const char *value);

extern int chk_zero(int result, char *msg);
extern int chk_positive(int result, char *msg);
extern int chk_non_negative(int result, char *msg);

extern char *trim_end(const char *str, char c);
extern char *ensure_end(const char *str, char c);
extern char *strlpart(char *txt, int maxlen);
extern char *strrpart(char *txt, int maxlen);

extern void strbuf_ensure_end(struct strbuf *sb, char c);

extern void cgit_add_ref(struct reflist *list, struct refinfo *ref);
extern void cgit_free_reflist_inner(struct reflist *list);
extern int cgit_refs_cb(const char *refname, const unsigned char *sha1,
			int flags, void *cb_data);

extern void *cgit_free_commitinfo(struct commitinfo *info);

extern int cgit_diff_files(const unsigned char *old_sha1,
			   const unsigned char *new_sha1,
			   unsigned long *old_size, unsigned long *new_size,
			   int *binary, int context, int ignorews,
			   linediff_fn fn);

extern void cgit_diff_tree(const unsigned char *old_sha1,
			   const unsigned char *new_sha1,
			   filepair_fn fn, const char *prefix, int ignorews);

extern void cgit_diff_commit(struct commit *commit, filepair_fn fn,
			     const char *prefix);

__attribute__((format (printf,1,2)))
extern char *fmt(const char *format,...);

__attribute__((format (printf,1,2)))
extern char *fmtalloc(const char *format,...);

extern struct commitinfo *cgit_parse_commit(struct commit *commit);
extern struct taginfo *cgit_parse_tag(struct tag *tag);
extern void cgit_parse_url(const char *url);

extern const char *cgit_repobasename(const char *reponame);

extern int cgit_parse_snapshots_mask(const char *str);

extern int cgit_open_filter(struct cgit_filter *filter);
extern int cgit_close_filter(struct cgit_filter *filter);

extern void cgit_prepare_repo_env(struct cgit_repo * repo);

extern int readfile(const char *path, char **buf, size_t *size);

extern char *expand_macros(const char *txt);

#endif /* CGIT_H */
