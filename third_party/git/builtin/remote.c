#include "builtin.h"
#include "config.h"
#include "parse-options.h"
#include "transport.h"
#include "remote.h"
#include "string-list.h"
#include "strbuf.h"
#include "run-command.h"
#include "rebase.h"
#include "refs.h"
#include "refspec.h"
#include "object-store.h"
#include "strvec.h"
#include "commit-reach.h"

static const char * const builtin_remote_usage[] = {
	N_("git remote [-v | --verbose]"),
	N_("git remote add [-t <branch>] [-m <master>] [-f] [--tags | --no-tags] [--mirror=<fetch|push>] <name> <url>"),
	N_("git remote rename <old> <new>"),
	N_("git remote remove <name>"),
	N_("git remote set-head <name> (-a | --auto | -d | --delete | <branch>)"),
	N_("git remote [-v | --verbose] show [-n] <name>"),
	N_("git remote prune [-n | --dry-run] <name>"),
	N_("git remote [-v | --verbose] update [-p | --prune] [(<group> | <remote>)...]"),
	N_("git remote set-branches [--add] <name> <branch>..."),
	N_("git remote get-url [--push] [--all] <name>"),
	N_("git remote set-url [--push] <name> <newurl> [<oldurl>]"),
	N_("git remote set-url --add <name> <newurl>"),
	N_("git remote set-url --delete <name> <url>"),
	NULL
};

static const char * const builtin_remote_add_usage[] = {
	N_("git remote add [<options>] <name> <url>"),
	NULL
};

static const char * const builtin_remote_rename_usage[] = {
	N_("git remote rename <old> <new>"),
	NULL
};

static const char * const builtin_remote_rm_usage[] = {
	N_("git remote remove <name>"),
	NULL
};

static const char * const builtin_remote_sethead_usage[] = {
	N_("git remote set-head <name> (-a | --auto | -d | --delete | <branch>)"),
	NULL
};

static const char * const builtin_remote_setbranches_usage[] = {
	N_("git remote set-branches <name> <branch>..."),
	N_("git remote set-branches --add <name> <branch>..."),
	NULL
};

static const char * const builtin_remote_show_usage[] = {
	N_("git remote show [<options>] <name>"),
	NULL
};

static const char * const builtin_remote_prune_usage[] = {
	N_("git remote prune [<options>] <name>"),
	NULL
};

static const char * const builtin_remote_update_usage[] = {
	N_("git remote update [<options>] [<group> | <remote>]..."),
	NULL
};

static const char * const builtin_remote_geturl_usage[] = {
	N_("git remote get-url [--push] [--all] <name>"),
	NULL
};

static const char * const builtin_remote_seturl_usage[] = {
	N_("git remote set-url [--push] <name> <newurl> [<oldurl>]"),
	N_("git remote set-url --add <name> <newurl>"),
	N_("git remote set-url --delete <name> <url>"),
	NULL
};

#define GET_REF_STATES (1<<0)
#define GET_HEAD_NAMES (1<<1)
#define GET_PUSH_REF_STATES (1<<2)

static int verbose;

static int fetch_remote(const char *name)
{
	const char *argv[] = { "fetch", name, NULL, NULL };
	if (verbose) {
		argv[1] = "-v";
		argv[2] = name;
	}
	printf_ln(_("Updating %s"), name);
	if (run_command_v_opt(argv, RUN_GIT_CMD))
		return error(_("Could not fetch %s"), name);
	return 0;
}

enum {
	TAGS_UNSET = 0,
	TAGS_DEFAULT = 1,
	TAGS_SET = 2
};

#define MIRROR_NONE 0
#define MIRROR_FETCH 1
#define MIRROR_PUSH 2
#define MIRROR_BOTH (MIRROR_FETCH|MIRROR_PUSH)

static void add_branch(const char *key, const char *branchname,
		       const char *remotename, int mirror, struct strbuf *tmp)
{
	strbuf_reset(tmp);
	strbuf_addch(tmp, '+');
	if (mirror)
		strbuf_addf(tmp, "refs/%s:refs/%s",
				branchname, branchname);
	else
		strbuf_addf(tmp, "refs/heads/%s:refs/remotes/%s/%s",
				branchname, remotename, branchname);
	git_config_set_multivar(key, tmp->buf, "^$", 0);
}

static const char mirror_advice[] =
N_("--mirror is dangerous and deprecated; please\n"
   "\t use --mirror=fetch or --mirror=push instead");

static int parse_mirror_opt(const struct option *opt, const char *arg, int not)
{
	unsigned *mirror = opt->value;
	if (not)
		*mirror = MIRROR_NONE;
	else if (!arg) {
		warning("%s", _(mirror_advice));
		*mirror = MIRROR_BOTH;
	}
	else if (!strcmp(arg, "fetch"))
		*mirror = MIRROR_FETCH;
	else if (!strcmp(arg, "push"))
		*mirror = MIRROR_PUSH;
	else
		return error(_("unknown mirror argument: %s"), arg);
	return 0;
}

static int add(int argc, const char **argv)
{
	int fetch = 0, fetch_tags = TAGS_DEFAULT;
	unsigned mirror = MIRROR_NONE;
	struct string_list track = STRING_LIST_INIT_NODUP;
	const char *master = NULL;
	struct remote *remote;
	struct strbuf buf = STRBUF_INIT, buf2 = STRBUF_INIT;
	const char *name, *url;
	int i;

	struct option options[] = {
		OPT_BOOL('f', "fetch", &fetch, N_("fetch the remote branches")),
		OPT_SET_INT(0, "tags", &fetch_tags,
			    N_("import all tags and associated objects when fetching"),
			    TAGS_SET),
		OPT_SET_INT(0, NULL, &fetch_tags,
			    N_("or do not fetch any tag at all (--no-tags)"), TAGS_UNSET),
		OPT_STRING_LIST('t', "track", &track, N_("branch"),
				N_("branch(es) to track")),
		OPT_STRING('m', "master", &master, N_("branch"), N_("master branch")),
		OPT_CALLBACK_F(0, "mirror", &mirror, "(push|fetch)",
			N_("set up remote as a mirror to push to or fetch from"),
			PARSE_OPT_OPTARG | PARSE_OPT_COMP_ARG, parse_mirror_opt),
		OPT_END()
	};

	argc = parse_options(argc, argv, NULL, options, builtin_remote_add_usage,
			     0);

	if (argc != 2)
		usage_with_options(builtin_remote_add_usage, options);

	if (mirror && master)
		die(_("specifying a master branch makes no sense with --mirror"));
	if (mirror && !(mirror & MIRROR_FETCH) && track.nr)
		die(_("specifying branches to track makes sense only with fetch mirrors"));

	name = argv[0];
	url = argv[1];

	remote = remote_get(name);
	if (remote_is_configured(remote, 1))
		die(_("remote %s already exists."), name);

	strbuf_addf(&buf2, "refs/heads/test:refs/remotes/%s/test", name);
	if (!valid_fetch_refspec(buf2.buf))
		die(_("'%s' is not a valid remote name"), name);

	strbuf_addf(&buf, "remote.%s.url", name);
	git_config_set(buf.buf, url);

	if (!mirror || mirror & MIRROR_FETCH) {
		strbuf_reset(&buf);
		strbuf_addf(&buf, "remote.%s.fetch", name);
		if (track.nr == 0)
			string_list_append(&track, "*");
		for (i = 0; i < track.nr; i++) {
			add_branch(buf.buf, track.items[i].string,
				   name, mirror, &buf2);
		}
	}

	if (mirror & MIRROR_PUSH) {
		strbuf_reset(&buf);
		strbuf_addf(&buf, "remote.%s.mirror", name);
		git_config_set(buf.buf, "true");
	}

	if (fetch_tags != TAGS_DEFAULT) {
		strbuf_reset(&buf);
		strbuf_addf(&buf, "remote.%s.tagopt", name);
		git_config_set(buf.buf,
			       fetch_tags == TAGS_SET ? "--tags" : "--no-tags");
	}

	if (fetch && fetch_remote(name))
		return 1;

	if (master) {
		strbuf_reset(&buf);
		strbuf_addf(&buf, "refs/remotes/%s/HEAD", name);

		strbuf_reset(&buf2);
		strbuf_addf(&buf2, "refs/remotes/%s/%s", name, master);

		if (create_symref(buf.buf, buf2.buf, "remote add"))
			return error(_("Could not setup master '%s'"), master);
	}

	strbuf_release(&buf);
	strbuf_release(&buf2);
	string_list_clear(&track, 0);

	return 0;
}

struct branch_info {
	char *remote_name;
	struct string_list merge;
	enum rebase_type rebase;
	char *push_remote_name;
};

static struct string_list branch_list = STRING_LIST_INIT_NODUP;

static const char *abbrev_ref(const char *name, const char *prefix)
{
	skip_prefix(name, prefix, &name);
	return name;
}
#define abbrev_branch(name) abbrev_ref((name), "refs/heads/")

static int config_read_branches(const char *key, const char *value, void *cb)
{
	const char *orig_key = key;
	char *name;
	struct string_list_item *item;
	struct branch_info *info;
	enum { REMOTE, MERGE, REBASE, PUSH_REMOTE } type;
	size_t key_len;

	if (!starts_with(key, "branch."))
		return 0;

	key += strlen("branch.");
	if (strip_suffix(key, ".remote", &key_len))
		type = REMOTE;
	else if (strip_suffix(key, ".merge", &key_len))
		type = MERGE;
	else if (strip_suffix(key, ".rebase", &key_len))
		type = REBASE;
	else if (strip_suffix(key, ".pushremote", &key_len))
		type = PUSH_REMOTE;
	else
		return 0;
	name = xmemdupz(key, key_len);

	item = string_list_insert(&branch_list, name);

	if (!item->util)
		item->util = xcalloc(1, sizeof(struct branch_info));
	info = item->util;
	switch (type) {
	case REMOTE:
		if (info->remote_name)
			warning(_("more than one %s"), orig_key);
		info->remote_name = xstrdup(value);
		break;
	case MERGE: {
		char *space = strchr(value, ' ');
		value = abbrev_branch(value);
		while (space) {
			char *merge;
			merge = xstrndup(value, space - value);
			string_list_append(&info->merge, merge);
			value = abbrev_branch(space + 1);
			space = strchr(value, ' ');
		}
		string_list_append(&info->merge, xstrdup(value));
		break;
	}
	case REBASE:
		/*
		 * Consider invalid values as false and check the
		 * truth value with >= REBASE_TRUE.
		 */
		info->rebase = rebase_parse_value(value);
		break;
	case PUSH_REMOTE:
		if (info->push_remote_name)
			warning(_("more than one %s"), orig_key);
		info->push_remote_name = xstrdup(value);
		break;
	default:
		BUG("unexpected type=%d", type);
	}

	return 0;
}

static void read_branches(void)
{
	if (branch_list.nr)
		return;
	git_config(config_read_branches, NULL);
}

struct ref_states {
	struct remote *remote;
	struct string_list new_refs, stale, tracked, heads, push;
	int queried;
};

static int get_ref_states(const struct ref *remote_refs, struct ref_states *states)
{
	struct ref *fetch_map = NULL, **tail = &fetch_map;
	struct ref *ref, *stale_refs;
	int i;

	for (i = 0; i < states->remote->fetch.nr; i++)
		if (get_fetch_map(remote_refs, &states->remote->fetch.items[i], &tail, 1))
			die(_("Could not get fetch map for refspec %s"),
				states->remote->fetch.raw[i]);

	states->new_refs.strdup_strings = 1;
	states->tracked.strdup_strings = 1;
	states->stale.strdup_strings = 1;
	for (ref = fetch_map; ref; ref = ref->next) {
		if (!ref->peer_ref || !ref_exists(ref->peer_ref->name))
			string_list_append(&states->new_refs, abbrev_branch(ref->name));
		else
			string_list_append(&states->tracked, abbrev_branch(ref->name));
	}
	stale_refs = get_stale_heads(&states->remote->fetch, fetch_map);
	for (ref = stale_refs; ref; ref = ref->next) {
		struct string_list_item *item =
			string_list_append(&states->stale, abbrev_branch(ref->name));
		item->util = xstrdup(ref->name);
	}
	free_refs(stale_refs);
	free_refs(fetch_map);

	string_list_sort(&states->new_refs);
	string_list_sort(&states->tracked);
	string_list_sort(&states->stale);

	return 0;
}

struct push_info {
	char *dest;
	int forced;
	enum {
		PUSH_STATUS_CREATE = 0,
		PUSH_STATUS_DELETE,
		PUSH_STATUS_UPTODATE,
		PUSH_STATUS_FASTFORWARD,
		PUSH_STATUS_OUTOFDATE,
		PUSH_STATUS_NOTQUERIED
	} status;
};

static int get_push_ref_states(const struct ref *remote_refs,
	struct ref_states *states)
{
	struct remote *remote = states->remote;
	struct ref *ref, *local_refs, *push_map;
	if (remote->mirror)
		return 0;

	local_refs = get_local_heads();
	push_map = copy_ref_list(remote_refs);

	match_push_refs(local_refs, &push_map, &remote->push, MATCH_REFS_NONE);

	states->push.strdup_strings = 1;
	for (ref = push_map; ref; ref = ref->next) {
		struct string_list_item *item;
		struct push_info *info;

		if (!ref->peer_ref)
			continue;
		oidcpy(&ref->new_oid, &ref->peer_ref->new_oid);

		item = string_list_append(&states->push,
					  abbrev_branch(ref->peer_ref->name));
		item->util = xcalloc(1, sizeof(struct push_info));
		info = item->util;
		info->forced = ref->force;
		info->dest = xstrdup(abbrev_branch(ref->name));

		if (is_null_oid(&ref->new_oid)) {
			info->status = PUSH_STATUS_DELETE;
		} else if (oideq(&ref->old_oid, &ref->new_oid))
			info->status = PUSH_STATUS_UPTODATE;
		else if (is_null_oid(&ref->old_oid))
			info->status = PUSH_STATUS_CREATE;
		else if (has_object_file(&ref->old_oid) &&
			 ref_newer(&ref->new_oid, &ref->old_oid))
			info->status = PUSH_STATUS_FASTFORWARD;
		else
			info->status = PUSH_STATUS_OUTOFDATE;
	}
	free_refs(local_refs);
	free_refs(push_map);
	return 0;
}

static int get_push_ref_states_noquery(struct ref_states *states)
{
	int i;
	struct remote *remote = states->remote;
	struct string_list_item *item;
	struct push_info *info;

	if (remote->mirror)
		return 0;

	states->push.strdup_strings = 1;
	if (!remote->push.nr) {
		item = string_list_append(&states->push, _("(matching)"));
		info = item->util = xcalloc(1, sizeof(struct push_info));
		info->status = PUSH_STATUS_NOTQUERIED;
		info->dest = xstrdup(item->string);
	}
	for (i = 0; i < remote->push.nr; i++) {
		const struct refspec_item *spec = &remote->push.items[i];
		if (spec->matching)
			item = string_list_append(&states->push, _("(matching)"));
		else if (strlen(spec->src))
			item = string_list_append(&states->push, spec->src);
		else
			item = string_list_append(&states->push, _("(delete)"));

		info = item->util = xcalloc(1, sizeof(struct push_info));
		info->forced = spec->force;
		info->status = PUSH_STATUS_NOTQUERIED;
		info->dest = xstrdup(spec->dst ? spec->dst : item->string);
	}
	return 0;
}

static int get_head_names(const struct ref *remote_refs, struct ref_states *states)
{
	struct ref *ref, *matches;
	struct ref *fetch_map = NULL, **fetch_map_tail = &fetch_map;
	struct refspec_item refspec;

	memset(&refspec, 0, sizeof(refspec));
	refspec.force = 0;
	refspec.pattern = 1;
	refspec.src = refspec.dst = "refs/heads/*";
	states->heads.strdup_strings = 1;
	get_fetch_map(remote_refs, &refspec, &fetch_map_tail, 0);
	matches = guess_remote_head(find_ref_by_name(remote_refs, "HEAD"),
				    fetch_map, 1);
	for (ref = matches; ref; ref = ref->next)
		string_list_append(&states->heads, abbrev_branch(ref->name));

	free_refs(fetch_map);
	free_refs(matches);

	return 0;
}

struct known_remote {
	struct known_remote *next;
	struct remote *remote;
};

struct known_remotes {
	struct remote *to_delete;
	struct known_remote *list;
};

static int add_known_remote(struct remote *remote, void *cb_data)
{
	struct known_remotes *all = cb_data;
	struct known_remote *r;

	if (!strcmp(all->to_delete->name, remote->name))
		return 0;

	r = xmalloc(sizeof(*r));
	r->remote = remote;
	r->next = all->list;
	all->list = r;
	return 0;
}

struct branches_for_remote {
	struct remote *remote;
	struct string_list *branches, *skipped;
	struct known_remotes *keep;
};

static int add_branch_for_removal(const char *refname,
	const struct object_id *oid, int flags, void *cb_data)
{
	struct branches_for_remote *branches = cb_data;
	struct refspec_item refspec;
	struct known_remote *kr;

	memset(&refspec, 0, sizeof(refspec));
	refspec.dst = (char *)refname;
	if (remote_find_tracking(branches->remote, &refspec))
		return 0;

	/* don't delete a branch if another remote also uses it */
	for (kr = branches->keep->list; kr; kr = kr->next) {
		memset(&refspec, 0, sizeof(refspec));
		refspec.dst = (char *)refname;
		if (!remote_find_tracking(kr->remote, &refspec))
			return 0;
	}

	/* don't delete non-remote-tracking refs */
	if (!starts_with(refname, "refs/remotes/")) {
		/* advise user how to delete local branches */
		if (starts_with(refname, "refs/heads/"))
			string_list_append(branches->skipped,
					   abbrev_branch(refname));
		/* silently skip over other non-remote refs */
		return 0;
	}

	string_list_append(branches->branches, refname);

	return 0;
}

struct rename_info {
	const char *old_name;
	const char *new_name;
	struct string_list *remote_branches;
};

static int read_remote_branches(const char *refname,
	const struct object_id *oid, int flags, void *cb_data)
{
	struct rename_info *rename = cb_data;
	struct strbuf buf = STRBUF_INIT;
	struct string_list_item *item;
	int flag;
	const char *symref;

	strbuf_addf(&buf, "refs/remotes/%s/", rename->old_name);
	if (starts_with(refname, buf.buf)) {
		item = string_list_append(rename->remote_branches, refname);
		symref = resolve_ref_unsafe(refname, RESOLVE_REF_READING,
					    NULL, &flag);
		if (symref && (flag & REF_ISSYMREF))
			item->util = xstrdup(symref);
		else
			item->util = NULL;
	}
	strbuf_release(&buf);

	return 0;
}

static int migrate_file(struct remote *remote)
{
	struct strbuf buf = STRBUF_INIT;
	int i;

	strbuf_addf(&buf, "remote.%s.url", remote->name);
	for (i = 0; i < remote->url_nr; i++)
		git_config_set_multivar(buf.buf, remote->url[i], "^$", 0);
	strbuf_reset(&buf);
	strbuf_addf(&buf, "remote.%s.push", remote->name);
	for (i = 0; i < remote->push.raw_nr; i++)
		git_config_set_multivar(buf.buf, remote->push.raw[i], "^$", 0);
	strbuf_reset(&buf);
	strbuf_addf(&buf, "remote.%s.fetch", remote->name);
	for (i = 0; i < remote->fetch.raw_nr; i++)
		git_config_set_multivar(buf.buf, remote->fetch.raw[i], "^$", 0);
	if (remote->origin == REMOTE_REMOTES)
		unlink_or_warn(git_path("remotes/%s", remote->name));
	else if (remote->origin == REMOTE_BRANCHES)
		unlink_or_warn(git_path("branches/%s", remote->name));
	strbuf_release(&buf);

	return 0;
}

struct push_default_info
{
	const char *old_name;
	enum config_scope scope;
	struct strbuf origin;
	int linenr;
};

static int config_read_push_default(const char *key, const char *value,
	void *cb)
{
	struct push_default_info* info = cb;
	if (strcmp(key, "remote.pushdefault") ||
	    !value || strcmp(value, info->old_name))
		return 0;

	info->scope = current_config_scope();
	strbuf_reset(&info->origin);
	strbuf_addstr(&info->origin, current_config_name());
	info->linenr = current_config_line();

	return 0;
}

static void handle_push_default(const char* old_name, const char* new_name)
{
	struct push_default_info push_default = {
		old_name, CONFIG_SCOPE_UNKNOWN, STRBUF_INIT, -1 };
	git_config(config_read_push_default, &push_default);
	if (push_default.scope >= CONFIG_SCOPE_COMMAND)
		; /* pass */
	else if (push_default.scope >= CONFIG_SCOPE_LOCAL) {
		int result = git_config_set_gently("remote.pushDefault",
						   new_name);
		if (new_name && result && result != CONFIG_NOTHING_SET)
			die(_("could not set '%s'"), "remote.pushDefault");
		else if (!new_name && result && result != CONFIG_NOTHING_SET)
			die(_("could not unset '%s'"), "remote.pushDefault");
	} else if (push_default.scope >= CONFIG_SCOPE_SYSTEM) {
		/* warn */
		warning(_("The %s configuration remote.pushDefault in:\n"
			  "\t%s:%d\n"
			  "now names the non-existent remote '%s'"),
			config_scope_name(push_default.scope),
			push_default.origin.buf, push_default.linenr,
			old_name);
	}
}


static int mv(int argc, const char **argv)
{
	struct option options[] = {
		OPT_END()
	};
	struct remote *oldremote, *newremote;
	struct strbuf buf = STRBUF_INIT, buf2 = STRBUF_INIT, buf3 = STRBUF_INIT,
		old_remote_context = STRBUF_INIT;
	struct string_list remote_branches = STRING_LIST_INIT_DUP;
	struct rename_info rename;
	int i, refspec_updated = 0;

	if (argc != 3)
		usage_with_options(builtin_remote_rename_usage, options);

	rename.old_name = argv[1];
	rename.new_name = argv[2];
	rename.remote_branches = &remote_branches;

	oldremote = remote_get(rename.old_name);
	if (!remote_is_configured(oldremote, 1))
		die(_("No such remote: '%s'"), rename.old_name);

	if (!strcmp(rename.old_name, rename.new_name) && oldremote->origin != REMOTE_CONFIG)
		return migrate_file(oldremote);

	newremote = remote_get(rename.new_name);
	if (remote_is_configured(newremote, 1))
		die(_("remote %s already exists."), rename.new_name);

	strbuf_addf(&buf, "refs/heads/test:refs/remotes/%s/test", rename.new_name);
	if (!valid_fetch_refspec(buf.buf))
		die(_("'%s' is not a valid remote name"), rename.new_name);

	strbuf_reset(&buf);
	strbuf_addf(&buf, "remote.%s", rename.old_name);
	strbuf_addf(&buf2, "remote.%s", rename.new_name);
	if (git_config_rename_section(buf.buf, buf2.buf) < 1)
		return error(_("Could not rename config section '%s' to '%s'"),
				buf.buf, buf2.buf);

	strbuf_reset(&buf);
	strbuf_addf(&buf, "remote.%s.fetch", rename.new_name);
	git_config_set_multivar(buf.buf, NULL, NULL, 1);
	strbuf_addf(&old_remote_context, ":refs/remotes/%s/", rename.old_name);
	for (i = 0; i < oldremote->fetch.raw_nr; i++) {
		char *ptr;

		strbuf_reset(&buf2);
		strbuf_addstr(&buf2, oldremote->fetch.raw[i]);
		ptr = strstr(buf2.buf, old_remote_context.buf);
		if (ptr) {
			refspec_updated = 1;
			strbuf_splice(&buf2,
				      ptr-buf2.buf + strlen(":refs/remotes/"),
				      strlen(rename.old_name), rename.new_name,
				      strlen(rename.new_name));
		} else
			warning(_("Not updating non-default fetch refspec\n"
				  "\t%s\n"
				  "\tPlease update the configuration manually if necessary."),
				buf2.buf);

		git_config_set_multivar(buf.buf, buf2.buf, "^$", 0);
	}

	read_branches();
	for (i = 0; i < branch_list.nr; i++) {
		struct string_list_item *item = branch_list.items + i;
		struct branch_info *info = item->util;
		if (info->remote_name && !strcmp(info->remote_name, rename.old_name)) {
			strbuf_reset(&buf);
			strbuf_addf(&buf, "branch.%s.remote", item->string);
			git_config_set(buf.buf, rename.new_name);
		}
		if (info->push_remote_name && !strcmp(info->push_remote_name, rename.old_name)) {
			strbuf_reset(&buf);
			strbuf_addf(&buf, "branch.%s.pushremote", item->string);
			git_config_set(buf.buf, rename.new_name);
		}
	}

	if (!refspec_updated)
		return 0;

	/*
	 * First remove symrefs, then rename the rest, finally create
	 * the new symrefs.
	 */
	for_each_ref(read_remote_branches, &rename);
	for (i = 0; i < remote_branches.nr; i++) {
		struct string_list_item *item = remote_branches.items + i;
		int flag = 0;

		read_ref_full(item->string, RESOLVE_REF_READING, NULL, &flag);
		if (!(flag & REF_ISSYMREF))
			continue;
		if (delete_ref(NULL, item->string, NULL, REF_NO_DEREF))
			die(_("deleting '%s' failed"), item->string);
	}
	for (i = 0; i < remote_branches.nr; i++) {
		struct string_list_item *item = remote_branches.items + i;

		if (item->util)
			continue;
		strbuf_reset(&buf);
		strbuf_addstr(&buf, item->string);
		strbuf_splice(&buf, strlen("refs/remotes/"), strlen(rename.old_name),
				rename.new_name, strlen(rename.new_name));
		strbuf_reset(&buf2);
		strbuf_addf(&buf2, "remote: renamed %s to %s",
				item->string, buf.buf);
		if (rename_ref(item->string, buf.buf, buf2.buf))
			die(_("renaming '%s' failed"), item->string);
	}
	for (i = 0; i < remote_branches.nr; i++) {
		struct string_list_item *item = remote_branches.items + i;

		if (!item->util)
			continue;
		strbuf_reset(&buf);
		strbuf_addstr(&buf, item->string);
		strbuf_splice(&buf, strlen("refs/remotes/"), strlen(rename.old_name),
				rename.new_name, strlen(rename.new_name));
		strbuf_reset(&buf2);
		strbuf_addstr(&buf2, item->util);
		strbuf_splice(&buf2, strlen("refs/remotes/"), strlen(rename.old_name),
				rename.new_name, strlen(rename.new_name));
		strbuf_reset(&buf3);
		strbuf_addf(&buf3, "remote: renamed %s to %s",
				item->string, buf.buf);
		if (create_symref(buf.buf, buf2.buf, buf3.buf))
			die(_("creating '%s' failed"), buf.buf);
	}
	string_list_clear(&remote_branches, 1);

	handle_push_default(rename.old_name, rename.new_name);

	return 0;
}

static int rm(int argc, const char **argv)
{
	struct option options[] = {
		OPT_END()
	};
	struct remote *remote;
	struct strbuf buf = STRBUF_INIT;
	struct known_remotes known_remotes = { NULL, NULL };
	struct string_list branches = STRING_LIST_INIT_DUP;
	struct string_list skipped = STRING_LIST_INIT_DUP;
	struct branches_for_remote cb_data;
	int i, result;

	memset(&cb_data, 0, sizeof(cb_data));
	cb_data.branches = &branches;
	cb_data.skipped = &skipped;
	cb_data.keep = &known_remotes;

	if (argc != 2)
		usage_with_options(builtin_remote_rm_usage, options);

	remote = remote_get(argv[1]);
	if (!remote_is_configured(remote, 1))
		die(_("No such remote: '%s'"), argv[1]);

	known_remotes.to_delete = remote;
	for_each_remote(add_known_remote, &known_remotes);

	read_branches();
	for (i = 0; i < branch_list.nr; i++) {
		struct string_list_item *item = branch_list.items + i;
		struct branch_info *info = item->util;
		if (info->remote_name && !strcmp(info->remote_name, remote->name)) {
			const char *keys[] = { "remote", "merge", NULL }, **k;
			for (k = keys; *k; k++) {
				strbuf_reset(&buf);
				strbuf_addf(&buf, "branch.%s.%s",
						item->string, *k);
				result = git_config_set_gently(buf.buf, NULL);
				if (result && result != CONFIG_NOTHING_SET)
					die(_("could not unset '%s'"), buf.buf);
			}
		}
		if (info->push_remote_name && !strcmp(info->push_remote_name, remote->name)) {
			strbuf_reset(&buf);
			strbuf_addf(&buf, "branch.%s.pushremote", item->string);
			result = git_config_set_gently(buf.buf, NULL);
			if (result && result != CONFIG_NOTHING_SET)
				die(_("could not unset '%s'"), buf.buf);
		}
	}

	/*
	 * We cannot just pass a function to for_each_ref() which deletes
	 * the branches one by one, since for_each_ref() relies on cached
	 * refs, which are invalidated when deleting a branch.
	 */
	cb_data.remote = remote;
	result = for_each_ref(add_branch_for_removal, &cb_data);
	strbuf_release(&buf);

	if (!result)
		result = delete_refs("remote: remove", &branches, REF_NO_DEREF);
	string_list_clear(&branches, 0);

	if (skipped.nr) {
		fprintf_ln(stderr,
			   Q_("Note: A branch outside the refs/remotes/ hierarchy was not removed;\n"
			      "to delete it, use:",
			      "Note: Some branches outside the refs/remotes/ hierarchy were not removed;\n"
			      "to delete them, use:",
			      skipped.nr));
		for (i = 0; i < skipped.nr; i++)
			fprintf(stderr, "  git branch -d %s\n",
				skipped.items[i].string);
	}
	string_list_clear(&skipped, 0);

	if (!result) {
		strbuf_addf(&buf, "remote.%s", remote->name);
		if (git_config_rename_section(buf.buf, NULL) < 1)
			return error(_("Could not remove config section '%s'"), buf.buf);

		handle_push_default(remote->name, NULL);
	}

	return result;
}

static void clear_push_info(void *util, const char *string)
{
	struct push_info *info = util;
	free(info->dest);
	free(info);
}

static void free_remote_ref_states(struct ref_states *states)
{
	string_list_clear(&states->new_refs, 0);
	string_list_clear(&states->stale, 1);
	string_list_clear(&states->tracked, 0);
	string_list_clear(&states->heads, 0);
	string_list_clear_func(&states->push, clear_push_info);
}

static int append_ref_to_tracked_list(const char *refname,
	const struct object_id *oid, int flags, void *cb_data)
{
	struct ref_states *states = cb_data;
	struct refspec_item refspec;

	if (flags & REF_ISSYMREF)
		return 0;

	memset(&refspec, 0, sizeof(refspec));
	refspec.dst = (char *)refname;
	if (!remote_find_tracking(states->remote, &refspec))
		string_list_append(&states->tracked, abbrev_branch(refspec.src));

	return 0;
}

static int get_remote_ref_states(const char *name,
				 struct ref_states *states,
				 int query)
{
	struct transport *transport;
	const struct ref *remote_refs;

	states->remote = remote_get(name);
	if (!states->remote)
		return error(_("No such remote: '%s'"), name);

	read_branches();

	if (query) {
		transport = transport_get(states->remote, states->remote->url_nr > 0 ?
			states->remote->url[0] : NULL);
		remote_refs = transport_get_remote_refs(transport, NULL);
		transport_disconnect(transport);

		states->queried = 1;
		if (query & GET_REF_STATES)
			get_ref_states(remote_refs, states);
		if (query & GET_HEAD_NAMES)
			get_head_names(remote_refs, states);
		if (query & GET_PUSH_REF_STATES)
			get_push_ref_states(remote_refs, states);
	} else {
		for_each_ref(append_ref_to_tracked_list, states);
		string_list_sort(&states->tracked);
		get_push_ref_states_noquery(states);
	}

	return 0;
}

struct show_info {
	struct string_list *list;
	struct ref_states *states;
	int width, width2;
	int any_rebase;
};

static int add_remote_to_show_info(struct string_list_item *item, void *cb_data)
{
	struct show_info *info = cb_data;
	int n = strlen(item->string);
	if (n > info->width)
		info->width = n;
	string_list_insert(info->list, item->string);
	return 0;
}

static int show_remote_info_item(struct string_list_item *item, void *cb_data)
{
	struct show_info *info = cb_data;
	struct ref_states *states = info->states;
	const char *name = item->string;

	if (states->queried) {
		const char *fmt = "%s";
		const char *arg = "";
		if (string_list_has_string(&states->new_refs, name)) {
			fmt = _(" new (next fetch will store in remotes/%s)");
			arg = states->remote->name;
		} else if (string_list_has_string(&states->tracked, name))
			arg = _(" tracked");
		else if (string_list_has_string(&states->stale, name))
			arg = _(" stale (use 'git remote prune' to remove)");
		else
			arg = _(" ???");
		printf("    %-*s", info->width, name);
		printf(fmt, arg);
		printf("\n");
	} else
		printf("    %s\n", name);

	return 0;
}

static int add_local_to_show_info(struct string_list_item *branch_item, void *cb_data)
{
	struct show_info *show_info = cb_data;
	struct ref_states *states = show_info->states;
	struct branch_info *branch_info = branch_item->util;
	struct string_list_item *item;
	int n;

	if (!branch_info->merge.nr || !branch_info->remote_name ||
	    strcmp(states->remote->name, branch_info->remote_name))
		return 0;
	if ((n = strlen(branch_item->string)) > show_info->width)
		show_info->width = n;
	if (branch_info->rebase >= REBASE_TRUE)
		show_info->any_rebase = 1;

	item = string_list_insert(show_info->list, branch_item->string);
	item->util = branch_info;

	return 0;
}

static int show_local_info_item(struct string_list_item *item, void *cb_data)
{
	struct show_info *show_info = cb_data;
	struct branch_info *branch_info = item->util;
	struct string_list *merge = &branch_info->merge;
	int width = show_info->width + 4;
	int i;

	if (branch_info->rebase >= REBASE_TRUE && branch_info->merge.nr > 1) {
		error(_("invalid branch.%s.merge; cannot rebase onto > 1 branch"),
			item->string);
		return 0;
	}

	printf("    %-*s ", show_info->width, item->string);
	if (branch_info->rebase >= REBASE_TRUE) {
		const char *msg;
		if (branch_info->rebase == REBASE_INTERACTIVE)
			msg = _("rebases interactively onto remote %s");
		else if (branch_info->rebase == REBASE_MERGES)
			msg = _("rebases interactively (with merges) onto "
				"remote %s");
		else
			msg = _("rebases onto remote %s");
		printf_ln(msg, merge->items[0].string);
		return 0;
	} else if (show_info->any_rebase) {
		printf_ln(_(" merges with remote %s"), merge->items[0].string);
		width++;
	} else {
		printf_ln(_("merges with remote %s"), merge->items[0].string);
	}
	for (i = 1; i < merge->nr; i++)
		printf(_("%-*s    and with remote %s\n"), width, "",
		       merge->items[i].string);

	return 0;
}

static int add_push_to_show_info(struct string_list_item *push_item, void *cb_data)
{
	struct show_info *show_info = cb_data;
	struct push_info *push_info = push_item->util;
	struct string_list_item *item;
	int n;
	if ((n = strlen(push_item->string)) > show_info->width)
		show_info->width = n;
	if ((n = strlen(push_info->dest)) > show_info->width2)
		show_info->width2 = n;
	item = string_list_append(show_info->list, push_item->string);
	item->util = push_item->util;
	return 0;
}

/*
 * Sorting comparison for a string list that has push_info
 * structs in its util field
 */
static int cmp_string_with_push(const void *va, const void *vb)
{
	const struct string_list_item *a = va;
	const struct string_list_item *b = vb;
	const struct push_info *a_push = a->util;
	const struct push_info *b_push = b->util;
	int cmp = strcmp(a->string, b->string);
	return cmp ? cmp : strcmp(a_push->dest, b_push->dest);
}

static int show_push_info_item(struct string_list_item *item, void *cb_data)
{
	struct show_info *show_info = cb_data;
	struct push_info *push_info = item->util;
	const char *src = item->string, *status = NULL;

	switch (push_info->status) {
	case PUSH_STATUS_CREATE:
		status = _("create");
		break;
	case PUSH_STATUS_DELETE:
		status = _("delete");
		src = _("(none)");
		break;
	case PUSH_STATUS_UPTODATE:
		status = _("up to date");
		break;
	case PUSH_STATUS_FASTFORWARD:
		status = _("fast-forwardable");
		break;
	case PUSH_STATUS_OUTOFDATE:
		status = _("local out of date");
		break;
	case PUSH_STATUS_NOTQUERIED:
		break;
	}
	if (status) {
		if (push_info->forced)
			printf_ln(_("    %-*s forces to %-*s (%s)"), show_info->width, src,
			       show_info->width2, push_info->dest, status);
		else
			printf_ln(_("    %-*s pushes to %-*s (%s)"), show_info->width, src,
			       show_info->width2, push_info->dest, status);
	} else {
		if (push_info->forced)
			printf_ln(_("    %-*s forces to %s"), show_info->width, src,
			       push_info->dest);
		else
			printf_ln(_("    %-*s pushes to %s"), show_info->width, src,
			       push_info->dest);
	}
	return 0;
}

static int get_one_entry(struct remote *remote, void *priv)
{
	struct string_list *list = priv;
	struct strbuf url_buf = STRBUF_INIT;
	const char **url;
	int i, url_nr;

	if (remote->url_nr > 0) {
		strbuf_addf(&url_buf, "%s (fetch)", remote->url[0]);
		string_list_append(list, remote->name)->util =
				strbuf_detach(&url_buf, NULL);
	} else
		string_list_append(list, remote->name)->util = NULL;
	if (remote->pushurl_nr) {
		url = remote->pushurl;
		url_nr = remote->pushurl_nr;
	} else {
		url = remote->url;
		url_nr = remote->url_nr;
	}
	for (i = 0; i < url_nr; i++)
	{
		strbuf_addf(&url_buf, "%s (push)", url[i]);
		string_list_append(list, remote->name)->util =
				strbuf_detach(&url_buf, NULL);
	}

	return 0;
}

static int show_all(void)
{
	struct string_list list = STRING_LIST_INIT_NODUP;
	int result;

	list.strdup_strings = 1;
	result = for_each_remote(get_one_entry, &list);

	if (!result) {
		int i;

		string_list_sort(&list);
		for (i = 0; i < list.nr; i++) {
			struct string_list_item *item = list.items + i;
			if (verbose)
				printf("%s\t%s\n", item->string,
					item->util ? (const char *)item->util : "");
			else {
				if (i && !strcmp((item - 1)->string, item->string))
					continue;
				printf("%s\n", item->string);
			}
		}
	}
	string_list_clear(&list, 1);
	return result;
}

static int show(int argc, const char **argv)
{
	int no_query = 0, result = 0, query_flag = 0;
	struct option options[] = {
		OPT_BOOL('n', NULL, &no_query, N_("do not query remotes")),
		OPT_END()
	};
	struct ref_states states;
	struct string_list info_list = STRING_LIST_INIT_NODUP;
	struct show_info info;

	argc = parse_options(argc, argv, NULL, options, builtin_remote_show_usage,
			     0);

	if (argc < 1)
		return show_all();

	if (!no_query)
		query_flag = (GET_REF_STATES | GET_HEAD_NAMES | GET_PUSH_REF_STATES);

	memset(&states, 0, sizeof(states));
	memset(&info, 0, sizeof(info));
	info.states = &states;
	info.list = &info_list;
	for (; argc; argc--, argv++) {
		int i;
		const char **url;
		int url_nr;

		get_remote_ref_states(*argv, &states, query_flag);

		printf_ln(_("* remote %s"), *argv);
		printf_ln(_("  Fetch URL: %s"), states.remote->url_nr > 0 ?
		       states.remote->url[0] : _("(no URL)"));
		if (states.remote->pushurl_nr) {
			url = states.remote->pushurl;
			url_nr = states.remote->pushurl_nr;
		} else {
			url = states.remote->url;
			url_nr = states.remote->url_nr;
		}
		for (i = 0; i < url_nr; i++)
			/*
			 * TRANSLATORS: the colon ':' should align
			 * with the one in " Fetch URL: %s"
			 * translation.
			 */
			printf_ln(_("  Push  URL: %s"), url[i]);
		if (!i)
			printf_ln(_("  Push  URL: %s"), _("(no URL)"));
		if (no_query)
			printf_ln(_("  HEAD branch: %s"), _("(not queried)"));
		else if (!states.heads.nr)
			printf_ln(_("  HEAD branch: %s"), _("(unknown)"));
		else if (states.heads.nr == 1)
			printf_ln(_("  HEAD branch: %s"), states.heads.items[0].string);
		else {
			printf(_("  HEAD branch (remote HEAD is ambiguous,"
				 " may be one of the following):\n"));
			for (i = 0; i < states.heads.nr; i++)
				printf("    %s\n", states.heads.items[i].string);
		}

		/* remote branch info */
		info.width = 0;
		for_each_string_list(&states.new_refs, add_remote_to_show_info, &info);
		for_each_string_list(&states.tracked, add_remote_to_show_info, &info);
		for_each_string_list(&states.stale, add_remote_to_show_info, &info);
		if (info.list->nr)
			printf_ln(Q_("  Remote branch:%s",
				     "  Remote branches:%s",
				     info.list->nr),
				  no_query ? _(" (status not queried)") : "");
		for_each_string_list(info.list, show_remote_info_item, &info);
		string_list_clear(info.list, 0);

		/* git pull info */
		info.width = 0;
		info.any_rebase = 0;
		for_each_string_list(&branch_list, add_local_to_show_info, &info);
		if (info.list->nr)
			printf_ln(Q_("  Local branch configured for 'git pull':",
				     "  Local branches configured for 'git pull':",
				     info.list->nr));
		for_each_string_list(info.list, show_local_info_item, &info);
		string_list_clear(info.list, 0);

		/* git push info */
		if (states.remote->mirror)
			printf_ln(_("  Local refs will be mirrored by 'git push'"));

		info.width = info.width2 = 0;
		for_each_string_list(&states.push, add_push_to_show_info, &info);
		QSORT(info.list->items, info.list->nr, cmp_string_with_push);
		if (info.list->nr)
			printf_ln(Q_("  Local ref configured for 'git push'%s:",
				     "  Local refs configured for 'git push'%s:",
				     info.list->nr),
				  no_query ? _(" (status not queried)") : "");
		for_each_string_list(info.list, show_push_info_item, &info);
		string_list_clear(info.list, 0);

		free_remote_ref_states(&states);
	}

	return result;
}

static int set_head(int argc, const char **argv)
{
	int i, opt_a = 0, opt_d = 0, result = 0;
	struct strbuf buf = STRBUF_INIT, buf2 = STRBUF_INIT;
	char *head_name = NULL;

	struct option options[] = {
		OPT_BOOL('a', "auto", &opt_a,
			 N_("set refs/remotes/<name>/HEAD according to remote")),
		OPT_BOOL('d', "delete", &opt_d,
			 N_("delete refs/remotes/<name>/HEAD")),
		OPT_END()
	};
	argc = parse_options(argc, argv, NULL, options, builtin_remote_sethead_usage,
			     0);
	if (argc)
		strbuf_addf(&buf, "refs/remotes/%s/HEAD", argv[0]);

	if (!opt_a && !opt_d && argc == 2) {
		head_name = xstrdup(argv[1]);
	} else if (opt_a && !opt_d && argc == 1) {
		struct ref_states states;
		memset(&states, 0, sizeof(states));
		get_remote_ref_states(argv[0], &states, GET_HEAD_NAMES);
		if (!states.heads.nr)
			result |= error(_("Cannot determine remote HEAD"));
		else if (states.heads.nr > 1) {
			result |= error(_("Multiple remote HEAD branches. "
					  "Please choose one explicitly with:"));
			for (i = 0; i < states.heads.nr; i++)
				fprintf(stderr, "  git remote set-head %s %s\n",
					argv[0], states.heads.items[i].string);
		} else
			head_name = xstrdup(states.heads.items[0].string);
		free_remote_ref_states(&states);
	} else if (opt_d && !opt_a && argc == 1) {
		if (delete_ref(NULL, buf.buf, NULL, REF_NO_DEREF))
			result |= error(_("Could not delete %s"), buf.buf);
	} else
		usage_with_options(builtin_remote_sethead_usage, options);

	if (head_name) {
		strbuf_addf(&buf2, "refs/remotes/%s/%s", argv[0], head_name);
		/* make sure it's valid */
		if (!ref_exists(buf2.buf))
			result |= error(_("Not a valid ref: %s"), buf2.buf);
		else if (create_symref(buf.buf, buf2.buf, "remote set-head"))
			result |= error(_("Could not setup %s"), buf.buf);
		else if (opt_a)
			printf("%s/HEAD set to %s\n", argv[0], head_name);
		free(head_name);
	}

	strbuf_release(&buf);
	strbuf_release(&buf2);
	return result;
}

static int prune_remote(const char *remote, int dry_run)
{
	int result = 0;
	struct ref_states states;
	struct string_list refs_to_prune = STRING_LIST_INIT_NODUP;
	struct string_list_item *item;
	const char *dangling_msg = dry_run
		? _(" %s will become dangling!")
		: _(" %s has become dangling!");

	memset(&states, 0, sizeof(states));
	get_remote_ref_states(remote, &states, GET_REF_STATES);

	if (!states.stale.nr) {
		free_remote_ref_states(&states);
		return 0;
	}

	printf_ln(_("Pruning %s"), remote);
	printf_ln(_("URL: %s"),
		  states.remote->url_nr
		  ? states.remote->url[0]
		  : _("(no URL)"));

	for_each_string_list_item(item, &states.stale)
		string_list_append(&refs_to_prune, item->util);
	string_list_sort(&refs_to_prune);

	if (!dry_run)
		result |= delete_refs("remote: prune", &refs_to_prune, 0);

	for_each_string_list_item(item, &states.stale) {
		const char *refname = item->util;

		if (dry_run)
			printf_ln(_(" * [would prune] %s"),
			       abbrev_ref(refname, "refs/remotes/"));
		else
			printf_ln(_(" * [pruned] %s"),
			       abbrev_ref(refname, "refs/remotes/"));
	}

	warn_dangling_symrefs(stdout, dangling_msg, &refs_to_prune);

	string_list_clear(&refs_to_prune, 0);
	free_remote_ref_states(&states);
	return result;
}

static int prune(int argc, const char **argv)
{
	int dry_run = 0, result = 0;
	struct option options[] = {
		OPT__DRY_RUN(&dry_run, N_("dry run")),
		OPT_END()
	};

	argc = parse_options(argc, argv, NULL, options, builtin_remote_prune_usage,
			     0);

	if (argc < 1)
		usage_with_options(builtin_remote_prune_usage, options);

	for (; argc; argc--, argv++)
		result |= prune_remote(*argv, dry_run);

	return result;
}

static int get_remote_default(const char *key, const char *value, void *priv)
{
	if (strcmp(key, "remotes.default") == 0) {
		int *found = priv;
		*found = 1;
	}
	return 0;
}

static int update(int argc, const char **argv)
{
	int i, prune = -1;
	struct option options[] = {
		OPT_BOOL('p', "prune", &prune,
			 N_("prune remotes after fetching")),
		OPT_END()
	};
	struct strvec fetch_argv = STRVEC_INIT;
	int default_defined = 0;
	int retval;

	argc = parse_options(argc, argv, NULL, options, builtin_remote_update_usage,
			     PARSE_OPT_KEEP_ARGV0);

	strvec_push(&fetch_argv, "fetch");

	if (prune != -1)
		strvec_push(&fetch_argv, prune ? "--prune" : "--no-prune");
	if (verbose)
		strvec_push(&fetch_argv, "-v");
	strvec_push(&fetch_argv, "--multiple");
	if (argc < 2)
		strvec_push(&fetch_argv, "default");
	for (i = 1; i < argc; i++)
		strvec_push(&fetch_argv, argv[i]);

	if (strcmp(fetch_argv.v[fetch_argv.nr-1], "default") == 0) {
		git_config(get_remote_default, &default_defined);
		if (!default_defined) {
			strvec_pop(&fetch_argv);
			strvec_push(&fetch_argv, "--all");
		}
	}

	retval = run_command_v_opt(fetch_argv.v, RUN_GIT_CMD);
	strvec_clear(&fetch_argv);
	return retval;
}

static int remove_all_fetch_refspecs(const char *key)
{
	return git_config_set_multivar_gently(key, NULL, NULL, 1);
}

static void add_branches(struct remote *remote, const char **branches,
			 const char *key)
{
	const char *remotename = remote->name;
	int mirror = remote->mirror;
	struct strbuf refspec = STRBUF_INIT;

	for (; *branches; branches++)
		add_branch(key, *branches, remotename, mirror, &refspec);

	strbuf_release(&refspec);
}

static int set_remote_branches(const char *remotename, const char **branches,
				int add_mode)
{
	struct strbuf key = STRBUF_INIT;
	struct remote *remote;

	strbuf_addf(&key, "remote.%s.fetch", remotename);

	remote = remote_get(remotename);
	if (!remote_is_configured(remote, 1))
		die(_("No such remote '%s'"), remotename);

	if (!add_mode && remove_all_fetch_refspecs(key.buf)) {
		strbuf_release(&key);
		return 1;
	}
	add_branches(remote, branches, key.buf);

	strbuf_release(&key);
	return 0;
}

static int set_branches(int argc, const char **argv)
{
	int add_mode = 0;
	struct option options[] = {
		OPT_BOOL('\0', "add", &add_mode, N_("add branch")),
		OPT_END()
	};

	argc = parse_options(argc, argv, NULL, options,
			     builtin_remote_setbranches_usage, 0);
	if (argc == 0) {
		error(_("no remote specified"));
		usage_with_options(builtin_remote_setbranches_usage, options);
	}
	argv[argc] = NULL;

	return set_remote_branches(argv[0], argv + 1, add_mode);
}

static int get_url(int argc, const char **argv)
{
	int i, push_mode = 0, all_mode = 0;
	const char *remotename = NULL;
	struct remote *remote;
	const char **url;
	int url_nr;
	struct option options[] = {
		OPT_BOOL('\0', "push", &push_mode,
			 N_("query push URLs rather than fetch URLs")),
		OPT_BOOL('\0', "all", &all_mode,
			 N_("return all URLs")),
		OPT_END()
	};
	argc = parse_options(argc, argv, NULL, options, builtin_remote_geturl_usage, 0);

	if (argc != 1)
		usage_with_options(builtin_remote_geturl_usage, options);

	remotename = argv[0];

	remote = remote_get(remotename);
	if (!remote_is_configured(remote, 1))
		die(_("No such remote '%s'"), remotename);

	url_nr = 0;
	if (push_mode) {
		url = remote->pushurl;
		url_nr = remote->pushurl_nr;
	}
	/* else fetch mode */

	/* Use the fetch URL when no push URLs were found or requested. */
	if (!url_nr) {
		url = remote->url;
		url_nr = remote->url_nr;
	}

	if (!url_nr)
		die(_("no URLs configured for remote '%s'"), remotename);

	if (all_mode) {
		for (i = 0; i < url_nr; i++)
			printf_ln("%s", url[i]);
	} else {
		printf_ln("%s", *url);
	}

	return 0;
}

static int set_url(int argc, const char **argv)
{
	int i, push_mode = 0, add_mode = 0, delete_mode = 0;
	int matches = 0, negative_matches = 0;
	const char *remotename = NULL;
	const char *newurl = NULL;
	const char *oldurl = NULL;
	struct remote *remote;
	regex_t old_regex;
	const char **urlset;
	int urlset_nr;
	struct strbuf name_buf = STRBUF_INIT;
	struct option options[] = {
		OPT_BOOL('\0', "push", &push_mode,
			 N_("manipulate push URLs")),
		OPT_BOOL('\0', "add", &add_mode,
			 N_("add URL")),
		OPT_BOOL('\0', "delete", &delete_mode,
			    N_("delete URLs")),
		OPT_END()
	};
	argc = parse_options(argc, argv, NULL, options, builtin_remote_seturl_usage,
			     PARSE_OPT_KEEP_ARGV0);

	if (add_mode && delete_mode)
		die(_("--add --delete doesn't make sense"));

	if (argc < 3 || argc > 4 || ((add_mode || delete_mode) && argc != 3))
		usage_with_options(builtin_remote_seturl_usage, options);

	remotename = argv[1];
	newurl = argv[2];
	if (argc > 3)
		oldurl = argv[3];

	if (delete_mode)
		oldurl = newurl;

	remote = remote_get(remotename);
	if (!remote_is_configured(remote, 1))
		die(_("No such remote '%s'"), remotename);

	if (push_mode) {
		strbuf_addf(&name_buf, "remote.%s.pushurl", remotename);
		urlset = remote->pushurl;
		urlset_nr = remote->pushurl_nr;
	} else {
		strbuf_addf(&name_buf, "remote.%s.url", remotename);
		urlset = remote->url;
		urlset_nr = remote->url_nr;
	}

	/* Special cases that add new entry. */
	if ((!oldurl && !delete_mode) || add_mode) {
		if (add_mode)
			git_config_set_multivar(name_buf.buf, newurl,
						       "^$", 0);
		else
			git_config_set(name_buf.buf, newurl);
		goto out;
	}

	/* Old URL specified. Demand that one matches. */
	if (regcomp(&old_regex, oldurl, REG_EXTENDED))
		die(_("Invalid old URL pattern: %s"), oldurl);

	for (i = 0; i < urlset_nr; i++)
		if (!regexec(&old_regex, urlset[i], 0, NULL, 0))
			matches++;
		else
			negative_matches++;
	if (!delete_mode && !matches)
		die(_("No such URL found: %s"), oldurl);
	if (delete_mode && !negative_matches && !push_mode)
		die(_("Will not delete all non-push URLs"));

	regfree(&old_regex);

	if (!delete_mode)
		git_config_set_multivar(name_buf.buf, newurl, oldurl, 0);
	else
		git_config_set_multivar(name_buf.buf, NULL, oldurl, 1);
out:
	strbuf_release(&name_buf);
	return 0;
}

int cmd_remote(int argc, const char **argv, const char *prefix)
{
	struct option options[] = {
		OPT__VERBOSE(&verbose, N_("be verbose; must be placed before a subcommand")),
		OPT_END()
	};
	int result;

	argc = parse_options(argc, argv, prefix, options, builtin_remote_usage,
		PARSE_OPT_STOP_AT_NON_OPTION);

	if (argc < 1)
		result = show_all();
	else if (!strcmp(argv[0], "add"))
		result = add(argc, argv);
	else if (!strcmp(argv[0], "rename"))
		result = mv(argc, argv);
	else if (!strcmp(argv[0], "rm") || !strcmp(argv[0], "remove"))
		result = rm(argc, argv);
	else if (!strcmp(argv[0], "set-head"))
		result = set_head(argc, argv);
	else if (!strcmp(argv[0], "set-branches"))
		result = set_branches(argc, argv);
	else if (!strcmp(argv[0], "get-url"))
		result = get_url(argc, argv);
	else if (!strcmp(argv[0], "set-url"))
		result = set_url(argc, argv);
	else if (!strcmp(argv[0], "show"))
		result = show(argc, argv);
	else if (!strcmp(argv[0], "prune"))
		result = prune(argc, argv);
	else if (!strcmp(argv[0], "update"))
		result = update(argc, argv);
	else {
		error(_("Unknown subcommand: %s"), argv[0]);
		usage_with_options(builtin_remote_usage, options);
	}

	return result ? 1 : 0;
}
