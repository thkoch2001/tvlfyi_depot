/* ui-snapshot.c: generate snapshot of a commit
 *
 * Copyright (C) 2006 Lars Hjemli
 *
 * Licensed under GNU General Public License v2
 *   (see COPYING for full license text)
 */

#include "cgit.h"

static int write_compressed_tar_archive(struct archiver_args *args,const char *filter)
{
	int rw[2];
	pid_t gzpid;
	int stdout2;
	int status;
	int rv;

	stdout2 = chk_non_negative(dup(STDIN_FILENO), "Preserving STDOUT before compressing");
	chk_zero(pipe(rw), "Opening pipe from compressor subprocess");
	gzpid = chk_non_negative(fork(), "Forking compressor subprocess");
	if(gzpid==0) {
		/* child */
		chk_zero(close(rw[1]), "Closing write end of pipe in child");
		chk_zero(close(STDIN_FILENO), "Closing STDIN");
		chk_non_negative(dup2(rw[0],STDIN_FILENO), "Redirecting compressor input to stdin");
		execlp(filter,filter,NULL);
		_exit(-1);
	}
	/* parent */
	chk_zero(close(rw[0]), "Closing read end of pipe");
	chk_non_negative(dup2(rw[1],STDOUT_FILENO), "Redirecting output to compressor");
	
	rv = write_tar_archive(args);

	chk_zero(close(STDOUT_FILENO), "Closing STDOUT redirected to compressor");
	chk_non_negative(dup2(stdout2,STDOUT_FILENO), "Restoring uncompressed STDOUT");
	chk_zero(close(stdout2), "Closing uncompressed STDOUT");
	chk_zero(close(rw[1]), "Closing write end of pipe in parent");
	chk_positive(waitpid(gzpid,&status,0), "Waiting on compressor process");
	if(! ( WIFEXITED(status) && WEXITSTATUS(status)==0 ) )
		cgit_print_error("Failed to compress archive");

	return rv;
}

static int write_tar_gzip_archive(struct archiver_args *args)
{
	return write_compressed_tar_archive(args,"gzip");
}
static int write_tar_bzip2_archive(struct archiver_args *args)
{
	return write_compressed_tar_archive(args,"bzip2");
}

static const struct snapshot_archive_t {
    	const char *suffix;
	const char *mimetype;
	write_archive_fn_t write_func;
}	snapshot_archives[] = {
	{ ".zip", "application/x-zip", write_zip_archive },
	{ ".tar.gz", "application/x-tar", write_tar_gzip_archive },
	{ ".tar.bz2", "application/x-tar", write_tar_bzip2_archive },
	{ ".tar", "application/x-tar", write_tar_archive }
};

void cgit_print_snapshot(struct cacheitem *item, const char *hex, 
			 const char *prefix, const char *filename)
{
	int fnl = strlen(filename);
	int f;
    	for(f=0;f<(sizeof(snapshot_archives)/sizeof(*snapshot_archives));++f) {
		const struct snapshot_archive_t* sat = &snapshot_archives[f];
		int sl = strlen(sat->suffix);
		if(fnl<sl || strcmp(&filename[fnl-sl],sat->suffix))
			continue;

		struct archiver_args args;
		struct commit *commit;
		unsigned char sha1[20];

		if(get_sha1(hex, sha1)) {
			cgit_print_error(fmt("Bad object id: %s", hex));
			return;
		}
		commit = lookup_commit_reference(sha1);

		if(!commit) {
			cgit_print_error(fmt("Not a commit reference: %s", hex));
			return;;
		}

		memset(&args,0,sizeof(args));
		args.base = fmt("%s/", prefix);
		args.tree = commit->tree;

		cgit_print_snapshot_start(sat->mimetype, filename, item);
		(*sat->write_func)(&args);
		return;
	}
	cgit_print_error(fmt("Unsupported snapshot format: %s", filename));
}

void cgit_print_snapshot_links(const char *repo,const char *hex)
{
    	char *filename;
	int f;
    	for(f=0;f<(sizeof(snapshot_archives)/sizeof(*snapshot_archives));++f) {
		const struct snapshot_archive_t* sat = &snapshot_archives[f];
		filename = fmt("%s-%s%s",repo,hex,sat->suffix);
		htmlf("<a href='%s'>%s</a><br/>",
			cgit_pageurl(repo,"snapshot",
			    fmt("id=%s&amp;name=%s",hex,filename)), filename);
	}
}
/* vim:set sw=8: */
