#ifndef UI_DIFF_H
#define UI_DIFF_H

extern void cgit_print_diff_ctrls();

extern void cgit_print_diffstat(const unsigned char *old_sha1,
				const unsigned char *new_sha1);

extern void cgit_print_diff(const char *new_hex, const char *old_hex,
			    const char *prefix, int show_ctrls);

extern struct diff_filespec *cgit_get_current_old_file(void);
extern struct diff_filespec *cgit_get_current_new_file(void);

extern unsigned char old_rev_sha1[20];
extern unsigned char new_rev_sha1[20];

#endif /* UI_DIFF_H */
