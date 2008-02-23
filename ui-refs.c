/* ui-refs.c: browse symbolic refs
 *
 * Copyright (C) 2006 Lars Hjemli
 *
 * Licensed under GNU General Public License v2
 *   (see COPYING for full license text)
 */

#include "cgit.h"
#include "html.h"

void cgit_print_refs()
{

	html("<table class='list nowrap'>");

	if (ctx.qry.path && !strncmp(ctx.qry.path, "heads", 5))
		cgit_print_branches(0);
	else if (ctx.qry.path && !strncmp(ctx.qry.path, "tags", 4))
		cgit_print_tags(0);
	else {
		cgit_print_branches(0);
		html("<tr class='nohover'><td colspan='4'>&nbsp;</td></tr>");
		cgit_print_tags(0);
	}

	html("</table>");
}
