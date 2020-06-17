"""Bizarre hacks to make Gerrit better."""

import collections
import re
import random
import mercurial

_ = mercurial.i18n._

cmdtable = {}
command = mercurial.registrar.command(cmdtable)

testedwith = '5.3.1'

_changeid_regex = re.compile(b'^Change-Id: (I.*)$', re.M)

def random_hash():
    """Returns a random SHA1-like hex string."""
    return b"%040x" % random.getrandbits(160)

def reposetup(ui, repo):

    class GerritRepo(repo.__class__):
        def commitctx(self, ctx, *args, **kwargs):
            match = _changeid_regex.search(ctx._text)
            if not match:
                ctx._text = ctx._text.rstrip(b'\n')
                ctx._text += b'\n\nChange-Id: I' + random_hash()
            return super().commitctx(ctx, *args, **kwargs)

    repo.__class__ = GerritRepo


@command(b'gerrit-obsolete', [], _(b'[options]'))
def gerritobsolete(ui, repo, **opts):
    """Mark draft commits as obsolete by public commits based on Gerrit Change-Id tag."""
    if repo.obsstore.readonly:
        ui.error(b'obsstore is readonly')
        return
    changesets = collections.defaultdict(set)
    drafts = set()
    for draft in repo.set('draft() - obsolete()'):
        match = _changeid_regex.search(draft.description())
        if not match:
            continue
        changesets[match.groups()[0]].add(draft)
        drafts.add(draft)
    if not drafts:
        return
    publicparent = next(repo.set(
        b'ancestor((public() and bookmark("master")), %s)' % (
            b', '.join(x.hex() for x in drafts))))
    megare = b're:(?ms)^Change-Id: (%s)$' % (b'|'.join(changesets.keys()),)
    markers = []
    for public in repo.set('(%s..(public() and master)) and desc(%s)', publicparent, megare):
        match = _changeid_regex.search(public.description())
        if not match:
            continue
        drafts = changesets[match.groups()[0]]
        if not drafts:
            continue
        markers.append((tuple(drafts), (public,)))
    mercurial.obsolete.createmarkers(repo, markers, operation=b'gerrit-obsolete')
