% This file implements the depot submit rule, containing the following
% checks:
%
% - All default Gerrit submit checks pass.
% - If a codepath has owners, the change must either have a +2 from one
%   of the owners or be proposed by one of them.
% - No unresolved comments exist.

append([],L,L).
append([H|T],L2,[H|L3]) :-
    append(T,L2,L3).

unresolved_comments(Check) :-
    gerrit:unresolved_comments_count(0),
    !,
    gerrit:uploader(Uploader),
    Check = label('All-Comments-Resolved', ok(Uploader)).

unresolved_comments(Check) :-
    gerrit:unresolved_comments_count(Count),
    Count > 0,
    Check = label('All-Comments-Resolved', need(_)).

submit_rule(S) :-
    gerrit:default_submit(D),
    D =.. [submit | DefaultChecks],

    % Retrieve all `Code-Review: +2` approvers, as well as the
    % uploader, and run the owners check against these.
    findall(U, gerrit:commit_label(label('Code-Review', 2), U), Approvers),
    gerrit:uploader(Uploader),
    gerrit_owners:add_owner_approval([Uploader | Approvers],
                                     DefaultChecks, ChecksWithOwners),

    % Check for unresolved comments (this is necessary because it's
    % easy to miss them in previous patch sets)
    unresolved_comments(CommentsCheck),
    append(ChecksWithOwners, [CommentsCheck], ChecksWithComments),

    S =.. [submit | ChecksWithComments].
