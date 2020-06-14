% This file implements the depot submit rule, containing the following
% checks:
%
% - All default Gerrit submit checks pass.
% - If a codepath has owners, the change must either have a +2 from one
%   of the owners or be proposed by one of them.

submit_rule(S) :-
    gerrit:default_submit(D),
    D =.. [submit | DefaultChecks],

    % Retrieve all `Code-Review: +2` approvers, as well as the
    % uploader, and run the owners check against these.
    findall(U, gerrit:commit_label(label('Code-Review', 2), U), Approvers),
    gerrit:uploader(Uploader),
    gerrit_owners:add_owner_approval([Uploader | Approvers],
                                     DefaultChecks, ChecksWithOwners),

    S =.. [submit | ChecksWithOwners].
