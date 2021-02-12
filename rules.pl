% -*- mode: prolog -*-
% This file implements the depot submit rule, containing the following
% checks:
%
% - All default Gerrit submit checks pass.
% - If a codepath has owners, the change must either have a +2 from one
%   of the owners or be proposed by one of them.
% - No unresolved comments exist.
% - The CI build has run successfully.
% - The commit message conforms to our guidelines.

% Helper predicate which relates a list and an element of that list
% to the list of elements before the first occurrence of the element.
% We use this in this file to get the first line of a commit message
% (as a list of codepoints).
%
% take_until_equal(23, [8,5,16,23,5,6], L).
%   L = [8, 5, 16].
take_until_equal(S, [], []).
take_until_equal(S, [X|Xs], Init) :-
  ( S == X ->
    Init = []
  ; Init = [X|Rest],
    take_till(S, Xs, Rest)
  ).

unresolved_comments(Check) :-
    gerrit:unresolved_comments_count(0),
    !,
    gerrit:uploader(Uploader),
    Check = label('All-Comments-Resolved', ok(Uploader)).

unresolved_comments(Check) :-
    gerrit:unresolved_comments_count(Count),
    Count > 0,
    Check = label('All-Comments-Resolved', need(_)).

commit_message(Check) :-
    % Check if the commit message uses the prescribed angular-style commit format
    gerrit:commit_message_matches('^(feat|fix|docs|style|refactor|test|chore|merge|revert)[\(:]'),
    % Check if the first line of the commit message is less than 68 characters long
    gerrit:commit_message(Message), name(Message, MessageCodes),
    take_until_equal(10, MessageCodes, FirstLine), length(FirstLine, FirstLineLength),
    FirstLineLength =< 68,
    !,
    gerrit:uploader(Uploader),
    Check = label('Conformant-Commit-Message', ok(Uploader)).

commit_message(Check) :-
    Check = label('Conformant-Commit-Message', need(_)).

submit_rule(S) :-
    % Code review with +2 is required, -2 blocks the submit process.
    gerrit:max_with_block(-2, 2, 'Code-Review', ReviewCheck),

    % CI verification is required, broken builds block the submit
    % process. The `depot-interventions` group can be used by admins
    % to override the verification status if CI is on fire.
    gerrit:max_with_block(-1, 1, 'Verified', BuildCheck),

    % Check for unresolved comments (this is necessary because it's
    % easy to miss them in previous patch sets)
    unresolved_comments(CommentsCheck),

    % Check that the commit message matches the format described in
    % CONTRIBUTING.md
    commit_message(CommitCheck),

    % Retrieve all `Code-Review: +2` approvers, as well as the
    % uploader, and run the owners check against these.
    findall(U, gerrit:commit_label(label('Code-Review', 2), U), Approvers),
    gerrit:uploader(Uploader),
    gerrit_owners:add_owner_approval([Uploader | Approvers],
                                     [ReviewCheck, BuildCheck, CommentsCheck, CommitCheck],
                                     AllChecks),

    S =.. [submit | AllChecks].
