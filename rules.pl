% -*- mode: prolog -*-
% This file implements the depot submit rule, containing the following
% checks:
%
% - All default Gerrit submit checks pass.
% - No unresolved comments exist.
% - The CI build has run successfully.
% - The commit message conforms to our guidelines.
%
% It further conditionally allows autosubmission of changes if the
% uploader was the change owner (see b/167 for background).
%
% It also implements the submit type decision, with the following
% rules:
%
% - subtree changes are submittable as 'fast_forward_only'
% - all other changes are submittable as 'rebase_if_necessary'

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
    take_until_equal(S, Xs, Rest)
  ).

% Determine the most applicable change owner.
%
% This is usually the Uploader, unless the Uploader is clbot in which
% case the change owner is used instead.
clbot(user(1000015)).
applicable_owner(Owner) :-
    gerrit:uploader(Uploader),
    clbot(Uploader),
    !,
    gerrit:change_owner(Owner).

applicable_owner(ChangeOwner) :-
    gerrit:uploader(ChangeOwner).

unresolved_comments(Check) :-
    gerrit:unresolved_comments_count(0),
    !,
    applicable_owner(ChangeOwner),
    Check = label('All-Comments-Resolved', ok(ChangeOwner)).

unresolved_comments(Check) :-
    gerrit:unresolved_comments_count(Count),
    Count > 0,
    Check = label('All-Comments-Resolved', need(_)).

commit_message(Check) :-
    % Check if the commit message uses the prescribed angular-style commit format
    gerrit:commit_message_matches('^(feat|fix|docs|style|refactor|test|chore|merge|revert|subtree)[\(:]'),
    % Check if the first line of the commit message is less than 72 characters long
    gerrit:commit_message(Message), name(Message, MessageCodes),
    take_until_equal(10, MessageCodes, FirstLine), length(FirstLine, FirstLineLength),
    FirstLineLength =< 72,
    !,
    applicable_owner(Owner),
    Check = label('Conformant-Commit-Message', ok(Owner)).

commit_message(Check) :-
    Check = label('Conformant-Commit-Message', need(_)).

% Require CI to pass, except for CLs to refs/meta/config.
build_check(Check) :-
    gerrit:change_branch('refs/meta/config'),
    Check = label('Verified', may(_)),
    !.
build_check(Check) :-
    gerrit:max_with_block(-1, 1, 'Verified', Check).

% Allow autosubmit if the uploader is clbot (autosubmit already in
% progress) or the change owner.
%
% This prepends to the existing checks because Gerrit's Prolog does
% not have append/3.
autosubmit(Current, New) :-
    gerrit:uploader(Uploader),
    (clbot(Uploader); gerrit:change_owner(Uploader)),
    New = [label('Autosubmit', may(_)) | Current],
    !.

% Otherwise, do not allow autosubmit.
autosubmit(Current, New) :-
    New = Current.

submit_rule(S) :-
    % Code review with +2 is required, -2 blocks the submit process.
    gerrit:max_with_block(-2, 2, 'Code-Review', ReviewCheck),

    % CI verification is required, broken builds block the submit
    % process. The `depot-interventions` group can be used by admins
    % to override the verification status if CI is on fire.
    build_check(BuildCheck),

    % Check for unresolved comments (this is necessary because it's
    % easy to miss them in previous patch sets)
    unresolved_comments(CommentsCheck),

    % Check that the commit message matches the format described in
    % CONTRIBUTING.md
    commit_message(CommitCheck),

    % Check whether autosubmit is allowed.
    autosubmit([], AllOtherChecks),

    S =.. [submit,
           ReviewCheck,
           BuildCheck,
           CommentsCheck,
           CommitCheck
           | AllOtherChecks ].

submit_type(fast_forward_only) :-
    % Check if the commit type is a subtree commit.
    gerrit:commit_message_matches('^subtree[\(]'),
    % Validate that it actually has multiple parents
    gerrit:commit_parent_count(ParentCount),
    ParentCount > 1,
    !.
submit_type(rebase_always).
