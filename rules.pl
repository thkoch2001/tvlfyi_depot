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
    % Check if the first line of the commit message is less than 68 characters long
    gerrit:commit_message(Message), name(Message, MessageCodes),
    take_until_equal(10, MessageCodes, FirstLine), length(FirstLine, FirstLineLength),
    FirstLineLength =< 68,
    !,
    applicable_owner(Owner),
    Check = label('Conformant-Commit-Message', ok(Owner)).

commit_message(Check) :-
    Check = label('Conformant-Commit-Message', need(_)).

code_owners(Checks) :-
    % Retrieve all `Code-Review: +2` approvers, as well as the
    % change owner, and run the owners check against these.
    findall(U, gerrit:commit_label(label('Code-Review', 2), U), Approvers),
    applicable_owner(ChangeOwner),
    gerrit_owners:add_owner_approval([ChangeOwner | Approvers], [], OwnerChecks),
    ( OwnerChecks = [] ->
      % gerrit_owners:add_owner_approval/3 only adds the label if there *isn't*
      % code-review from owners, but it's nice for UI consistency if we also
      % have an ok label if there *is* - so we check for no output from
      % gerrit_owners:add_owner_approval/3 and add an ok(ChangeOwner) if so.
      Checks = [label('Code-Review-from-owners', ok(ChangeOwner))]
    ; Checks = OwnerChecks
    ).

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

    % Allow the Autosubmit label to appear optionally.
    Autosubmit = label('Autosubmit', may(_)),

    code_owners(OwnerChecks),

    S =.. [submit,
           ReviewCheck,
           BuildCheck,
           CommentsCheck,
           CommitCheck,
           Autosubmit
           | OwnerChecks].

submit_type(fast_forward_only) :-
    % Check if the commit type is a subtree commit.
    gerrit:commit_message_matches('^subtree[\(]'),
    % Validate that it actually has multiple parents
    gerrit:commit_parent_count(ParentCount),
    ParentCount > 1,
    !.
submit_type(rebase_always).
