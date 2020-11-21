test_expect_success "setup receive.procReceiveRefs" '
	git -C "$upstream" config --add receive.procReceiveRefs refs/for
'

test_expect_success "setup proc-receive hook" '
	write_script "$upstream/hooks/proc-receive" <<-EOF
	printf >&2 "# proc-receive hook\n"
	test-tool proc-receive -v \
		-r "ok refs/for/master/topic1" \
		-r "option fall-through" \
		-r "ok refs/for/master/topic2" \
		-r "option refname refs/for/changes/23/123/1" \
		-r "option new-oid $A" \
		-r "ok refs/for/master/topic2" \
		-r "option refname refs/for/changes/24/124/2" \
		-r "option old-oid $B" \
		-r "option new-oid $A" \
		-r "option forced-update" \
		-r "ng refs/for/next/topic target branch not exist"
	EOF
'

# Refs of upstream : master(A)
# Refs of workbench: master(A)  tags/v123
# git push         : (B)                   refs/for/master/topic1(A)  foo(A)  refs/for/next/topic(A)  refs/for/master/topic2(A)
test_expect_success "proc-receive: report status v1" '
	{
		if test -z "$GIT_DEFAULT_HASH" || test "$GIT_DEFAULT_HASH" = "sha1"
		then
			printf "%s %s refs/heads/master\0report-status\n" \
				$A $B | packetize
		else
			printf "%s %s refs/heads/master\0report-status object-format=$GIT_DEFAULT_HASH\n" \
				$A $B | packetize
		fi &&
		printf "%s %s refs/for/master/topic1\n" \
			$ZERO_OID $A | packetize &&
		printf "%s %s refs/heads/foo\n" \
			$ZERO_OID $A | packetize &&
		printf "%s %s refs/for/next/topic\n" \
			$ZERO_OID $A | packetize &&
		printf "%s %s refs/for/master/topic2\n" \
			$ZERO_OID $A | packetize &&
		printf 0000 &&
		printf "" | git -C "$upstream" pack-objects --stdout
	} | git receive-pack "$upstream" --stateless-rpc \
	>out 2>&1 &&
	make_user_friendly_and_stable_output <out >actual &&
	cat >expect <<-EOF &&
	# pre-receive hook
	pre-receive< <COMMIT-A> <COMMIT-B> refs/heads/master
	pre-receive< <ZERO-OID> <COMMIT-A> refs/for/master/topic1
	pre-receive< <ZERO-OID> <COMMIT-A> refs/heads/foo
	pre-receive< <ZERO-OID> <COMMIT-A> refs/for/next/topic
	pre-receive< <ZERO-OID> <COMMIT-A> refs/for/master/topic2
	# proc-receive hook
	proc-receive< <ZERO-OID> <COMMIT-A> refs/for/master/topic1
	proc-receive< <ZERO-OID> <COMMIT-A> refs/for/next/topic
	proc-receive< <ZERO-OID> <COMMIT-A> refs/for/master/topic2
	proc-receive> ok refs/for/master/topic1
	proc-receive> option fall-through
	proc-receive> ok refs/for/master/topic2
	proc-receive> option refname refs/for/changes/23/123/1
	proc-receive> option new-oid <COMMIT-A>
	proc-receive> ok refs/for/master/topic2
	proc-receive> option refname refs/for/changes/24/124/2
	proc-receive> option old-oid <COMMIT-B>
	proc-receive> option new-oid <COMMIT-A>
	proc-receive> option forced-update
	proc-receive> ng refs/for/next/topic target branch not exist
	000eunpack ok
	0019ok refs/heads/master
	001eok refs/for/master/topic1
	0016ok refs/heads/foo
	0033ng refs/for/next/topic target branch not exist
	001eok refs/for/master/topic2
	0000# post-receive hook
	post-receive< <COMMIT-A> <COMMIT-B> refs/heads/master
	post-receive< <ZERO-OID> <COMMIT-A> refs/for/master/topic1
	post-receive< <ZERO-OID> <COMMIT-A> refs/heads/foo
	post-receive< <ZERO-OID> <COMMIT-A> refs/for/changes/23/123/1
	post-receive< <COMMIT-B> <COMMIT-A> refs/for/changes/24/124/2
	EOF
	test_cmp expect actual &&

	git -C "$upstream" show-ref >out &&
	make_user_friendly_and_stable_output <out >actual &&
	cat >expect <<-EOF &&
	<COMMIT-A> refs/for/master/topic1
	<COMMIT-A> refs/heads/foo
	<COMMIT-B> refs/heads/master
	EOF
	test_cmp expect actual
'
