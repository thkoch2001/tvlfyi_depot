#!/bin/sh

test_description='progress display'

. ./test-lib.sh

show_cr () {
	tr '\015' Q | sed -e "s/Q/<CR>\\$LF/g"
}

test_expect_success 'simple progress display' '
	cat >expect <<-\EOF &&
	Working hard: 1<CR>
	Working hard: 2<CR>
	Working hard: 5<CR>
	Working hard: 5, done.
	EOF

	cat >in <<-\EOF &&
	update
	progress 1
	update
	progress 2
	progress 3
	progress 4
	update
	progress 5
	EOF
	test-tool progress "Working hard" <in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_expect_success 'progress display with total' '
	cat >expect <<-\EOF &&
	Working hard:  33% (1/3)<CR>
	Working hard:  66% (2/3)<CR>
	Working hard: 100% (3/3)<CR>
	Working hard: 100% (3/3), done.
	EOF

	cat >in <<-\EOF &&
	progress 1
	progress 2
	progress 3
	EOF
	test-tool progress --total=3 "Working hard" <in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_expect_success 'progress display breaks long lines #1' '
	sed -e "s/Z$//" >expect <<\EOF &&
Working hard.......2.........3.........4.........5.........6:   0% (100/100000)<CR>
Working hard.......2.........3.........4.........5.........6:   1% (1000/100000)<CR>
Working hard.......2.........3.........4.........5.........6:                   Z
   10% (10000/100000)<CR>
  100% (100000/100000)<CR>
  100% (100000/100000), done.
EOF

	cat >in <<-\EOF &&
	progress 100
	progress 1000
	progress 10000
	progress 100000
	EOF
	test-tool progress --total=100000 \
		"Working hard.......2.........3.........4.........5.........6" \
		<in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_expect_success 'progress display breaks long lines #2' '
	# Note: we do not need that many spaces after the title to cover up
	# the last line before breaking the progress line.
	sed -e "s/Z$//" >expect <<\EOF &&
Working hard.......2.........3.........4.........5.........6:   0% (1/100000)<CR>
Working hard.......2.........3.........4.........5.........6:   0% (2/100000)<CR>
Working hard.......2.........3.........4.........5.........6:                   Z
   10% (10000/100000)<CR>
  100% (100000/100000)<CR>
  100% (100000/100000), done.
EOF

	cat >in <<-\EOF &&
	update
	progress 1
	update
	progress 2
	progress 10000
	progress 100000
	EOF
	test-tool progress --total=100000 \
		"Working hard.......2.........3.........4.........5.........6" \
		<in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_expect_success 'progress display breaks long lines #3 - even the first is too long' '
	# Note: we do not actually need any spaces at the end of the title
	# line, because there is no previous progress line to cover up.
	sed -e "s/Z$//" >expect <<\EOF &&
Working hard.......2.........3.........4.........5.........6:                   Z
   25% (25000/100000)<CR>
   50% (50000/100000)<CR>
   75% (75000/100000)<CR>
  100% (100000/100000)<CR>
  100% (100000/100000), done.
EOF

	cat >in <<-\EOF &&
	progress 25000
	progress 50000
	progress 75000
	progress 100000
	EOF
	test-tool progress --total=100000 \
		"Working hard.......2.........3.........4.........5.........6" \
		<in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_expect_success 'progress display breaks long lines #4 - title line matches terminal width' '
	cat >expect <<\EOF &&
Working hard.......2.........3.........4.........5.........6.........7.........:
   25% (25000/100000)<CR>
   50% (50000/100000)<CR>
   75% (75000/100000)<CR>
  100% (100000/100000)<CR>
  100% (100000/100000), done.
EOF

	cat >in <<-\EOF &&
	progress 25000
	progress 50000
	progress 75000
	progress 100000
	EOF
	test-tool progress --total=100000 \
		"Working hard.......2.........3.........4.........5.........6.........7........." \
		<in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

# Progress counter goes backwards, this should not happen in practice.
test_expect_success 'progress shortens - crazy caller' '
	cat >expect <<-\EOF &&
	Working hard:  10% (100/1000)<CR>
	Working hard:  20% (200/1000)<CR>
	Working hard:   0% (1/1000)  <CR>
	Working hard: 100% (1000/1000)<CR>
	Working hard: 100% (1000/1000), done.
	EOF

	cat >in <<-\EOF &&
	progress 100
	progress 200
	progress 1
	progress 1000
	EOF
	test-tool progress --total=1000 "Working hard" <in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_expect_success 'progress display with throughput' '
	cat >expect <<-\EOF &&
	Working hard: 10<CR>
	Working hard: 20, 200.00 KiB | 100.00 KiB/s<CR>
	Working hard: 30, 300.00 KiB | 100.00 KiB/s<CR>
	Working hard: 40, 400.00 KiB | 100.00 KiB/s<CR>
	Working hard: 40, 400.00 KiB | 100.00 KiB/s, done.
	EOF

	cat >in <<-\EOF &&
	throughput 102400 1000
	update
	progress 10
	throughput 204800 2000
	update
	progress 20
	throughput 307200 3000
	update
	progress 30
	throughput 409600 4000
	update
	progress 40
	EOF
	test-tool progress "Working hard" <in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_expect_success 'progress display with throughput and total' '
	cat >expect <<-\EOF &&
	Working hard:  25% (10/40)<CR>
	Working hard:  50% (20/40), 200.00 KiB | 100.00 KiB/s<CR>
	Working hard:  75% (30/40), 300.00 KiB | 100.00 KiB/s<CR>
	Working hard: 100% (40/40), 400.00 KiB | 100.00 KiB/s<CR>
	Working hard: 100% (40/40), 400.00 KiB | 100.00 KiB/s, done.
	EOF

	cat >in <<-\EOF &&
	throughput 102400 1000
	progress 10
	throughput 204800 2000
	progress 20
	throughput 307200 3000
	progress 30
	throughput 409600 4000
	progress 40
	EOF
	test-tool progress --total=40 "Working hard" <in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_expect_success 'cover up after throughput shortens' '
	cat >expect <<-\EOF &&
	Working hard: 1<CR>
	Working hard: 2, 800.00 KiB | 400.00 KiB/s<CR>
	Working hard: 3, 1.17 MiB | 400.00 KiB/s  <CR>
	Working hard: 4, 1.56 MiB | 400.00 KiB/s<CR>
	Working hard: 4, 1.56 MiB | 400.00 KiB/s, done.
	EOF

	cat >in <<-\EOF &&
	throughput 409600 1000
	update
	progress 1
	throughput 819200 2000
	update
	progress 2
	throughput 1228800 3000
	update
	progress 3
	throughput 1638400 4000
	update
	progress 4
	EOF
	test-tool progress "Working hard" <in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_expect_success 'cover up after throughput shortens a lot' '
	cat >expect <<-\EOF &&
	Working hard: 1<CR>
	Working hard: 2, 1000.00 KiB | 1000.00 KiB/s<CR>
	Working hard: 3, 3.00 MiB | 1.50 MiB/s      <CR>
	Working hard: 3, 3.00 MiB | 1024.00 KiB/s, done.
	EOF

	cat >in <<-\EOF &&
	throughput 1 1000
	update
	progress 1
	throughput 1024000 2000
	update
	progress 2
	throughput 3145728 3000
	update
	progress 3
	EOF
	test-tool progress "Working hard" <in 2>stderr &&

	show_cr <stderr >out &&
	test_i18ncmp expect out
'

test_done
