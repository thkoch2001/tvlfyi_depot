package PlAvif;

use strict;
use warnings;

sub GetDependencies
{
	return ('emCore');
}

sub IsEssential
{
	return 0;
}

sub GetFileHandlingrules
{
	return ();
}

sub GetExtraBuildOptions
{
	return ();
}

sub Build
{
	shift;
	my %options=@_;

	my @libAvifFlags=();
	if ($options{'avif-inc-dir'} eq '' && $options{'avif-lib-dir'} eq '') {
		@libAvifFlags=split("\n",readpipe(
			"perl \"".$options{'utils'}."/PkgConfig.pl\" libavif"
		));
	}
	if (!@libAvifFlags) {
		if ($options{'avif-inc-dir'} ne '') {
			push(@libAvifFlags, "--inc-search-dir", $options{'avif-inc-dir'});
		}
		if ($options{'avif-lib-dir'} ne '') {
			push(@libAvifFlags, "--lib-search-dir", $options{'avif-lib-dir'});
		}
		push(@libAvifFlags, "--link", "avif");
	}

	system(
		@{$options{'unicc_call'}},
		"--math",
		"--rtti",
		"--exceptions",
		"--bin-dir"       , "bin",
		"--lib-dir"       , "lib",
		"--obj-dir"       , "obj",
		"--inc-search-dir", "include",
		@libAvifFlags,
		"--link"          , "emCore",
		"--type"          , "dynlib",
		"--name"          , "PlAvif",
		"src/PlAvif.cpp"
	)==0 or return 0;

	return 1;
}
