src_inc += include_directories('.')

nix_src = files(
  join_paths(meson.source_root(), 'src/nix/add-to-store.cc'),
  join_paths(meson.source_root(), 'src/nix/build.cc'),
  join_paths(meson.source_root(), 'src/nix/cat.cc'),
  join_paths(meson.source_root(), 'src/nix/command.cc'),
  join_paths(meson.source_root(), 'src/nix/copy.cc'),
  join_paths(meson.source_root(), 'src/nix/doctor.cc'),
  join_paths(meson.source_root(), 'src/nix/dump-path.cc'),
  join_paths(meson.source_root(), 'src/nix/edit.cc'),
  join_paths(meson.source_root(), 'src/nix/eval.cc'),
  join_paths(meson.source_root(), 'src/nix/hash.cc'),
  join_paths(meson.source_root(), 'src/nix/installables.cc'),
  join_paths(meson.source_root(), 'src/nix/legacy.cc'),
  join_paths(meson.source_root(), 'src/nix/log.cc'),
  join_paths(meson.source_root(), 'src/nix/ls.cc'),
  join_paths(meson.source_root(), 'src/nix/main.cc'),
  join_paths(meson.source_root(), 'src/nix/optimise-store.cc'),
  join_paths(meson.source_root(), 'src/nix/path-info.cc'),
  join_paths(meson.source_root(), 'src/nix/ping-store.cc'),
  join_paths(meson.source_root(), 'src/nix/progress-bar.cc'),
  join_paths(meson.source_root(), 'src/nix/repl.cc'),
  join_paths(meson.source_root(), 'src/nix/run.cc'),
  join_paths(meson.source_root(), 'src/nix/search.cc'),
  join_paths(meson.source_root(), 'src/nix/show-config.cc'),
  join_paths(meson.source_root(), 'src/nix/show-derivation.cc'),
  join_paths(meson.source_root(), 'src/nix/sigs.cc'),
  join_paths(meson.source_root(), 'src/nix/upgrade-nix.cc'),
  join_paths(meson.source_root(), 'src/nix/verify.cc'),
  join_paths(meson.source_root(), 'src/nix/why-depends.cc'),

  join_paths(meson.source_root(), 'src/build-remote/build-remote.cc'),
  join_paths(meson.source_root(), 'src/nix-build/nix-build.cc'),
  join_paths(meson.source_root(), 'src/nix-channel/nix-channel.cc'),
  join_paths(meson.source_root(), 'src/nix-collect-garbage/nix-collect-garbage.cc'),
  join_paths(meson.source_root(), 'src/nix-copy-closure/nix-copy-closure.cc'),
  join_paths(meson.source_root(), 'src/nix-daemon/nix-daemon.cc'),
  join_paths(meson.source_root(), 'src/nix-env/nix-env.cc'),
  join_paths(meson.source_root(), 'src/nix-env/user-env.cc'),
  join_paths(meson.source_root(), 'src/nix-instantiate/nix-instantiate.cc'),
  join_paths(meson.source_root(), 'src/nix-prefetch-url/nix-prefetch-url.cc'),
  join_paths(meson.source_root(), 'src/nix-store/dotgraph.cc'),
  join_paths(meson.source_root(), 'src/nix-store/graphml.cc'),
  join_paths(meson.source_root(), 'src/nix-store/nix-store.cc'))

nix_headers = files (
  join_paths(meson.source_root(), 'src/nix/command.hh'),
  join_paths(meson.source_root(), 'src/nix/legacy.hh'),
  join_paths(meson.source_root(), 'src/nix/progress-bar.hh'),
  join_paths(meson.source_root(), 'src/nix-env/user-env.hh'),
  join_paths(meson.source_root(), 'src/nix-store/dotgraph.hh'),
  join_paths(meson.source_root(), 'src/nix-store/graphml.hh'))

nix_dep_list = [
  boost_dep,
  editline_dep,
  gc_dep,
  glog_dep,
  libdl_dep,
  libsodium_dep,
  pthread_dep,
]

nix_link_list = [
  libutil_lib,
  libstore_lib,
  libmain_lib,
  libexpr_lib,
]

nix_bin = executable(
  'nix',
  install : true,
  install_mode : 'rwxr-xr-x',
  install_dir : bindir,
  include_directories : src_inc,
  sources : nix_src,
  link_with : nix_link_list,
  dependencies : nix_dep_list)
