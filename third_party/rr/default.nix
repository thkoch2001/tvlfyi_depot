{ pkgs, ... }:

pkgs.originals.rr.overrideAttrs(_: {
  patches = [
    ./0001-PerfCounters-don-t-exit-on-AMD-ThreadRipper-3960X.patch
  ];
})
