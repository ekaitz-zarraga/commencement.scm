# RISC-V commencement

This `commencement.scm` describes a bootstrapping path for RISC-V for Guix.

It was started from the `gnu/packages/commencement.scm` file from Guix, but in
order to make it work for RISC-V many packages have been replaced, and the
bootstrapping path has been heavily edited.

## Main differences

There are several differences with the current Guix bootstrap process that have
to be taken in account when merging in upstream Guix.

- This new path introduces `musl` as the `glibc` versions that can be built
  with the early compilers we have available don't support RISC-V.

- The GCC-4 used in the project was forked from GCC 4.6.4 and RISC-V support
  was ported to it. The project [is accessible here][gcc-4].

- The file does not describe a full bootstrap process, but enough to reach a
  modern GCC, which should be able to build the world later.

- The first steps in the process have been already merged in Guix's
  `core-updates` branch. This part is split in two different `make` packages
  for RISC-V and x86. After it was decided to continue only with RISC-V as the
  path diverged too much to keep them both in this file.

[gcc-4]: https://github.com/ekaitz-zarraga/gcc


## Current issues

- The bootstrapping path is **not** compatible with `x86`, `arm` or other
  arches yet [^efforts].

- `gash` is removed from the bootstrapping path temporarily (see TODO's in the
  file) because a bug hangs the process. It is reported upstream.

- `flex` and `bison` are not bootstrapped yet as they require some extra
  work[^flex-bison].

- The final GCC package does not support C++ yet, as it requires extra work for
  Guix compatibility (mostly configure paths), but has been proven to work
  manually in live-bootstrap.

[^efforts]: There are efforts for including `x86` support in this bootstrapping
    path. See <https://gitlab.com/janneke/commencement.scm.git>

[^flex-bison]: They have been manually bootstrapped in live-bootstrap, so this
    work is doable.

## How to run

``` shell
guix build -L . -e '(@@ (commencement) gcc-muslboot-9)' --no-grafts --system=riscv64-linux -K
```

Should build the whole bootstrap chain until a GCC-9.

## Related projects

- Live-bootstrap: <https://github.com/fosslinux/live-bootstrap>
