;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2018, 2019, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020, 2021, 2022, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2022 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022, 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (commencement)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages musl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module ((guix store) #:select (%store-monad))
  #:use-module (guix monads)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix git-download) #:select (git-reference git-file-name))
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; This is the commencement, this is where things start.  Before the
;;; commencement, of course, there's the 'bootstrap' module, which provides us
;;; with the initial binaries.  This module uses those bootstrap binaries to
;;; actually build up the whole tool chain that make up the implicit inputs of
;;; 'gnu-build-system'.
;;;
;;; To avoid circular dependencies, this module should not be imported
;;; directly from anywhere.
;;;
;;; Below, we frequently use "inherit" to create modified packages.  The
;;; reason why we use "inherit" instead of "package/inherit" is because we do
;;; not want these commencement packages to inherit grafts.  By definition,
;;; these packages are not depended on at run time by any of the packages we
;;; use.  Thus it does not make sense to inherit grafts.  Furthermore, those
;;; grafts would often lead to extra overhead for users who would end up
;;; downloading those "-boot0" packages just to build package replacements
;;; that are in fact not going to be used.
;;;
;;; Code:

(define* (git-fetch-from-tarball tarball)
  "Return an <origin> method equivalent to 'git-fetch', except that it fetches
the checkout from TARBALL, a tarball containing said checkout.

  The purpose of this procedure is to work around bootstrapping issues:
'git-fetch' depends on Git, which is much higher in the dependency graph."
  (lambda* (url hash-algo hash
                #:optional name
                #:key (system (%current-system))
                (guile %bootstrap-guile))
    (mlet %store-monad ((guile (package->derivation guile system)))
      (gexp->derivation
       (or name "git-checkout")
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils)
                          (ice-9 ftw)
                          (ice-9 match))
             (setenv "PATH"
                     #+(file-append %bootstrap-coreutils&co "/bin"))
             (invoke "tar" "xf" #$tarball)
             (match (scandir ".")
               (("." ".." directory)
                (copy-recursively directory #$output)))))
       #:recursive? #t
       #:hash-algo hash-algo
       #:hash hash
       #:system system
       #:guile-for-build guile
       #:graft? #f
       #:local-build? #t))))

(define bootar
  (package
    (name "bootar")
    (version "1b")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "mirror://gnu/guix/mirror/bootar-" version ".ses")
                         (string-append
                          "https://files.ngyro.com/bootar/bootar-"
                          version ".ses")))
              (sha256
               (base32
                "0cf5vj5yxfvkgzvjvh2l7b2nz5ji5l534n9g4mfp8f5jsjqdrqjc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:imported-modules ((guix build gnu-bootstrap)
                           ,@%gnu-build-system-modules)
       #:phases
       (begin
         (use-modules (guix build gnu-bootstrap))
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((source (assoc-ref inputs "source"))
                      (guile-dir (assoc-ref inputs "guile"))
                      (guile (string-append guile-dir "/bin/guile")))
                 (invoke guile "--no-auto-compile" source)
                 (chdir "bootar"))))
           (replace 'configure (bootstrap-configure "Bootar" ,version
                                                    '(".") "scripts"))
           (replace 'build (bootstrap-build '(".")))
           (replace 'install (bootstrap-install '(".") "scripts"))))))
    (inputs `(("guile" ,%bootstrap-guile)))
    (home-page "https://git.ngyro.com/bootar")
    (synopsis "Tar decompression and extraction in Guile Scheme")
    (description "Bootar is a simple Tar extractor written in Guile
Scheme.  It supports running 'tar xvf' on uncompressed tarballs or
tarballs that are compressed with BZip2, GZip, or XZ.  It also provides
standalone scripts for 'bzip2', 'gzip', and 'xz' that each support
decompression to standard output.

What makes this special is that Bootar is distributed as a
self-extracting Scheme (SES) program.  That is, a little script that
outputs the source code of Bootar.  This makes it possible to go from
pure Scheme to Tar and decompression in one easy step.")
    (license license:gpl3+)))

(define gash-boot
  (package
    (inherit gash)
    (name "gash-boot")
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:imported-modules ((guix build gnu-bootstrap)
                           ,@%gnu-build-system-modules)
       #:phases
       (begin
         (use-modules (guix build gnu-bootstrap))
         (modify-phases %standard-phases
           (replace 'configure
             (bootstrap-configure "Gash" ,(package-version gash)
                                  '("gash") "scripts"))
           (replace 'build (bootstrap-build '("gash")))
           (replace 'install (bootstrap-install '("gash") "scripts"))
           (add-after 'install 'install-symlinks
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (symlink (string-append out "/bin/gash")
                          (string-append out "/bin/sh"))
                 (symlink (string-append out "/bin/gash")
                          (string-append out "/bin/bash")))))))))
    (inputs `(("guile" ,%bootstrap-guile)))
    (native-inputs `(("bootar" ,bootar)))))

(define gash-utils-boot
  (package
    (inherit gash-utils)
    (name "gash-utils-boot")
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:imported-modules ((guix build gnu-bootstrap)
                           ,@%gnu-build-system-modules)
       #:phases
       (begin
         (use-modules (guix build gnu-bootstrap))
         (modify-phases %standard-phases
           (add-after 'unpack 'set-load-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((gash (assoc-ref inputs "gash")))
                 (add-to-load-path (string-append gash "/share/guile/site/"
                                                  (effective-version))))))
           (add-before 'configure 'pre-configure
             (lambda _
               (format #t "Creating gash/commands/testb.scm~%")
               (copy-file "gash/commands/test.scm"
                          "gash/commands/testb.scm")
               (substitute* "gash/commands/testb.scm"
                 (("gash commands test") "gash commands testb")
                 (("apply test [(]cdr") "apply test/bracket (cdr"))
               (for-each (lambda (script)
                           (let ((target (string-append "scripts/"
                                                        script ".in")))
                             (format #t "Creating scripts/~a~%" target)
                             (copy-file "scripts/template.in" target)
                             (substitute* target
                               (("@UTILITY@") script))))
                         '("awk" "basename" "cat" "chmod" "cmp" "command"
                           "compress" "cp" "cut" "diff" "dirname" "env"
                           "expr" "false" "find" "grep" "head" "ln" "ls"
                           "mkdir" "mv" "printf" "pwd" "reboot" "rm" "rmdir"
                           "sed" "sleep" "sort" "tar" "test" "touch" "tr"
                           "true" "uname" "uniq" "wc" "which"))
               (format #t "Creating scripts/[.in~%")
               (copy-file "scripts/template.in" "scripts/[.in")
               (substitute* "scripts/[.in"
                 (("@UTILITY@") "testb"))
               (delete-file "scripts/template.in")))
           (replace 'configure
             (bootstrap-configure "Gash-Utils" ,(package-version gash-utils)
                                  '("gash" "gash-utils") "scripts"))
           (replace 'build (bootstrap-build '("gash" "gash-utils")))
           (replace 'install
             (bootstrap-install '("gash" "gash-utils") "scripts"))
           ;; XXX: The scripts should add Gash to their load paths and
           ;; this phase should not exist.
           (add-after 'install 'copy-gash
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (moddir (string-append out "/share/guile/site/"
                                             (effective-version)))
                      (godir (string-append out "/lib/guile/"
                                            (effective-version)
                                            "/site-ccache"))
                      (gash (assoc-ref inputs "gash"))
                      (gash-moddir (string-append gash "/share/guile/site/"
                                                  (effective-version)))
                      (gash-godir (string-append gash "/lib/guile/"
                                                 (effective-version)
                                                 "/site-ccache")))
                 (copy-file (string-append gash-moddir "/gash/compat.scm")
                            (string-append moddir "/gash/compat.scm"))
                 (copy-recursively (string-append gash-moddir "/gash/compat")
                                   (string-append moddir "/gash/compat"))
                 (copy-file (string-append gash-godir "/gash/compat.go")
                            (string-append godir "/gash/compat.go"))
                 (copy-recursively (string-append gash-godir "/gash/compat")
                                   (string-append godir "/gash/compat")))))
           ;; We need an external echo.
           (add-after 'install 'make-echo
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (gash (assoc-ref inputs "gash")))
                 (with-output-to-file (string-append out "/bin/echo")
                   (lambda ()
                     (display (string-append "#!" gash "/bin/gash\n"))
                     (newline)
                     (display "echo \"$@\"")
                     (newline)))
                 (chmod (string-append out "/bin/echo") #o755))))))))
    (inputs `(("gash" ,gash-boot)
              ("guile" ,%bootstrap-guile)))
    (native-inputs `(("bootar" ,bootar)))))

(define (%boot-gash-inputs)
  `(("bash" , bash)                ; gnu-build-system wants "bash" TODO: we use bash because GASH hangs
    ("coreutils" , gash-utils-boot)
    ("bootar" ,bootar)
    ("guile" ,%bootstrap-guile)))

(define stage0-posix
  ;; The initial bootstrap package: no binary inputs except those from
  ;; `bootstrap-seeds, for x86 a 357 byte binary seed: `x86/hex0-seed'.
    (package
      (name "stage0-posix")
      (version "1.6.0")
      (source (origin
                (method url-fetch)
                (uri (string-append
                       "https://github.com/oriansj/" name "/releases/download/"
                       "Release_" version "/" name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "0p06wn95y6xbp2kcd81h2fm3wxvldd1qqyxgav0farl34xlzyq4j"))))
      (supported-systems '("i686-linux" "x86_64-linux"
                           "aarch64-linux"
                           "riscv64-linux"))
      (native-inputs (%boot-gash-inputs))
      (build-system trivial-build-system)
      (arguments
       (list
        #:guile %bootstrap-guile
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let* ((source #$(package-source this-package))
                   (tar #$(this-package-native-input "bootar"))
                   (bash #$(this-package-native-input "bash"))
                   (coreutils #$(this-package-native-input "coreutils"))
                   (guile #$(this-package-input "guile"))
                   (out #$output)
                   (bindir (string-append out "/bin"))
                   (target (or #$(%current-target-system)
                               #$(%current-system)))
                   (stage0-cpu
                    (cond
                     ((or #$(target-x86-64?) #$(target-x86-32?))
                      "x86")
                     (#$(target-aarch64?)
                      "AArch64")
                     (#$(target-riscv64?)
                      "riscv64")
                     (else
                      (error "stage0-posix: system not supported" target))))
                   (kaem (string-append "bootstrap-seeds/POSIX/"
                                        stage0-cpu "/kaem-optional-seed")))
              (setenv "PATH" (string-append tar "/bin:"
                                            coreutils "/bin:"
                                            bash "/bin"))
              (invoke "tar" "xvf" source)
              (chdir (string-append "stage0-posix-" #$version))
              (mkdir-p bindir)
              ;; Keep the same capitalization between the file name and the folder.
              (rename-file "kaem.aarch64" "kaem.AArch64")
              (invoke kaem (string-append "kaem." stage0-cpu))
              (with-directory-excursion (string-append stage0-cpu "/bin")
                (install-file "hex2" bindir)
                (install-file "M1" bindir)
                (install-file "blood-elf" bindir)
                (install-file "kaem" bindir)
                (install-file "get_machine" bindir)
                (install-file "M2-Planet" bindir))))))
      (home-page "https://github.com/oriansj/stage0-posix/")
      (synopsis "The initial bootstrap package, builds stage0 up to M2-Planet")
      (description "Starting from the 357-byte hex0-seed binary provided by
the bootstrap-seeds, the stage0-posix package first builds hex0 and then all
the way up: hex1, catm, hex2, M0, cc_x86, M1, M2, get_machine (that's all of
MesCC-Tools), and finally M2-Planet.")
      (license license:gpl3+)))


(define mes-boot
  (package
    (inherit mes)
    (name "mes-boot")
    (version "wip")
    (source
      (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ekaitz-zarraga/mes/")
                       (commit version)
                       (recursive? #t)))
                (sha256
                  (base32
                    "1ibkdkdwhgn4pypfzd0q0bf37hw77k8mxkc2biz6hrb0k6l1vmgh")))
      #;(origin
              (method url-fetch)
              (uri (list (string-append "mirror://gnu/mes/"
                                   "mes-" version ".tar.gz")
                         (string-append "https://lilypond.org/janneke/mes/"
                                        "mes-" version ".tar.gz")))
              (sha256
               (base32
                "03np6h4qx94givjdvq2rmhvab38y5f91254n0avg4vq2j0cx78in"))))
    (inputs '())
    (propagated-inputs '())
    (supported-systems '("i686-linux" "x86_64-linux" "riscv64-linux"))
    (native-inputs
     `(("m2-planet" ,stage0-posix)
       ("nyacc-source" ,(bootstrap-origin
                         (origin (inherit (package-source nyacc-1.00.2))
                                 (snippet #f))))
       ,@(%boot-gash-inputs)))
    (arguments
     (list
      #:implicit-inputs? #f
      #:tests? #f
      #:guile %bootstrap-guile
      #:strip-binaries? #f              ;no strip yet
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-seeds
            (lambda _
              (let ((nyacc-source #$(this-package-native-input "nyacc-source")))
                (with-directory-excursion ".."
                  (invoke "tar" "-xvf" nyacc-source)))))
          (replace 'configure
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out #$output)
                    (gash #$(this-package-native-input "bash"))
                    (dir (with-directory-excursion ".." (getcwd))))
                (setenv "GUILE_LOAD_PATH" (string-append
                                           dir "/nyacc-1.00.2/module"))
                (invoke "bash" "configure.sh" ; TODO: using bash because of the gash hanging problem
                        (string-append "--prefix=" out)
                        "--host=" #$(or (%current-target-system)
                                        (%current-system))))))
          (replace 'build
            (lambda _
              ;; TODO: GUILE_LOAD_PATH is leaking. We need to clean it.
              (substitute* "kaem.run"
                (("cp bin/mes-m2 bin/mes" all)
                 (string-append "GUILE_LOAD_PATH=/fubar\n" all)))
              (invoke "bash" "bootstrap.sh"))) ; TODO: using bash because of the gash hanging problem
          (delete 'check)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "install.sh" ; show some progress
                ((" -xf") " -xvf")
                (("^( *)((cp|mkdir|tar) [^']*[^\\])\n" all space cmd)
                 (string-append space "echo '" cmd "'\n"
                                space cmd "\n")))
              (invoke "bash" "install.sh") ; TODO: using bash because of the gash hanging problem
              ;; Keep ASCII output, for friendlier comparison and bisection
              (let* ((out #$output)
                     (cache (string-append out "/lib/cache")))
                (define (objects-in-dir dir)
                  (find-files dir
                              (lambda (name stat)
                                (and (equal? (dirname name) dir)
                                     (or (string-suffix? ".M1" name)
                                         (string-suffix? ".hex2" name)
                                         (string-suffix? ".o" name)
                                         (string-suffix? ".s" name))))))
                (for-each (lambda (x) (install-file x cache))
                          (append (objects-in-dir "m2")
                                  (objects-in-dir ".")
                                  (objects-in-dir "mescc-lib")))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))
           (search-path-specification
            (variable "MES_PREFIX")
            (separator #f)
            (files '("")))))))


(define tcc-boot0
  ;; Pristine tcc cannot be built by MesCC, we are keeping a delta of 30
  ;; patches.  In a very early and rough form they were presented to the
  ;; TinyCC developers, who at the time showed no interest in supporting the
  ;; bootstrappable effort; we will try again later.  These patches have been
  ;; ported to 0.9.27, alas the resulting tcc is buggy.  Once MesCC is more
  ;; mature, this package should use the 0.9.27 sources (or later).
  (package
    (inherit tcc)
    (name "tcc-boot0")
    ;(version "0.9.26-1150-ga0de0ae4")
    (version "riscv-mes")
    (source  (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ekaitz-zarraga/tcc/")
                       (commit version)
                       (recursive? #t)))
                (sha256
                  (base32
                    "0fiml0yk257c3qhzwsainlfvhzrql9nkvpjjjwa865xk828pq08k")))
            #;(origin
              (method url-fetch)
              (uri (list
                    (string-append "mirror://gnu/guix/mirror/"
                                   "tcc-" version ".tar.gz")
                    (string-append "https://lilypond.org/janneke/tcc/"
                                   "tcc-" version ".tar.gz")))
              (sha256
               (base32
                "1b8wrf6w4ygwpcg63v9lcrqllnhnpa026fhr67fsq2s666qxwzwq"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux" "riscv64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs
     `(("mes" ,mes-boot)
       ("mescc-tools" ,stage0-posix)
       ("nyacc-source" ,(bootstrap-origin
                         (origin (inherit (package-source nyacc-1.00.2))
                                 (snippet #f))))
       ,@(%boot-gash-inputs)))
    (arguments
     (list
      #:implicit-inputs? #f
      #:guile %bootstrap-guile
      #:validate-runpath? #f            ; no dynamic executables
      #:strip-binaries? #f              ; no strip yet
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-extra-sources
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((nyacc-source #$(this-package-native-input "nyacc-source")))
                (with-directory-excursion ".."
                  (invoke "tar" "-xvf" nyacc-source)))))
          (replace 'configure
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out #$output)
                     (dir (with-directory-excursion ".." (getcwd)))
                     (interpreter "/lib/mes-loader")
                     (mes #$(this-package-native-input "mes"))
                     (mescc (string-append mes "/bin/mescc")))
                (substitute* "conftest.c"
                  (("volatile") ""))
                (setenv "prefix" out)
                (setenv "GUILE_LOAD_PATH"
                        (string-append dir "/nyacc-1.00.2/module"))
                (setenv "ONE_SOURCE" "true")
                (invoke "sh" "configure"
                        "--cc=mescc"
                        (string-append "--prefix=" out)
                        (string-append "--elfinterp=" interpreter)
                        "--crtprefix=."
                        "--tccdir=."))))
          (replace 'build
            (lambda _
              (substitute* "bootstrap.sh" ; Show some progress
                (("^( *)((cp|ls|mkdir|rm|[.]/tcc|[.]/[$][{program_prefix[}]tcc) [^\"]*[^\\])\n" all space cmd)
                 (string-append space "echo \"" cmd "\"\n"
                                space cmd "\n")))
              (invoke "sh" "bootstrap.sh")))
          (replace 'check
            (lambda _
              ;; fail fast tests
              (system* "./tcc" "--help") ; --help exits 1
              ;; (invoke "sh" "test.sh" "mes/scaffold/tests/30-strlen")
              ;; (invoke "sh" "-x" "test.sh" "mes/scaffold/tinycc/00_assignment")
              ;; TODO: add sensible check target (without depending on make)
              ;; (invoke "sh" "check.sh")
              ))
          (replace 'install
            (lambda _
              (substitute* "install.sh" ; Show some progress
                (("^( *)((cp|ls|mkdir|rm|tar|./[$][{PROGRAM_PREFIX[}]tcc) [^\"]*[^\\])\n" all space cmd)
                 (string-append space "echo \"" cmd "\"\n"
                                space cmd "\n")))

              (invoke "sh" "install.sh"))))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))))))

(define gzip-mesboot
  ;; The initial gzip.  We keep this scripted gzip build before building make
  ;; to soften the dependency on Gash Core Utils gzip.
  (package
    (inherit gzip)
    (version "1.2.4")
    (name "gzip-mesboot")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gzip/gzip-" version ".tar"))
              (sha256
               (base32
                "1rhgk2vvmdvnn6vygf0dja92ryyng00knl0kz5srb77k2kryjb2d"))))
    (supported-systems '("i686-linux" "x86_64-linux" "riscv64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("tcc" ,tcc-boot0)
                     ,@(%boot-gash-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "util.c"
               (("^char [*]strlwr" all) (string-append all "_tcc_cannot_handle_dupe")))))
         (replace 'build
           (lambda _
             (let ((files '("bits" "crypt" "deflate" "getopt" "gzip"
                            "inflate" "lzw" "trees" "unlzh" "unlzw"
                            "unpack" "unzip" "util" "zip")))
               (define (compile x)
                 (invoke "tcc" "-c" "-D NO_UTIME=1" "-D HAVE_UNISTD_H=1"
                         (string-append x ".c")))
               (for-each compile files)
               (apply invoke
                      (cons* "tcc" "-o" "gzip"
                             (map (lambda (x) (string-append x ".o")) files)))
               (link "gzip" "gunzip"))))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "gzip" bin)
               (install-file "gunzip" bin))))
         (replace 'check
           (lambda _
             (invoke "./gzip" "--version")))
         ;; no gzip yet
         (delete 'compress-documentation))))))

(define gnu-make-mesboot0
  ;; The initial make
  (package
    (inherit gnu-make)
    (name "make-mesboot0")
    (version "3.80")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/make/make-" version ".tar.gz"))
              (sha256
               (base32
                "1pb7fb7fqf9wz9najm85qdma1xhxzf1rhj5gwrlzdsz2zm0hpcv4"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("tcc" ,tcc-boot0)
                     ,@(%boot-gash-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("CC=tcc"
                           "CPP=tcc -E"
                           "LD=tcc"
                           ;; TODO This is not enough to make the changes on MesLibC
                           ;; work. It's still detecting we have newer getdents and
                           ;; fails in x86_64
                           "--build=i686-unknown-linux-gnu"
                           "--host=i686-unknown-linux-gnu"
                           "--disable-nls")
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "build.sh.in"
               (("@LIBOBJS@") "getloadavg.o")
               (("@REMOTE@") "stub"))))
         (add-after 'configure 'configure-fixup
           (lambda _
             (substitute* "make.h"
               (("^extern long int lseek.*" all) (string-append "// " all)))))
         (replace 'build
           (lambda _
             (invoke "sh" "./build.sh")))
         (replace 'check                ; proper check needs awk
           (lambda _
             (invoke "./make" "--version")))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "make" bin)))))))))

(define gnu-make-mesboot0-riscv64
  ;; The initial make
  ;; It's special for riscv as it doesn't know how to `configure` it, we need to do
  ;; it ourselves.
  (package
    (inherit gnu-make-mesboot0)
    (name "make-mesboot0-riscv64")
    (version "3.82")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/make/make-" version ".tar.gz"))
              (sha256
               (base32
                "1rs2f9hmvy3q6zkl15jnlmnpgffm0bhw5ax0h5c7q604wqrip69x"))))
    (supported-systems '("riscv64-linux"))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "build.sh.in"
               (("@LIBOBJS@") "getloadavg.o")
               (("@REMOTE@") "stub"))))
         (add-after 'configure 'configure-fixup
           (lambda _
             (substitute* "make.h"
               (("^extern long int lseek.*" all) (string-append "// " all)))))
         (replace 'configure (lambda _
           (call-with-output-file "config.h" (lambda (p) (display "" p)))
           (call-with-output-file "putenv_stub.c"
            (lambda (p) (display "int putenv(char *string) { return 0; }" p)))))
         (replace 'build
           (lambda _
            (invoke "tcc" "-c" "-g" "-DNO_FLOAT" "getopt.c")
            (invoke "tcc" "-c" "-g" "-DNO_FLOAT" "getopt1.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DHAVE_FCNTL_H" "arscan.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DFILE_TIMESTAMP_HI_RES=0" "commands.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DSCCS_GET=\"/nullop\"" "default.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "expand.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DFILE_TIMESTAMP_HI_RES=0" "file.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-Dvfork=fork" "function.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "implicit.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DHAVE_DUP2" "-DHAVE_STRCHR" "-Dvfork=fork"
                    "job.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DLOCALEDIR=\"/fake-locale\""
                    "-DPACKAGE=\"fake-make\"" "-DHAVE_MKTEMP" "-DHAVE_GETCWD"
                    "main.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DHAVE_STRERROR" "-DHAVE_VPRINTF"
                    "-DHAVE_ANSI_COMPILER" "-DHAVE_STDARG_H" "misc.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DFILE_TIMESTAMP_HI_RES=0" "-DHAVE_FCNTL_H"
                    "-DLIBDIR=\"/lib\"" "remake.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "rule.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "signame.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "strcache.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "variable.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "vpath.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "hash.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "remote-stub.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-Iglob" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DHAVE_STDINT_H" "ar.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-Iglob" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DHAVE_DIRENT_H" "dir.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-Iglob" "-DHAVE_INTTYPES_H"
                    "-DHAVE_SA_RESTART" "-DINCLUDEDIR=\"/include\"" "read.c")
            (invoke "tcc" "-c" "-g" "-I." "-DNO_FLOAT" "-DVERSION=\"3.82\""
                    "version.c")
            (invoke "tcc" "-c" "-g" "-DNO_FLOAT" "-DHAVE_FCNTL_H" "getloadavg.c")
            (invoke "tcc" "-c" "-g" "-DNO_FLOAT" "-Iglob" "-DSTDC_HEADERS"
                    "glob/fnmatch.c")
            (invoke "tcc" "-c" "-g" "-DNO_FLOAT" "-Iglob" "-DHAVE_STRDUP"
                    "-DHAVE_DIRENT_H" "glob/glob.c")
            (invoke "tcc" "-c" "-g" "-DNO_FLOAT" "putenv_stub.c")
            (invoke "tcc" "-g" "-static" "-o" "make" "getopt.o" "getopt1.o" "ar.o"
                    "arscan.o" "commands.o" "default.o" "dir.o" "expand.o" "file.o"
                    "function.o" "implicit.o" "job.o" "main.o" "misc.o" "read.o"
                    "remake.o" "rule.o" "signame.o" "strcache.o" "variable.o"
                    "version.o" "vpath.o" "hash.o" "remote-stub.o" "getloadavg.o"
                    "fnmatch.o" "glob.o" "putenv_stub.o")))
         (replace 'check                ; proper check needs awk
           (lambda _
             (invoke "./make" "--version")))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "make" bin)))))))))

(define (%boot-tcc0-inputs)
  `(("make" ,(cond
              ((target-riscv64?) gnu-make-mesboot0-riscv64)
              (else gnu-make-mesboot0)))
    ("tcc" ,tcc-boot0)
    ,@(%boot-gash-inputs)))

(define (tcc-system)
  (cond
    ((target-x86-32?) "i386")
    ((target-x86-64?) "x86_64")
    ((target-aarch64?) "aarch64")
    ((target-riscv64?) "riscv64")))

(define (mes-system)
  (cond
    ((target-x86-32?) "x86")
    ((target-x86-64?) "x86_64")
    ((target-aarch64?) "aarch64")
    ((target-riscv64?) "riscv64")))

(define tcc-boot
  ;; The final tcc.
  (package
    (inherit tcc-boot0)
    (name "tcc-boot")
    (version "mob")
    (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ekaitz-zarraga/tcc/")
                       (commit version)
                       (recursive? #t)))
                (sha256
                  (base32
                    "031fs6nl123qfy76hfcc16g7jp3islmvdbx8by0yzjz6qqw2g4av")))
      #;(origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/tinycc/tcc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "177bdhwzrnqgyrdv1dwvpd04fcxj68s5pm1dzwny6359ziway8yy"))))
    (build-system gnu-build-system)
    (inputs '())
    (propagated-inputs '())
    ;; We don't need "make" for this anymore
    (native-inputs (modify-inputs (%boot-tcc0-inputs) (delete "make")))

    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:validate-runpath? #f           ; no dynamic executables
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'make-static
           (lambda _
             (substitute* "libtcc.c"
               (("s->nocommon = 1;" all)
                (string-append all " s->static_link = 1;")))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (call-with-output-file
                 "config.h"
                 (lambda (port)
                   (display "#define TCC_VERSION \"0.9.28rc\" " port)))))

         (replace 'build
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tcc (assoc-ref inputs "tcc"))
                    (libc (assoc-ref inputs "libc"))
                    (interpreter "/mes/loader"))
               (invoke
                "tcc"
                "-g"
                "-vvv"
                "-D" "ONE_SOURCE=1"
                "-D" (string-append "TCC_TARGET_" (string-upcase ,(tcc-system)) "=1")
                "-D" "TCC_VERSION=\"0.9.28rc\""
                "-D" "CONFIG_TCC_STATIC=1"
                "-D" "CONFIG_USE_LIBGCC=1"
                "-D" "CONFIG_TCC_SEMLOCK=0"
                "-D" (string-append "CONFIG_TCCDIR=\"" out "/lib/tcc\"")
                "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" out "/lib:{B}/lib:.\"")
                "-D" (string-append "CONFIG_TCC_ELFINTERP=\"" interpreter "\"")
                "-D" (string-append "CONFIG_TCC_LIBPATHS=\"" tcc "/lib" ":"
                                                             out "/lib" ":"
                                                             "{B}/lib:.\"")
                "-D" (string-append "CONFIG_TCC_SYSINCLUDEPATHS=\"" tcc "/include" ":"
                                                                    out "/include" ":"
                                                                    "{B}/include\"")
                "-D" (string-append "TCC_LIBGCC=\"" tcc "/lib/libc.a\"")
                "-o" "tcc"
                "tcc.c"))))

         (add-after 'build 'build-libtcc1.a
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (invoke "./tcc"
                     "-g" "-vvv"
                     "-I" (string-append "include")
                     "-D" (string-append "TCC_TARGET_" (string-upcase ,(tcc-system)) "=1")
                     "-c" "-o" "libtcc1.o" "lib/libtcc1.c")
             (cond
               (,(or (target-aarch64?) (target-riscv64?))
                 (invoke "./tcc"
                         "-g" "-vvv"
                         "-I" (string-append "include")
                         "-D" (string-append "TCC_TARGET_" (string-upcase ,(tcc-system)) "=1")
                         "-c" "-o" "lib-arm64.o" "lib/lib-arm64.c")
                 (invoke "./tcc" "-ar" "rc" "libtcc1.a" "libtcc1.o" "lib-arm64.o"))
               (else
                 (invoke "./tcc" "-ar" "rc" "libtcc1.a" "libtcc1.o")))))

        (add-after 'build-libtcc1.a 'rebuild-libc.a
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tcc (assoc-ref inputs "tcc")))
               (invoke "./tcc" "-g" "-vvv"
                       "-c" "-o" "libc.o"
                       "-I" (string-append tcc "/include")
                       "-I" (string-append tcc "/include/linux/" ,(mes-system))
                       "-I" "include"
                       (string-append tcc "/share/libc.c"))
               (invoke "./tcc" "-ar" "rc" "libc.a" "libc.o"))))

        (add-after 'rebuild-libc.a 'rebuild-libgetopt.a
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (tcc (assoc-ref inputs "tcc")))
              (invoke "./tcc" "-g" "-vvv"
                      "-c" "-o" "libgetopt.o"
                      "-I" (string-append tcc "/include")
                      "-I" (string-append tcc "/include/linux/" ,(mes-system))
                      "-I" "include"
                      (string-append tcc "/share/libgetopt.c"))
              (invoke "./tcc" "-ar" "rc" "libgetopt.a" "libgetopt.o"))))

        (add-after 'rebuild-libgetopt.a 'rebuild-crts
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (tcc (assoc-ref inputs "tcc")))
              (invoke "./tcc" "-g" "-vvv"
                      "-c" "-o" "crt1.o"
                      "-I" (string-append tcc "/include")
                      "-I" (string-append tcc "/include/linux/" ,(mes-system))
                      "-I" "include"
                      (string-append tcc "/share/crt1.c"))
              ;; These are empty
              (copy-file (string-append tcc "/lib/crti.o") "crti.o")
              (copy-file (string-append tcc "/lib/crtn.o") "crtn.o"))))

        (replace 'check
           (lambda _
             ;; FIXME: add sensible check target (without depending on make)
             ;; ./check.sh ?
             (= 1 (status:exit-val (system* "./tcc" "--help")))))

        (replace 'install
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (tcc (assoc-ref inputs "tcc")))
              (mkdir-p (string-append out "/bin"))
              (copy-file "tcc" (string-append out "/bin/tcc"))
              (copy-recursively (string-append "include")
                                (string-append out "/include"))
              (mkdir-p (string-append out "/lib/tcc/"))
              (copy-file "libtcc1.a" (string-append out "/lib/libtcc1.a"))
              (copy-file "libtcc1.a" (string-append out "/lib/tcc/libtcc1.a")))))

        (add-after 'install 'install-libtcc1.a
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out")))
              (mkdir-p (string-append out "/lib/tcc"))
              (copy-file "libtcc1.a" (string-append out "/lib/libtcc1.a"))
              (copy-file "libtcc1.a" (string-append out "/lib/tcc/libtcc1.a")))))

        (add-after 'install-libtcc1.a 'install-libc.a
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out")))
              (mkdir-p (string-append out "/lib"))
              (copy-file "libc.a" (string-append out "/lib/libc.a")))))

        (add-after 'install-libc.a 'install-libgetopt.a
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out")))
              (mkdir-p (string-append out "/lib"))
              (copy-file "libgetopt.a" (string-append out "/lib/libgetopt.a")))))

        (add-after 'install-libc.a 'install-crts
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (tcc (assoc-ref inputs "tcc")))
              (and
               (mkdir-p (string-append out "/lib"))
               (copy-file "crt1.o" (string-append out "/lib/crt1.o"))
               (copy-file "crti.o" (string-append out "/lib/crti.o"))
               (copy-file "crtn.o" (string-append out "/lib/crtn.o"))))))

        (add-after 'install-crts 'install-extras
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (tcc (assoc-ref inputs "tcc")))
              (and
               ;; Install from previous tcc
               (copy-recursively (string-append tcc "/share")
                                 (string-append out "/share"))
               (copy-recursively (string-append tcc "/include")
                                 (string-append out "/include"))
               (delete-file-recursively (string-append out "/share/doc")))))))))))

(define patch-mesboot
  ;; The initial patch.
  (package
    (inherit patch)
    (name "patch-mesboot")
    (version "2.5.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/patch/patch-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "12nv7jx3gxfp50y11nxzlnmqqrpicjggw6pcsq0wyavkkm3cddgc"))))
    (supported-systems '("i686-linux" "x86_64-linux" "riscv64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:tests? #f            ; check is naive, also checks non-built PROGRAMS
       #:strip-binaries? #f   ; no strip yet
       #:configure-flags '("AR=tcc -ar" "CC=tcc" "LD=tcc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             ;; avoid another segfault
             (substitute* "pch.c"
               (("while [(]p_end >= 0[)]" all)
                "p_end = -1;\nwhile (0)"))))
         ;; FIXME: no compressing gzip yet
         (delete 'compress-documentation))))))

(define (%boot-tcc-inputs)
  `(("gzip" ,gzip-mesboot)
    ("patch" ,patch-mesboot)
    ,@(modify-inputs (%boot-tcc0-inputs)
                     (replace "tcc" tcc-boot))))


(define musl-boot0
  (package
    (inherit musl)
    (name "musl-boot0")
    (version "1.1.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.musl-libc.org/releases/"
                                  "musl-" version ".tar.gz"))
              (sha256
               (base32
                "18r2a00k82hz0mqdvgm7crzc7305l36109c0j9yjmkxj2alcjw0k"))))
    (native-inputs (%boot-tcc-inputs))
    (arguments
     (list
       #:tests? #f                      ; musl has no tests
       #:guile %bootstrap-guile
       #:implicit-inputs? #f
       #:strip-binaries? #f
       #:make-flags
       #~(list
           (string-append "SHELL=" #$(this-package-native-input "bash")
                          "/bin/bash")
           "CC=tcc"
           "AR=tcc -ar"
           "RANLIB=true"
           "CFLAGS=-DSYSCALL_NO_TLS -D__riscv_float_abi_soft -U__riscv_flen")
       #:configure-flags
       #~(let ((bash #$(this-package-native-input "bash")))
           (list "CC=tcc"
                 (string-append "CONFIG_SHELL=" bash "/bin/sh")
                 "--disable-shared"
                 "--disable-gcc-wrapper"))
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'build 'add-libg
             (lambda _
               (substitute* "Makefile"
                   (("(EMPTY_LIB_NAMES = )(.*)" all var content)
                    (string-append var "g " content)))))
           (add-before 'build 'patch-shebang-in-makefile
             (lambda _
               (let ((bash #$(this-package-native-input "bash")))
                 (substitute* "Makefile"
                   (("#!/bin/sh") (string-append "#!" bash "/bin/bash"))))))
           (add-after 'configure 'remove-complex
             (lambda _
               (delete-file-recursively "src/complex"))))))))

(define tcc-boot-musl
  (package
    (inherit tcc-boot)
    (name "tcc-boot-musl")
    (native-inputs `(("libc" ,musl-boot0)
                     ,@(modify-inputs (package-native-inputs tcc-boot)
                                      (replace "tcc" tcc-boot))))
    (arguments
      (substitute-keyword-arguments (package-arguments tcc-boot)
        ((#:phases phases)
         #~(modify-phases #$phases
             (delete 'rebuild-libc.a)
             (delete 'install-libc.a)
             (delete 'rebuild-libgetopt.a)
             (delete 'install-libgetopt.a)
             (delete 'rebuild-crts)
             (delete 'install-crts)
             (delete 'install-extras)
             (replace 'build
               (lambda* (#:key outputs inputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (libc (assoc-ref inputs "libc"))
                        (tcc (assoc-ref inputs "tcc"))
                        (interpreter "/musl/loader"))
                   (invoke
                    "tcc"
                    "-g"
                    "-vvv"
                    "-I" (string-append tcc "/include")
                    "-L" (string-append tcc "/lib")
                    "-D" "ONE_SOURCE=1"
                    "-D" (string-append "TCC_TARGET_" (string-upcase #$(tcc-system)) "=1")
                    "-D" "TCC_VERSION=\"0.9.28rc\""
                    "-D" "CONFIG_TCC_STATIC=1"
                    "-D" "CONFIG_USE_LIBGCC=1"
                    "-D" "CONFIG_TCC_SEMLOCK=0"
                    "-D" (string-append "CONFIG_TCCDIR=\"" out "/lib/tcc\"")
                    "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" libc "/lib\"")
                    "-D" (string-append "CONFIG_TCC_ELFINTERP=\"" interpreter "\"")
                    "-D" (string-append "CONFIG_TCC_LIBPATHS=\"" libc "/lib" ":"
                                                                 out "/lib" ":"
                                                                 "{B}/lib:.\"")
                    "-D" (string-append "CONFIG_TCC_SYSINCLUDEPATHS=\"" libc "/include" ":"
                                                                        out "/include" ":"
                                                                        "{B}/include\"")
                    "-D" (string-append "TCC_LIBGCC=\"" libc "/lib/libc.a\"")
                    "-o" "tcc"
                    "tcc.c"))))))))))

(define tcc-musl
  (package
    (inherit tcc-boot-musl)
    (name "tcc-musl")
    (native-inputs (modify-inputs (package-native-inputs tcc-boot-musl)
                                  (replace "tcc"  tcc-boot-musl)))
    (arguments
      (substitute-keyword-arguments (package-arguments tcc-boot-musl)
        ((#:phases phases)
         #~(modify-phases #$phases
             (replace 'build
               (lambda* (#:key outputs inputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (libc (assoc-ref inputs "libc"))
                        (interpreter "/musl/loader"))
                   (invoke
                    "tcc"
                    "-g"
                    "-vvv"
                    "-D" "REG_PC=0"
                    "-D" "REG_S0=8"
                    "-D" "ONE_SOURCE=1"
                    "-D" (string-append "TCC_TARGET_" (string-upcase #$(tcc-system)) "=1")
                    "-D" "TCC_VERSION=\"0.9.28rc\""
                    "-D" "CONFIG_TCC_STATIC=1"
                    "-D" "CONFIG_USE_LIBGCC=1"
                    "-D" "CONFIG_TCC_SEMLOCK=0"
                    "-D" (string-append "CONFIG_TCCDIR=\"" out "/lib/tcc\"")
                    "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" libc "/lib\"")
                    "-D" (string-append "CONFIG_TCC_ELFINTERP=\"" interpreter "\"")
                    "-D" (string-append "CONFIG_TCC_LIBPATHS=\"" libc "/lib" ":"
                                                                 out "/lib" ":"
                                                                 "{B}/lib:.\"")
                    "-D" (string-append "CONFIG_TCC_SYSINCLUDEPATHS=\"" libc "/include" ":"
                                                                        out "/include" ":"
                                                                        "{B}/include\"")
                    "-D" (string-append "TCC_LIBGCC=\"" libc "/lib/libc.a\"")
                    "-o" "tcc"
                    "tcc.c"))))))))))

(define (%boot-tcc-musl-inputs)
  (modify-inputs (%boot-tcc-inputs)
    (replace "tcc" tcc-musl)))


(define binutils-muslboot0
  ;; The initial Binutils
  (package
    (inherit binutils)
    (name "binutils-muslboot0")
    (version "2.30")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/binutils/binutils-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1sp9g7zrrcsl25hxiqzmmcrdlbm7rbmj0vki18lks28wblcm0f4c")))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc-musl-inputs))
    (supported-systems '("i686-linux" "x86_64-linux" "riscv64-linux"))
    (arguments
     (list #:implicit-inputs? #f
           #:guile %bootstrap-guile
           #:tests? #f ; runtest: command not found
           #:parallel-build? #f
           #:strip-binaries? #f ; no strip yet
           #:phases
           #~(modify-phases %standard-phases
             (add-after 'configure 'fix-build
               (lambda _
                 ;; Meslibc doesn't have wchar.h
                 (substitute* "gas/read.c"
                   (("#include \"wchar.h\"") ""))
                 ;; bfd/po doesn't have a Makefile, so the recursive calls just
                 ;; fail. We add files with the same name Make targets have, to
                 ;; trick Make into thinking there's nothing to do.
                 (call-with-output-file "bfd/po/install"
                   (lambda (p) (display "" p)))
                 (call-with-output-file "bfd/po/all"
                   (lambda (p) (display "" p)))
                 (call-with-output-file "bfd/po/info"
                   (lambda (p) (display "" p))))))
           #:configure-flags
           #~(let ((bash (assoc-ref %build-inputs "bash")))
               `(,(string-append "CONFIG_SHELL=" bash "/bin/sh")
                 "CFLAGS=-g"
                 "CC=tcc"
                 "LD=tcc"
                 "AR=tcc -ar"
                 "MAKEINFO=true"
                 "RANLIB=true"
                 "--enable-64-bit-bfd"
                 "--disable-nls"
                 "--enable-static"
                 "--disable-shared"
                 "--disable-werror"
                 "--disable-plugins"
                 "--enable-deterministic-archives"
                 "--with-sysroot=/"
                 ,(string-append "--build="
                                #$(cond
                                    ((target-x86-64?) "i686-linux-gnu")
                                    (#t (platform-system->target
                                          (%current-system)))))
                 ,(string-append "--host="
                          #$(cond
                              ((target-x86-64?) "i686-linux-gnu")
                              (#t (platform-system->target
                                    (%current-system)))))))))))


(define (%boot-muslboot0-inputs)
  `(("kernel-headers" ,%bootstrap-linux-libre-headers)
    ("libc" ,musl-boot0)
    ("binutils" ,binutils-muslboot0)
    ,@(%boot-tcc-musl-inputs)))


(define m4-boot
  (package
    (inherit m4)
    (name "m4-boot")
    (version "1.4.7")
    (source (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/m4/m4-" version ".tar.gz"))
      (sha256
       (base32 "00w3dp8l819x44g4r7b755qj8rch9rzqiky184ga2qzmcwvrjg09"))))
    (native-inputs (%boot-muslboot0-inputs))
    (arguments (list
       #:tests? #f    ;; They fail but we'll be fine XXX: check why they fail (tcc?)
       #:guile %bootstrap-guile
       #:implicit-inputs? #f
       #:strip-binaries? #f
       #:configure-flags #~(list "CC=tcc" "AR=ar" "AS=as")))))

(define (%boot-multiprecision-inputs)
  `(("m4" ,m4-boot)
    ("libc" ,musl-boot0)
    ("binutils" ,binutils-muslboot0)
    ,@(%boot-tcc-musl-inputs)))


(define gmp-boot
  (package
    (inherit gmp)
    (name "gmp-boot")
    (version "6.3.0")
    (source (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gmp/gmp-" version ".tar.gz"))
      (sha256
       (base32 "0k5jkamdsjdcq9fcglqhc7bbw5hwnqa5m8asanh342c1fsfxavz5"))))
    (native-inputs (%boot-multiprecision-inputs))
    (arguments (list
       #:guile %bootstrap-guile
       #:tests? #f
       #:implicit-inputs? #f
       #:strip-binaries? #f
       #:configure-flags #~(list "CC=tcc"
                                 "CFLAGS=-DHAVE_ALLOCA_H"
                                 "--enable-static"
                                 "--disable-shared"
                                 "--disable-assembly")))))

(define mpfr-boot
  (package
    (inherit mpfr)
    (name "mpfr-boot")
    (version "4.1.0")
    (source (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/mpfr/mpfr-" version ".tar.gz"))
      (sha256
       (base32 "1mm2zxjqxxqlacd87cxlyi63pwrxwafqks7lmpqa3wqq6a0zw9ri"))))
    (native-inputs (%boot-multiprecision-inputs))
    (propagated-inputs (list gmp-boot))
    (arguments (list
       #:guile %bootstrap-guile
       #:tests? #f
       #:implicit-inputs? #f
       #:strip-binaries? #f
       #:configure-flags #~(list "CC=tcc"
                                 "CFLAGS=-DHAVE_ALLOCA_H"
                                 "--enable-static"
                                 "--disable-shared"
                                 "--disable-assembly")))))

(define mpc-boot
  (package
    (inherit mpc)
    (name "mpc-boot")
    (version "1.2.1")
    (source (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/mpc/mpc-" version ".tar.gz"))
      (sha256
       (base32 "0n846hqfqvmsmim7qdlms0qr86f1hck19p12nq3g3z2x74n3sl0p"))))
    (native-inputs (%boot-multiprecision-inputs))
    (propagated-inputs (list gmp-boot mpfr-boot))
    (arguments (list
       #:guile %bootstrap-guile
       #:tests? #f
       #:implicit-inputs? #f
       #:strip-binaries? #f
       #:configure-flags #~(list "CC=tcc"
                                 "CFLAGS=-DHAVE_ALLOCA_H"
                                 "--enable-static"
                                 "--disable-shared"
                                 "--disable-assembly")))))

(define flex-boot
  (package
    (inherit flex)
    (version "2.5.39")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://github.com/westes/flex"
                     "/releases/download/v" version "/"
                     "flex-" version ".tar.gz"))
              (sha256
                (base32
                  "0j49664wfmm2bvsdxxcaf1835is8kq0hr0sc20km14wc2mc1ppbi"))))
    (native-inputs (%boot-multiprecision-inputs))
    (arguments (list
       #:guile %bootstrap-guile
       #:tests? #f
       #:implicit-inputs? #f
       #:strip-binaries? #f
       #:configure-flags #~(list "CC=tcc"
                                 "CC_FOR_BUILD=tcc"
                                 "CFLAGS=-DHAVE_ALLOCA_H"
                                 "--disable-shared"
                                 "--enable-static")))))


(define gcc-muslboot0
  (package
    (inherit gcc)
    (name "gcc-muslboot0")
    (version "4.6.4.1")
    (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ekaitz-zarraga/gcc/")
                       (commit (string-append "releases/gcc-" version))))
                (sha256
                  (base32
                    "1sdg5d6nkj5jlqxisal8rgxi0dwbhwyz228598w5lc1a0m3ydq2g"))))
    (supported-systems '("i686-linux" "x86_64-linux" "riscv64-linux"))
    (inputs (list flex   ;; TODO: bootstrap me
                  bison  ;; TODO: bootstrap me
                  gmp-boot
                  mpfr-boot
                  mpc-boot))
    (outputs '("out"))
    (native-inputs (%boot-muslboot0-inputs))
    (arguments
     (list #:implicit-inputs? #f
           #:guile %bootstrap-guile
           #:tests? #f
           #:modules '((guix build gnu-build-system)
                       (guix build utils)
                       (srfi srfi-1))
           #:parallel-build? #t
           #:configure-flags
           #~(let ((out  (assoc-ref %outputs "out"))
                   (musl (assoc-ref %build-inputs "libc")))
               (list (string-append "--prefix=" out)
                     ;(string-append "--build=" #$(%current-system)) ;; TODO: add -musl
                     ;(string-append "--host="  #$(%current-system)) ;; TODO: add -musl
                     "--build=riscv64-unknown-linux-musl"
                     "--build=riscv64-unknown-linux-musl"
                     (string-append "--with-build-sysroot=" musl "/include")
                     "--disable-bootstrap"
                     "--disable-decimal-float"
                     "--disable-libatomic"
                     "--disable-libcilkrts"
                     "--disable-libgomp"
                     "--disable-libitm"
                     "--disable-libmudflap"
                     "--disable-libquadmath"
                     "--disable-libsanitizer"
                     "--disable-libssp"
                     "--disable-libvtv"
                     "--disable-lto"
                     "--disable-lto-plugin"
                     "--disable-multilib"
                     "--disable-plugin"
                     "--disable-threads"
                     "--enable-languages=c"
                     "--enable-static"
                     "--disable-shared"
                     "--enable-threads=single"
                     "--disable-libstdcxx-pch"
                     "--disable-build-with-cxx"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-alloca
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (list "libiberty/alloca.c"
                                      "include/libiberty.h")
                     (("C_alloca") "alloca"))))
               (add-after 'unpack 'patch-for-modern-libc
                 (lambda _
                   (for-each
                     (lambda (dir)
                       (substitute* (string-append "gcc/config/"
                                                   dir "/linux-unwind.h")
                                    (("struct ucontext") "ucontext_t")))
                     '("alpha" "bfin" "i386" "pa" "sh" "xtensa" "riscv"))))
               (add-before 'configure 'setenv
                 (lambda* (#:key outputs inputs #:allow-other-keys)
                   (let* ((out      (assoc-ref outputs "out"))
                          (binutils (assoc-ref inputs "binutils"))
                          (bash     (assoc-ref inputs "bash"))
                          (tcc      (assoc-ref inputs "tcc"))
                          (musl     (assoc-ref inputs "libc")))
                     (setenv "CC" "tcc")
                     (setenv "AR" "ar")
                     (setenv "AS" "as")
                     (setenv "CFLAGS" "-D HAVE_ALLOCA_H")
                     (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                     (setenv "C_INCLUDE_PATH" (string-append (or (getenv "C_INCLUDE_PATH") "")
                                               ":" musl "/include"))
                     (setenv "LIBRARY_PATH" (string-append (or (getenv "LIBRARY_PATH") "")
                                                           ":" musl "/lib"
                                                           ":" tcc "/lib"))
                     (format (current-error-port) "C_INCLUDE_PATH=~a\n" (getenv "C_INCLUDE_PATH"))
                     (format (current-error-port) "LIBRARY_PATH=~a\n"
                             (getenv "LIBRARY_PATH"))))))))
    (native-search-paths
      (list (search-path-specification
              (variable "C_INCLUDE_PATH")
              (files '("include")))
            (search-path-specification
              (variable "LIBRARY_PATH")
              (files '("lib")))))))

(define musl-boot
  (package
    (inherit musl)
    (name "musl-boot")
    (native-inputs `(("gcc" ,gcc-muslboot0)
                   ,@(modify-inputs (package-native-inputs musl-boot0)
                                    (delete "tcc")
                                    (append binutils-muslboot0))))
    (arguments
     (substitute-keyword-arguments (package-arguments musl-boot0)
       ((#:guile _) %bootstrap-guile)
       ((#:implicit-inputs _) #f)
       ((#:make-flags _)
        #~(list (string-append "SHELL=" #$(this-package-native-input "bash")
                          "/bin/bash")
                "RANLIB=true"))
       ((#:configure-flags _)
        #~(list (string-append "CONFIG_SHELL="
                               #$(this-package-native-input "bash")
                               "/bin/sh")
                "CC=gcc"
                "--disable-shared"))))))


(define gnu-make-muslboot
  (package
    (inherit gnu-make-mesboot0-riscv64)
    (name "make-muslboot")
    (native-inputs `(("gcc" ,gcc-muslboot0)
                     ("libc" ,musl-boot)
                   ,@(modify-inputs (package-native-inputs musl-boot0)
                                    (delete "tcc")
                                    (append binutils-muslboot0))))
    (arguments
      (substitute-keyword-arguments (package-arguments gnu-make-mesboot0-riscv64)
        ((#:guile _) %bootstrap-guile)
        ((#:implicit-inputs _) #f)
        ((#:phases _) #~%standard-phases)))))


(define gcc-muslboot
  ;; GCC 4.6.4 is the latest modular distribution. We backported RISC-V support
  ;; here.
  (package
    (inherit gcc-muslboot0)
    (name "gcc-muslboot")
    (native-inputs `(("gcc" ,gcc-muslboot0)
                     ("libc" ,musl-boot)
                     ("find" ,findutils) ; TODO: bootstrap me
                     ,@(modify-inputs (package-native-inputs gcc-muslboot0)
                                      (replace "make" gnu-make-muslboot)
                                      (delete "libc")
                                      (delete "tcc"))))
    (arguments
     (substitute-keyword-arguments (package-arguments gcc-muslboot0)
       ((#:configure-flags flags)
        #~(let ((out   (assoc-ref %outputs "out"))
                (musl  (assoc-ref %build-inputs "libc")))
               (list (string-append "--prefix=" out)
                     ;; We need -musl suffix here for libstdc++ not to use some
                     ;; glibc specific optimizations
                     (string-append "--build=" "riscv64-unknown-linux-musl") ; TODO: generalize for every arch
                     (string-append "--host="  "riscv64-unknown-linux-musl") ; TODO: generalize for every arch
                     (string-append "--with-build-sysroot=" musl "/include")
                     "--disable-bootstrap"
                     "--disable-decimal-float"
                     "--disable-libatomic"
                     "--disable-libcilkrts"
                     "--disable-libgomp"
                     "--disable-libitm"
                     "--disable-libmudflap"
                     "--disable-libquadmath"
                     "--disable-libsanitizer"
                     "--disable-libssp"
                     "--disable-libvtv"
                     "--disable-lto"
                     "--disable-lto-plugin"
                     "--disable-multilib"
                     "--disable-plugin"
                     "--disable-threads"
                     "--enable-languages=c,c++"
                     "--enable-static"
                     "--disable-shared"
                     "--enable-threads=single"
                     "--disable-libstdcxx-pch"
                     "--disable-build-with-cxx")))
       ((#:phases phases)
        #~(modify-phases #$phases
          (replace 'setenv
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out      (assoc-ref outputs "out"))
                     (binutils (assoc-ref inputs "binutils"))
                     (bash     (assoc-ref inputs "bash"))
                     (gcc      (assoc-ref inputs "gcc"))
                     (musl     (assoc-ref inputs "libc")))
                (setenv "CC" "gcc")
                (setenv "AR" "ar")
                (setenv "AS" "as")
                (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                (setenv "CPLUS_INCLUDE_PATH" (string-append (or (getenv "C_INCLUDE_PATH") "")
                                          ":" musl "/include"))
                (setenv "C_INCLUDE_PATH" (string-append (or (getenv "C_INCLUDE_PATH") "")
                                          ":" musl "/include"))
                (setenv "LIBRARY_PATH" (string-append (or (getenv "LIBRARY_PATH") "")
                                                      ":" musl "/lib"
                                                      ":" gcc "/lib")))))))))
   (native-search-paths
      (list (search-path-specification
              (variable "C_INCLUDE_PATH")
              (files '("include")))
            (search-path-specification
              (variable "CPLUS_INCLUDE_PATH")
              (files '("include/c++" "include")))
            (search-path-specification
              (variable "LIBRARY_PATH")
              (files '("lib")))))))


(define (%boot-muslboot-inputs)
  `(("gcc" ,gcc-muslboot)
    ,@(modify-inputs (%boot-muslboot0-inputs)
                     (replace "make" gnu-make-muslboot)
                     (replace "libc" musl-boot)
                     (delete "tcc"))))

(define hello-muslboot
  ;; Check for Scheme-only bootstrap.  Note that newer versions of Hello
  ;; break due to the way that newer versions of Gnulib handle
  ;; "limits.h".  Hence, we stick to 2.10.
  (package
    (inherit hello)
    (name "hello-muslboot")
    (version "2.10")
    (source
     (origin
       (inherit (package-source hello))
       (uri (string-append "mirror://gnu/hello/hello-" version
                           ".tar.gz"))
       (sha256
        (base32
         "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-muslboot-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       ;; checking for grep that handles long lines and -e...
       ;; configure: error: no acceptable grep could be found
       #:configure-flags '("ac_cv_path_GREP=grep")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./hello"))))))))
