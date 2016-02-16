;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages cross-base)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages hurd)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (cross-binutils
            cross-libc
            cross-gcc))

(define %xgcc
  ;; GCC package used as the basis for cross-compilation.  It doesn't have to
  ;; be 'gcc' and can be a specific variant such as 'gcc-4.8'.
  gcc)

(define (cross p target)
  (package (inherit p)
    (name (string-append (package-name p) "-cross-" target))
    (arguments
     (substitute-keyword-arguments (package-arguments p)
       ((#:configure-flags flags)
        `(cons ,(string-append "--target=" target)
               ,flags))))))

(define (package-with-patch original patch)
  "Return package ORIGINAL with PATCH applied."
  (package (inherit original)
    (source (origin (inherit (package-source original))
              (patches (list patch))))))

(define (cross-binutils target)
  "Return a cross-Binutils for TARGET."
  (let ((binutils (package (inherit binutils)
                    (arguments
                     (substitute-keyword-arguments (package-arguments
                                                    binutils)
                       ((#:configure-flags flags)
                        ;; Build with `--with-sysroot' so that ld honors
                        ;; DT_RUNPATH entries when searching for a needed
                        ;; library.  This works because as a side effect
                        ;; `genscripts.sh' sets `USE_LIBPATH=yes', which tells
                        ;; elf32.em to use DT_RUNPATH in its search list.
                        ;; See <http://sourceware.org/ml/binutils/2013-05/msg00312.html>.
                        ;;
                        ;; In theory choosing / as the sysroot could lead ld
                        ;; to pick up native libs instead of target ones.  In
                        ;; practice the RUNPATH of target libs only refers to
                        ;; target libs, not native libs, so this is safe.
                        `(cons "--with-sysroot=/" ,flags)))))))

    ;; For Xtensa, apply Qualcomm's patch.
    (cross (if (string-prefix? "xtensa-" target)
               (package-with-patch binutils
                                   (search-patch
                                    "ath9k-htc-firmware-binutils.patch"))
               binutils)
           target)))

(define (cross-gcc-arguments target libc)
  "Return build system arguments for a cross-gcc for TARGET, using LIBC (which
may be either a libc package or #f.)"
  ;; Set the current target system so that 'glibc-dynamic-linker' returns the
  ;; right name.
  (parameterize ((%current-target-system target))
    ;; Disable stripping as this can break binaries, with object files of
    ;; libgcc.a showing up as having an unknown architecture.  See
    ;; <http://lists.fedoraproject.org/pipermail/arm/2010-August/000663.html>
    ;; for instance.
    (let ((args `(#:strip-binaries? #f
                  ,@(package-arguments %xgcc))))
     (substitute-keyword-arguments args
       ((#:configure-flags flags)
        `(append (list ,(string-append "--target=" target)
                       ,@(if libc
                             `( ;; Disable libcilkrts because it is not
                                ;; ported to GNU/Hurd. 
                               "--disable-libcilkrts")
                             `( ;; Disable features not needed at this stage.
                               "--disable-shared" "--enable-static"
                               "--enable-languages=c,c++"

                               ;; libstdc++ cannot be built at this stage
                               ;; ("Link tests are not allowed after
                               ;; GCC_NO_EXECUTABLES.").
                               "--disable-libstdc++-v3"

                               "--disable-threads" ;libgcc, would need libc
                               "--disable-libatomic"
                               "--disable-libmudflap"
                               "--disable-libgomp"
                               "--disable-libssp"
                               "--disable-libquadmath"
                               "--disable-decimal-float" ;would need libc
                               "--disable-libcilkrts"

                               ;; When target is any OS other than 'none' these
                               ;; libraries will fail if there is no libc
                               ;; present. See
                               ;; <https://lists.gnu.org/archive/html/guix-devel/2016-02/msg01311.html>
                               "--disable-libitm"
                               "--disable-libvtv"
                               "--disable-libsanitizer"
                               )))

                 ,(if libc
                      flags
                      `(remove (cut string-match "--enable-languages.*" <>)
                               ,flags))))
       ((#:make-flags flags)
        (if libc
            `(let ((libc (assoc-ref %build-inputs "libc")))
               ;; FLAGS_FOR_TARGET are needed for the target libraries to receive
               ;; the -Bxxx for the startfiles.
               (cons (string-append "FLAGS_FOR_TARGET=-B" libc "/lib")
                     ,flags))
            flags))
       ((#:phases phases)
        (let ((phases
               `(alist-cons-after
                 'install 'make-cross-binutils-visible
                 (lambda* (#:key outputs inputs #:allow-other-keys)
                   (let* ((out      (assoc-ref outputs "out"))
                          (libexec  (string-append out "/libexec/gcc/"
                                                   ,target))
                          (binutils (string-append
                                     (assoc-ref inputs "binutils-cross")
                                     "/bin/" ,target "-"))
                          (wrapper  (string-append
                                     (assoc-ref inputs "ld-wrapper-cross")
                                     "/bin/" ,target "-ld")))
                     (for-each (lambda (file)
                                 (symlink (string-append binutils file)
                                          (string-append libexec "/"
                                                         file)))
                               '("as" "nm"))
                     (symlink wrapper (string-append libexec "/ld"))
                     #t))
                 (alist-replace
                  'install
                  (lambda _
                    ;; Unlike our 'strip' phase, this will do the right thing
                    ;; for cross-compilers.
                    (zero? (system* "make" "install-strip")))
                  ,phases))))
          (if libc
              `(alist-cons-before
                'configure 'set-cross-path
                (lambda* (#:key inputs #:allow-other-keys)
                  ;; Add the cross kernel headers to CROSS_CPATH, and remove them
                  ;; from CPATH.
                  (let ((libc  (assoc-ref inputs "libc"))
                        (linux (assoc-ref inputs
                                          "xlinux-headers"))
                        (mach  (assoc-ref inputs
                                          "libc/cross-gnumach-headers"))
                        (hurd  (assoc-ref inputs
                                          "libc/cross-hurd-headers"))
                        (hurd-minimal (assoc-ref inputs
                                                 "libc/cross-hurd-minimal")))
                    (define (cross? x)
                      ;; Return #t if X is a cross-libc or cross Linux.
                      (or (string-prefix? libc x)
                          (if linux        (string-prefix? linux x) #f)
                          (if hurd         (string-prefix? hurd  x) #f)
                          (if mach         (string-prefix? mach  x) #f)
                          (if hurd-minimal (string-prefix? hurd-minimal x) #f)))

                    (setenv "CROSS_CPATH"
                            (string-append libc "/include"
                             (if linux
                                 (string-append ":" linux "/include")
                                 "")
                             (if hurd
                                 (string-append ":" hurd  "/include"
                                                ":" mach  "/include")
                                 "")))
                    (setenv "CROSS_LIBRARY_PATH"
                            (string-append libc "/lib"
                             (if hurd-minimal
                                 (string-append ":" hurd-minimal "/lib")
                                 "")))

                    (let ((cpath   (search-path-as-string->list
                                    (getenv "CPATH")))
                          (libpath (search-path-as-string->list
                                    (getenv "LIBRARY_PATH"))))
                      (setenv "CPATH"
                              (list->search-path-as-string
                               (remove cross? cpath) ":"))
                      (setenv "LIBRARY_PATH"
                              (list->search-path-as-string
                               (remove cross? libpath) ":"))
                      #t)))
                ,phases)
              phases)))))))

(define (cross-gcc-patches target)
  "Return GCC patches needed for TARGET."
  (cond ((string-prefix? "xtensa-" target)
         ;; Patch by Qualcomm needed to build the ath9k-htc firmware.
         (search-patches "ath9k-htc-firmware-gcc.patch"))
        (else '())))

(define* (cross-gcc target
                    #:optional (xbinutils (cross-binutils target)) libc)
  "Return a cross-compiler for TARGET, where TARGET is a GNU triplet.  Use
XBINUTILS as the associated cross-Binutils.  If LIBC is false, then build a
GCC that does not target a libc; otherwise, target that libc."
  
  (define (cross-kernel-headers target libc)
    "Return headers depending on TARGET."
    (match target
      ("i586-pc-gnu"
       (assoc-ref (package-propagated-inputs libc) "hurd-kernel-headers"))
      (_
       (assoc-ref (package-propagated-inputs libc) "linux-headers"))))
  
  (package (inherit %xgcc)
    (name (string-append "gcc-cross-"
                         (if libc "" "sans-libc-")
                         target))
    (source (origin (inherit (package-source %xgcc))
              (patches
               (append
                (origin-patches (package-source %xgcc))
                (cons (search-patch "gcc-cross-environment-variables.patch")
                      (cross-gcc-patches target))))))

    ;; For simplicity, use a single output.  Otherwise libgcc_s & co. are not
    ;; found by default, etc.
    (outputs '("out"))

    (arguments
     `(#:implicit-inputs? #f
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 regex)
                  (srfi srfi-1)
                  (srfi srfi-26))

       ,@(cross-gcc-arguments target libc)))

    (native-inputs
     `(("ld-wrapper-cross" ,(make-ld-wrapper
                             (string-append "ld-wrapper-" target)
                             #:target target
                             #:binutils xbinutils))
       ("binutils-cross" ,xbinutils)

       ;; Call it differently so that the builder can check whether the "libc"
       ;; input is #f.
       ("libc-native" ,@(assoc-ref %final-inputs "libc"))

       ;; Remaining inputs.
       ,@(let ((inputs (append (package-inputs %xgcc)
                               (alist-delete "libc" %final-inputs))))
           (if libc
               `(("libc" ,libc)
                 ("xkernel-headers"                ;the target headers
                  ,@(cross-kernel-headers target libc))
                 ,@inputs)
               inputs))))

    (inputs '())

    ;; Only search target inputs, not host inputs.
    (search-paths
     (list (search-path-specification
            (variable "CROSS_CPATH")
            (files '("include")))
           (search-path-specification
            (variable "CROSS_LIBRARY_PATH")
            (files '("lib" "lib64")))))
    (native-search-paths '())))

(define* (cross-libc target
                     #:optional
                     (xgcc (cross-gcc target))
                     (xbinutils (cross-binutils target)))
  "Return a libc cross-built for TARGET, a GNU triplet.  Use XGCC and
XBINUTILS and the cross tool chain."
  (define xlinux-headers
    (package (inherit linux-libre-headers)
      (name (string-append (package-name linux-libre-headers)
                           "-cross-" target))
      (arguments
       (substitute-keyword-arguments
           `(#:implicit-cross-inputs? #f
             ,@(package-arguments linux-libre-headers))
         ((#:phases phases)
          `(alist-replace
            'build
            (lambda _
              (setenv "ARCH" ,(system->linux-architecture target))
              (format #t "`ARCH' set to `~a' (cross compiling)~%" (getenv "ARCH"))

              (and (zero? (system* "make" "defconfig"))
                   (zero? (system* "make" "mrproper" "headers_check"))))
            ,phases))))
      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(package-native-inputs linux-libre-headers)))))

  (define xgnumach-headers
    (package (inherit gnumach-headers)
      (name (string-append (package-name gnumach-headers)
                           "-cross-" target))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(package-native-inputs gnumach-headers)))))

  (define xmig
    (package (inherit mig)
      (name (string-append "mig-cross"))
      (arguments
       `(#:phases (alist-cons-before
                   'configure 'set-cross-headers-path
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((mach (assoc-ref inputs "cross-gnumach-headers")))
                       (setenv "CROSS_CPATH"
                               (string-append mach "/include"))))
                   %standard-phases)
         #:configure-flags (list ,(string-append "--target=" target))
         ,@(package-arguments mig)))

      (propagated-inputs `(("cross-gnumach-headers" ,xgnumach-headers)))
      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(package-native-inputs mig)))))

  (define xhurd-headers
    (package (inherit hurd-headers)
      (name (string-append (package-name hurd-headers)
                           "-cross-" target))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(alist-delete "mig"(package-native-inputs hurd-headers))))))

  (define xglibc/hurd-headers
    (package (inherit glibc/hurd-headers)
      (name (string-append (package-name glibc/hurd-headers)
                           "-cross-" target))

      (arguments
       (substitute-keyword-arguments (package-arguments glibc/hurd-headers)
         ((#:phases phases)
          `(alist-cons-before
            'pre-configure 'set-cross-headers-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((mach (assoc-ref inputs "gnumach-headers"))
                    (hurd (assoc-ref inputs "hurd-headers")))
                (setenv "CROSS_CPATH"
                        (string-append mach "/include:"
                                       hurd "/include"))))
            ,phases))))

      (propagated-inputs `(("gnumach-headers" ,xgnumach-headers)
                           ("hurd-headers" ,xhurd-headers)))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(alist-delete "mig"(package-native-inputs glibc/hurd-headers))))))

  (define xhurd-minimal
    (package (inherit hurd-minimal)
      (name (string-append (package-name hurd-minimal)
                           "-cross-" target))
      (arguments
       (substitute-keyword-arguments (package-arguments hurd-minimal)
         ((#:phases phases)
          `(alist-cons-before
            'configure 'set-cross-headers-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((glibc-headers (assoc-ref inputs "cross-glibc-hurd-headers")))
                (setenv "CROSS_CPATH"
                        (string-append glibc-headers "/include:"))))
            ,phases))))

      (inputs `(("cross-glibc-hurd-headers" ,xglibc/hurd-headers)))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(alist-delete "mig"(package-native-inputs hurd-minimal))))))

  (define xhurd-kernel-headers
    (package (inherit hurd-kernel-headers)
      (name (string-append (package-name hurd-kernel-headers)
                           "-cross-" target))

      (inputs `(("gnumach-headers" ,xgnumach-headers)
                ("hurd-headers" ,xhurd-headers)
                ("hurd-minimal" ,xhurd-minimal)))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(package-native-inputs hurd-kernel-headers)))))

  ;; Choose libc based on target
  (match target
    ("i586-pc-gnu"
     (package (inherit glibc/hurd)
       (name (string-append "glibc-hurd-cross-" target))
       (arguments
        (substitute-keyword-arguments (package-arguments glibc/hurd)
          ((#:phases phases)
           `(alist-cons-before
             'pre-configure 'set-cross-hurd-headers-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((kernel (assoc-ref inputs "hurd-kernel-headers")))
                 (setenv "CROSS_CPATH"
                         (string-append kernel "/include"))
                 (setenv "CROSS_LIBRARY_PATH"
                         (string-append kernel "/lib:"))))
             ,phases))))

       (propagated-inputs `(("hurd-kernel-headers" ,xhurd-kernel-headers)))

       (native-inputs `(("cross-gcc" ,xgcc)
                        ("cross-binutils" ,xbinutils)
                        ("cross-mig" ,xmig)
                        ,@(alist-delete "mig"(package-native-inputs glibc/hurd))))))
    (_
     (package (inherit glibc)
       (name (string-append "glibc-cross-" target))
       (arguments
        (substitute-keyword-arguments
            `(;; Disable stripping (see above.)
              #:strip-binaries? #f

              ;; This package is used as a target input, but it should not have
              ;; the usual cross-compilation inputs since that would include
              ;; itself.
              #:implicit-cross-inputs? #f

              ,@(package-arguments glibc))
          ((#:configure-flags flags)
           `(cons ,(string-append "--host=" target)
                  ,flags))
          ((#:phases phases)
           `(alist-cons-before
             'configure 'set-cross-linux-headers-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((linux (assoc-ref inputs "linux-headers")))
                 (setenv "CROSS_CPATH"
                         (string-append linux "/include"))
                 #t))
             ,phases))))

       ;; Shadow the native "linux-headers" because glibc's recipe expect the
       ;; "linux-headers" input to point to the right thing.
       (propagated-inputs `(("linux-headers" ,xlinux-headers)))

       ;; FIXME: 'static-bash' should really be an input, not a native input, but
       ;; to do that will require building an intermediate cross libc.
       (inputs '())

       (native-inputs `(("cross-gcc" ,xgcc)
                        ("cross-binutils" ,xbinutils)
                        ,@(package-inputs glibc)     ;FIXME: static-bash
                        ,@(package-native-inputs glibc)))))))


;;;
;;; Concrete cross toolchains.
;;;

(define-public xgcc-mips64el
  (let* ((triplet "mips64el-linux-gnuabi64")      ;N64 ABI
         (xgcc    (cross-gcc triplet
                             (cross-binutils triplet)
                             (cross-libc triplet))))
    ;; Don't attempt to build this cross-compiler on i686;
    ;; see <http://bugs.gnu.org/19598>.
    (package (inherit xgcc)
      (supported-systems (fold delete
                               (package-supported-systems xgcc)
                               '("mips64el-linux" "i686-linux"))))))

(define-public xgcc-avr
  ;; AVR cross-compiler, used to build AVR-Libc.
  (let ((triplet "avr"))
    (cross-gcc triplet
               (cross-binutils triplet))))

(define-public xgcc-xtensa
  ;; Bare-bones Xtensa cross-compiler, used to build the Atheros firmware.
  (cross-gcc "xtensa-elf"))

(define-public xgcc-armhf
  (let* ((triplet "arm-linux-gnueabihf")
         (xgcc    (cross-gcc triplet
                             (cross-binutils triplet)
                             (cross-libc triplet))))
    (package (inherit xgcc)
      (supported-systems (delete "armhf-linux" %supported-systems)))))

;; (define-public xgcc-armel
;;   (let ((triplet "armel-linux-gnueabi"))
;;     (cross-gcc triplet
;;                (cross-binutils triplet)
;;                (cross-libc triplet))))
