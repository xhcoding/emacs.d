(define-module (emacs-x)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages tls)
  )

(define-public emacs-git
  (let ((commit "7a7491a23eacaae41c07568d833e668ec1d351cf")
        (revision "1"))
   (package
    (inherit emacs)
    (name "emacs-git")
    (version (git-version "30.0.50" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-mirror/emacs.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pf1jz02l3h6da3bw07lg82nxf765s6rrr0r6r779p2ncir37n0p"))
       (patches
        (search-patches "emacs-next-exec-path.patch"
                        "emacs-fix-scheme-indent-function.patch"
                        "emacs-next-native-comp-driver-options.patch"
                        "emacs-pgtk-super-key-fix.patch"))))
    (arguments
     (substitute-keyword-arguments
      (package-arguments emacs)
      ((#:configure-flags flags #~'())
       #~(cons* "--with-native-compilation=no" (delete "--with-native-compilation=aot" #$flags))))))))


(define (parent-directory file)
  "Return FILE's parent directory.
FILE should be an absolute file name."
  ;; Trim trailing slash as FILE may be "/tmp/foo" or "/tmp/foo/".
  (let* ((file       (string-trim-right file #\/))
         (last-slash (string-rindex file #\/))
         (parent     (and last-slash
                          (substring file 0 last-slash))))
    ;; For "/foo" and "/" the parent is "/".
    (if (or (not parent)
            (string=? "" parent))
        "/"
        parent)))

(define %source-dir (parent-directory (dirname (current-filename))))

(define (skip-git-and-build-directory file stat)
  "Skip the `.git` and `build` and `guix_profile` directory when collecting the sources."
  (let ((name (basename file)))
    (not (or (string=? name ".git")
             (string=? name "build")
             (string-prefix? "guix_profile" name)))))

(define-public emacs-config
  (package
   (name "emacs-config")
   (version "1.0.0")
   (source (local-file %source-dir
                       #:recursive? #t
                       #:select? skip-git-and-build-directory))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan '(("." "./share/emacs.d"))))
   (home-page "https://github.com/xhcoding/.emacs.d")
   (synopsis "Minimal emacs config for programing")
   (description "Minimal emacs config for programing.")
   (license license:gpl3+)))

