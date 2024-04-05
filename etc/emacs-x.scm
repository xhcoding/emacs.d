(define-module (emacs-x)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tree-sitter)
  )

(define-public emacs-git
  (let ((commit "170c6557922dad7e6e9bc0d6dadf6c080108fd42")
        (revision "2"))
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
        (base32 "04carva3b6h9fnlzazrsxsj41hcnjc26kxjij07l159azi40l6sk"))
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

(define (tree-sitter-delete-generated-files grammar-directories)
  #~(begin
      (use-modules (guix build utils))
      (delete-file "binding.gyp")
      (delete-file-recursively "bindings")
      (for-each
       (lambda (lang)
         (with-directory-excursion lang
           (delete-file "src/grammar.json")
           (delete-file "src/node-types.json")
           (delete-file "src/parser.c")
           (delete-file-recursively "src/tree_sitter")))
       '#$grammar-directories)))

(define* (tree-sitter-grammar
          name text hash version
          #:key
          (commit (string-append "v" version))
          (repository-url
           (format #f "https://github.com/tree-sitter/tree-sitter-~a" name))
          (grammar-directories '("."))
          (article "a")
          (inputs '())
          (get-cleanup-snippet tree-sitter-delete-generated-files)
          (license license:expat))
  "Returns a package for Tree-sitter grammar.  NAME will be used with
tree-sitter- prefix to generate package name and also for generating
REPOSITORY-URL value if it's not specified explicitly, TEXT is a string which
will be used in description and synopsis. GET-CLEANUP-SNIPPET is a function,
it recieves GRAMMAR-DIRECTORIES as an argument and should return a G-exp,
which will be used as a snippet in origin."
  (let* ((multiple? (> (length grammar-directories) 1))
         (grammar-names (string-append text " grammar" (if multiple? "s" "")))
         (synopsis (string-append "Tree-sitter " grammar-names))
         (description
          (string-append "This package provides "
                         (if multiple? "" article) (if multiple? "" " ")
                         grammar-names " for the Tree-sitter library."))
         (name (string-append "tree-sitter-" name)))
    (package
      (name name)
      (version version)
      (home-page repository-url)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url repository-url)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32 hash))
                (snippet
                 (get-cleanup-snippet grammar-directories))))
      (build-system tree-sitter-build-system)
      (arguments (list #:grammar-directories grammar-directories))
      (inputs inputs)
      (synopsis synopsis)
      (description description)
      (license license))))

(define-public tree-sitter-elisp
  (let ((commit "e5524fdccf8c22fc726474a910e4ade976dfc7bb")
        (revision "0"))
    (tree-sitter-grammar
     "elisp" "emacs-lisp"
     "1wyzfb27zgpvm4110jgv0sl598mxv5dkrg2cwjw3p9g2bq9mav5d"
     (git-version "1.3.0" revision commit)
     #:repository-url "https://github.com/Wilfred/tree-sitter-elisp"
     #:commit commit)))

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

(define-public my-glibc-locales
  (make-glibc-utf8-locales
   glibc
   #:locales (list "en_US" "zh_CN")
   #:name "glibc-zh-utf8-locales"))
