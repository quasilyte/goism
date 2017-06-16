;; {{ define "public/customization" }}
;; Package related configuration.

(defgroup goism nil
  "Configure Go->Emacs Lisp translator."
  :group 'development)

(defcustom goism-utils-path ""
  "Specifies path which is used to find required binaries.
If remain empty, system PATH is used."
  :group 'goism
  :type 'directory)

(defcustom goism-emacs-gopath "~/.emacs.d/goism"
  "GOPATH used for Emacs Go packages."
  :group 'goism
  :type 'directory)

(defcustom goism-output-buffer-name "*goisl compile*"
  "Temporary buffer name that is used for output."
  :group 'goism
  :type 'buffer-name)

;; {{ end }}
