;; {{ define "public/customization" }}
;; Package related configuration.

(defgroup Go.el nil
  "Configure Go->Emacs Lisp translator."
  :group 'development)

(defcustom Go-utils-path ""
  "Specifies path which is used to find required binaries.
If remain empty, system PATH is used."
  :group 'Go.el
  :type 'directory)

(defcustom Go-emacs-gopath "~/.emacs.d/Go.el"
  "GOPATH used for Emacs Go packages."
  :group 'Go.el
  :type 'directory)

(defcustom Go-output-buffer-name "*Go.el compile*"
  "Temporary buffer name that is used for output."
  :group 'Go.el
  :type 'buffer-name)

;; {{ end }}
