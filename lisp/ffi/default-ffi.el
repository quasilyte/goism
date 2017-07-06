;;; -*- lexical-binding: t -*-

(goism-declare
 ;;; <RT>
 ;;; Functions that are required by emacs/rt:
 (error Error (:string format :any &args) :void)
 (eq Eq (:any obj1 :any obj2) :bool)
 (mapconcat MapConcat
            (:any function :object sequence :string separator)
            :string)
 (not Not (:any object) :bool)
 (concat Concat (:any &sequences) :string)
 (aref ArefString (:string array :int idx) :char)
 (aset Aset (:object array :int idx :any newElt) :void)
 (length Length (:any sequence) :int)
 (string-bytes StringBytes (:string s) :int)
 (min MinInt (:int &xs) :int)
 (multibyte-string-p IsMultibyteString (:string object) :bool)
 (booleanp IsBool (:object object) :bool)
 (integerp IsInt (:object object) :bool)
 (floatp IsFloat (:object object) :bool)
 (stringp IsString (:object object) :bool)
 (symbolp IsSymbol (:object object) :bool)
 (prin1-to-string Prin1ToString (:object object) :string)
 )
