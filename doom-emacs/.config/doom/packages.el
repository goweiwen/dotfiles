;;; private/default/coq/packages.el -*- lexical-binding: t; -*-

(package! company-coq
  :recipe (:fetcher github
                    :repo "cpitclaudel/company-coq"
                    :files ("*")))
