;; Copyright (C) 2015  Joakim Jalap

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is NOT part of any Emacs.

;; This file provides an input method for the programmers dvorak keyboard layout
;; by Roland Kaufman (<http://www.kaufmann.no/roland/dvorak/>).

(quail-define-package
 "programmers-dvorak" "English" "DVP@" t
 "English (ASCII) input method for programmers dvorak layout."
 nil t t t t nil nil nil nil nil t)

;; &%  [7  {5  }3  (1  =9  *0  )2  +4  ]6  !8  #`  $~
;;  ;:  ,<  .>  pP  yY  fF  gG  cC  rR  lL  /?  @^
;;   aA  oO  eE  uU  iI  dD  hH  tT  nN  sS  -_  \|
;;    '"  qQ  jJ  kK  xX  bB  mM  wW  vV  zZ
;;

(quail-define-rules
 ("-" ?!)
 ("=" ?#)
 ("`" ?$)
 ("q" ?\;)
 ("w" ?,)
 ("e" ?.)
 ("r" ?p)
 ("t" ?y)
 ("y" ?f)
 ("u" ?g)
 ("i" ?c)
 ("o" ?r)
 ("p" ?l)
 ("[" ?/)
 ("]" ?@)
 ("a" ?a)
 ("s" ?o)
 ("d" ?e)
 ("f" ?u)
 ("g" ?i)
 ("h" ?d)
 ("j" ?h)
 ("k" ?t)
 ("l" ?n)
 (";" ?s)
 ("'" ?-)
 ("\\" ?\\)
 ("z" ?\')
 ("x" ?q)
 ("c" ?j)
 ("v" ?k)
 ("b" ?x)
 ("n" ?b)
 ("m" ?m)
 ("," ?w)
 ("." ?v)
 ("/" ?z)

 ("_" ?8)
 ("+" ?`)
 ("~" ?~)
 ("Q" ?:)
 ("W" ?<)
 ("E" ?>)
 ("R" ?P)
 ("T" ?Y)
 ("Y" ?F)
 ("U" ?G)
 ("I" ?C)
 ("O" ?R)
 ("P" ?L)
 ("{" ??)
 ("}" ?^)
 ("A" ?A)
 ("S" ?O)
 ("D" ?E)
 ("F" ?U)
 ("G" ?I)
 ("H" ?D)
 ("J" ?H)
 ("K" ?T)
 ("L" ?N)
 (":" ?S)
 ("\"" ?_)
 ("|" ?|)
 ("Z" ?\")
 ("X" ?Q)
 ("C" ?J)
 ("V" ?K)
 ("B" ?X)
 ("N" ?B)
 ("M" ?M)
 ("<" ?W)
 (">" ?V)
 ("?" ?Z)

 ("1" ?&)
 ("2" ?\[)
 ("3" ?{)
 ("4" ?})
 ("5" ?\()
 ("6" ?=)
 ("7" ?\*)
 ("8" ?\))
 ("9" ?+)
 ("0" ?\])

 ("!" ?%)
 ("@" ?7)
 ("#" ?5)
 ("$" ?3)
 ("%" ?1)
 ("^" ?9)
 ("&" ?0)
 ("*" ?2)
 ("(" ?4)
 (")" ?6)
 )

(provide 'programmers-dvorak)
