;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phonset for ara_norm
;;;

;;;  Feeel free to add new feature values, or new features to this
;;;  list to make it more appropriate to your language

;; This is where it'll fall over if you haven't defined a 
;; a phoneset yet, if you have, delete this, if you haven't
;; define one then delete this error message
;;(error "You have not yet defined a phoneset for norm (and others things ?)\n            Define it in festvox/ara_norm_nawar_phoneset.scm\n")

(defPhoneSet
  ara_norm
  ;;;  Phone Features
  (;; vowel or consonant
   ;;(clst + - 0)
   (vc + - 0)
   ;; vowel length: short long dipthong schwa
   (vlng s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 0 -)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 0 -)
   ;; lip rounding
   (vrnd + - 0)
   ;; consonant type: stop fricative affricative nasal liquid approximant
   (ctype s f a n l r 0)
   ;; place of articulation: labial alveolar palatal
   ;; labio-dental dental velar glottal
   (cplace l a p b d v g 0)
   ;; consonant voicing
   (cvox + - 0)
   ;;(asp  + - 0)
   ;;(nuk + - 0)
   )
  (
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (<   -   0   0   0   0   0   0   -) ;; hamza(ء) لا يهم اذا كانت فوق الف او واو او ياء او وحدها المهم صوتها))
   (<<  -   0   0   0   0   0   0   -) ;; hamza+shada

   (b   -   0   0   0   0   0   0   -) ;; ba(ب)
   (bb  -   0   0   0   0   0   0   -) ;; ba+shada

   (t   -   0   0   0   0   0   0   -) ;;ta(ت)
   (tt  -   0   0   0   0   0   0   -) ;;ta+shada

   (^   -   0   0   0   0   0   0   -) ;; ^a(ث)
   (^^  -   0   0   0   0   0   0   -) ;; ^a+ishala

   (j   -   0   0   0   0   0   0   -) ;;jim(ج)
   (jj  -   0   0   0   0   0   0   -) ;;jim+shada

   (H   -   0   0   0   0   0   0   -) ;; 7a(ح)
   (HH  -   0   0   0   0   0   0   -) ;; 7a+shada

   (x   -   0   0   0   0   0   0   -) ;;xa(خ)
   (xx  -   0   0   0   0   0   0   -) ;;xa+shada

   (d   -   0   0   0   0   0   0   -) ;; dal(د)
   (dd  -   0   0   0   0   0   0   -) ;; dal+shada

   (*   -   0   0   0   0   0   0   -) ;; thal(ذ)
   (**  -   0   0   0   0   0   0   -) ;; dal+shada

   (r   -   0   0   0   0   0   0   -) ;;ra(ر)
   (rr  -   0   0   0   0   0   0   -) ;;ra+shada

   (z   -   0   0   0   0   0   0   -) ;;za(ز)
   (zz  -   0   0   0   0   0   0   -) ;;za+shada

   (s   -   0   0   0   0   f   a   -) ;;sin(س)
   (ss  -   0   0   0   0   0   0   -) ;;sin+shada

   ($   -   0   0   0   0   0   0   -) ;; shin(ش)
   ($$  -   0   0   0   0   0   0   -) ;; shin+shada 

   (S   -   0   0   0   0   0   0   -) ;; sad(ص)
   (SS  -   0   0   0   0   0   0   -) ;; sad+shada

   (D   -   0   0   0   0   0   0   -) ;; dad(ض)
   (DD  -   0   0   0   0   0   0   -) ;; dad+shada

   (T   -   0   0   0   0   0   0   -) ;; Ta(ط)
   (TT  -   0   0   0   0   0   0   -) ;; Ta+shada

   (Z   -   0   0   0   0   0   0   -) ;; dad_ishala(ظ)
   (ZZ  -   0   0   0   0   0   0   -) ;; dad_ishala+shada

   (E   -   0   0   0   0   0   0   -) ;; ain(ع)
   (EE  -   0   0   0   0   0   0   -) ;; ain+shada

   (g   -   0   0   0   0   0   0   -) ;; ghin(غ)
   (gg  -   0   0   0   0   0   0   -) ;; ghin+shada

   (f   -   0   0   0   0   0   0   -) ;; fa(ف)
   (ff  -   0   0   0   0   0   0   -) ;; fa+shada

   (q   -   0   0   0   0   0   0   -) ;;qaf(ق)
   (qq  -   0   0   0   0   0   0   -) ;;qaf+shada

   (k   -   0   0   0   0   0   0   -) ;;kaf(ك)
   (kk  -   0   0   0   0   0   0   -) ;;kaf+shada

   (l   -   0   0   0   0   0   0   -) ;;lam(ل)
   (ll  -   0   0   0   0   0   0   -) ;;lam+shada

   (m   -   0   0   0   0   0   0   -) ;;mim(م)
   (mm  -   0   0   0   0   0   0   -) ;;mim+shada

   (n   -   0   0   0   0   0   0   -) ;;noun(ن)
   (nn  -   0   0   0   0   0   0   -) ;;noun+shada 

   (h   -   0   0   0   0   0   0   -) ;; ha(هـ)
   (hh  -   0   0   0   0   0   0   -) ;; ha+shada

   (w   -   0   0   0   0   0   0   -) ;;wa(و)
   (ww  -   0   0   0   0   0   0   -) ;;wa+shada

   (y   -   0   0   0   0   0   0   -) ;;ya(ي)
   (yy  -   0   0   0   0   0   0   -) ;;ya+shada

   (v   -   0   0   0   0   0   0   -) ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (a   -   0   0   0   0   0   0   -) ;;fatha
   (aa  -   0   0   0   0   0   0   -) ;;fatha+mad

   (A   -   0   0   0   0   0   0   -) ;; 
   (AA  -   0   0   0   0   0   0   -) ;;

   (i   -   0   0   0   0   0   0   -) ;;nawar:i0 kasra
   (i:  -   0   0   0   0   0   0   -) ;;nawar:i1
   (ii  -   0   0   0   0   0   0   -) ;;nawar:ii0 kasra+mad

   (I   -   0   0   0   0   0   0   -) ;;nawar:I0 
   (II  -   0   0   0   0   0   0   -) ;;nawar:II0

   (u   -   0   0   0   0   0   0   -) ;;nawar:u0 dama
   (uu  -   0   0   0   0   0   0   -) ;;nawar:uu0 dama+mad
   
   (U  -   0   0   0   0   0   0   -) ;;nawar:U0
   (UU -   0   0   0   0   0   0   -) ;;nawar:UU0

   (u:  -   0   0   0   0   0   0   -) ;;nawar:u1
   (uu: -   0   0   0   0   0   0   -) ;;nawar:uu1

   (sil -   0   0   0   0   0   0   -) ;;

   (pau -   0   0   0   0   0   0   -) ;;

   ;; insert the phones here, see examples in 
   ;; festival/lib/*_phones.scm

  )
)

(PhoneSet.silences '(pau))

(define (ara_norm_nawar::select_phoneset)
  "(ara_norm_nawar::select_phoneset)
Set up phone set for ara_norm."
  (Parameter.set 'PhoneSet 'ara_norm)
  (PhoneSet.select 'ara_norm)
)

(define (ara_norm_nawar::reset_phoneset)
  "(ara_norm_nawar::reset_phoneset)
Reset phone set for ara_norm."
  t
)

(provide 'ara_norm_nawar_phoneset)
