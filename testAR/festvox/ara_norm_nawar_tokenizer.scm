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
;;; Tokenizer for norm
;;;
;;;  To share this among voices you need to promote this file to
;;;  to say festival/lib/ara_norm/ so others can use it.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load any other required files

;; Punctuation for the particular language
(set! ara_norm_nawar::token.punctuation "\"'`.,:;!?(){}[]")
(set! ara_norm_nawar::token.prepunctuation "\"'`({[")
(set! ara_norm_nawar::token.whitespace " \t\n\r")
(set! ara_norm_nawar::token.singlecharsymbols "")

;;; Voice/norm token_to_word rules 
(define (ara_norm_nawar::token_to_words token name)
  "(ara_norm_nawar::token_to_words token name)
Specific token to word rules for the voice ara_norm_nawar.  Returns a list
of words that expand given token with name."
  (cond
   ((string-matches name "[1-9][0-9]+")
    (ara_norm::number token name))
   (t ;; when no specific rules apply do the general ones
    (list name))))

(define (ara_norm::number token name)
  "(ara_norm::number token name)
Return list of words that pronounce this number in norm."

	(arabic_number name)

  ;;(error "ara_norm::number to be written\n")

)

(define (ara_norm_nawar::select_tokenizer)
  "(ara_norm_nawar::select_tokenizer)
Set up tokenizer for norm."
  (Parameter.set 'Language 'ara_norm)
  (set! token.punctuation ara_norm_nawar::token.punctuation)
  (set! token.prepunctuation ara_norm_nawar::token.prepunctuation)
  (set! token.whitespace ara_norm_nawar::token.whitespace)
  (set! token.singlecharsymbols ara_norm_nawar::token.singlecharsymbols)

  (set! token_to_words ara_norm_nawar::token_to_words)
)

(define (ara_norm_nawar::reset_tokenizer)
  "(ara_norm_nawar::reset_tokenizer)
Reset any globals modified for this voice.  Called by 
(ara_norm_nawar::voice_reset)."
  ;; None

  t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (arabic_number name)
"(arabic_number name)
Convert a string of digits into a list of words saying the number."
  (if (string-matches name "0")
	(list "SifroN")
	(arabic_number_from_digits (symbolexplode name))
); endif
); end define

(define (just_zeros digits)
"(just_zeros digits)
If this only contains 0s then we just do something different."
 (cond
  ((not digits) t)
  ((string-equal "0" (car digits))
   (just_zeros (cdr digits)))
  (t nil)))

(define (arabic_number_from_digits digits)
  "(arabic_number_from_digits digits)
Takes a list of digits and converts it to a list of words
saying the number."
(let ((l (length digits)))
    (cond
     ((equal? l 0)
      nil)
     ((string-equal (car digits) "0")
      (arabic_number_from_digits (cdr digits)))
     ((equal? l 1);; single digit
      (cond 
       ((string-equal (car digits) "0") (list "SifroN"))
       ((string-equal (car digits) "1") (list "waAHidN"))
       ((string-equal (car digits) "2") (list "<^onaAn"))
       ((string-equal (car digits) "3") (list "^alaA^apN"))
       ((string-equal (car digits) "4") (list ">arobaEapN"))
       ((string-equal (car digits) "5") (list "xamosapN"))
       ((string-equal (car digits) "6") (list "sit~apN"))
       ((string-equal (car digits) "7") (list "saboEapN"))
       ((string-equal (car digits) "8") (list "^amaAniyapN"))
       ((string-equal (car digits) "9") (list "tisoEapN"))

       (t (list "raqomN"))));; else
     ((equal? l 2);; less than 100
      (cond
       ((string-equal (car digits) "0");; 0x
	(arabic_number_from_digits (cdr digits)))
     
       ((string-equal (car digits) "1");; 1x
	(cond
	 ((string-equal (car (cdr digits)) "0") (list "Ea$orapN"))
	 ((string-equal (car (cdr digits)) "1") (list ">Hada Ea$ar"))
	 ((string-equal (car (cdr digits)) "2") (list "Ai^onaA Ea$ar"))
	 ((string-equal (car (cdr digits)) "3") (list "^alaA^apa Ea$ara"))
	 ((string-equal (car (cdr digits)) "4") (list ">arobaEapa Ea$ara"))
	 ((string-equal (car (cdr digits)) "5") (list "xamosapa Ea$ara"))
	((string-equal (car (cdr digits)) "6") (list "sit~apa Ea$ara"))
	((string-equal (car (cdr digits)) "7") (list "saboEapa Ea$ara"))
	((string-equal (car (cdr digits)) "8") (list "^amaAniyapa Ea$ara"))
	((string-equal (car (cdr digits)) "9") (list "tisoEapa Ea$ara"))

	 (t (list "raqomN"))));; else
     
       ((string-equal (car digits) "2");; 2x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "Ei$oruwn")
	    (cons (arabic_number_from_digits (cdr digits)) " wa Ei$oruwn")))

       ((string-equal (car digits) "3");; 3x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "^alaA^uwn")
	    (cons (arabic_number_from_digits (cdr digits)) " wa ^alaA^uwn")))

       ((string-equal (car digits) "4");; 4x
	(if (string-equal (car (cdr digits)) "0") 
	    (list ">arobaEuwna")
	    (cons (arabic_number_from_digits (cdr digits)) " wa >arobaEuwn")))

       ((string-equal (car digits) "5");; 5x
	(if (string-equal (car (cdr digits)) "0") 
	    (list ">arobaEuwn")
	    (cons (arabic_number_from_digits (cdr digits)) " wa >arobaEuwn")))

       ((string-equal (car digits) "6");; 6x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "sit~uwn")
	    (cons (arabic_number_from_digits (cdr digits)) " wa sit~uwna")))

       ((string-equal (car digits) "7");; 7x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "saboEuwn")
	    (cons (arabic_number_from_digits (cdr digits)) " wa saboEuwn")))

       ((string-equal (car digits) "8");; 8x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "^amAnwn")
	    (cons (arabic_number_from_digits (cdr digits)) " wa ^amAnwn")))

       ((string-equal (car digits) "9");; 9x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "tisoEuwn")
	    (cons (arabic_number_from_digits (cdr digits)) " wa tisoEuwn")))

       ))

     ((equal? l 3);; in the hundreds
      (cond 
     
       ((string-equal (car digits) "1");; 1xx
	(if (just_zeros (cdr digits)) (list "miA}apN")
	    (cons "miA}apN wa " (arabic_number_from_digits (cdr digits)))))

	((string-equal (car digits) "2");; 2xx
	(if (just_zeros (cdr digits)) (list "mi}ataAni")
	    (cons "mi}ataAni wa " (arabic_number_from_digits (cdr digits)))))

	((string-equal (car digits) "3");; 3xx
	(if (just_zeros (cdr digits)) (list "^alaA^miA}ap")
	    (cons "^alaA^miA}ap wa " (arabic_number_from_digits (cdr digits)))))

	((string-equal (car digits) "4");; 4xx
	(if (just_zeros (cdr digits)) (list ">arobaEimiA}apK")
	    (cons ">arobaEimiA}apK wa " (arabic_number_from_digits (cdr digits)))))

	((string-equal (car digits) "5");; 5xx
	(if (just_zeros (cdr digits)) (list "xamosimiA}apN")
	    (cons "xamosimiA}apN wa " (arabic_number_from_digits (cdr digits)))))

	((string-equal (car digits) "6");; 6xx
	(if (just_zeros (cdr digits)) (list "sit~imiA}apK")
	    (cons "sit~imiA}apK wa " (arabic_number_from_digits (cdr digits)))))

	((string-equal (car digits) "7");; 7xx
	(if (just_zeros (cdr digits)) (list "saboEimA}apK")
	    (cons "saboEimA}apK wa " (arabic_number_from_digits (cdr digits)))))

	((string-equal (car digits) "8");; 8xx
	(if (just_zeros (cdr digits)) (list "^amAnimiA}apN")
	    (cons "^amAnimiA}apN wa " (arabic_number_from_digits (cdr digits)))))

	((string-equal (car digits) "9");; 9xx
	(if (just_zeros (cdr digits)) (list "tisoEimiA}apN")
	    (cons "tisoEimiA}apN wa " (arabic_number_from_digits (cdr digits)))))

       ))

     (t
      (list "raqomN" "kabyrN")))))

(provide 'ara_norm_nawar_tokenizer)
