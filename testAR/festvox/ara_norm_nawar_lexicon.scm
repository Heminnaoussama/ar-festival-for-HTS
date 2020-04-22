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
;;; Lexicon, LTS and Postlexical rules for ara_norm
;;;

;;; Load any necessary files here

(define (ara_norm_addenda)
  "(ara_norm_addenda)
Basic lexicon should (must ?) have basic letters, symbols and punctuation."

;;; Pronunciation of letters in the alphabet
;(lex.add.entry '("a" nn (((a) 0))))
;(lex.add.entry '("b" nn (((b e) 0))))
;(lex.add.entry '("c" nn (((th e) 0))))
;(lex.add.entry '("d" nn (((d e) 0))))
;(lex.add.entry '("e" nn (((e) 0))))
; ...

;;; Symbols ...
;(lex.add.entry 
; '("*" n (((a s) 0) ((t e) 0) ((r i1 s) 1)  ((k o) 0))))
;(lex.add.entry 
; '("%" n (((p o r) 0) ((th i e1 n) 1) ((t o) 0))))

;; Basic punctuation must be in with nil pronunciation
(lex.add.entry '("." punc nil))
;(lex.add.entry '("." nn (((p u1 n) 1) ((t o) 0))))
(lex.add.entry '("'" punc nil))
(lex.add.entry '(":" punc nil))
(lex.add.entry '(";" punc nil))
(lex.add.entry '("," punc nil))
;(lex.add.entry '("," nn (((k o1) 1) ((m a) 0))))
(lex.add.entry '("-" punc nil))
(lex.add.entry '("\"" punc nil))
(lex.add.entry '("`" punc nil))
(lex.add.entry '("?" punc nil))
(lex.add.entry '("!" punc nil))

;;;; waraj~aHa ->  w a r a jj a H a 
;(lex.add.entry  
;'("waraj~aHa" nil (((w a) 0) ((r a jj) 1) ((a H a) 0))))

)

(require 'lts)

;;;  Function called when word not found in lexicon
;;;  and you've trained letter to sound rules
;(define (ara_norm_lts_function word features)
;  "(ara_norm_lts_function WORD FEATURES)
;Return pronunciation of word not in lexicon."

  ;; If you have nothing ...
;  (format t "Unknown word %s\n" word)
;  (list word features nil)

  ;; If you have lts rules (trained or otherwise)
;  (if (not boundp 'ara_norm_lts_rules)
;      (require 'ara_norm_lts_rules))
;  (let ((dword (downcase word)) (phones) (syls))
;    (set! phones (lts_predict dword ara_norm_lts_rules))
;    (set! syls (ara_norm_lex_syllabify_phstress phones))
;    (list word features syls))
;  )

;(define (ara_norm_map_modify ps)
;  (cond
;   ((null ps) nil)
;   ((null (cdr ps)) ps)
;   ((assoc_string (string-append (car ps) (cadr ps))
;                   ara_norm_nawar_char_phone_map)
;    (cons
;     (string-append (car ps) (cadr ps))
;     (ara_norm_map_modify (cddr ps))))
;   (t
;    (cons
;     (car ps)
;     (ara_norm_map_modify (cdr ps))))))

;(define (ara_norm_map_phones p)
;  (cond
;   ((null p) nil)
;   (t
;    (let ((a (assoc_string (car p) ara_norm_nawar_char_phone_map)))
;      (cond
;       (a (cons (cadr a) (ara_norm_map_phones (cdr p))))
;       (t (ara_norm_map_phones (cdr p))))))))

(define (ara_norm_is_vowel x)
  (string-equal "+" (phone_feature x "vc")))

(define (ara_norm_contains_vowel l)
  (member_string
   t
   (mapcar (lambda (x) (ara_norm_is_vowel x)) l)))

(define (ara_norm_lex_sylbreak currentsyl remainder)
  "(ara_norm_lex_sylbreak currentsyl remainder)
t if this is a syl break, nil otherwise."
  (cond
   ((not (ara_norm_contains_vowel remainder))
    nil)
   ((not (ara_norm_contains_vowel currentsyl))
    nil)
   (t
    ;; overly naive, I mean wrong
    t))
)

(define (ara_norm_lex_syllabify_phstress phones)
 (let ((syl nil) (syls nil) (p phones) (stress 0))
    (while p
     (set! syl nil)
     (set! stress 0)
     (while (and p (not (ara_norm_lex_sylbreak syl p)))
       (if (string-matches (car p) "xxxx")
           (begin
             ;; whatever you do to identify stress
             (set! stress 1)
             (set syl (cons (car p-stress) syl)))
           (set! syl (cons (car p) syl)))
       (set! p (cdr p)))
     (set! syls (cons (list (reverse syl) stress) syls)))
    (reverse syls)))

    ;; utf8-sampa map based on unitran 
;(if (probe_file (path-append ara_norm_nawar::dir "festvox/ara_norm_nawar_char_phone_map.scm"))
;    (begin
;      (set! ara_norm_nawar_char_phone_map
;            (load (path-append ara_norm_nawar::dir 
;                               "festvox/ara_norm_nawar_char_phone_map.scm") t))
;	(load (path-append ara_norm_nawar::dir 
;                           "festvox/unicode_sampa_mapping.scm"))

    ;; utf8-indic-sampa letter based one
;    (define (ara_norm_lts_function word features)
;      "(ara_norm_lts_function WORD FEATURES)
;Return pronunciation of word not in lexicon."
;      (let ((dword word) (phones) (syls) (aphones))
;        (set! aphones (ara_norm_map_modify (utf8explode dword)))
;        (set! phones (ara_norm_map_phones aphones))
;	(set! phones (sampa_lookup phones))
;        (set! phones (indic_unicode_lts sphones))
;        (set! syls (ara_norm_lex_syllabify_phstress phones))
;        (list word features syls)))
;    ))

;(define (sampa_lookup gphones)
;  (let ((phlist nil) (sp nil))
;    (mapcar 
;     (lambda (gg)
;       (set! sp (assoc_string gg unicode_sampa_mapping))
;       (if sp
;           (set! phlist (append (car (cadr sp)) phlist))
;           (set! phlist (cons gg phlist))))
;     gphones)
;    (reverse phlist)))

;(define (indic_unicode_lts phlist)
;	(set! finallist (list))
;	(set! graphemecount 0)
;	(set! prevgrapheme (list))
;	(set! totgcnt (- (length phlist) 1))
;	(mapcar (lambda (ggg)
;		(if (symbol? (car ggg))
;		(begin
;		(cond
;			;; schwa deletion for the last consonant
;			((equal? graphemecount totgcnt)
;			(begin
;				(if (string-equal (phone_feature (car ggg) 'vc) "-")
;				(begin 
;					(if (string-equal (phone_feature (car prevgrapheme) 'vc) "-") 
;					(set! finallist (append  finallist prevgrapheme)))
;					;(set! finallist (append finallist (list (car ggg)))) ;appropriate for hindi
;					(set! finallist (append finallist  ggg)) ; for generic (non-schwa final) indic
;				)
;				(begin 
;					(if (string-equal (phone_feature (car prevgrapheme) 'vc) "-") 
;					(set! finallist (append finallist (list (car prevgrapheme)))))
;					(set! finallist (append finallist (list (car ggg))))
;				))
;			))
;			;; generic treatment for an intermediate grapheme
;			((and (> graphemecount 0) (< graphemecount totgcnt))
;			(begin
;				(cond 
;					;; If current is vowel, remove the previous schwa
;					((and (string-equal (phone_feature (car ggg) 'vc) "+") (string-equal (phone_feature (car prevgrapheme) 'vc) "-"))
;					(begin 
;						(set! finallist (append finallist (list (car prevgrapheme))))
;						(set! finallist (append finallist (list (car ggg))))
;					))
;					;; If current is consonant and previous is consonant, dump all of previous 
;					((and  (string-equal (phone_feature (car ggg) 'vc) "-") (string-equal (phone_feature (car prevgrapheme) 'vc) "-"))
;					(set! finallist (append finallist prevgrapheme)))
;					(t 
;					 t)
;				)
;			))
;			((and (eq? graphemecount 0) (string-equal (phone_feature (car ggg) 'vc) "+"))
;				(set! finallist (list (car ggg)))
;			)
;			(t 
;			t)
;		)
;		(set! graphemecount (+ 1 graphemecount))
;		(set! prevgrapheme ggg)
;		)
;		(begin 
;			(cond
;				((equal? (car ggg) '(P))
;					(set! finallist (append finallist (list (car prevgrapheme))))
;					(set! prevgrapheme (list))
;				)
;				((equal? (car ggg) '(M))
;					(if (string-equal (phone_feature (car prevgrapheme) 'vc) "-") (set! finallist (append finallist prevgrapheme)))
;					(set! finallist (append finallist (list "nB")))
;					(set! prevgrapheme (list))
;				)
;				((equal? (car ggg) '(CD))
;					(if (string-equal (phone_feature (car prevgrapheme) 'vc) "-") (set! finallist (append finallist prevgrapheme)))
;					(set! finallist (append finallist (list "nB")))
;					(set! prevgrapheme (list))
;				)
;				(t
;				t)
;				;(format t "debug: todo \n")
;			)
;			(set! graphemecount (+ 1 graphemecount))
;		)
;	)
;	) phlist)
;finallist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR: Hand written letter to sound rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ;;;  Function called when word not found in lexicon
 (define (ara_norm_lts_function word features)
   "(ara_norm_lts_function WORD FEATURES)
 Return pronunciation of word not in lexicon."

;   (format stderr "failed to find pronunciation for %s\n" word)
   (let ((dword (downcase word)))
;     ;; Note you may need to use a letter to sound rule set to do
;     ;; casing if the language has non-ascii characters in it.
     (if (lts.in.alphabet word 'ara_norm)
 	(list
 	 word
 	 features
; 	 ;; This syllabification is almost certainly wrong for
; 	 ;; this language (its not even very good for English)
; 	 ;; but it will give you something to start off with
 	 (lex.syllabify.phstress
 	   (lts.apply word 'ara_norm)))
 	(begin
; 	  (format stderr "unpronouncable word %s\n" word)
; 	  ;; Put in a word that means "unknown" with its pronunciation
;	  '("majhool" nil (((m a j) 0) ((h u:) 0) ((l) 0)))
)
)
)
 )

; ;; You may or may not be able to write a letter to sound rule set for
; ;; your language.  If its largely lexicon based learning a rule
; ;; set will be better and easier that writing one (probably).

 (lts.ruleset ;;start lts.rules
  ara_norm

  (;;start list
	
	;;first list unicode
	  ( cs > < } & \ b t ^ j H x d * r z s $ S D T Z E g f q k l m n h | ) ;;consonants
	  ( ept D S T Z g x q ) ;;emphatics
	  ( fwe g x ) ;;forwardEmphatics
	  ( amcsn l w y p ) ;;ambiguousConsonant ;;These consonants are only unambiguous in certain contexts

	  ( dct o a u i F N K ~ ) ;;diacritics
	  ( dtws o a u i F N K ) ;;diacriticsWithoutShadda
	;;end

	;;the list used after rules
	  ( vy a i u ) ;;vowel
	  ( cnt < b t ^ j H x d * r z s $ S D T Z E g f q l m n h w y ) ;;consonant


  );;end list
   

  (;;start rules

	  ( # [ s i l ] # = sil );;

	;;;; Al chamsiya
	  ( [ A l t ~ ] = < a tt )
	  ( [ A l ^ ~ ] = < a ^^ )
	  ( [ A l d ~ ] = < a dd )
	  ( [ A l * ~ ] = < a ** )
	  ( [ A l r ~ ] = < a rr )
	  ( [ A l z ~ ] = < a zz )
	  ( [ A l s ~ ] = < a ss )
	  ( [ A l $ ~ ] = < a $$ )
	  ( [ A l S ~ ] = < a SS )
	  ( [ A l D ~ ] = < a DD )
	  ( [ A l T ~ ] = < a TT )
	  ( [ A l Z ~ ] = < a ZZ )
	  ( [ A l l ~ ] = < a ll )
	  ( [ A l n ~ ] = < a nn )
	;;;;

	;;;;shada+kasratayn (mouchkil : al kasratayn touktaban kabla shada wa la nastati3 tahwil shada wahdaha)
	  ( [ < K ~ ] = << i n )
	  ( [ b K ~ ] = bb i n )
	  ( [ t K ~ ] = tt i n )
	  ( [ ^ K ~ ] = ^^ i n )
	  ( [ j K ~ ] = jj i n )	
	  ( [ H K ~ ] = HH i n )  
	  ( [ x K ~ ] = xx i n )
	  ( [ d K ~ ] = dd i n )
	  ( [ * K ~ ] = ** i n )
	  ( [ r K ~ ] = rr i n )
	  ( [ z K ~ ] = zz i n )
	  ( [ s K ~ ] = ss i n )
	  ( [ $ K ~ ] = $$ i n )
	  ( [ S K ~ ] = SS i n )
	  ( [ D K ~ ] = DD i n )
	  ( [ T K ~ ] = TT i n )
	  ( [ Z K ~ ] = ZZ i n )
	  ( [ E K ~ ] = EE i n )
	  ( [ g K ~ ] = gg i n )
	  ( [ f K ~ ] = ff i n )
	  ( [ q K ~ ] = qq i n )
	  ( [ k K ~ ] = kk i n )
	  ( [ l K ~ ] = ll i n )
	  ( [ m K ~ ] = mm i n )
	  ( [ n K ~ ] = nn i n )
	  ( [ h K ~ ] = hh i n )
	  ( [ w K ~ ] = ww i n )
	  ( [ y K ~ ] = yy i n )
	;;
	  ( [ > K ~ ] = << i n )		;;alif fawkaha hamza+shada
	  ( [ "\'" K ~ ] = << i n )	;;hamza+shada
	  ( [ } K ~ ] = << i n )		;;alif maksora fawkaha hamza+shada
	  ( [ & K ~ ] = << i n )		;;waw fawkaha hamza+shada 
	  ( [ | K ~ ] = << aa i n)	;;alif almad+shada 
	;;;;
	
	;;;;shada
	  ( [ < ~ ] = << )
	  ( [ b ~ ] = bb )
	  ( [ t ~ ] = tt )
	  ( [ ^ ~ ] = ^^ )
	  ( [ j ~ ] = jj )	
	  ( [ H ~ ] = HH )  
	  ( [ x ~ ] = xx )
	  ( [ d ~ ] = dd )
	  ( [ * ~ ] = ** )
	  ( [ r ~ ] = rr )
	  ( [ z ~ ] = zz )
	  ( [ s ~ ] = ss )
	  ( [ $ ~ ] = $$ )
	  ( [ S ~ ] = SS )
	  ( [ D ~ ] = DD )
	  ( [ T ~ ] = TT )
	  ( [ Z ~ ] = ZZ )
	  ( [ E ~ ] = EE )
	  ( [ g ~ ] = gg )
	  ( [ f ~ ] = ff )
	  ( [ q ~ ] = qq )
	  ( [ k ~ ] = kk )
	  ( [ l ~ ] = ll )
	  ( [ m ~ ] = mm )
	  ( [ n ~ ] = nn )
	  ( [ h ~ ] = hh )
	  ( [ w ~ ] = ww )
	  ( [ y ~ ] = yy )
	;;
	  ( [ > ~ ] = << )	;;alif fawkaha hamza+shada
	  ( [ "\'" ~ ] = << )	;;hamza+shada
	  ( [ } ~ ] = << )	;;alif maksora fawkaha hamza+shada
	  ( [ & ~ ] = << )	;;waw fawkaha hamza+shada 
	  ( [ | ~ ] = << aa )	;;alif almad+shada 
	;;
	;;;;

	;;;;soukoun
	  ( [ < o ] = < )
	  ( [ b o ] = b )
	  ( [ t o ] = t )
	  ( [ ^ o ] = ^ )
	  ( [ j o ] = j )	
	  ( [ H o ] = H )  
	  ( [ x o ] = x )
	  ( [ d o ] = d )
	  ( [ * o ] = * )
	  ( [ r o ] = r )
	  ( [ z o ] = z )
	  ( [ s o ] = s )
	  ( [ $ o ] = $ )
	  ( [ S o ] = S )
	  ( [ D o ] = D )
	  ( [ T o ] = T )
	  ( [ Z o ] = Z )
	  ( [ E o ] = E )
	  ( [ g o ] = g )
	  ( [ f o ] = f )
	  ( [ q o ] = q )
	  ( [ k o ] = k )
	  ( [ l o ] = l )
	  ( [ m o ] = m )
	  ( [ n o ] = n )
	  ( [ h o ] = h )
	  ( [ w o ] = w )
	  ( [ y o ] = y )
	;;
	  ( [ > o ] = < )	;;alif fawkaha hamza+soukoun
	  ( [ "\'" o ] = < )	;;hamza+soukoun
	  ( [ } o ] = < )	;;alif maksora fawkaha hamza+soukoun
	  ( [ & o ] = < )	;;waw fawkaha hamza+soukoun
	  ( [ | o ] = < aa )	;;alif almad+soukoun
	;;

	;;;;do some normalisation
	  ( [ A F ] = a n )	;;fathatan+alif=fathatan
	  ( [ F A ] = a n)
	  ( [ F ] = a n)
	  ( [ N ] = u n)
	  ( [ K ] = i n)
	;;;;

	;;;;alif al kamariya
	  ( # [ A ] = < a)
	;;;;

	;;;;alif maksora
	  ( [ a Y ] = aa)	
	  ( [ Y a ] = aa)
	  ( [ i Y ] = ii)
	  ( [ Y ] = aa)			
	;;;;

	;;;;alif
	  ( [ a A ] = aa)	;;fatha+alif
	  ( [ A ] = aa)		;;alif
	;;;;

	;;;;waw
	  ( [ u w A ] # = uu)	;;dama+waw+alif
	  ( [ u w ] # = uu)	;;dama+waw
	  ( [ ~ w ] # = uu)	;;shada+waw
	  ( [ u w ] cnt = uu)	;;dama+waw+harf
	;;;;

	;;;;kasra+ya
	  ( [ i y ] # = ii)
	  ( [ ~ y ] # = ii)
	  ( [ i y ] cnt = ii)
	;;;;

        ;;;;ta marbota
	  ( [ p ] # =  )
	  ( [ p ] = t )
        ;;;;
 
        ;;;;vowel
	  ( [ a ] = a )
	  ( [ i ] = i )
	  ( [ u ] = u )
	  ( [ o ] =  )
        ;;;;

	;;;;Consonant
	  ( [ < ] = < )
	  ( [ b ] = b )
	  ( [ t ] = t )
	  ( [ ^ ] = ^ )
	  ( [ j ] = j )	
	  ( [ H ] = H )  
	  ( [ x ] = x )
	  ( [ d ] = d )
	  ( [ * ] = * )
	  ( [ r ] = r )
	  ( [ z ] = z )
	  ( [ s ] = s )
	  ( [ $ ] = $ )
	  ( [ S ] = S )
	  ( [ D ] = D )
	  ( [ T ] = T )
	  ( [ Z ] = Z )
	  ( [ E ] = E )
	  ( [ g ] = g )
	  ( [ f ] = f )
	  ( [ q ] = q )
	  ( [ k ] = k )
	  ( [ l ] = l )
	  ( [ m ] = m )
	  ( [ n ] = n )
	  ( [ h ] = h )
	  ( [ w ] = w )
	  ( [ y ] = y )
	;;
	  ( [ > ] = < )		;;alif fawkaha hamza
	  ( [ "\'" ] = < )	;;hamza
	  ( [ } ] = < )		;;alif maksora fawkaha hamza
	  ( [ & ] = < )		;;waw fawkaha hamza 
	  ( [ | ] = < aa )	;;alif almad 
        ;;

        ;;;;madda
        ;;;;

        ;;;;nunation
        ;;;;

   );;end rules

 );;end lts.rules

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Postlexical Rules 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ara_norm::postlex_rule1 utt)
  "(ara_norm::postlex_rule1 utt)
A postlexical rule form correcting phenomena over word boundaries."
  (mapcar
   (lambda (s)
     ;; do something
     )
   (utt.relation.items utt 'Segment))
   utt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon definition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lex.create "ara_norm")
(lex.set.phoneset "ara_norm")
(lex.set.lts.method 'ara_norm_lts_function)
;(if (probe_file (path-append ara_norm_nawar::dir "festvox/ara_norm_lex.out"))
;    (lex.set.compile.file (path-append ara_norm_nawar::dir 
;                                       "festvox/ara_norm_lex.out")))
(ara_norm_addenda)
;(if (probe_file (path-append ara_norm_nawar::dir "festvox/ara_norm_addenda.scm"))
;    (load (path-append ara_norm_nawar::dir "festvox/ara_norm_addenda.scm")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ara_norm_nawar::select_lexicon)
  "(ara_norm_nawar::select_lexicon)
Set up the lexicon for ara_norm."
  (lex.select "ara_norm")

  ;; Post lexical rules
  (set! postlex_rules_hooks (list ara_norm::postlex_rule1))
)

(define (ara_norm_nawar::reset_lexicon)
  "(ara_norm_nawar::reset_lexicon)
Reset lexicon information."
  t
)

(provide 'ara_norm_nawar_lexicon)
