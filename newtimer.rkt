#lang racket
;;racket -i -e '(enter! "newtimer.rkt")'
;(require racket/base)

(require racket/port)
(require racket/pretty)
(require racket/system)
(require racket/gui/base)

;https://htdp.org/2018-01-06/Book/part_prologue.html

(define file (open-input-file "33-34Savitri.txt"))
 
(define sfile (port->string file))


(define slen (string-length sfile))
(define (sl arg) (string-length arg))
(define (ss s f) (substring sfile s f)) ;substring
(define (iss pair) (substring sfile (car pair) (cdr pair)))
(define (rss s f) (substring sfile s (+ f s)))
(define (s2l list) (list->string list))
(define (l2s string) (string->list string))

(define sslice (ss 0 8000))
(define (p linenum) (ss linenum (+ linenum 110)))
(define (bp linenum) (ss linenum (+ linenum 4000)))

(define findcon (regexp-match-positions "CONTENTS" sslice))
(define (find arg) (regexp-match-positions arg sfile))
(define (findall arg) (regexp-match-positions* arg sfile))
(define (findin in query) (regexp-match-positions* query in))
(define (findcanto in) (regexp-match-positions* "Canto" in))

(define pagelen (length (findin (ss 1495 2045) "\n")))

(define (d linenum) (display (p linenum)))
(define (bd linenum) (display (bp linenum)))
(define (fp arg) (p (ex (find arg))))
;(let main ((for ([i (in-range 0 10)]) (display i) (print "done"))))
(define (ex pair) (car (car pair)))

(define (first pair) (car (car pair)))
(define (second pair) (cdr (car pair)))

(define (foreach arg) ((for ([i (in-range 0 (length arg))]) (println (car (list-ref arg i))))))
(define (forleneach arg) (for ([i (in-range 0 pagelen)]) (println (car (list-ref arg i)))))
(define (newforeach arg) (for ([i (in-range 0 (length arg))]) (println (p (car (list-ref arg i))))))
(define (foreachwloc arg loc) (for ([i (in-range 0 (length arg))]) (println (p (+ (car (list-ref arg i)) loc)))))



;abs and rel location pairs?)

(define lbcount (length (findin sfile "\n")))
(define lbinfile (findin sfile "\n"))


(define toc (cons (ex (findall "CONTENTS")) (second(findall "715"))))
(define (addtoc rloc) (+ rloc (car toc)))
(define stoc '(ss (car toc) (car (cdr toc))))
(define bookintoc (findin (ss (car toc) (cdr toc)) #px"Book.{2,7}\n"))
(define abookintoc (map addtoc (map car bookintoc)))

;(newforeachwloc (findin (eval stoc) "CONTENTS") (car toc))
(define authstart (list (ex (findall "Author.s Note"))))
(define authend (+ (cdar (findin (ss (car authstart) slen) "SRI AUROBINDO")) (car authstart)))
(define auth (cons (car authstart) authend))

(define bookbreak (map cons (map addtoc (map car bookintoc)) (map addtoc (map cdr bookintoc))))

(define p1start (+ (caar (findin (ss (cdr auth) slen) "PART ONE")) (cdr auth)))
(define p1end (+ (cdar (findin (ss (cdr auth) slen) "END OF PART ONE")) (cdr auth)))
(define (addp1start loc) (+ loc p1start))
(define partone (cons p1start p1end)) ;WORKING
(define p2start (+ (caar (findin (ss p1end slen) "PART TWO")) p1end))
(define p2end (+ (cdar (findin (ss p1end slen) "END OF PART TWO")) p1end))
(define parttwo (cons p2start p2end)) ;WORKING
(define p3start (+ (caar (findin (ss p2end slen) "PART THREE")) p2end))
(define p3end (+ (cdar (findin (ss p2end slen) "THE END")) p2end))
(define partthree (cons p3start p3end))

(define (range pair) (ss (car pair) (cdr pair)))

(define (sum list*) (+ list*)) ;not working
(define (add num1 num2) (+ num1 num2))
(define (sub1 num) (- num 1))
(define front (map car bookbreak))
(define back (map cdr bookbreak))
(define booklist (map (lambda (number1 number2) (ss number1 number2)) front (map sub1 back)))
(define ubooklist (map string-upcase booklist))

(define eof 1071150)
(define main (cons 5682 1071150))
(define (loader book) (findin (ss (car main) (cdr main)) book))
(define bookloc (map car (map car (map loader ubooklist))))
(define abooklocstart (map addp1start bookloc))

(define newbooklist (map cons (map car bookbreak) (flatten (append (map car (cdr bookbreak)) (cdr toc)))))
(define cantomap (map findcanto (map iss newbooklist)))

;(for ([i 10])(display i))(display "\n")

(close-input-port file)
(define newfile (open-input-file "33-34Savitri.txt"))
(define rdypartone (cons (add 0 (car partone)) (cdr partone)))
(define printspd 30)

(define partone-port (open-input-string (iss rdypartone)))

(define splitlist (regexp-split "\n" (iss (cons 0 (cdr partone)))))


(define (finder arg) (map cons (map addp1start (map car (findin (iss partone) arg))) (map p (map addp1start (map car (findin (iss partone) arg))))))
(define (newpre arg) (sub1 (length (regexp-split "\n" (iss (cons 0 (caar (finder arg))))))))



(define splash (read-line))

(define count (newpre splash))
;(displayln count)
(define loopspd 1)
(define the-timer
  (new timer% [notify-callback
                (lambda ()
                  (set! count (+ count 1))
                  ;(when (= (modulo count loopspd) 0) (display (read-line partone-port)) (display "\n"))
                  (when (= (modulo count loopspd) 0) (display (list-ref splitlist (sub1 count))) (display "\n"))
                  (when (< count ender) (e p1))
                  (when (> count ender) (e pl))
                  (when (> count 10000)
                    (print "done")
                    (send the-timer stop)))]
              [interval 5000]))



(define end '(set! count 10000))

(define p5 '(send the-timer start 5000))
(define p2 '(send the-timer start 2000))
(define p1 '(send the-timer start 250))
(define play '(send the-timer start 20000))
(define pause '(send the-timer stop))
(define e eval)
(define pl play)
(define pa pause)
(define cantostrmap (map cons (map addp1start (map car (findin (iss partone) "Canto"))) (map p (map addp1start (map car (findin (iss partone) "Canto"))))))




(define pre (iss (cons 0 (caadr cantostrmap))))
(define split (regexp-split "\n" pre))
(define precount (length split))

;(define newcount (set! count (sub1 precount)))
(define (newcount arg) (set! count arg))


;(define newprecount (string-length newpre))

(define (peek arg) (display (list-ref splitlist arg)) (display "\n"))
(define (ready arg) (newcount (newpre arg)))
;(enter! "newtimer.rkt")
;racket -i -e '(enter! "newtimer.rkt")'

(define ender (+ count 20))
(define newender '(set! ender (+ count 20)))
;(define (roll) (if (equal? count ender) (e pa) (begin (e p1) (print count) roll)))

(define (push) (begin ender (e p1)))

;(define (ss s f) (substring sfile s f)) ;substring
;(define (p linenum) (ss linenum (+ linenum 110)))

(define (s arg) (map p (map addp1start (map car (findin (iss partone) arg)))))
(define (sv query) (sort (s query) string<?))
;(string-append (number->string (caar cantostrmap)) " = " (cdar cantostrmap)) (cdr cantostrmap)
;"5744 = Canto One\n\nThe Symbol Dawn\nIT WAS the hour before the Gods awake.\nAcross the path of the divine Event\nThe huge"


(define keys (open-input-file "/home/oem/Documents/Code/KEYS/keys"))
(define (f2s fn) (port->string fn))

(define skeys (f2s keys))
  
(define (newsplit str) (regexp-split "\n" str))
(define (sk query) (findin skeys query))
(define (nss start finish) (substring skeys start finish))
(define (nf finish) (substring skeys 0 finish))
(define (np linenum) (nss linenum (+ linenum 45)))
(define (rp linenum) (nss (- linenum 85) (+ linenum 15)))
(define (ns arg) (map np (map car (sk arg))))

;(map rp (map car (sk "Savitri")))
(define (newlook linenum)(nss (car (list-ref (findin skeys "\n") linenum)) (car (list-ref (findin skeys "\n") (add1 linenum)) )))

(define (lbin in) (regexp-match-positions* "\n" in))

;(newlook (sub1 (length (findin (nf (car (map car (sk "Divine Grace")))) "\n"))))
;"\nWith faith in the Divine Grace, all difficulties are solved.<p>~ The Mother"
;(newlook (sub1 (length (findin (nf (car (map car (sk "Divine Grace")))) "\n"))))
;"\nWith faith in the Divine Grace, all difficulties are solved.<p>~ The Mother"
;(length (map nf (map car (sk "Divine Grace"))))
;36

; bingo. prints out all divine grace matches
; (map newlook (map sub1 (map length (map lbin (map nf (map car (sk "Divine Grace")))))))
(define (see query) (map newlook (map sub1 (map length (map lbin (map nf (map car (sk query))))))))



(define (smap item) (and (display item) (display "\n"))  )
(define (seep query) (map smap (map newlook (map sub1 (map length (map lbin (map nf (map car (sk query))))))))                                                                                                                               )



;(define (roll num) (if (equal? num 0) (null) (e pl)

;(define p10 (for ([i 10]) (add1 count)))

;(define p10 (do ([1]) 10 (set! count (+ count 1))))

;(define thelist (append (list (read-line partone-port))))

;(define forever (let loop ([count 1]) (printf "looping ~a\n" count) (loop (add1 count))))

;(when (pair? (regexp-match-positions "Book" (list-ref splitlist 5)))
;(and (display (list-ref splitlist 5)) (display "--5")))
;=> The Book of Beginnings--5

;(for-each (lambda (item) (print (regexp-match-positions "Canto" item))) splitlist)
;results is sea of #f with some pairs

;(for-each (lambda (item) (when (pair? (regexp-match-positions "Canto" item)) (println item))) splitlist)
;print all the instances of "Canto" in splitlist. getting there.

;(for-each (lambda (item) (when (pair? (regexp-match-positions "Canto" item)) (and (println item) (println (regexp-match-positions "Canto" item))))) splitlist)
; prints only relative canto location, not in list.
;I want to return the current count?

;(map p (map addp1start (map car (findin (iss partone) "Canto"))))
;cantos in partone

;(map cons (map p (map addp1start (map car (findin (iss partone) "Canto")))) (map addp1start (map car (findin (iss partone) "Canto"))))
; cantos & stringloc. almostttt

;(list-ref (regexp-split "\n" (iss partone)) 1)
