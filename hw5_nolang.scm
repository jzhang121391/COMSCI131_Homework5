#lang scheme

(define backtrack-point #f)

(define (accept frag)
  (call/cc
   (lambda (cont)
     (cons frag (lambda () (cont #f))))))
  
(define match-junk
  (lambda (k frag accept)
    (or (accept frag)
	(and (< 0 k)
	     (pair? frag)
	     (match-junk (- k 1) (cdr frag) accept)))))

(define match-*
  (lambda (matcher frag accept)
    (or (accept frag)
	(matcher frag
		 (lambda (frag1)
		   (and (not (eq? frag frag1))
			(match-* matcher frag1 accept)))))))

(define make-matcher-old
  (lambda (pat)
    (cond

     ((symbol? pat)
      (lambda (frag accept)
	(and (pair? frag)
	     (eq? pat (car frag))
	     (accept (cdr frag)))))

     ((eq? 'or (car pat))
      (let make-or-matcher ((pats (cdr pat)))
	(if (null? pats)
	    (lambda (frag accept) #f)
	    (let ((head-matcher (make-matcher-old (car pats)))
		  (tail-matcher (make-or-matcher (cdr pats))))
	      (lambda (frag accept)
		(or (head-matcher frag accept)
		    (tail-matcher frag accept)))))))

     ((eq? 'list (car pat))
      (let make-list-matcher ((pats (cdr pat)))
	(if (null? pats)
	    (lambda (frag accept) (accept frag))
	    (let ((head-matcher (make-matcher-old (car pats)))
		  (tail-matcher (make-list-matcher (cdr pats))))
	      (lambda (frag accept)
		(head-matcher frag
			      (lambda (frag1)
				(tail-matcher frag1 accept))))))))

     ((eq? 'junk (car pat))
      (let ((k (cadr pat)))
	(lambda (frag accept)
	  (match-junk k frag accept))))

     ((eq? '* (car pat))
      (let ((matcher (make-matcher-old (cadr pat))))
	(lambda (frag accept)
	  (match-* matcher frag accept)))))))

(define (make-matcher pat)
  (lambda (frag)
    ((make-matcher-old pat) frag accept)))