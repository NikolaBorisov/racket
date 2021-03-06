(module shapes mzscheme

(require sgl/gl
         sgl/gl-vectors
         sgl)

(provide makedots makebucky makebevelcube makecylinder 
         makeuvsphere makediamond makepyramid makeicosahedron
         makespiky makedisc)


(define (norm p1 p2 p3 dir)
  (let*
    ( (v1 (map (lambda (x y) (- x y)) p2 p1))
      (v2 (map (lambda (x y) (- x y)) p3 p1))
      (nx (- (* (list-ref v1 1) (list-ref v2 2))
             (* (list-ref v2 1) (list-ref v1 2))))
      (ny (- (* (list-ref v2 0) (list-ref v1 2))
             (* (list-ref v1 0) (list-ref v2 2))))
      (nz (- (* (list-ref v1 0) (list-ref v2 1))
             (* (list-ref v2 0) (list-ref v1 1))))
    )
    (glNormal3f (* dir nx) (* dir ny) (* dir nz))
  )
)
  
 
; -------------------------------------------------------------------
(define (makedots size)
  (let*
    ( (dots 12)
      (a #f) (u #f) (v #f)
    )
    (glPointSize 3.0)
    (glDisable GL_LIGHTING)
    (glBegin GL_POINTS)
    (do ((i 0 (+ i 1))) ((= i dots))
      (set! a (* i 3.1415928 (/ 2.0 dots)))
      (set! u (* size (cos a)))
      (set! v (* size (sin a)))
      (glVertex3f u v 0.0)
      (glVertex3f u 0.0 v)
    )
    (glEnd)
    (glEnable GL_LIGHTING)
  )
)


; -------------------------------------------------------------------

(define bucky-points
'#(
(-0.449358 0.730026 0.514918  )
(-0.277718 0.201774 0.939234  )
(-0.277718 -0.201774 0.939234 )
(-0.555436 0.403548 0.727076  )
(-0.555436 -0.403548 0.727076 )
(-0.833155 0.201774 0.514918  )
(-0.833155 -0.201774 0.514918 )
(0.106079 -0.326477 0.939234  )
(0.212158 -0.652955 0.727076  )
(-0.449358 -0.730026 0.514918 )
(-0.065560 -0.854729 0.514918 )
(0.343279 0.000000 0.939234   )
(0.686557 0.000000 0.727076   )
(0.555436 -0.652955 0.514918  )
(0.792636 -0.326477 0.514918  )
(0.661515 0.730026 -0.171639  )
(0.898715 0.403548 -0.171639  )
(0.489876 0.854729 0.171639   )
(0.964275 0.201774 0.171639   )
(0.555436 0.652955 0.514918   )
(0.792636 0.326477 0.514918   )
(-0.489876 0.854729 -0.171639 )
(-0.106079 0.979432 -0.171639 )
(-0.661515 0.730026 0.171639  )
(0.106079 0.979432 0.171639   )
(-0.065560 0.854729 0.514918  )
(-0.964275 -0.201774 -0.171639 )
(-0.964275 0.201774 -0.171639 )
(-0.898715 -0.403548 0.171639 )
(-0.898715 0.403548 0.171639  )
(-0.106079 -0.979432 -0.171639 )
(-0.489876 -0.854729 -0.171639 )
(0.106079 -0.979432 0.171639  )
(-0.661515 -0.730026 0.171639 )
(0.898715 -0.403548 -0.171639 )
(0.661515 -0.730026 -0.171639 )
(0.964275 -0.201774 0.171639  )
(0.489876 -0.854729 0.171639  )
(0.065560 0.854729 -0.514918  )
(0.449358 0.730026 -0.514918  )
(-0.792636 0.326477 -0.514918 )
(-0.555436 0.652955 -0.514918 )
(-0.555436 -0.652955 -0.514918 )
(-0.792636 -0.326477 -0.514918 )
(0.449358 -0.730026 -0.514918 )
(0.065560 -0.854729 -0.514918 )
(0.833155 0.201774 -0.514918 )
(0.833155 -0.201774 -0.514918 )
(0.277718 0.201774 -0.939234 )
(-0.106079 0.326477 -0.939234 )
(0.555436 0.403548 -0.727076 )
(-0.212158 0.652955 -0.727076 )
(-0.343279 0.000000 -0.939234 )
(-0.686557 0.000000 -0.727076 )
(-0.106079 -0.326477 -0.939234 )
(-0.212158 -0.652955 -0.727076 )
(0.277718 -0.201774 -0.939234 )
(0.555436 -0.403548 -0.727076 )
(0.106079 0.326477 0.939234 )
(0.212158 0.652955 0.727076  )
  )
)
  
  
(define (hex-point n size)
  (apply glVertex3f (map (lambda (x) (* x size))
                         (vector-ref bucky-points n)))
)
  

(define (hex p1 p2 p3 p4 p5 p6 size)
  (norm (vector-ref bucky-points p1) 
        (vector-ref bucky-points p3)
        (vector-ref bucky-points p2) 
        1.0)
  (glPolygonMode GL_FRONT GL_FILL)
  (glBegin GL_POLYGON)
  (hex-point p6 size)
  (hex-point p5 size)
  (hex-point p4 size)
  (hex-point p3 size)
  (hex-point p2 size)
  (hex-point p1 size)
  (glEnd)
  (glPolygonMode GL_FRONT GL_FILL)
)
  
  
(define (pent p1 p2 p3 p4 p5 size)
  (norm (vector-ref bucky-points p1) 
        (vector-ref bucky-points p3)
        (vector-ref bucky-points p2)
        1.0)
  (glBegin GL_TRIANGLE_STRIP)
  (hex-point p1 size)
  (hex-point p5 size)
  (hex-point p2 size)
  (hex-point p4 size)
  (hex-point p3 size)
  (glEnd)
)
  
  
(define (makebucky size)
  (glEnable GL_NORMALIZE)
  
  (hex 2 7 8 10 9 4 size)
  (hex 1 2 4 6 5 3 size)
  (hex 7 11 12 14 13 8 size)
  (hex 9 10 32 30 31 33 size)
  (hex 5 6 28 26 27 29 size)
  (hex 0 25 59 58 1 3 size)
  (hex 11 58 59 19 20 12 size)
  (hex 21 22 24 25 00 23 size)
  (hex 30 32 37 35 44 45 size)
  (hex 26 28 33 31 42 43 size)
  (hex 15 17 24 22 38 39 size)
  (hex 15 16 18 20 19 17 size)
  (hex 38 51 49 48 50 39 size)
  (hex 13 14 36 34 35 37 size)
  (hex 16 46 47 34 36 18 size)
  (hex 21 23 29 27 40 41 size)
  (hex 40 53 52 49 51 41 size)
  (hex 44 57 56 54 55 45 size)
  (hex 46 50 48 56 57 47 size)
  (hex 42 55 54 52 53 43 size) 
  
  (pent 1 58 11 7 2 size)
  (pent 8 13 37 32 10 size)
  (pent 4 9 33 28 6 size)
  (pent 0 3 5 29 23 size)
  (pent 17 19 59 25 24 size)
  (pent 12 20 18 36 14 size)
  (pent 30 45 55 42 31 size)
  (pent 21 41 51 38 22 size)
  (pent 48 49 52 54 56 size)
  (pent 15 39 50 46 16 size)
  (pent 34 47 57 44 35 size)
  (pent 26 43 53 40 27 size) 
  
)
  
  
; -------------------------------------------------------------------


(define (makebevelcube scale)
  (let*
    ( (sizex (* 0.6 scale))
      (sizey (* 0.6 scale))
      (sizez (* 0.6 scale))
      (bevel (* 0.15 scale))
      (bsizex (+ sizex bevel))
      (bsizey (+ sizey bevel))
      (bsizez (+ sizez bevel))
    )
    
    (glEnable GL_NORMALIZE)
    
    (glBegin GL_QUADS)
    (glNormal3f 0.0 sizey 0.0)
    (glVertex3f sizex bsizey sizez)
    (glVertex3f sizex bsizey (- sizez))
    (glVertex3f (- sizex) bsizey (- sizez))
    (glVertex3f (- sizex) bsizey sizez)

    (glNormal3f 0.0 0.0 sizez)
    (glVertex3f sizex sizey bsizez)
    (glVertex3f (- sizex) sizey bsizez)
    (glVertex3f (- sizex) (- sizey)  bsizez)
    (glVertex3f sizex (- sizey)  bsizez)

    (glNormal3f 0.0 0.0 (- sizez))
    (glVertex3f (- sizex) (- sizey) (- bsizez))
    (glVertex3f (- sizex) sizey (- bsizez))
    (glVertex3f sizex sizey (- bsizez))
    (glVertex3f sizex (- sizey) (- bsizez))

    (glNormal3f sizex 0.0 0.0)
    (glVertex3f bsizex sizey sizez)
    (glVertex3f bsizex (- sizey) sizez)
    (glVertex3f bsizex (- sizey) (- sizez))
    (glVertex3f bsizex sizey (- sizez))

    (glNormal3f (- sizex) 0.0 0.0)
    (glVertex3f (- bsizex) (- sizey) (- sizez))
    (glVertex3f (- bsizex) (- sizey) sizez)
    (glVertex3f (- bsizex)  sizey  sizez)
    (glVertex3f (- bsizex)  sizey (- sizez))

    (glNormal3f  0.0 (- sizey)  0.0);
    (glVertex3f (- sizex) (- bsizey) (- sizez));
    (glVertex3f  sizex (- bsizey) (- sizez));
    (glVertex3f  sizex (- bsizey)  sizez);
    (glVertex3f (- sizex) (- bsizey)  sizez);

;	setmaterial(blue);

    (glNormal3f  0.0  sizey  sizez);
    (glVertex3f  (- sizex)  bsizey  sizez);
    (glVertex3f  (- sizex)  sizey  bsizez);
    (glVertex3f   sizex  sizey  bsizez);
    (glVertex3f   sizex  bsizey  sizez);

    (glNormal3f  sizex  0.0  sizez);
    (glVertex3f  bsizex  sizey  sizez);
    (glVertex3f  sizex  sizey  bsizez);
    (glVertex3f  sizex  (- sizey)  bsizez);
    (glVertex3f  bsizex  (- sizey)  sizez);

    (glNormal3f  sizex  sizey  0.0);
    (glVertex3f bsizex  sizey  (- sizez));
    (glVertex3f sizex  bsizey  (- sizez));
    (glVertex3f sizex  bsizey   sizez);
    (glVertex3f bsizex  sizey   sizez);

    (glNormal3f  0.0  (- sizey)  (- sizez));
    (glVertex3f  (- sizex)  (- bsizey)  (- sizez));
    (glVertex3f  (- sizex)  (- sizey)  (- bsizez));
    (glVertex3f   sizex  (- sizey)  (- bsizez));
    (glVertex3f   sizex  (- bsizey)  (- sizez));

    (glNormal3f  (- sizex)  0.0  (- sizez));
    (glVertex3f  (- bsizex)   sizey  (- sizez));
    (glVertex3f  (- sizex)   sizey  (- bsizez));
    (glVertex3f  (- sizex)  (- sizey)  (- bsizez));
    (glVertex3f  (- bsizex)  (- sizey)  (- sizez));

    (glNormal3f  (- sizex)  (- sizey)  0.0);
    (glVertex3f (- bsizex)  (- sizey)   (- sizez));
    (glVertex3f (- sizex)  (- bsizey)   (- sizez));
    (glVertex3f (- sizex)  (- bsizey)  sizez);
    (glVertex3f (- bsizex)  (- sizey)  sizez);

    (glNormal3f  0.0  (- sizey)  sizez);
    (glVertex3f   sizex  (- bsizey)  sizez);
    (glVertex3f   sizex  (- sizey)  bsizez);
    (glVertex3f  (- sizex)  (- sizey)  bsizez);
    (glVertex3f  (- sizex)  (- bsizey)  sizez);

    (glNormal3f  0.0  sizey  (- sizez));
    (glVertex3f  (- sizex)  sizey  (- bsizez));
    (glVertex3f  (- sizex)  bsizey  (- sizez));
    (glVertex3f   sizex  bsizey  (- sizez));
    (glVertex3f   sizex  sizey  (- bsizez));

    (glNormal3f  (- sizex)  0.0  sizez);
    (glVertex3f  (- bsizex)  (- sizey)  sizez);
    (glVertex3f  (- sizex)  (- sizey)  bsizez);
    (glVertex3f  (- sizex)  sizey  bsizez);
    (glVertex3f  (- bsizex) sizey  sizez);

    (glNormal3f  sizex  0.0  (- sizez));
    (glVertex3f  sizex  sizey  (- bsizez));
    (glVertex3f  bsizex  sizey  (- sizez));
    (glVertex3f  bsizex  (- sizey)  (- sizez));
    (glVertex3f  sizex (- sizey)  (- bsizez));

    (glNormal3f  (- sizex)  sizey  0.0);
    (glVertex3f  (- bsizex) sizey  sizez);
    (glVertex3f  (- sizex)  bsizey  sizez);
    (glVertex3f  (- sizex)  bsizey  (- sizez));
    (glVertex3f  (- bsizex)  sizey  (- sizez));

    (glNormal3f  sizex  (- sizey)  0.0);
    (glVertex3f  sizex  (- bsizey)  (- sizez));
    (glVertex3f  bsizex  (- sizey)  (- sizez));
    (glVertex3f  bsizex  (- sizey)  sizez);
    (glVertex3f  sizex  (- bsizey)  sizez);

    (glEnd);

;	setmaterial(red);
    (glBegin GL_TRIANGLES);

    (glNormal3f sizex sizey sizez);
    (glVertex3f bsizex sizey sizez);
    (glVertex3f sizex bsizey sizez);
    (glVertex3f sizex sizey bsizez);

    (glNormal3f (- sizex) sizey sizez);
    (glVertex3f (- sizex) bsizey sizez);
    (glVertex3f (- bsizex) sizey sizez);
    (glVertex3f (- sizex) sizey bsizez);

    (glNormal3f (- sizex) (- sizey) sizez);
    (glVertex3f (- bsizex) (- sizey) sizez);
    (glVertex3f (- sizex) (- bsizey) sizez);
    (glVertex3f (- sizex) (- sizey) bsizez);

    (glNormal3f sizex (- sizey) sizez);
    (glVertex3f sizex (- bsizey) sizez);
    (glVertex3f bsizex (- sizey) sizez);
    (glVertex3f sizex (- sizey) bsizez);


    (glNormal3f (- sizex) (- sizey) (- sizez));
    (glVertex3f (- sizex) (- sizey) (- bsizez));
    (glVertex3f (- sizex) (- bsizey) (- sizez));
    (glVertex3f (- bsizex) (- sizey) (- sizez));

    (glNormal3f sizex (- sizey) (- sizez));
    (glVertex3f sizex (- sizey) (- bsizez));
    (glVertex3f bsizex (- sizey) (- sizez));
    (glVertex3f sizex (- bsizey) (- sizez));

    (glNormal3f sizex sizey (- sizez));
    (glVertex3f sizex sizey (- bsizez));
    (glVertex3f sizex bsizey (- sizez));
    (glVertex3f bsizex sizey (- sizez));

    (glNormal3f (- sizex) sizey (- sizez));
    (glVertex3f (- sizex) sizey (- bsizez));
    (glVertex3f (- bsizex) sizey (- sizez));
    (glVertex3f (- sizex) bsizey (- sizez));

    (glEnd); 
  )
)


; -------------------------------------------------------------------

(define (makecylinder size)
  (let* 
    ( (csqueeze 0.8)
      (csides 12)
      (x (make-vector csides 0.0))
      (z (make-vector csides 0.0))
      (a #f)
      (cur #f) (prev #f)
    )
    
    (do ((i 0 (+ i 1))) ((= i csides))
      (set! a (/ (* i 3.1415928 2.0) csides))
      (vector-set! x i (* (cos a) size csqueeze))
      (vector-set! z i (* (sin a) size csqueeze))
    )
    
    (glEnable GL_NORMALIZE)
    ; bottom
    (glNormal3f 0.0 -1.0 0.0)
    (glBegin GL_POLYGON)
    (do ((i 0 (+ i 1))) ((= i csides))
      (glVertex3f (vector-ref x i)
                  (- size)
                  (vector-ref z i))
    )
    (glEnd)
    ; top
    (glNormal3f 0.0 1.0 0.0)
    (glBegin GL_POLYGON)
    (do ((i 0 (+ i 1))) ((= i csides))
      (glVertex3f (vector-ref x (- csides 1 i))
                  (- size)
                  (vector-ref z (- csides 1 i)))
    )
    (glEnd)
    ;side
    (glBegin GL_QUAD_STRIP)
    (do ((i 0 (+ i 1))) ((= i (+ csides 1)))
      (set! cur (if (< i csides) i (- i csides)))
      (if (> i 0)
        (glNormal3f (/ (+ (vector-ref x cur)
                          (vector-ref x prev)) 2.0)
                    0.0
                    (/ (+ (vector-ref z cur)
                          (vector-ref z prev)) 2.0))
      )
      (glVertex3f (vector-ref x cur)
                  (- size)
                  (vector-ref z cur))
      (glVertex3f (vector-ref x cur)
                  size
                  (vector-ref z cur))
      (set! prev cur)
    )
    (glEnd)
  )
)

; -------------------------------------------------------------------

(define (makeuvsphere size)
  (let*
    ( (usides 15)
      (vsides 9)
      (x (make-vector usides 0.0))
      (z (make-vector usides 0.0))
      (a #f)  (t #f)
      (c1 #f) (s1 #f) (c2 #f) (s2 #f)
    )

    (do ((i 0 (+ i 1))) ((= i usides))
      (set! a (/ (* i 3.1415928 2.0) usides))
      (vector-set! x i (* (cos a) size))
      (vector-set! z i (* (sin a) size))
    )
    
    (glEnable GL_NORMALIZE)
    
    (do ((i 0 (+ i 1))) ((= i vsides))
      (set! a (/ (* i 3.1415927) vsides))
      (set! c1 (cos a))
      (set! s1 (sin a))
      (set! a (/ (* (+ i 1) 3.1415927) vsides))
      (set! c2 (cos a))
      (set! s2 (sin a))
      
      (glBegin GL_QUAD_STRIP)
      (do ((j 0 (+ j 1))) ((= j (+ usides 1)))
        (set! t (if (< j usides) j (- j usides)))
        (if (not (= j 0))
          (let*
            ( (c #f) (s #f) )
            (set! a (/ (* (+ i 0.5) 3.1415927) vsides))
            (set! c (cos a))
            (set! s (sin a))
            (set! a (/ (* (- j 0.5) 3.1415927 2.0) usides))
            (glNormal3f (* (cos a) s)
                        c
                        (* (sin a) s))
          )
        )
        (glVertex3f (* (vector-ref x t) s2)
                    (* c2 size)
                    (* (vector-ref z t) s2))
        (glVertex3f (* (vector-ref x t) s1)
                    (* c1 size)
                    (* (vector-ref z t) s1))
      )
      (glEnd)
      
    )
    

  )
)
  
  
; -------------------------------------------------------------------

(define (makediamond size)
  (let*
    ( (dsides 9)
      (x (make-vector dsides 0.0))
      (z (make-vector dsides 0.0))
      (a #f) (p1 #f) (p2 #f)
      (c #f) (d #f) (h #f) (s #f) (j #f) (o #f)
    )
    
    (do ((i 0 (+ i 1))) ((= i dsides))
      (set! a (/ (* i 3.1415928 2.0) dsides))
      (vector-set! x i (* (cos a) size))
      (vector-set! z i (* (sin a) size))
    )
    
    (glEnable GL_NORMALIZE)
    
    (set! p2 (* size 0.5))
    (do ((t 0 (+ t 1))) ((= t 2))
      (if (remainder t 2)
        (set! p1 (- size))
        (set! p1 size)
      )
      
      (glBegin GL_TRIANGLE_FAN)
      (glVertex3f 0.0 p1 0.0)
      (set! d (if (= t 0) (- size p2) (+ size p2)))
      (set! h (sqrt (+ (* size size) (* d d))))
      (set! c (if (= t 0) (/ size h) (/ (- size) h)))
      (set! s (/ d h))
      (do ((i 0 (+ i 1))) ((= i (+ dsides 1)))
        (set! j (if (< i dsides) i (- i dsides)))
        (if (= t 0)
          (set! j (- dsides 1 j))
        )
        (if (> i 0)
          (glNormal3f (* (/ (+ (vector-ref x j)
                               (vector-ref x o)) 2.0) s)
                      (* size c)
                      (* (/ (+ (vector-ref z j)
                               (vector-ref z o)) 2.0) s) )
        )
        (glVertex3f (vector-ref x j) p2 (vector-ref z j))
        (set! o j)
      )
      (glEnd)
    )
    
  )
)


(define (makedisc size)
  (let ([q (gl-new-quadric)])
    (gl-quadric-draw-style q 'fill)
    (gl-quadric-normals q 'smooth)
    (gl-sphere q size 25 25)))

; -------------------------------------------------------------------

(define (makepyramid size)
  (glEnable GL_NORMALIZE)
  
  (glBegin GL_QUADS)
  (glNormal3f 0.0 (- size) 0.0)
  (glVertex3f    size  (- size)    size)
  (glVertex3f (- size) (- size)    size)
  (glVertex3f (- size) (- size) (- size))
  (glVertex3f    size  (- size) (- size))
  (glEnd)
  
  (glBegin GL_TRIANGLE_FAN)
  (glVertex3f     0.0     size      0.0)

  (glVertex3f    size  (- size)    size)
  (glNormal3f     2.0     -1.0      0.0)
  (glVertex3f    size  (- size) (- size))
  (glNormal3f     0.0     -1.0     -2.0)
  (glVertex3f (- size) (- size) (- size))
  (glNormal3f    -2.0     -1.0      0.0)
  (glVertex3f (- size) (- size)    size)
  (glNormal3f     0.0     -1.0      2.0)
  (glVertex3f    size  (- size)    size)
  (glEnd)

)

; -------------------------------------------------------------------

(define (makeicosahedron scale)
  (let*
    ( (coord #( #(-0.525731112119133606 0.0  0.850650808352039932)
                #( 0.525731112119133606 0.0  0.850650808352039932)
                #(-0.525731112119133606 0.0 -0.850650808352039932)
                #( 0.525731112119133606 0.0 -0.850650808352039932)
                
                #(0.0  0.850650808352039932  0.525731112119133606)
                #(0.0  0.850650808352039932 -0.525731112119133606)
                #(0.0 -0.850650808352039932  0.525731112119133606)
                #(0.0 -0.850650808352039932 -0.525731112119133606)

                #( 0.850650808352039932  0.525731112119133606 0.0)
                #(-0.850650808352039932  0.525731112119133606 0.0)
                #( 0.850650808352039932 -0.525731112119133606 0.0)
                #(-0.850650808352039932 -0.525731112119133606 0.0) ) )
      (indices #( #(1 4 0)  #(4 9 0)  #(4 5 9)  #(8 5 4)  #(1 8 4)
                  #(1 10 8) #(10 3 8) #(8 3 5)  #(3 2 5)  #(3 7 2)
                  #(3 10 7) #(10 6 7) #(6 11 7) #(6 0 11) #(6 1 0)
                  #(10 1 6) #(11 0 9) #(2 11 9) #(5 2 9)  #(11 2 7)))
      (triang #f)
      (p0 #f) (p1 #f) (p2 #f)
    )
    (glEnable GL_NORMALIZE)
    (glBegin GL_TRIANGLES)
    (do ((i 0 (+ i 1))) ((= i 20))
      (set! triang (vector-ref indices i))
      (set! p0 (vector-ref coord (vector-ref triang 0)))
      (set! p1 (vector-ref coord (vector-ref triang 1)))
      (set! p2 (vector-ref coord (vector-ref triang 2)))
      (norm (vector->list p0)
            (vector->list p1)
            (vector->list p2)
            1.0)
      (glVertex3f (* (vector-ref p0 0) scale)
                  (* (vector-ref p0 1) scale)
                  (* (vector-ref p0 2) scale) )
      (glVertex3f (* (vector-ref p1 0) scale)
                  (* (vector-ref p1 1) scale)
                  (* (vector-ref p1 2) scale) )
      (glVertex3f (* (vector-ref p2 0) scale)
                  (* (vector-ref p2 1) scale)
                  (* (vector-ref p2 2) scale) )
    )
    (glEnd)
  )
)

; -------------------------------------------------------------------

(define (makespiky scale)
  (let*
    ( (spikes  12)
      (spikez  0.5)
      (spikein 0.7)
      (x1 (make-vector spikes 0.0))
      (y1 (make-vector spikes 0.0))
      (x2 (make-vector spikes 0.0))
      (y2 (make-vector spikes 0.0))
      (p0 (make-vector 3 0.0))
      (p1 (make-vector 3 0.0))
      (p2 (make-vector 3 0.0))
      (b  (/ (* 3.1415927 2.0) spikes))
      (b2 (/ b 2.0))
      (a  #f) (j #f)
    )
    
    (glEnable GL_NORMALIZE)
    (do ((i 0 (+ i 1))) ((= i spikes))
      (set! a (* i b))
      (vector-set! x1 i (* (cos a) scale spikein))
      (vector-set! y1 i (* (sin a) scale spikein))
      (vector-set! x2 i (* (cos (+ b2 a)) scale))
      (vector-set! y2 i (* (sin (+ b2 a)) scale))
    )

    ; first side
    (glBegin GL_TRIANGLE_FAN)
      
    (vector-set! p0 0 0.0)
    (vector-set! p0 1 0.0)
    (vector-set! p0 2 (* spikez scale))
    (glVertex3fv (vector->gl-float-vector p0))
      
    (vector-set! p1 0 (vector-ref x1 0))
    (vector-set! p1 1 (vector-ref y1 0))
    (vector-set! p1 2 0.0)
    (glVertex3fv (vector->gl-float-vector p1))
      
    (do ((i 0 (+ i 1))) ((= i spikes))
      (set! j (+ i 1))
      (if (>= j spikes)
        (set! j (- j spikes))
      )
      
      (vector-set! p2 0 (vector-ref x2 i))
      (vector-set! p2 1 (vector-ref y2 i))
      (vector-set! p2 2 0.0)
      (norm (vector->list p0)
            (vector->list p1)
            (vector->list p2)
            1.0)
      (glVertex3fv (vector->gl-float-vector p2))

      (vector-set! p1 0 (vector-ref x1 j))
      (vector-set! p1 1 (vector-ref y1 j))
      (vector-set! p1 2 0.0)
      (norm (vector->list p0)
            (vector->list p2)
            (vector->list p1)
            1.0)
      (glVertex3fv (vector->gl-float-vector p1))
    )
      
    (glEnd)

    ; second side
    (glBegin GL_TRIANGLE_FAN)
      
    (vector-set! p0 0 0.0)
    (vector-set! p0 1 0.0)
    (vector-set! p0 2 (* (- spikez) scale))
    (glVertex3fv (vector->gl-float-vector p0))
      
    (vector-set! p1 0 (vector-ref x1 0))
    (vector-set! p1 1 (vector-ref y1 0))
    (vector-set! p1 2 0.0)
    (glVertex3fv (vector->gl-float-vector p1))
      
    (do ((i 0 (+ i 1))) ((= i spikes))
      (set! j (+ i 1))
      (if (>= j spikes)
        (set! j (- j spikes))
      )
      
      (vector-set! p2 0 (vector-ref x2 i))
      (vector-set! p2 1 (vector-ref y2 i))
      (vector-set! p2 2 0.0)
      (norm (vector->list p0)
            (vector->list p1)
            (vector->list p2)
            -1.0)
      (glVertex3fv (vector->gl-float-vector p2))

      (vector-set! p1 0 (vector-ref x1 j))
      (vector-set! p1 1 (vector-ref y1 j))
      (vector-set! p1 2 0.0)
      (norm (vector->list p0)
            (vector->list p2)
            (vector->list p1)
            -1.0)
      (glVertex3fv (vector->gl-float-vector p1))
    )
      
    (glEnd)
    
  )
)


; -------------------------------------------------------------------

) ; end of module

