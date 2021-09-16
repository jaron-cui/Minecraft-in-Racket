;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname MinecraftInRacket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct xyz (x y z))
; An XYZ is a (make-xyz Number Number Number) and represents a three-dimensional composite value.

; A Position is an XYZ that represents a point in 3D space.
; An Angle is an XYZ that represents the angular orientation of a vector in 3D space.

(define-struct vector (pos angle))
; A Vector is a (make-vector Position Angle) and represents a vector in 3D space.

(define-struct save (player world actions))
; A Save is a (make-save Vector World [Map-of Key [Save -> Save]]) that represents game data where 'cam' is the player view
; orientation and 'world' is the world data.

(define-struct face (origin points color))
; A Face is a (make-face Position [List-of Position] Color) and represents a geometric face in 3D space.

(define-struct f-group (id pos faces))
; A [FaceGroup X] is a (make-f-group X Position [List-of Face]) and represents a grouping of faces
; for optimizational or organizational purposes.

(define-struct world (chunks))
; A World is a (make-world [List-of Chunk]) and represents the world data for a game instance.

(define-struct chunk (pos blocks entities block-faces entity-faces))
; A Chunk is a (make-chunk Position [Tree-of [Tree-of [Tree-of Block]]] [List-of Entity] [List-of [FaceGroup Position]]) that
; represents a 16x16x16 chunk of the world.

(define-struct collider (hrad vrad pos vel))
; A Collider is a (make-collider NNN NNN Position XYZ) and represents a simulated body which collides
; with blocks.

(define-struct entity (id pos vel rads angle faces))
; An Entity is a (make-entity Natural Vector [List-of Face]) that represents a non-player entity in the
; world.

#;(define (save-cam save)
    (local [(define player (save-player save))
            (define pos (collider-pos (entity-hitbox player)))
            (define angle (entity-angle player))]
      (make-vector pos angle)))

; spawn : String NNN NNN FaceGroup -> Entity
; Creates a new default entity.
(define (spawn id rads faces)
  (make-entity id (make-xyz 0 0 0) (make-xyz 0 0 0) rads (make-xyz 0 0 0) faces))

; tp : Entity Position -> Entity
; Teleports the entity to a new position in blocks.
(define (tp entity pos)
  (set-pos entity (scale-xyz pos block-size)))

(define (set-pos entity pos)
  (make-entity (entity-id entity) pos (entity-vel entity) (entity-rads entity)
               (entity-angle entity) (entity-faces entity)))

; set-vel : Entity Velocity -> Entity
; Sets the velocity of the entity.
(define (set-vel entity vel)
  (make-entity (entity-id entity) (entity-pos entity) vel (entity-rads entity)
               (entity-angle entity) (entity-faces entity)))

; direct : Entity Angle -> Entity
; Directs the angle of the entity to a new angle.
(define (direct entity angle)
  (make-entity (entity-id entity) (entity-pos entity) (entity-vel entity) (entity-rads entity)
               angle (entity-faces entity)))

; PHYSICS
(define gravity -.4)
(define make-steve (λ (pos) (tp (spawn "steve" (make-xyz .3 .3 1) '()) pos)))

#;(define (set-collider-pos collider new-pos)
    (make-collider (collider-hrad collider) (collider-vrad collider) new-pos
                   (collider-vel collider)))
#;(define (set-collider-vel collider new-vel)
    (make-collider (collider-hrad collider) (collider-vrad collider)
                   (collider-pos collider) new-vel))

; horizontal-speed : Entity -> NNN
; Returns the horizontal speed of the entity.
(define (horizontal-speed entity)
  (local [(define (sqr-field accessor)
            (expt (accessor (entity-vel entity)) 2))]
    (sqrt (+ (sqr-field xyz-x) (sqr-field xyz-y)))))

(define (accelerate entity axis acceleration limit)
  (local [(define vel (entity-vel entity))
          (define axis-vel (axis vel))
          (define new-vel
            (if (>= (abs axis-vel) limit)
                vel (local [(define incremented (+ axis-vel acceleration))
                            (define set-axis (axis xyz-setters))]
                      (if (>= (abs incremented) limit)
                          (set-axis vel (* (sign incremented) limit))
                          (set-axis vel incremented)))))]
    (set-vel entity new-vel)))

; accelerate-horizontally : Entity Number Number Number -> Entity
; Accelerates the entity in a horizontal direction with a speed limit.
(define (accelerate-horizontally entity direction acceleration limit)
  (local [(define vel (entity-vel entity))
          (define (limited-accel axis scale)
            (local [(define accel (* scale acceleration))
                    (define limited (* scale limit))
                    (define axis-vel (axis vel))
                    (define s (sign axis-vel))
                    (define axis-speed (abs axis-vel))
                    (define unlimited (+ axis-vel accel))
                    (define unlimited-mag (abs unlimited))]
              (if (and (>= unlimited-mag axis-speed) (>= unlimited-mag limited))
                  limited unlimited)))
          (define (accelerate-axis entity axis scale)
            (accelerate entity axis (* scale acceleration) (abs (* scale limit))))]
    (accelerate-axis (accelerate-axis entity xyz-y (deg-sin direction)) xyz-x (deg-cos direction))))

#;(define (accelerate entity direction acceleration limit))


(define (set-x xyz x)
  (make-xyz x (xyz-y xyz) (xyz-z xyz)))
(define (set-y xyz y)
  (make-xyz (xyz-x xyz) y (xyz-z xyz)))
(define (set-z xyz z)
  (make-xyz (xyz-x xyz) (xyz-y xyz) z))
(define xyz-setters (make-xyz set-x set-y set-z))

(define (round-custom edge-case)
  (λ (num) (local [(define low (floor num))
                   (define high (ceiling num))
                   (define diff (- num low))]
             (cond [(= diff .5) (edge-case num low high)]
                   [(< diff .5) low]
                   [else high]))))

(define round-down (round-custom (λ (num low high) low)))
(define round-up (round-custom (λ (num low high) high)))

(define (round-toward num direction)
  ((if (< direction 0) round-down round-up) num))

(define (search-block-layer world condition? axis-pos from1 from2 to1 to2 axis perif1 perif2)
  (local [(define start1 (min from1 to1))
          (define start2 (min from2 to2))
          (define end1 (max from1 to1))
          (define end2 (max from2 to2))

          (define (pack-pos axis-pos perif1-pos perif2-pos)
            ((axis xyz-setters)
             ((perif1 xyz-setters)
              ((perif2 xyz-setters) (make-xyz #f #f #f)
                                    perif2-pos) perif1-pos) axis-pos))
          
          (define (iterate1 at1 at2)
            (if (<= at1 end1)
                (local [(define pos (pack-pos axis-pos at1 at2))]
                  (if (condition? (get-world-block world pos))
                      pos (iterate1 (add1 at1) at2)))
                #f))
          
          (define (iterate2 at2)
            (if (<= at2 end2)
                (local [(define search1 (iterate1 start1 at2))]
                  (if (boolean? search1)
                      (iterate2 (add1 at2)) search1))
                #f))]
    (iterate2 start2)))

(define (search-block-layers world condition? from to from1 from2 to1 to2 axis perif1 perif2)
  (local [(define negative-inc? (> from to))
          (define not-done? (if negative-inc? >= <=))
          (define inc (if negative-inc? -1 1))
          
          (define (iterate at)
            (if (not-done? at to)
                (local [(define search (search-block-layer world condition? at from1 from2 to1 to2
                                                           axis perif1 perif2))]
                  (if (boolean? search)
                      (iterate (+ at inc)) search))
                #f))]
    (iterate from)))

(define (block-collision-pos entity world block-condition? axis perif1 perif2)
  (local [(define vel (/ (axis (entity-vel entity)) block-size))]
    (if (= vel 0) #f
        (local [(define rads (entity-rads entity))
                (define pos (realpos->blockpos (entity-pos entity)))
         
                (define s (sign vel))
                (define axis-pos (axis pos))
                (define axis-rad (axis rads))
                (define (block-at at)
                  (round-toward (+ at s) (- s)))
                ; (block-collision-pos (set-vel (make-steve (make-xyz 0 0 1.5)) (make-xyz 0 0 0)) world0 xyz-z xyz-x xyz-y)
                (define perif1-pos (perif1 pos))
                (define perif1-rad (perif1 rads))
                (define perif2-pos (perif2 pos))
                (define perif2-rad (perif2 rads))
                (define perif1-start (round-up (- perif1-pos perif1-rad)))
                (define perif1-end (round-down (+ perif1-pos perif1-rad)))
                (define perif2-start (round-up (- perif2-pos perif2-rad)))
                (define perif2-end (round-down (+ perif2-pos perif2-rad)))
                (define hitbox-edge (+ axis-pos (* s axis-rad)))
                (define possible-dest (+ hitbox-edge vel))
                (define axis-start (round-toward (+ hitbox-edge s) (- s)))
                (define axis-end (round-toward possible-dest s))]
          (if (= (round-toward hitbox-edge (- s)) (round-toward possible-dest s))
              #f (search-block-layers world block-condition? axis-start axis-end
                                      perif1-start perif2-start perif1-end perif2-end
                                      axis perif1 perif2))))))

(define (do-for-until start end inc op pred base)
  (if (not (or (= (- start inc) end) (pred base)))
      (do-for-until (+ start inc) end inc op pred (op start base)) base))

(define (block-collision-distance entity world block-condition? axis perif1 perif2)
  (local [(define axis-at (axis (entity-pos entity)))
          (define dest-pos
            (block-collision-pos entity world block-condition? axis perif1 perif2))]
    (if (xyz? dest-pos)
        (local [(define delta (- (* (axis dest-pos) block-size) axis-at))]
          (- delta (* (sign delta) (* (+ (axis (entity-rads entity)) .5) block-size))));(error (- delta (* (sign delta) (+ (axis (entity-rads entity)) .5))) " " axis-at " " delta))
        #f)))

;(search-block-layers world condition? from to from1 from2 to1 to2 axis perif1 perif2)
(define (grounded? entity world)
  (local [(define pos (scale-xyz (entity-pos entity) (/ 1 block-size)))
          (define x (xyz-x pos))
          (define y (xyz-y pos))
          (define rads (entity-rads entity))
          (define foot-z (- (xyz-z pos) (xyz-z rads)))
          (define xrad (xyz-x rads))
          (define yrad (xyz-y rads))
          
          (define ground-level (- foot-z .5))
          (define x-start (round-up (- x xrad)))
          (define x-end (round-down (+ x xrad)))
          (define y-start (round-up (- y yrad)))
          (define y-end (round-down (+ y yrad)))]
    (and (integer? ground-level)
         (xyz? (search-block-layers world solid? ground-level ground-level
                                    x-start y-start x-end y-end xyz-z xyz-x xyz-y)))))

(define cursor-range 8)

(define (<=others subject a b)
  (and (<= subject a) (<= subject b)))
#;(define (<=others subject a b)
    (local [(define mag (abs subject))]
      (and (<= mag (abs a)) (<= mag (abs b)))))
(define (cursor-blockface-intersects pos pitch yaw world)
  (local [(define (round-to-blockface coord dir)
            (+ (round-toward (- coord (* dir .5)) dir) (* dir .5)))

          (define xy-scale (abs (deg-cos pitch)))
          (define x-dist (* xy-scale cursor-range (deg-cos yaw)))
          (define y-dist (* xy-scale cursor-range (deg-sin yaw)))
          (define z-dist (* cursor-range (deg-sin (- pitch))))
          (define x-sign (sign x-dist))
          (define y-sign (sign y-dist))
          (define z-sign (sign z-dist))
          (define distances (make-xyz x-dist y-dist z-dist))

          (define (relative-start axis)
            (- (round-to-blockface (axis pos) (sign (axis distances))) (axis pos)))

          (define (block-pos-at-ratio ratio)
            (local [(define (helper axis)
                      (round-toward (+ (* ratio (axis distances)) (axis pos)) (axis distances)))]
              (make-xyz (helper xyz-x) (helper xyz-y) (helper xyz-z))))
          (define (divide-or-2 dividend divisor)
            (if (= divisor 0)
                2 (/ dividend divisor)))
          (define (search x-face y-face z-face)
            (local [(define x-ratio (divide-or-2 x-face x-dist))
                    (define y-ratio (divide-or-2 y-face y-dist))
                    (define z-ratio (divide-or-2 z-face z-dist))
                    (define (iterate ratio new-x-face new-y-face new-z-face)
                      (if (<= (abs ratio) 1)
                          (local [(define block-pos (block-pos-at-ratio ratio))]
                            (if (solid? (get-world-block world block-pos))
                                (local [(define (h val)
                                          (if (= val (- (ceiling val) .5)) val (round val)))]
                                  (xyz-map (add-xyz pos (scale-xyz distances ratio)) h))
                                (search new-x-face new-y-face new-z-face)))
                          #f))]
              (cond [(<=others x-ratio y-ratio z-ratio)
                     (iterate x-ratio (+ x-face x-sign) y-face z-face)]
                    [(<=others y-ratio x-ratio z-ratio)
                     (iterate y-ratio x-face (+ y-face y-sign) z-face)]
                    [(<=others z-ratio x-ratio y-ratio)
                     (iterate z-ratio x-face y-face (+ z-face z-sign))])))]
    (search (relative-start xyz-x) (relative-start xyz-y) (relative-start xyz-z))
    ))

(define (f save)
  (local [(define player (save-player save))
          (define world (save-world save))]
    (cursor-blockface-intersects
     (realpos->blockpos (entity-pos player))
     (xyz-x (entity-angle player))
     (xyz-z (entity-angle player))
     world)))

#;(define (cursor-block player world)
    (local [(define pos (entity-pos player))]))

(define ground-friction .3) ;.3)

(define (step-entity-physics ungravitated world)
  (local [(define entity (set-vel ungravitated (add-z (entity-vel ungravitated) gravity)))
          (define pos (entity-pos entity))
          (define vel (entity-vel entity))
          
          (define (find-axis-collision-vel axis perif1 perif2)
            (block-collision-distance entity world solid? axis perif1 perif2))
          
          (define collision-vel
            (make-xyz (find-axis-collision-vel xyz-x xyz-z xyz-y)
                      (find-axis-collision-vel xyz-y xyz-x xyz-z)
                      (find-axis-collision-vel xyz-z xyz-x xyz-y)))
          
          (define (new-axis-pos get-axis)
            (local [(define axis-collision-vel (get-axis collision-vel))]
              (+ (get-axis pos)
                 (if (number? axis-collision-vel)
                     axis-collision-vel (get-axis vel)))))
          
          (define (new-axis-vel get-axis friction)
            (local [(define axis-vel (get-axis vel))]
              (if (or (number? (get-axis collision-vel)) (>= friction (abs axis-vel)))
                  0 (- axis-vel (* (sign axis-vel) friction)))))
          
          (define new-pos (make-xyz (new-axis-pos xyz-x) (new-axis-pos xyz-y) (new-axis-pos xyz-z)))
          (define new-vel (make-xyz (new-axis-vel xyz-x ground-friction) (new-axis-vel xyz-y ground-friction)
                                    (new-axis-vel xyz-z 0)))]
    (set-pos (set-vel entity new-vel) new-pos)))


(define (realpos->blockpos pos)
  (scale-xyz pos (/ 1 block-size)))


; A [Tree-of X Y] is one of:
(define-struct node (key val left right))
; - (make-node Y 'none [Tree-of X Y] [Tree-of X Y])
; - (make-node Y X [Tree-of X Y] [Tree-of X Y])
(define-struct leaf ())
(define lf (make-leaf))
; - (make-leaf)

; floor-div : Number Number -> Integer
; Returns the floor of the quotient of the given numbers.
(define (floor-div a b)
  (floor (/ a b)))

; generate-binary-tree : {X} X Integer Integer -> [Tree-of X Integer]
; Generates an ordered binary tree containing all keys in the range [a, b) for some
; a and b, filled with an item of type X.
(check-expect (generate-binary-tree 'none 0 0) lf)
(check-expect (generate-binary-tree 'none 0 1)
              (make-node 0 'none lf lf))
(check-expect (generate-binary-tree 'none 1 3)
              (make-node 2 'none
                         (make-node 1 'none lf lf) lf))
(check-expect (generate-binary-tree 'none 4 7)
              (make-node 5 'none
                         (make-node 4 'none lf lf)
                         (make-node 6 'none lf lf)))

(define (generate-binary-tree content min max)
  (local [(define mid (floor-div (+ min max) 2))]
    (if (= min max)
        lf
        (make-node mid content
                   (generate-binary-tree content min mid)
                   (generate-binary-tree content (add1 mid) max)))))

#;(define empty-block-tree
    (local [(define empty-block-row (generate-binary-tree 0 0 16))
            (define empty-block-layer (generate-binary-tree empty-block-row 0 16))]
      (generate-binary-tree empty-block-layer 0 16)))

(define empty-block-tree (make-node 8 (make-node 8 (make-node 8 0 lf lf) lf lf) lf lf))


; t-get : [Tree-of X Integer] Integer X -> X
(define (t-get tree index default)
  (local [(define (nav key)
            (cond [(= key index) (node-val tree)]
                  [(< index key) (t-get (node-left tree) index default)]
                  [else (t-get (node-right tree) index default)]))]
    (cond [(leaf? tree) default]
          [(node? tree) (nav (node-key tree))])))

;; ABSTRACTED VERSIONS:
#;(define (t-get tree index default)
    (t-element-op tree index default
                  (λ (t) (t-get (node-left t) index default))
                  (λ (t) (t-get (node-right t) index default))
                  node-val))

#;(define (t-element-op tree index leaf-result left-op right-op dest-op)
    (local [(define (nav key)
              (cond [(= key index) (dest-op tree)]
                    [(< key index) (left-op tree)]
                    [else (right-op tree)]))]
      (cond [(leaf? tree) leaf-result]
            [(node? tree) (nav (node-key tree))])))

(define (t-element-op tree index op)
  (local [(define (nav key)
            (cond [(= index key)
                   (make-node key (op (node-val tree)) (node-left tree) (node-right tree))]
                  [(< index key)
                   (make-node key (node-val tree)
                              (t-element-op (node-left tree) index op) (node-right tree))]
                  [else (make-node key (node-val tree)
                                   (node-left tree) (t-element-op (node-right tree) index op))]))]
    (cond [(leaf? tree) lf]
          [(node? tree) (nav (node-key tree))])))

; get-world-block : World Position -> Block
(define (get-world-block world pos)
  (local [(define local-pos (xyz-map pos (λ (axis) (modulo axis 16))))
          (define chunk (chunk-at-block pos world))]
    (if (chunk? chunk)
        (get-chunk-block chunk local-pos) 1)))

(define (chunk-at-block pos world)
  (find-first (λ (chunk) (xyz=? (chunk-pos chunk) (xyz-map pos (λ (val) (floor-div val 16)))))
              (world-chunks world)))

(define (get-chunk-block chunk pos)
  (local [(define blocks (chunk-blocks chunk))
          (define default 0)
          (define (try-get tree accessor)
            (if (node? tree) (t-get tree (accessor pos) default) default))]
    (try-get (try-get (try-get blocks xyz-x) xyz-y) xyz-z)))

(define (world-to-chunk-pos pos)
  (xyz-map (λ (axis) (mod axis 16)) pos))

(define (set-world-block world pos type)
  (local [(define local-pos (world-to-chunk-pos pos))]
    (t-element-op )))
;(t-element-op tree index op)
;(pos blocks entities block-faces entity-faces)

; set-chunk-block : Chunk Position Block -> Chunk
#;(define (set-chunk-block chunk pos type)
    (local [(define new-blocks
              (t-element-op (chunk-blocks chunk) (xyz-x pos)
                            (λ (x-axis) (t-element-op x-axis (xyz-y pos)
                                                      (λ (y-axis) (t-element-op y-axis (xyz-z pos)
                                                                                (λ (_) type)))))))]
      (make-chunk (chunk-pos chunk) new-blocks (chunk-entities chunk) (chunk-block-faces chunk)
                  (chunk-entity-faces chunk))))

(define (set-chunk-block chunk pos type)
  (local [(define new-blocks
            (t-gen-op (chunk-blocks chunk) (xyz-x pos)
                      (λ (node) (t-gen-op node (xyz-y pos)
                                          (λ (node) (t-gen-op node (xyz-z pos) (λ (_) type) 0 8))
                                          (make-node 8 0 lf lf) 8))
                      (make-node 8 (make-node 8 0 lf lf) lf lf) 8))]
    (make-chunk (chunk-pos chunk) new-blocks (chunk-entities chunk) (chunk-block-faces chunk)
                (chunk-entity-faces chunk))))

(define (t-gen-op node index op default inc)
  (local [(define (dec n)
            (max 1 (quotient n 2)))
          (define (grow-branch closer in history)
            (local [(define e (if (= in 0) (error "inc is 0, index " index ", arrived at " closer ", default is " default " TREE "
                                                  " history: " history) '()))
                    (define inc (dec in))]
              (cond [(= closer index)
                     (make-node index (op default) lf lf)]
                    [(< index closer) (make-node closer default
                                                 (grow-branch (- closer inc) inc (cons closer history)) lf)]
                    [else (make-node closer default lf (grow-branch (+ closer inc) inc (cons closer history)))])))
          
          (define (navigate node key inc history)
            (cond [(= index key)
                   (make-node key (op (node-val node)) (node-left node) (node-right node))]
                  [(< index key)
                   (make-node key (node-val node)
                              (t-gen-op-acc (node-left node) (dec inc) key (cons key history))
                              (node-right node))]
                  [else (make-node key (node-val node)
                                   (node-left node)
                                   (t-gen-op-acc (node-right node) (dec inc) key (cons key history)))]))
          (define (t-gen-op-acc tree inc last history)
            (cond [(leaf? tree)
                   (grow-branch (if (< index last) (- last inc) (+ last inc)) inc (cons last (cons 'leaf! history)))]
                  [(node? tree)
                   (navigate tree (node-key tree) inc (cons last (cons 'node! history)))]))]
    (t-gen-op-acc node inc (node-key node) '())))

(define (add-x xyz n)
  (make-xyz (+ (xyz-x xyz) n) (xyz-y xyz) (xyz-z xyz)))

(define (add-y xyz n)
  (make-xyz (xyz-x xyz) (+ (xyz-y xyz) n) (xyz-z xyz)))

(define (add-z xyz n)
  (make-xyz (xyz-x xyz) (xyz-y xyz) (+ (xyz-z xyz) n)))

(define (sub-x xyz n)
  (add-x xyz (- n)))

(define (sub-y xyz n)
  (add-y xyz (- n)))

(define (sub-z xyz n)
  (add-z xyz (- n)))

(define (add-xyz a b)
  (local [(define (add-axis accessor)
            (+ (accessor a) (accessor b)))]
    (make-xyz (add-axis xyz-x) (add-axis xyz-y) (add-axis xyz-z))))

(define (gen-block-face origin color base-add axis1-add axis2-add)
  (local [(define rad (/ block-size 2))
          (define center (base-add origin rad))
          (define wing-a (axis1-add center rad))
          (define wing-b (axis1-add center (- rad)))
          (define p1 (axis2-add wing-a rad))
          (define p2 (axis2-add wing-b rad))
          (define p3 (axis2-add wing-b (- rad)))
          (define p4 (axis2-add wing-a (- rad)))]
    (make-face center (list p1 p2 p3 p4) color)))


; cull-nonfacing : [FaceGroup Position] Position -> [FaceGroup Number]
(define (cull-nonfacing group from)
  (local [(define center (f-group-pos group))
          (define (ordered? a b c)
            (or (> a b c) (< a b c)))

          (define (cull-faces faces)
            (cond [(empty? faces) '()]
                  [else (local [(define face (first faces))
                                (define pos (face-origin face))
                                (define culled (cull-faces (rest faces)))]
                          (if (or (ordered? (xyz-x from) (xyz-x pos) (xyz-x center))
                                  (ordered? (xyz-y from) (xyz-y pos) (xyz-y center))
                                  (ordered? (xyz-z from) (xyz-z pos) (xyz-z center)))
                              (cons face culled) culled))]))
          (define culled (cull-faces (f-group-faces group)))]
    (make-f-group (distance from center) (f-group-pos group) culled)))


;(define-struct chunk (pos blocks entities block-faces entity-faces))

; gen-chunk-terrain-faces : Chunk -> Chunk
#;(define (gen-chunk-terrain-faces chunk)
    (local [(define (gen-z x y z generated)
              (local [(define local-pos (make-xyz x y z))
                      (define type (get-chunk-block chunk local-pos))]
                (if (< z 16)
                    (gen-z x y (add1 z)
                           (if (solid? type)
                               (cons (gen-block-exposed-faces chunk local-pos type) generated)
                               generated)) generated)))
            (define (gen-y x y generated)
              (if (< y 16)
                  (gen-y x (add1 y) (gen-z x y 0 generated)) generated))
            (define (gen-x x generated)
              (if (< x 16)
                  (gen-x (add1 x) (gen-y x 0 generated)) generated))]
      (make-chunk (chunk-pos chunk) (chunk-blocks chunk) (chunk-entities chunk) ;(gen-x 0 '())
                  (chunk-entity-faces chunk))))

(define-struct block-texture (id top bottom side1 side2 side3 side4))
(define top block-texture-top)
(define bottom block-texture-bottom)
(define side1 block-texture-side1)
(define side2 block-texture-side1)
(define side3 block-texture-side1)
(define side4 block-texture-side1)

(define (monocolor-texture id color)
  (make-block-texture id color color color color color color))

(define grass-texture
  (make-block-texture
   1 "forest green" "sienna" "sienna" "sienna" "sienna" "sienna"))
(define stone-texture (monocolor-texture 2 "light slate gray"))
(define sand-texture (monocolor-texture 3 "beige"))
(define dirt-texture (monocolor-texture 4 "sienna"))
(define wood-texture
  (make-block-texture
   5 "burlywood" "burlywood" "saddle brown" "saddle brown" "saddle brown" "saddle brown"))
(define leaves-texture (monocolor-texture 6 "dark olive green"))
(define water-texture (monocolor-texture 7 "teal"))


(define block-textures (list grass-texture stone-texture sand-texture dirt-texture wood-texture
                             leaves-texture water-texture))

(define (get-block-texture type side)
  (local [(define (search textures)
            (cond [(empty? textures) "pink"]
                  [(cons? textures) (local [(define texture (first textures))]
                                      (if (= (block-texture-id texture) type)
                                          (side texture) (search (rest textures))))]))]
    (search block-textures)))

; gen-block-exposed-faces : Block Integer Integer Integer -> [FaceGroup Position]
; Generates a face group containing all exposed faces of the block identified by the block origin
; in actual global coordinates.
(define (gen-block-exposed-faces chunk type local-pos)
  (local [(define global-pos (add-xyz (scale-xyz (chunk-pos chunk) 16) local-pos))
          (define origin
            (scale-xyz global-pos block-size))
          
          (define (gen-if-exposed add-facing add-side1 add-side2 side base)
            (if (air? (get-chunk-block chunk (add-facing local-pos 1)))
                (cons (gen-block-face origin
                                      (get-block-texture type side)
                                      add-facing add-side1 add-side2) base) base))
          
          (define f1 (gen-if-exposed add-x add-y add-z side1 '()))
          (define f2 (gen-if-exposed add-y add-x add-z side2 f1))
          (define f3 (gen-if-exposed add-z add-x add-y top f2))
          (define f4 (gen-if-exposed sub-x add-y add-z side3 f3))
          (define f5 (gen-if-exposed sub-y add-x add-z side4 f4))
          (define f6 (gen-if-exposed sub-z add-x add-y bottom f5))]
    (make-f-group type origin f6)))

(define (air? block-type)
  (= block-type 0))
(define (solid? block-type)
  (not (or (= block-type 0) (= block-type 7))))

(define width 900)
(define height 600)
(define screen-tolerance 30)
(define background (rectangle width height "solid" "lightblue"))
(define block-size 16)

(define current-pos (make-vector (make-xyz (* 5 16) (* 5 16) (* 2 16)) (make-xyz 0 0 45)))
(define example-point (make-xyz 10 10 -3))

; render-world : World Vector -> Image
; Given the world data and player orientation, renders the world onto a background.
(define (render-world world player)
  (local [(define pos (entity-pos player))
          (define focus-chunk (chunk-at pos world))
          (define err (if (chunk? focus-chunk) #f (error (scale-xyz pos (/ 1 16)))))
          (define chunk-faces (cull-groups (chunk-block-faces focus-chunk) pos))
          (define (render-face face img)
            (render-face-onto face img player))]
    (foldr (λ (group result) (foldr render-face result (f-group-faces group))) background chunk-faces)
    ))

#;(define (cull-groups groups pos)
    (cond [(empty? groups) '()]
          [(cons? groups)
           (local [(define culled (cull-nonfacing (first groups) pos))
                   (define culled-rest (cull-groups (rest groups) pos))]
             (if (empty? (f-group-faces culled))
                 culled-rest (cons culled culled-rest)))]))

(define empty-map (λ (same-pred?) (make-linmap same-pred? '())))

#;(define (render-f-groups groups player img)
    (local [; Add the points in each face to the point cache
            (define (cache-faces faces point-map)
              (cond [(empty? faces) point-map]
                    [(cons? faces)
                     (cache-faces (rest faces)
                                  (cache-face-points (face-points (first faces)) point-map))]))
            (define (cache-face-points points point-map)
              (cond [(empty? points) point-map]
                    [(cons? points)
                     (local [(define point (first points))]
                       (cache-face-points (rest points)
                                          (map-transform point-map point
                                                         (λ (_) (screen-pos point player)) #f)))]))
            (define (render-and-cache groups point-map img)
              (cond [(empty? groups) img]
                    [(cons? groups)
                     (local [(define group (first groups))
                             (define updated-map (cache-faces (f-group-faces group) point-map))]
                       (render-and-cache (rest groups) updated-map
                                         (render-faces (f-group-faces group) updated-map img)))]))
            (define (render-faces faces point-map img)
              (cond [(empty? faces) img]
                    [(cons? faces)
                     (render-faces (rest faces) point-map
                                   (render-face-from-cache (first faces) point-map img))]))
            ]
      (render-and-cache (cull-groups groups (vector-pos player)) (empty-map xyz=?) img)))

(define (render-face-from-cache face point-map img)
  (local [(define points (face-points face))]
    (if (length-at-least? points 3)
        (scene+polygon img (map (λ (point) (map-get point-map point)) points) "outline" "black")
        img)))


(define (cull-groups groups pos)
  (local [(define (closer? group-a group-b)
            (< (manhattan-distance pos (f-group-pos group-a))
               (manhattan-distance pos (f-group-pos group-b))))
          ; clean up: no longer does sorting.
          (define (cull-groups-sort groups sorted)
            (cond [(empty? groups) sorted]
                  [(cons? groups)
                   (local [(define processed (cull-nonfacing (first groups) pos))
                           (define remaining (rest groups))]
                     (if (empty? (f-group-faces processed))
                         (cull-groups-sort remaining sorted)
                         (cull-groups-sort remaining
                                           (cons processed sorted)
                                           ;(insert-sorted sorted processed farther?)
                                           )))]))
          ;(define done (filter (λ (group) (cons? (f-group-faces group))) (map (λ (group) (cull-nonfacing group pos)) groups)))
          ]
    (sort
     ;(cull-groups-sort groups '())
     (filter (λ (group) (cons? (f-group-faces group)))
             (map (λ (group) (cull-nonfacing group pos)) groups))
     closer?)))
; HERE!
(define (manhattan-distance pos-a pos-b)
  (local [(define (d accessor)
            (abs (- (accessor pos-a) (accessor pos-b))))]
    (+ (d xyz-x) (d xyz-y) (d xyz-z))))

(define (insert-sorted list item less?)
  (cond [(empty? list) (cons item '())]
        [(cons? list)
         (local [(define current (first list))
                 (define list-rest (rest list))]
           (if (less? item current)
               (cons current (insert-sorted list-rest item less?))
               (cons item list)))]))

(define (chunk-at pos world)
  (get-chunk (xyz-map pos (λ (axis) (floor-div axis (* 16 block-size))))
             (world-chunks world)))

(define (get-chunk pos chunks)
  (find-first (λ (chunk) (xyz=? (chunk-pos chunk) pos)) chunks))

; xyz=? : XYZ XYZ -> Boolean
; Returns #true if all parallel dimensions of the XYZs are equal and #false otherwise.
(define (xyz=? xyz-a xyz-b)
  (local [(define (same-for? field)
            (= (field xyz-a) (field xyz-b)))]
    (and (same-for? xyz-x) (same-for? xyz-y) (same-for? xyz-z))))

; xyz-map : XYZ [Number -> Number] -> XYZ
; Maps the provided numerical operation over the fields of an XYZ.
(define (xyz-map xyz op)
  (make-xyz (op (xyz-x xyz)) (op (xyz-y xyz)) (op (xyz-z xyz))))

; map-verts : [List-of Position] Vector -> [List-of Posn]
; Maps 3D points to the screen based on a camera orientation.
(define (map-verts verts player)
  (local []
    (cond [(empty? verts) '()]
          [else (local [(define mapped (screen-pos (first verts) player))
                        (define da (xyz-angle-diff (entity-angle player)
                                                   (xyz-angle-to (entity-pos player) (first verts))))
                        (define rest-verts (map-verts (rest verts) player))]
                  (if (and (< (abs (xyz-z da)) (+ fov-rad 15)) (in-bounds? mapped))
                      (cons mapped rest-verts) rest-verts))])))

; in-bounds? : Posn -> Boolean
; Returns #true if the 2D position is within bounds of the visible screen.
(define (in-bounds? posn)
  (local [(define x (posn-x posn))
          (define y (posn-y posn))]
    (and (> x (- screen-tolerance)) (> y (- screen-tolerance))
         (< x (+ width screen-tolerance)) (< y (+ width screen-tolerance)))))

; render-face-onto : Face Image Vector -> Image
; Renders a face onto an image given a camera angle.
(define (render-face-onto face img player)
  (local [(define vertices (map-verts (face-points face) player);(map (λ (point) (screen-pos point player)) (face-points face))
            )]
    (if (length-at-least? vertices 3)
        (scene+polygon (scene+polygon img vertices "solid" (face-color face))
                       vertices "outline" "black")
        img)))

; fill-list : {X} X Natural [List-of X] -> [List-of X]
; Adds a value to the front of a list a certain number of times.
(define (fill-list content length base)
  (if (> length 0)
      (cons content (fill-list content (- length 1) base))
      base))

; gen-chunk : Natural Position -> Chunk
; Generates a chunk with a certain number of solid layers beginning from the bottom and air for the
; remaining.
#;(define (gen-chunk layers coords)
    (local [(define (fill-layer content)
              (fill-list (fill-list content 16 '()) 16 '()))
            (define filled (fill-layer 1))
            (define air (fill-layer 0))]
      (make-chunk coords (fill-list filled layers (fill-list air (- 16 layers) '())) '())))

#;(define (gen-chunk layers coords)
    (local [(define blank-chunk (make-chunk coords empty-block-tree '() #false #false))
            (define b1 (set-chunk-block blank-chunk (make-xyz 5 5 4) 1))
            (define b2 (set-chunk-block b1 (make-xyz 5 5 3) 1))
            (define b3 (set-chunk-block b2 (make-xyz 5 5 2) 1))
            (define b4 (set-chunk-block b3 (make-xyz 5 5 1) 1))]
      (gen-chunk-terrain-faces b4)))

; gen-block-face : Natural Natural Natural Natural -> Face
; Generates a block face for a given block type at the block coordinates.
#;(define (gen-block-face type x y z)
    (local [(define top
              (make-face (cons (make-xyz x y z)
                               (cons (make-xyz (+ x block-size) y z)
                                     (cons (make-xyz (+ x block-size) (+ y block-size) z)
                                           (cons (make-xyz x (+ y block-size) z) '()))))))]
      (if (= type 1)
          top (make-face '()))))

; scale-xyz : XYZ Number -> XYZ
; Scales the values in an XYZ.
(define (scale-xyz xyz factor)
  (xyz-map xyz (λ (value) (* value factor))))

; gen-chunk-faces : Chunk -> [List-of [FaceGroup Position]]
; Generates the faces for each block in a chunk.
#;(define (gen-chunk-faces chunk)
    (local [(define offset (scale-xyz (chunk-pos chunk) 16))
            (define (gen-row-faces row x y z base)
              (if (< x 16)
                  (cons (gen-block-face (first row)
                                        (* (+ x (xyz-x offset)) block-size)
                                        (* (+ y (xyz-y offset)) block-size)
                                        (* (+ z (xyz-z offset)) block-size))
                        (gen-row-faces (rest row) (add1 x) y z base))
                  base))
            (define (gen-layer-faces layer y z base)
              (if (< y 16)
                  (gen-row-faces (first layer) 0 y z (gen-layer-faces (rest layer) (add1 y) z base))
                  base))
            (define (gen-faces layers z)
              (if (< z 16)
                  (gen-layer-faces (first layers) 0 z (gen-faces (rest layers) (add1 z)))
                  '()))]
      (gen-faces (chunk-blocks chunk) 0)))

#;(define (gen-chunk-faces chunk)
    (local [(define (iterate-x tree groups)
              (cond [(leaf? tree) groups]
                    [(node? tree)
                     (local [(define x (node-key tree))]
                       (iterate-x (node-left tree)
                                  (iterate-x (node-right tree)
                                             (iterate-y (node-val tree) groups x))))]))
            (define (iterate-y tree groups x)
              (cond [(leaf? tree) groups]
                    [(node? tree)
                     (local [(define y (node-key tree))]
                       (iterate-y (node-left tree)
                                  (iterate-y (node-right tree)
                                             (iterate-z (node-val tree) groups x y) x) x))]))
            (define (iterate-z tree groups x y)
              (cond [(leaf? tree) groups]
                    [(node? tree)
                     (local [(define z (node-key tree))]
                       (iterate-z (node-left tree)
                                  (iterate-z (node-right tree)
                                             (cons (op (node-val tree)) groups))))]))]
      ()))

(define (gen-chunk-terrain-faces chunk)
  (local [
          (define (generate? type x y z)
            (not (air? type)))
          (define (for-block type x y z)
            (gen-block-exposed-faces chunk type (make-xyz x y z)))]
    (make-chunk (chunk-pos chunk) (chunk-blocks chunk) (chunk-entities chunk)
                (filter-map-over-blocks generate? for-block chunk) (chunk-entity-faces chunk))))

(define (filter-map-over-blocks pred op chunk)
  (local [(define (iterate-x tree groups)
            (cond [(leaf? tree) groups]
                  [(node? tree)
                   (local [(define x (node-key tree))]
                     (iterate-x (node-left tree)
                                (iterate-x (node-right tree)
                                           (iterate-y (node-val tree) groups x))))]))
          (define (iterate-y tree groups x)
            (cond [(leaf? tree) groups]
                  [(node? tree)
                   (local [(define y (node-key tree))]
                     (iterate-y (node-left tree)
                                (iterate-y (node-right tree)
                                           (iterate-z (node-val tree) groups x y) x) x))]))
          (define (iterate-z tree groups x y)
            (cond [(leaf? tree) groups]
                  [(node? tree)
                   (local [(define z (node-key tree))
                           (define type (node-val tree))]
                     (iterate-z (node-left tree)
                                (iterate-z (node-right tree)
                                           (if (pred type x y z)
                                               (cons (op type x y z) groups)
                                               groups) x y) x y))]))]
    (iterate-x (chunk-blocks chunk) '())))

; Linear Congruential Generator - pseudo-randomized number generator
(define (lcg seed n)
  (local [(define a 8)
          (define c 3)
          (define m 23)
          (define (run-sequence n)
            (cond [(zero? n) seed]
                  [else (modulo (+ (* a (run-sequence (sub1 n))) c) m)]))]
    (/ (run-sequence (abs n)) m)))

(define (randint seed n range)
  (- (round (* 2 range (lcg seed n))) range))

(define world-seed 5)

(define (hash-pos pos)
  (+ (* (xyz-x pos) 5) (* (xyz-y pos) 11) (* (xyz-z pos) 2)))

(define (randoffset-pos pos range)
  (local [(define hashed (hash-pos pos))
          (define xo (randint world-seed hashed range))
          (define yo (randint world-seed (sub1 hashed) range))
          (define zo (randint world-seed (add1 hashed) range))]
    (add-xyz pos (make-xyz xo yo zo))))

(define (gen-block-at pos)
  (local [(define chunk-pos (xyz-map pos (λ (n) (floor-div n 16))))
          (define surface-pos (set-z (scale-xyz chunk-pos 16) 1))
          (define corner1 (randoffset-pos surface-pos 7))
          (define corner2 (randoffset-pos (add-x surface-pos 16) 7))
          (define corner3 (randoffset-pos (add-y surface-pos 16) 7))
          (define corner4 (randoffset-pos (add-x (add-y surface-pos 16) 16) 7))
          (define corners (list corner1 corner2 corner3 corner4))
          (define (xy-posn xyz)
            (make-posn (xyz-x xyz) (xyz-y xyz)))
          (define (closer? a b)
            (< (distance (xy-posn pos) (xy-posn a)) (distance (xy-posn pos) (xy-posn b))))
          (define (closest-corner corners closest)
            (cond [(empty? corners) closest]
                  [(cons? corners) (closest-corner (rest corners)
                                                   (if (closer? (first corners) closest)
                                                       (first corners) closest))]))
          (define (weighted-average corners heights distances total)
            (cond [(empty? corners) (foldr (λ (height dist acc) (+ acc (* height (/ dist total)))) 0 heights distances)
                                    ]
                  [(cons? corners) (local [(define corner (first corners))
                                           (define dist (distance (xy-posn pos) (xy-posn corner)))]
                                     (weighted-average (rest corners)
                                                       (cons (xyz-z corner) heights)
                                                       (cons dist distances) (+ total dist)))]))
          (define surface-height (ceiling (weighted-average corners '() '() 0)))
          (define z (xyz-z pos))]
    (cond [(< z surface-height) (if (< z (- surface-height 2))
                                    2 4)]
          [(= z surface-height) (if (< z 2) 3 1)]
          [(> z surface-height) (if (< z 2) 7 0)])
    ))

; gen-chunk : Position [Integer Integer Integer] -> Block
(define (gen-chunk pos block-picker)
  (local [(define blank-chunk (make-chunk pos empty-block-tree '() '() '()))
          (define generated
            (do-for
             0 15
             (λ (x chunk)
               (do-for
                0 15
                (λ (y chunk)
                  (do-for
                   0 15
                   (λ (z chunk)
                     (local [(define local-pos (make-xyz x y z))
                             (define block (block-picker (chunkpos->globalpos chunk local-pos)))]
                       (if (not (air? block))
                           (set-chunk-block chunk local-pos block) chunk)))
                   chunk)) chunk)) blank-chunk))
          (define cache-geometry (gen-chunk-terrain-faces generated))
          (define finished cache-geometry)]
    finished))


(define (chunkpos->globalpos chunk pos)
  (add-xyz (scale-xyz (chunk-pos chunk) 16) pos))

; do-for : {X} Integer Integer [Integer X -> X] X -> X
(define (do-for start end op base)
  (if (<= start end)
      (op start (do-for (add1 start) end op base)) base))

; xyz-angle-diff : XYZ XYZ -> XYZ
; Returns the degree difference in angles represented with the lowest magnitude.
(define (xyz-angle-diff angle-a angle-b)
  (local [(define (diff get-axis)
            (angle-diff (get-axis angle-a) (get-axis angle-b)))]
    (make-xyz (diff xyz-x) (diff xyz-y) (diff xyz-z))))



; mod : Number Number -> Number
; The remainder operation, but functioning for non-integer values.
(define (mod a b)
  (- a (* (floor (/ a b)) b)))

; angle-diff : Number Number -> Number
; Returns the degree difference in angle in the representation with the least absolute value.
(define (angle-diff val-a val-b)
  (local [(define a val-a)
          (define b val-b)
          (define delta (mod (- a b) 360))]
    (if (> delta 180) (- delta 360) delta)))

; xyz-angle-to : Position Position -> Angle
; Finds the 3D angle from one position to another.
(define (xyz-angle-to pos-a pos-b)
  (local [(define dx (- (xyz-x pos-b) (xyz-x pos-a)))
          (define dy (- (xyz-y pos-b) (xyz-y pos-a)))
          (define dz (- (xyz-z pos-b) (xyz-z pos-a)))
          (define xa (deg-atan dz dy))
          (define ya (deg-atan dz dx))
          (define za (deg-atan dy dx))]
    (make-xyz xa ya za)))





(define fov-rad 40)



; screen-pos : Position Vector -> Posn
; Returns the screen position of a 3D point from a given point of view.
(define (screen-pos point player)
  (local [(define da (xyz-angle-diff (entity-angle player)
                                     (xyz-angle-to (entity-pos player) point)))
          (define z-facing (xyz-z (entity-angle player)))
          (define cardinal (* (floor (/ z-facing 90)) 90))
          (define weight (/ (- z-facing cardinal) 90))
          (define d-pitch (angle-diff (pitch (entity-pos player) point) (xyz-x (entity-angle player))))
          (define x (+ (/ width 2) (* (/ (xyz-z da) fov-rad) (/ width 2))))
          (define y (+ (/ height 2) (* (/ d-pitch fov-rad) (/ width 2))))]
    (make-posn x y)))

(define (t da)
  (local [(define z-facing (xyz-z da))
          (define cardinal (* (floor (/ z-facing 90)) 90))
          (define weight (/ (- z-facing cardinal) 90))
          (define vertical-angle-diff (+ (* (abs (deg-sin cardinal)) (- 1 weight) (xyz-y da)) (* (abs (deg-cos cardinal)) weight (xyz-x da))))]
    vertical-angle-diff))



; pitch : Position Position -> Number
; Returns the pitch needed for a vector at one position to point at the other position.
(define (pitch from to)
  (local [(define (xy-posn xyz)
            (make-posn (xyz-x xyz) (xyz-y xyz)))]
    (deg-atan (- (xyz-z from) (xyz-z to))
              (distance (xy-posn from) (xy-posn to)))))

; distance : 
(define (distance from to)
  (local [(define (sqr-diff field-accessor)
            (expt (- (field-accessor from) (field-accessor to)) 2))]
    (sqrt (if (posn? to)
              (+ (sqr-diff posn-x) (sqr-diff posn-y))
              (+ (sqr-diff xyz-x) (sqr-diff xyz-y) (sqr-diff xyz-z))))))

; deg->rad : Number -> Number
; Converts from degrees to radians.
(define (deg->rad a)
  (* (/ a 360) 2 pi))

; rad->deg : Number -> Number
; Converts from radians to degrees.
(define (rad->deg a)
  (* (/ a (* 2 pi)) 360))

; deg-sin : Number -> Number
; Sine for degrees.
(define (deg-sin a)
  (sin (deg->rad a)))

; deg-cos : Number -> Number
; Cosine for degrees.
(define (deg-cos a)
  (cos (deg->rad a)))

; deg-tan : Number -> Number
; Tangent for degrees.
(define (deg-tan a)
  (tan (deg->rad a)))

; deg-atan : Number Number -> Number
; Arctangent for degrees.
(define (deg-atan dx dy)
  (if (= 0 dx dy)
      0
      (rad->deg (atan (deg->rad dx) (deg->rad dy)))))

; find-first : {X} [X -> Boolean] -> [Maybe X]
; Returns the first item in a list that satisfies the predicate, or false if no such item is present.
(define (find-first pred? list)
  (cond [(empty? list) #false]
        [else (if (pred? (first list)) (first list) (find-first pred? (rest list)))]))

; length-at-least? : {X} [List-of X] Natural -> Boolean
; Returns #true if the length of the list is at least a specified number and #false otherwise.
(define (length-at-least? list length)
  (or (= length 0) (and (cons? list) (length-at-least? (rest list) (sub1 length)))))

; sign : Number -> Integer
; Returns 1 if the number is >= 0 and -1 otherwise.
(define (sign n)
  (if (< n 0) -1 1))

(define (xyz->string xyz)
  (local [(define (axis->string axis)
            (local [(define val (axis xyz))]
              (cond [(number? val) (number->string val)]
                    [(string? val) val])))]
    (string-append "(" (axis->string xyz-x) ", " (axis->string xyz-y) ", " (axis->string xyz-z) ")")))

(define (set-dec-precision dec precision)
  (local [(define multiplier (expt 10 precision))]
    (/ (round (* dec multiplier)) multiplier)))

(define (format-number-string num places length)
  (local [(define (pad str n)
            (if (< (string-length str) n)
                (pad (string-append str "0") (sub1 n)) str))]
    (pad (string-append (number->string (set-dec-precision num places)) ".") length)))

(define (render-save save)
  (local [(define cursor-block (f save))
          (define world-render (render-world (save-world save) (save-player save)))
          (define angle (xyz->string (xyz-map (entity-angle (save-player save)) (λ (num) (format-number-string num 3 8)))))
          
          (define indicator (circle 10 "outline" "black"))]
    (if (boolean? cursor-block)
        world-render
        (local [(define sp (screen-pos (scale-xyz cursor-block block-size) (save-player save)))]
          (place-image indicator (posn-x sp) (posn-y sp) (overlay (above (text angle 12 "black") (text (xyz->string cursor-block) 12 "black"))
                                                                  world-render))))))
;(text (number->string (xyz-z (vector-angle (save-cam save)))) 12 "black")
(define (rotate-player save)
  (local [(define vec (save-player save))
          (define rot (entity-angle vec))
          (define new-rot (make-xyz (+ (xyz-x rot) 0) (+ (xyz-y rot) 0) (+ (xyz-z rot) 0)))]
    (set-player-vector save (make-vector (vector-pos vec) new-rot))))

(define (move-player save)
  (local [(define vec (save-player save))
          (define loc (entity-pos vec))
          (define rot (entity-angle vec))
          (define new-loc (make-xyz (xyz-x loc) (+ (xyz-y loc) 0) (+ (xyz-z loc) 0)))
          (define new-rot (make-xyz (+ (xyz-x rot) 0) (+ (xyz-y rot) 0) (+ (xyz-z rot) 1)))]
    (set-player-vector save (make-vector new-loc new-rot))))

(define-struct pair (fst snd))

(define-struct linmap (equal-pred mappings))
; A [Map X Y] is a (make-linmap [X X -> Boolean] [List-of [Pair X Y]]) and represents a mapping of
; unique keys to values.

; map-get : {X Y} [Map X Y] X -> [Maybe Y]
; Returns the value linked to the given key if present in the map, and #false otherwise.
(define (map-get map key)
  (local [(define same-key? (linmap-equal-pred map))
          (define (search mappings)
            (cond [(empty? mappings) #f]
                  [(cons? mappings)
                   (local [(define mapping (first mappings))
                           (define current-key (pair-fst mapping))]
                     (if (same-key? current-key key)
                         (pair-snd mapping) (search (rest mappings))))]))]
    (search (linmap-mappings map))))

(define (map-put map key val)
  (local [(define same-key? (linmap-equal-pred map))
          (define new-mapping (make-pair key val))
          (define (rebuild mappings)
            (cond [(empty? mappings) (cons new-mapping '())]
                  [(cons? mappings)
                   (local [(define mapping (first mappings))
                           (define current-key (pair-fst mapping))]
                     (if (same-key? current-key key)
                         (cons new-mapping (rest mappings))
                         (cons mapping (rebuild (rest mappings)))))]))]
    (make-linmap same-key? (rebuild (linmap-mappings map)))))

(define (map-transform map key op default)
  (local [(define same-key? (linmap-equal-pred map))
          (define (rebuild mappings)
            (cond [(empty? mappings) (cons (make-pair key (op default)) '())]
                  [(cons? mappings)
                   (local [(define mapping (first mappings))
                           (define current-key (pair-fst mapping))]
                     (if (same-key? current-key key)
                         (cons (make-pair key (op (pair-snd mapping))) (rest mappings))
                         (cons mapping (rebuild (rest mappings)))))]))]
    (make-linmap same-key? (rebuild (linmap-mappings map)))))

(define (map-remove map key)
  (local [(define same-key? (linmap-equal-pred map))
          (define (rebuild mappings)
            (cond [(empty? mappings) '()]
                  [(cons? mappings)
                   (local [(define mapping (first mappings))
                           (define current-key (pair-fst mapping))]
                     (if (same-key? current-key key)
                         (rest mappings) (cons mapping (rebuild (rest mappings)))))]))]
    (make-linmap same-key? (rebuild (linmap-mappings map)))))

; fold-over-map : {X Y Z} [Map X Y] [X Y Z -> Z] Z -> Z
(define (fold-over-map map op base)
  (local [(define (fold mappings)
            (cond [(empty? mappings) base]
                  [(cons? mappings)
                   (local [(define mapping (first mappings))]
                     (op (pair-fst mapping) (pair-snd mapping) (fold (rest mappings))))]))]
    (fold (linmap-mappings map))))

(define chunkgen-solid-below (λ (top) (λ (pos) (if (<= (xyz-z pos) top) 1 0))))

(define walk-accel (+ ground-friction .3))
(define walk-speed 1)
(define jump-speed 4)

(define walk-action
  (λ (direction)
    (λ (player)
      (accelerate-horizontally player (+ (horizontal-direction player) direction)
                               walk-accel walk-speed))))

(define to-player
  (λ (action)
    (λ (save)
      (set-save-player save (action (save-player save))))))

(define keymap
  (make-linmap string=?
               (list (make-pair "w" (to-player (walk-action 0)))
                     (make-pair "a" (to-player (walk-action 90)))
                     (make-pair "s" (to-player (walk-action 180)))
                     (make-pair "d" (to-player (walk-action 270)))
                     (make-pair
                      " "
                      (λ (save)
                        (local [(define player (save-player save))]
                          (if (grounded? player (save-world save))
                              (set-save-player
                               save (set-vel player (add-z (entity-vel player) jump-speed)))
                              save))))
                     (make-pair
                      "up"
                      (to-player (λ (player)
                                   (pan-vertically player (- pan-speed)))))
                     (make-pair
                      "down"
                      (to-player(λ (player)
                                  (pan-vertically player pan-speed))))
                     (make-pair
                      "left"
                      (to-player (λ (player)
                                   (pan-horizontally player pan-speed))))
                     (make-pair
                      "right"
                      (to-player (λ (player)
                                   (pan-horizontally player (- pan-speed))))))))

(define (action-logger save key)
  (local [(define (log-action action)
            (make-save (save-player save) (save-world save) (map-put (save-actions save) key action)))
          (define action (map-get keymap key))]
    (if (boolean? action)
        save (log-action action))))

(define (action-ender save key)
  (local [(define (delog-action action)
            (make-save (save-player save) (save-world save) (map-remove (save-actions save) action)))]
    (delog-action key)))

(define (contains? list item)
  (and (cons? list) (or (string=? (first list) item) (contains? (rest list) item))))

(define (set-add list item)
  (if (contains? list item) list (cons item list)))
#;(define (remove list item)
    (cond [(empty? list) '()]
          [(cons? list) (if (equals? (first list) item)
                            (rest list)
                            (cons (first list) (remove (rest list) item)))]))

(define pan-speed 2)

(define (set-save-player save player)
  (make-save player (save-world save) (save-actions save)))

(define (gen-current-chunk save)
  (local [(define block-pos (realpos->blockpos (entity-pos (save-player save))))
          (define chunk-pos (xyz-map block-pos (λ (val) (floor-div val 16))))
          (define world (save-world save))]
    (if (boolean? (chunk-at-block block-pos world))
        (make-save (save-player save) (make-world (cons (gen-chunk chunk-pos gen-block-at)
                                                        (world-chunks world))) (save-actions save))
        save)))
; Actions boolean
(define (process-actions save)
  (local [(define actions (save-actions save))
          (define player (save-player save))
          (define apply-actions
            (fold-over-map
                                 actions (λ (_ do-action old-save) (do-action old-save))
                   (set-save-player save (step-entity-physics (save-player save) (save-world save)))))]
    (gen-current-chunk apply-actions)))

(define (set-collider entity collider)
  (make-entity (entity-id entity) collider (entity-angle entity) (entity-faces entity)))
#;(define (step-player save)
    (local [(define player (save-player save))
            (define new-player (set-collider player (step-collider (entity-hitbox player) (save-world save))))]
      (make-save new-player (save-world save) (save-actions save))))

(define (set-player-vector save vector)
  (local [(define player (save-player save))
          (define collider player)
          (define new-player
            (make-entity 0 (set-pos collider (vector-pos vector))
                         (vector-angle vector) (entity-faces player)))]
    (make-save new-player (save-world save) (save-actions save))))

(define (pan-horizontally entity angle)
  (direct entity (add-z (entity-angle entity) angle)))

(define (pan-vertically entity angle)
  (direct entity (add-x (entity-angle entity) angle)))

(define (horizontal-direction player)
  (xyz-z (entity-angle player)))

(define (shift-horizontally entity distance direction)
  (local [(define dx (* distance (deg-cos direction)))
          (define dy (* distance (deg-sin direction)))]
    (set-pos entity (add-xyz (entity-pos entity) (make-xyz dx dy 0)))))

#;(define chunks (list (gen-chunk 1 (make-xyz 0 -1 0))
                       (gen-chunk 1 (make-xyz -1 -1 0))
                       (gen-chunk 1 (make-xyz -1 0 0))
                       (gen-chunk 1 (make-xyz 0 0 0)) '()))
;(define chunk0 (gen-chunk (make-xyz 0 0 0) (chunkgen-solid-below 0)))
(define chunk0 (gen-chunk (make-xyz 0 -1 0) gen-block-at))
(define chunk1 (set-chunk-block chunk0 (make-xyz 5 5 3) 0))
(define chunk2 (set-chunk-block chunk1 (make-xyz 5 5 2) 0))
(define chunk3 (set-chunk-block chunk2 (make-xyz 5 5 1) 0))
(define chunk4 (set-chunk-block chunk3 (make-xyz 5 5 0) 0))
(define chunks (list (gen-chunk-terrain-faces chunk4)))
(define world0 (make-world chunks))
(define save0 (make-save (direct (set-vel (make-steve (make-xyz 5 -4 9)) (make-xyz 0 0 -.1)) (make-xyz -10 0 0)) world0 (make-linmap string=? '())))
(big-bang save0
    (to-draw render-save)
    (on-key action-logger)
    (on-release action-ender)
    (on-tick process-actions))

(define (s save new-angle)
  (set-save-player save (direct (save-player save) new-angle)))

(define (test angle)
  (local [(define found (f (s save0 angle)))]
    (if (boolean? found)
        #f (xyz-map found (λ (num) (set-dec-precision num 3))))))

(define c (make-collider .3 1 (scale-xyz (make-xyz 5 5 4) block-size) (scale-xyz (make-xyz 0 0 -40) block-size)))
(block-collision-pos (set-vel (make-steve (make-xyz 0 0 2.46)) (make-xyz 0 0 -.02)) world0 solid? xyz-z xyz-x xyz-y)