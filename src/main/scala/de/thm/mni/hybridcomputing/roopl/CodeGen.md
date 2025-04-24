// Expressions

// Statements, Method Calls

// Laufzeitrepräsentation von Objekten

Objekt = Tupel von Werten
Run Time Type Information



rel method:


x : List

call x::map :

switch(x.type_id){
    case TYPE_ID_NIL:
        // call nil.map
    case TYPE_ID_CONS:
        // call cons.map
}

(
    (cons.map, cons.sum, cons.fold),
    (:head, :tail)
)

(
    (nil.map, nil.sum, nil.fold)
    ()
)

(10, (1, 2, 3, 4))

m.0 := memory.new 10 := ()
m.5 := memory.new 10 := ()

(m.1, y) := memory.swap i := (m.0, x)

() := ~memory.new 10 := m.1

x := add 1 := x

rel cons.map:
    // TODO

// Vtable als Relation
rel cons.vtable i:
    (),0 := begin<-
    i.1 := dup i := ()
    ->L0, L1, L2 := (), i.1

    (), 0 := L0<-
    res := dup cons.map := ()
    -> L10 := res, 0



    res, i.1 := L10,L11,L12<-
    () := ~dup i := i.1
    ->end := res,0

rel cons.constructor:
    (),0 := begin<-
    o := memory.new 10 := ()
    cons.map.cpy := dup cons.map := ()
    o.1, 0 := memory.rw 0 := o, cons.map
    o.2, 0 := memory.rw 1 := o.1, cons.sum
    o.3, 0 := memory.rw 2 := o.2, cons.fold
    o.4, 0 := memory.rw 3 := o.3, 0 // head
    o.5, 0 := memory.rw 4 := o.4, 0 // tail
    ->end := o.5,0

rel cons.constructor m:
    (),0 := begin<-
    o := memory.dup (m, 11) := ()
    -> end := o, 0






Statement => Set[Block] | Seq[Statement]

[Expression] => (Seq[Assignment], Ident)

x <=> y

y0
x0

x1 := id := y0
y1 := id := x0


(x1, y1) := id := (y0, x0)



V -> (V <-> V)

+:
(0, 1) -> 0, 1
(1, 0) -> 1, 1

<:
(0, 1) -> (0, 1), 1


c := less (x,y) := ()
y := add 5 := x

->L1,L2,L3 := (), 2



















local int x = y * 7
    x += 5
delocal int x = y * 7 / 5




(), 0 := L0<-
x := mul (y, 7) := ()
->L1 := x, 0

x,0 := L1<-
x.1 := add 5 := x
->L2 := x.1,0

x,0 := L2<-
t.1 := mul (y, 7) := ()
t.2 := div (t.1, 5) := ()

() := ~dup t.2 := x

() := ~div (t.1, 5) := t.2
() := ~mul (y, 7) := t.1

-> L8 := (), 0










