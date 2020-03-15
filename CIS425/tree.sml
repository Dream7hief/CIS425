datatype 'a tree = LEAF of 'a | NODE of 'a tree * 'a tree

fun maptree f (LEAF x) = LEAF (f x)
  | maptree f (NODE(l,r)) = NODE(maptree f l, maptree f r)

fun reducetree f (LEAF x) = x
  | reducetree f (NODE(l,r)) = f(reducetree f l, reducetree f r)

fun curry f = fn x => fn y => f (x,y)
fun uncurry f = fn (x,y) => f x y

fun curry2 f x y = f (x,y)
fun uncurry2 f (x,y) = f x y
