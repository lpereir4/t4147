set terminal png size 1280, 640
set output "linear20.png"
set style data lines

set title "Compared cost of X insertion and X removal (Ordered insertion)"
set xlabel "Cardinality"
set ylabel "Time in ms"
set size ratio 0.5
set grid

plot "./linear20" using 1:2 title "scala.collection.immutable.TreeSet", "./linear20" using 1:3 title "mutable TreeSet based on an immutable AVL Tree", "./linear20" using 1:4 title "java.util.TreeSet"
