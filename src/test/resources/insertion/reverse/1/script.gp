set terminal png size 1024, 512
set output "insertion.reverse.png"
set style data lines

set title "Time cost of X insertion (Inverted insertion)"
set xlabel "Cardinality"
set ylabel "Time in ms"
set size ratio 0.5
set grid

plot "./insertion.reverse" using 1:2 title "scala.collection.immutable.TreeSet", "./insertion.reverse" using 1:3 title "mutable TreeSet based on an immutable AVL Tree", "./insertion.reverse" using 1:4 title "java.util.TreeSet"
