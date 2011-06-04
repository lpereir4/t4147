set terminal png size 1024, 512
set output "insertion.regular.png"
set style data lines

set title "Time cost of X insertion (Ordered insertion)"
set xlabel "Cardinality"
set ylabel "Time in ms"
set size ratio 0.5
set grid

plot "./insertion.regular" using 1:2 title "scala.collection.immutable.TreeSet", "./insertion.regular" using 1:3 title "mutable TreeSet based on an immutable AVL Tree", "./insertion.regular" using 1:4 title "java.util.TreeSet"
