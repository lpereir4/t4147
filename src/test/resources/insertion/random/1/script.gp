set terminal png size 1024, 512
set output "insertion.random.png"
set style data lines

set title "Time cost of X insertion"
set xlabel "Cardinality"
set ylabel "Time in ms"
set size ratio 0.5
set grid

plot "./insertion.random" using 1:2 title "scala.collection.immutable.TreeSet", "./insertion.random" using 1:3 title "mutable TreeSet based on an immutable AVL Tree", "./insertion.random" using 1:4 title "java.util.TreeSet"
