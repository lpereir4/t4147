set terminal png size 1024, 512
set output "removal.regular.png"
set style data lines

set title "Time cost of emptying a set of X elements"
set xlabel "Cardinality"
set ylabel "Time in ms"
set size ratio 0.5
set grid

plot "./removal.regular" using 1:2 title "scala.collection.immutable.TreeSet", "./removal.regular" using 1:3 title "mutable TreeSet based on an immutable AVL Tree", "./removal.regular" using 1:4 title "java.util.TreeSet"
