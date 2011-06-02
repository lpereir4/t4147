set terminal png size 1280, 640
set output "fibo20.png"
set style data lines

set title "Compared cost of X insertion and X removal (Fibonacci numbers)"
set xlabel "Cardinality"
set ylabel "Time in ms"
set size ratio 0.5
set grid

plot "./fibo20" using 1:2 title "scala.collection.immutable.TreeSet", "./fibo20" using 1:3 title "mutable TreeSet based on an immutable AVL Tree", "./fibo20" using 1:4 title "java.util.TreeSet"
