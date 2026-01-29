set terminal pngcairo size 1400,500 enhanced font 'Verdana,10'
set xrange [0:2.5]
set output 'y-x2.png'
plot 'y-x2.data' u 2:1
