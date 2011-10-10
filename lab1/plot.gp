plot 'times-map' with linespoints, \
     'times-strict-map' with linespoints, \
     'times-sort' with linespoints, \
     'times-py' with linespoints
set xlabel "number of words [1 = ~200k words]"
set ylabel "time [s]"
set terminal png
set output "times.png"
set grid
replot
