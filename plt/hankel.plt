set yrange [-2:2]
set term png size 1000,600
set output "hankelTwo.png"
plot "massive.txt" using 2:3 w li title "real","massive.txt" using 2:5 w li title "imag"