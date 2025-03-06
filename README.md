# Uniprocessor Scheduling Simulator
A basic terminal-based program built in Haskell to simulate process scheduling in a uniprocessor CPU. Made for my Computer Operating Systems course to gain a better understanding of CPU scheduling and to learn Haskell.

### Scheduling
The scheduler takes an input file that is formatted `{arrival_time} {priority} {cpu_burst}` with the first line of the input file being the number of processes. It runs each process in order of priority once it has arrived, and runs round-robin if two processes have equal priority.

### Issues/Future Features
A major issue that has yet to be corrected is that the scheduler messes up when more than two processes have equal priority. 
Future features will include fixing issues and creating a GUI window that shows that ready queue and output in a more readable format.
