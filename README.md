# Uniprocessor Scheduling Simulator
A basic terminal-based program built in Haskell to simulate process scheduling in a uniprocessor CPU. Made for my Computer Operating Systems course.

### Scheduling
The scheduler takes an input file that is formatted `{arrival_time} {priority} {cpu_burst}` with the first line of the input file being the number of processes. It runs each process in order of priority once it has arrived, and runs round-robin if two processes have equal priority.
