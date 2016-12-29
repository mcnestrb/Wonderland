# Wonderland

##### A Monadic Interpreter where you can go through the looking glass for evaluating simple expressions made with Monad Transformers


### Stack Project
It's a stack project that I built and you run the program with `stack ghci`.
You then do `main` followed by either of the test files: `test1.txt` or `test2.txt`.

### Read File and Step Through Program
A file is read in and and the Statement is first read and then if it contains any Seqs, they are removed and it is split up into a list of Statements. Press `n` to step through the program line by line after that. While loops and if statements are executed all in one go. The environment was changed so that for each named variable, a list of values is maintained so that the history of each variable can be maintained. Two lists are also passed through the `step` function, one being the list of statements that will be executed and the other being the list of statements that have been executed already. This was done for the stepping backwards for later.

### Inspect + History
Press `i` to inspect at any time and it gives you a list of the variables available to inspect just to reduce the chance of the user inspecting something that hasn't been declared yet. Then you type in the name and and it shows you the history of the variable in form of a list of values, with the head of the list being the most recent value and the end of the list being the least recent

### Step Backward (Through the Looking Glass)
Press `u` to step backward on assignment or print statements only (I ran out of time to complete the rest of this section). On Assignment statements, the most recent value of the variable is discarded from the list. The head of the list of already executed statements is then added to the list of statements to be executed.

##### I didn't have time to complete the rest of the assignment unfortunately
