# Thinking

???#????????.???#??. 1,1,5,6

#..#.#####...######. 1,1,5,6
#..#..#####..######. 1,1,5,6
#..#...#####.######. 1,1,5,6
.#.#.#####...######. 1,1,5,6
.#.#..#####..######. 1,1,5,6
.#.#...#####.######. 1,1,5,6
...#.#.#####.######. 1,1,5,6 (REALLY the issue is hashes in the cell area)

.?#???????????? 2,3,1,2,1
.##.###.#.##.#. 2,3,1,2,1
.##.###.#.##..# 2,3,1,2,1
.##.###.#..##.# 2,3,1,2,1
.##.###..#.##.# 2,3,1,2,1
.##..###.#.##.# 2,3,1,2,1 (happy with not one)
..##.###.#.##.# 2,3,1,2,1

?###???????? 3,2,1
.###.##.#... 3,2,1
.###.##..#.. 3,2,1
.###.##...#. 3,2,1
.###.##....# 3,2,1
.###..##.#.. 3,2,1
.###..##..#. 3,2,1
.###..##...# 3,2,1
.###...##.#. 3,2,1
.###...##..# 3,2,1
.###....##.# 3,2,1

###.|....###
###....|.###

###.|....###
###....|.###

Maybe if a node could only define the amount of space to the right of
it? Though that breaks my main optimisation (?). How to disambiguate /
make it not matteer Maybe.. If I start with them all valid? And then
shift them around? I think this is the solution.

# New Strat

```
start with all blocks positioned as far left as possible.
discover all possibilities starting with the final node.

for each node
    for each character index
    add to node-array if it can go there

for each node
    for each character index starting with the lowst
        place here if it is not occluded by another node
        otherwise move to the next character index

recursion with the global state being the node-table
provided with the locations of higher nodes
    base case
        this is given by the number of node positions from the first to the last
        return the positions you are able to assume using the node table
    return the combinations of positions lower nodes can assume with you here
        is does combinations{"node-start"} already exist?
           return that
        otherwise compute recurse on lower nodes with the remaining space after assuming each of your positions
        for every position, cache the value combinations{"node-start"}
```
