# Approach

```
for each map, parse intervals and source by source start
bundle ranges as a series of blocks in the form (start, end)
for each mapping step:
   for each block:
       for each interval:
           if i overlap?
              if there an unmapped part i need to get rid of it?
                  dump that part
              construct new block out of overlapping segment
                  dump that part
sort blocks by start index
take smallest block
```
