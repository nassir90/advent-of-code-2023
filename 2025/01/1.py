# Absolute slop code sorry if you had to read this I've been using
# cursor for weeks atp
DIAL_START = 50
DIAL_MAX = 100

class PartOne:
    def __init__(self):
        self.dial_state = 50
        self.hits = 0

    def operate(self, direction: int, operation: int):
        assert direction != 0
        hits = 0
        if direction > 0:
            distance_to_edge = 100 - self.dial_state
            if distance_to_edge <= operation:
                hits += 1
                self.dial_state += direction * distance_to_edge
                operation -= distance_to_edge
                hits += operation // DIAL_MAX
                self.dial_state = operation % DIAL_MAX
            else:
                self.dial_state += direction * operation
        elif direction < 0:
            distance_to_edge = self.dial_state
            if distance_to_edge <= operation:
                hits += 1
                self.dial_state += direction * distance_to_edge
                operation -= distance_to_edge
                hits += operation // DIAL_MAX
                self.dial_state = DIAL_MAX - operation % DIAL_MAX
            else:
                self.dial_state += direction * operation
        # self.hits += hits
        self.dial_state = self.dial_state % DIAL_MAX
        self.hits += 1 if self.dial_state == 0 else 0
                
    def count(self):
        pass

import sys

part_one = PartOne()

for line in sys.stdin:
    line = line.strip()
    operation, count = line[0], int(line[1:])
    direction = -1 if operation == "L" else 1
    part_one.operate(direction, count)
    print(f"state: {part_one.dial_state}, hitS: {part_one.hits} ({line})")

class PartTwo:
    def __init__(self):
        self.dial_state = 50
        self.hits = 0

    def operate(self, direction: int, operation: int):
        assert direction != 0
        hits = 0
        if direction > 0:
            distance_to_edge = 100 - self.dial_state
            if distance_to_edge <= operation:
                hits += 1
                self.dial_state += direction * distance_to_edge
                operation -= distance_to_edge
                hits += operation // DIAL_MAX
                self.dial_state = operation % DIAL_MAX
            else:
                self.dial_state += direction * operation
        elif direction < 0:
            distance_to_edge = self.dial_state
            if distance_to_edge <= operation:
                hits += 1
                self.dial_state += direction * distance_to_edge
                operation -= distance_to_edge
                hits += operation // DIAL_MAX
                self.dial_state = DIAL_MAX - operation % DIAL_MAX
            else:
                self.dial_state += direction * operation
        # self.hits += hits
        self.dial_state = self.dial_state % DIAL_MAX
        self.hits += 1 if self.dial_state == 0 else 0
