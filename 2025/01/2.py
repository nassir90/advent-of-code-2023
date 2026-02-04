import sys

DIAL_START = 50
DIAL_MAX = 100

class PartTwo:
    def __init__(self):
        self.dial_state = 50
        self.hits = 0

    def operate(self, direction: int, operation: int):
        assert direction != 0
        virtual_state = (direction * self.dial_state) % DIAL_MAX
        # Introduces a state outside of modular arithmetic that
        # implicitly handles the case where virtual_state is at the
        # edge. Where the dial state is at an edge (the internal dial
        # state can not assume the value DIAL_MAX), the edge is
        # considered to be very far away (at DIAL_MAX) which is what
        # we want. If we are at the edge and not going more than
        # DIAL_MAX steps then we are doing some non-wrapping modular
        # math. If we ARE going beyond 100, we add the wrap event plus
        # the number of modular wraps after the first wrap.
        distance_to_edge = (100 - virtual_state)
        if distance_to_edge <= operation:
            # In this case our ambition to click exceeds the modular
            # cage that binds us. Alas we are ultimately wrapped and
            # constrained.
            self.hits += 1 + (operation - distance_to_edge) // DIAL_MAX
        virtual_state = virtual_state + operation
        self.dial_state = (direction * virtual_state) % DIAL_MAX

part_two = PartTwo()

for line in open(sys.argv[1]):
    line = line.strip()
    operation, count = line[0], int(line[1:])
    direction = -1 if operation == "L" else 1
    part_two.operate(direction, count)
    print(f"state: {part_two.dial_state}, hits: {part_two.hits} ({line})")
